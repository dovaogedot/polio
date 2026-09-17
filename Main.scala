package polio

import cats.effect.std.Console
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import com.monovore.decline.{Command, Opts}
import java.io.{OutputStream, PrintStream}

/** The version that polio prints. */
val VERSION = "0.5.0"

/** The action that one command line asks for. */
private enum Action {

  /** polio init: guided setup on a terminal. */
  case Init

  /** polio suggest: pick common config files to track from a list. */
  case Suggest

  /** polio bind: connect this host to the remote at url. */
  case Bind(url: String)

  /** polio sync: mode says how to handle a file that changed on both sides. */
  case DoSync(mode: ConflictMode)

  /** polio sync --abort: discard every parked conflict. */
  case AbortSync

  /** polio status: report every tracked file. */
  case ShowStatus

  /** polio add: track the file or directory at path. */
  case Add(path: String)

  /** polio remove: stop tracking the file or directory at path. */
  case Remove(path: String)

  /** polio updates: setting turns the check for a newer polio on or off; without one it is reported. */
  case Updates(setting: Option[String])
}

private val initCommand: Opts[Action] =
  Opts.subcommand("init", "guided setup: choose the remote, pick common config files, offer a sync on shell start"):
    Opts(Action.Init)

private val suggestCommand: Opts[Action] =
  Opts.subcommand("suggest", "pick common config files to track from a list"):
    Opts(Action.Suggest)

private val bindCommand: Opts[Action] =
  Opts.subcommand("bind", "set the git remote that stores the config files"):
    Opts.argument[String]("repo").map(Action.Bind(_))

private val syncCommand: Opts[Action] =
  Opts.subcommand("sync", "pull, reconcile every tracked file with this host, push"):
    val force = Opts
      .flag("force", "keep the host copy when both sides changed", short = "f")
      .as(Action.DoSync(ConflictMode.Force))
    val yolo = Opts
      .flag("yolo", "keep the repo copy when both sides changed", short = "y")
      .as(Action.DoSync(ConflictMode.Yolo))
    val abort = Opts
      .flag("abort", "discard parked conflicts; both sides stay as they are")
      .as(Action.AbortSync)
    val chosen = force <+> yolo <+> abort
    chosen.withDefault(Action.DoSync(ConflictMode.Ask))

private val statusCommand: Opts[Action] =
  Opts.subcommand("status", "show every tracked file and what polio sync would do"):
    Opts(Action.ShowStatus)

private val addCommand: Opts[Action] =
  Opts.subcommand("add", "track a file, or every file inside a directory"):
    Opts.argument[String]("path").map(Action.Add(_))

private val removeCommand: Opts[Action] =
  Opts.subcommand("remove", "stop tracking a file or directory (host copies stay)"):
    Opts.argument[String]("path").map(Action.Remove(_))

private val updatesCommand: Opts[Action] =
  Opts.subcommand("updates", "turn the check for a newer polio on or off, or show the setting"):
    Opts.argument[String]("on|off").orNone.map(Action.Updates(_))

/**
 * The flags every command takes. run removes them from the command line before the parser sees it, so
 * here they only describe themselves in the help text.
 */
private val globalOptions: Opts[Unit] = {
  val quiet     = Opts.flag("quiet", "print nothing on stdout", short = "q").orFalse
  val shush     = Opts.flag("shush", "print nothing on stdout or stderr", short = "s").orFalse
  val porcelain = Opts
    .flag(
      "porcelain",
      "output for scripts: two-letter codes like git status --short, repo side then host side."
        + " M changed, A new, D gone, UU conflict, UH/UR conflict with the host/repo copy kept, PP parked, RR resolved,"
        + " -- untracked, ++ tracking, == already tracked. Up-to-date files are left out; notes start with #",
    )
    .orFalse
  val noColor = Opts.flag("no-color", "no colors; also off when stdout is not a terminal or NO_COLOR is set").orFalse
  (quiet, shush, porcelain, noColor).tupled.void
}

/** The parser for the full command line: polio with all its subcommands. */
private val command: Command[Action] = {
  val actions =
    initCommand
      <+> suggestCommand
      <+> bindCommand
      <+> syncCommand
      <+> statusCommand
      <+> addCommand
      <+> removeCommand
      <+> updatesCommand
  Command(
    name = "polio",
    header =
      s"polio $VERSION — sync config files across hosts through a git repo. Data lives in ~/.local/share/polio and"
        + " ~/.local/state/polio; POLIO_HOME puts both in one place.",
  )(globalOptions *> actions)
}

/** Replaces the chosen output streams with a sink that drops everything written to it. */
private def silence(out: Boolean, err: Boolean): IO[Unit] = IO.blocking {
  val sink = PrintStream(OutputStream.nullOutputStream)
  if out then System.setOut(sink)
  if err then System.setErr(sink)
}

/** Entry point. Parses the command line, runs the action, and prints the result or the error. */
object Main extends IOApp {
  private def execute(action: Action, style: Style): IO[ExitCode] = {
    val program: IO[Report] = action match
      case Action.Init         => init(style)
      case Action.Suggest      => suggest(style)
      case Action.Bind(url)    => bind(url)
      case Action.DoSync(mode) => sync(mode)
      case Action.AbortSync    => syncAbort
      case Action.ShowStatus   => status
      case Action.Add(path)    => add(path)
      case Action.Remove(path) => remove(path)
      case Action.Updates(set) => updates(set)
    program.attemptNarrow[PolioError].flatMap:
      case Right(report) =>
        val text = report.render(style)
        IO.println(text).whenA(text.nonEmpty).as(ExitCode.Success)
      case Left(error) =>
        val text = Ansi.paint("polio:", Tone.Bad.code, style.color) + error.render.stripPrefix("polio:")
        Console[IO].errorln(text).as(ExitCode.Error)
  }

  /** The flags that apply to every command and are taken out before the subcommand parser runs. */
  private val globalFlags = Set("-q", "--quiet", "-s", "--shush", "--porcelain", "--no-color")

  /**
   * Handles the global flags, then runs the rest of the command line. Colors are on when stdout is a
   * terminal, unless --no-color, --porcelain or a non-empty NO_COLOR turns them off.
   */
  def run(args: List[String]): IO[ExitCode] = {
    val quiet     = args.exists(arg => arg == "-q" || arg == "--quiet")
    val shush     = args.exists(arg => arg == "-s" || arg == "--shush")
    val porcelain = args.contains("--porcelain")
    val noColor   = args.contains("--no-color") || envGet("NO_COLOR").exists(_.nonEmpty)
    val rest      = args.filterNot(globalFlags)
    val style     = Stdin.isTty(1).map: tty =>
      Style(color = tty && !noColor && !porcelain, porcelain = porcelain)
    silence(quiet || shush, shush) *> style.flatMap(dispatch(rest, _))
  }

  private def dispatch(args: List[String], style: Style): IO[ExitCode] = args match
    case ("version" | "--version" | "-V") :: Nil => IO.println(s"polio $VERSION").as(ExitCode.Success)
    case _                                       =>
      val argv = args match
        case Nil                     => List("--help")
        case ("help" | "-h") :: rest => "--help" :: rest
        case other                   => other
      command.parse(argv, sys.env) match
        case Left(help) if help.errors.isEmpty => IO.println(help.toString).as(ExitCode.Success)
        case Left(help)                        => Console[IO].errorln(help.toString).as(ExitCode.Error)
        case Right(action)                     => execute(action, style)
}
