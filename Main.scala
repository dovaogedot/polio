package polio

import cats.effect.std.Console
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import com.monovore.decline.{Command, Opts}
import java.io.{OutputStream, PrintStream}

/** The version that polio prints. */
val VERSION = "0.2.0"

/** The action that one command line asks for. */
private enum Action {

  /** polio init: guided setup on a terminal. */
  case Init

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
}

private val initCommand: Opts[Action] =
  Opts.subcommand("init", "guided setup: choose the remote, offer a sync on shell start, show the next steps"):
    Opts(Action.Init)

private val bindCommand: Opts[Action] =
  Opts.subcommand("bind", "set the git remote that stores the config files"):
    Opts.argument[String]("repo").map(Action.Bind(_))

private val syncCommand: Opts[Action] =
  Opts.subcommand("sync", "pull, reconcile every tracked file with this host, push"):
    val force = Opts
      .flag("force", "keep the host copy when both sides changed", short = "f")
      .as(Action.DoSync(ConflictMode.Force))
    val abort = Opts
      .flag("abort", "discard parked conflicts; both sides stay as they are")
      .as(Action.AbortSync)
    val chosen = force <+> abort
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

/** The parser for the full command line: polio with all its subcommands. */
private val command: Command[Action] = {
  val actions =
    initCommand
      <+> bindCommand
      <+> syncCommand
      <+> statusCommand
      <+> addCommand
      <+> removeCommand
  Command(
    name = "polio",
    header =
      s"polio $VERSION — sync config files across hosts through a git repo; data lives in ~/.polio"
        + " (override with POLIO_HOME); -q/--quiet silences stdout, -s/--shush also stderr",
  )(actions)
}

/** Replaces the chosen output streams with a sink that drops everything written to it. */
private def silence(out: Boolean, err: Boolean): IO[Unit] = IO.blocking {
  val sink = PrintStream(OutputStream.nullOutputStream)
  if out then System.setOut(sink)
  if err then System.setErr(sink)
}

/** Entry point. Parses the command line, runs the action, and prints the result or the error. */
object Main extends IOApp {
  private def execute(action: Action): IO[ExitCode] = {
    val program: IO[String] = action match
      case Action.Init         => init
      case Action.Bind(url)    => bind(url)
      case Action.DoSync(mode) => sync(mode)
      case Action.AbortSync    => syncAbort
      case Action.ShowStatus   => status
      case Action.Add(path)    => add(path)
      case Action.Remove(path) => remove(path)
    program.attemptNarrow[PolioError].flatMap:
      case Right(message) => IO.println(message).whenA(message.nonEmpty).as(ExitCode.Success)
      case Left(error)    => Console[IO].errorln(error.render).as(ExitCode.Error)
  }

  /** Handles the -q and -s flags, then runs the rest of the command line. */
  def run(args: List[String]): IO[ExitCode] = {
    val quiet = args.exists(arg => arg == "-q" || arg == "--quiet")
    val shush = args.exists(arg => arg == "-s" || arg == "--shush")
    val rest  = args.filterNot(arg => arg == "-q" || arg == "--quiet" || arg == "-s" || arg == "--shush")
    silence(quiet || shush, shush) *> dispatch(rest)
  }

  private def dispatch(args: List[String]): IO[ExitCode] = args match
    case ("version" | "--version" | "-V") :: Nil => IO.println(s"polio $VERSION").as(ExitCode.Success)
    case _                                       =>
      val argv = args match
        case Nil                     => List("--help")
        case ("help" | "-h") :: rest => "--help" :: rest
        case other                   => other
      command.parse(argv, sys.env) match
        case Left(help) if help.errors.isEmpty => IO.println(help.toString).as(ExitCode.Success)
        case Left(help)                        => Console[IO].errorln(help.toString).as(ExitCode.Error)
        case Right(action)                     => execute(action)
}
