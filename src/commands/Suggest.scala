package polio

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import fs2.io.file.Path
import java.lang.ProcessBuilder.Redirect
import java.nio.charset.StandardCharsets.UTF_8

/** Config files and directories that many machines have, relative to home. Only the ones that exist are offered. */
private val commonConfigs: List[String] = List(
  ".bashrc",
  ".bash_profile",
  ".profile",
  ".zshrc",
  ".zprofile",
  ".zshenv",
  ".p10k.zsh",
  ".config/fish/config.fish",
  ".config/starship.toml",
  ".inputrc",
  ".gitconfig",
  ".config/git/config",
  ".config/git/ignore",
  ".vimrc",
  ".vim/vimrc",
  ".ideavimrc",
  ".config/nvim",
  ".config/helix",
  ".config/zed/settings.json",
  ".config/Code/User/settings.json",
  ".config/Code/User/keybindings.json",
  ".tmux.conf",
  ".config/tmux/tmux.conf",
  ".config/zellij",
  ".config/alacritty",
  ".config/kitty",
  ".config/wezterm",
  ".wezterm.lua",
  ".ssh/config",
  ".config/gh/config.yml",
  ".config/lazygit",
  ".config/bat",
  ".config/htop/htoprc",
  ".config/atuin/config.toml",
  ".config/mise/config.toml",
  ".tool-versions",
  ".cargo/config.toml",
  ".config/pip/pip.conf",
  ".editorconfig",
  ".curlrc",
  ".wgetrc",
  ".Xresources",
  ".xinitrc",
  ".config/i3/config",
  ".config/sway/config",
  ".config/hypr",
  ".config/waybar",
  ".config/gtk-3.0/settings.ini",
  ".config/fontconfig",
  ".config/mpv",
  ".config/ranger",
  ".config/yazi",
  ".claude/CLAUDE.md",
  ".claude/settings.json",
)

/** The escape character that starts every terminal control sequence. */
private val esc: Char = 27.toChar

/** One offered config: where it is, whether the manifest tracks it, and whether the user wants it tracked. */
private final case class Pick(target: Target, path: Path, tracked: Boolean, selected: Boolean) {

  /** The mark before the name: empty, or full in green for tracked, or full in red for tracked and dropped. */
  def mark(color: Boolean): String = (tracked, selected) match
    case (_, true)      => if color then Ansi.paint("●", Tone.Good.code, true) else "[+]"
    case (true, false)  => if color then Ansi.paint("●", Tone.Bad.code, true) else "[-]"
    case (false, false) => if color then "○" else "[ ]"

  /** The same pick with the selection flipped. */
  def toggled: Pick = copy(selected = !selected)
}

/** What one key press asks the picker to do. */
private enum Key {

  /** Move the cursor up. */
  case Up

  /** Move the cursor down. */
  case Down

  /** Flip the selection under the cursor. */
  case Toggle

  /** Apply the selection. */
  case Confirm

  /** Leave without changes. */
  case Quit

  /** A key the picker ignores. */
  case Other
}

/** The terminal in raw mode: keys arrive one at a time without echo, and the cursor is hidden. Everything comes back on release. */
private object RawMode {

  /** Runs stty on the terminal of this process and returns what it printed. */
  private def stty(args: String*): IO[String] =
    IO.blocking {
      val pb = new ProcessBuilder(("stty" +: args)*)
      pb.redirectInput(Redirect.INHERIT)
      pb.redirectError(Redirect.INHERIT)
      val process = pb.start
      val printed = new String(process.getInputStream.readAllBytes, UTF_8).trim
      process.waitFor
      printed
    }.orIoError("stty", args.mkString(" "))

  private def control(sequence: String): IO[Unit] = IO.blocking(System.err.print(s"$esc[$sequence"))

  /** Raw mode for as long as the resource is held. */
  val held: Resource[IO, Unit] = {
    val enter = stty("-g") <* stty("-icanon", "-echo") <* control("?25l")
    Resource.make(enter)(saved => control("?25h") *> stty(saved).void).void
  }

  /** Reads one key. Arrow keys arrive as escape sequences; the end of input counts as quit. */
  val readKey: IO[Key] = IO.blocking {
    Stdin.readByte match
      case -1 | 'q' => Key.Quit
      case ' '      => Key.Toggle
      case 10 | 13  => Key.Confirm
      case 'k'      => Key.Up
      case 'j'      => Key.Down
      case 27       =>
        if Stdin.readByte != '['
        then Key.Quit
        else
          Stdin.readByte match
            case 'A' => Key.Up
            case 'B' => Key.Down
            case _   => Key.Other
      case _ => Key.Other
  }
}

/** The list as drawn: one line per pick with the cursor marker, then the key help. */
private def draw(picks: Vector[Pick], cursor: Int, color: Boolean): String = {
  val lines = picks.zipWithIndex.map: (pick, i) =>
    val pointer = if i == cursor then ">" else " "
    s"$pointer ${pick.mark(color)} ${pick.target.value}"
  val help = Ansi.paint("↑/↓ move   space toggle   enter apply   q cancel", Ansi.dim, color)
  (lines :+ help).mkString("", "\n", "\n")
}

/** Runs the picker until enter or quit. Returns the picks as the user left them, or None on quit. */
private def choose(picks: Vector[Pick], cursor: Int, color: Boolean): IO[Option[Vector[Pick]]] = {
  val show   = IO.blocking(System.err.print(draw(picks, cursor, color)))
  val redraw = IO.blocking(System.err.print(s"$esc[${picks.length + 1}A"))
  val again  = (next: Vector[Pick], at: Int) => redraw *> choose(next, at, color)
  show *> RawMode.readKey.flatMap:
    case Key.Up      => again(picks, (cursor - 1).max(0))
    case Key.Down    => again(picks, (cursor + 1).min(picks.length - 1))
    case Key.Toggle  => again(picks.updated(cursor, picks(cursor).toggled), cursor)
    case Key.Other   => again(picks, cursor)
    case Key.Confirm => IO.pure(Some(picks))
    case Key.Quit    => IO.pure(None)
}

/** The common configs that exist on this host, marked as the manifest tracks them. */
private def candidates(layout: Layout, manifest: Manifest): IO[Vector[Pick]] =
  commonConfigs.toVector.filterA(rel => layout.home.resolve(rel).isPresent).map: present =>
    present.map { rel =>
      val path    = layout.home.resolve(rel)
      val target  = Target.contract(path, layout.home)
      val tracked = manifest.files.values.exists(_.within(target))
      Pick(target, path, tracked, tracked)
    }

/** Tracks the newly selected picks and untracks the dropped ones. Returns the combined report. */
private def applyPicks(picks: Vector[Pick]): IO[Report] = {
  val added   = picks.filter(p => p.selected && !p.tracked).toList
  val dropped = picks.filter(p => p.tracked && !p.selected).toList
  for
    adds  <- added.traverse(p => add(p.path.toString))
    drops <- dropped.traverse(p => remove(p.path.toString))
  yield
    val reports = adds ::: drops
    val notes   = if reports.isEmpty then List("nothing changed") else reports.flatMap(_.notes).distinct
    Report(reports.flatMap(_.rows), notes)
}

/**
 * polio suggest: offers the common config files found on this host in a list. Arrows or j/k move,
 * space marks a file to track or untrack, enter applies, q leaves everything as it is. Needs a
 * terminal.
 */
def suggest(style: Style): IO[Report] =
  for
    interactive <- Stdin.isTerminal
    _           <- IO.raiseUnless(interactive)(PolioError.Usage("suggest needs a terminal; run: polio add <path>"))
    layout      <- Layout.resolve
    _           <- layout.requireBound
    manifest    <- Manifest.load(layout)
    picks       <- candidates(layout, manifest)

    chosen <-
      if picks.isEmpty
      then IO.pure(None)
      else RawMode.held.surround(choose(picks, 0, style.color))

    report <- chosen match
      case Some(picked)          => applyPicks(picked)
      case None if picks.isEmpty =>
        IO.pure(Report.notes("no common config files found; polio add <path> tracks any file"))
      case None => IO.pure(Report.notes("nothing changed"))
  yield report
