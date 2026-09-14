package polio

import cats.effect.IO
import cats.syntax.all.*
import fs2.io.file.Path

/** The line a shell profile runs to sync on every shell start. It does nothing when polio is not installed. */
private val startupLine = "command -v polio >/dev/null && polio sync -q  # polio: sync config files on shell start"

/** The commands to use after the setup. */
private val quickStart =
  """|next:
     |  polio add ~/.bashrc     track a file; a directory tracks every file inside
     |  polio sync              pull, reconcile with this host, push
     |  polio status            every tracked file and what sync would do
     |  polio remove ~/.bashrc  stop tracking; the host copy stays
     |on another machine: polio init, then polio sync""".stripMargin

/** Printed before the shell start question: the sync runs in the foreground, so every new shell waits for it. */
private val blockingNote =
  "note: the sync runs before the prompt appears, so every new shell waits for the pull; a slow or unreachable"
    + " remote makes the wait long, and a file changed on both sides opens the conflict menu in the new shell."

/** Printed when the remote asks for a login, because a sync on shell start then prompts on every new shell. */
private val loginNote =
  "note: an https remote asks for a login on every sync. For a sync on shell start, use an ssh remote (git@host:path) with a key."

/** The profile of the shell named by SHELL. None for a shell without a known profile. */
private def shellProfile(home: Path): Option[Path] =
  envGet("SHELL").map(_.split('/').last).collect:
    case "zsh"  => home / ".zshrc"
    case "bash" => home / ".bashrc"
    case "fish" => home / ".config/fish/config.fish"

/** Asks for the remote until one is typed. An empty answer keeps current. The end of input is a usage error. */
private def askRemote(current: Option[String]): IO[String] = {
  val hint = current.fold("")(url => s" [$url]")
  val none = IO.raiseError(PolioError.Usage("init: no remote given; run: polio bind <repo>"))
  Stdin.ask(s"git remote that stores the config files$hint: ").flatMap:
    case None      => none
    case Some("")  => current.fold(askRemote(current))(IO.pure)
    case Some(url) => IO.pure(url)
}

extension (profile: Path) {

  /** Adds the startup line to the profile. A profile that already runs polio sync is left alone. */
  private def enableStartupSync: IO[Unit] =
    profile.readTextIfExists.flatMap { content =>
      val text    = content.getOrElse("")
      val newline = if text.isEmpty || text.endsWith("\n") then "" else "\n"
      profile.writeText(s"$text$newline$startupLine\n").unlessA(text.contains("polio sync"))
    }
}

/** Offers a sync on every shell start and applies the answer. Returns the line that reports the outcome. */
private def offerStartupSync(layout: Layout, url: String): IO[String] = {
  val login = url.startsWith("http://") || url.startsWith("https://")
  val notes = IO.blocking {
    System.err.println(blockingNote)
    if login then System.err.println(loginNote)
  }
  shellProfile(layout.home) match
    case None          => IO.pure(s"unknown shell; to sync on shell start, add to your profile:\n  $startupLine")
    case Some(profile) =>
      val shown  = layout.display(profile)
      val yes    = Stdin.confirm(s"run polio sync when a shell starts? adds one line to $shown [y/N] ", default = false)
      val enable = profile.enableStartupSync.as(s"sync on shell start: added to $shown")
      notes *> yes.ifM(enable, IO.pure("sync on shell start: skipped"))
}

/**
 * polio init: guided setup on a terminal. Asks for the remote and binds to it, offers the common config
 * files found on this host, offers a sync on every shell start, and reports the commands to use next. Without a terminal it fails and names bind.
 */
def init(style: Style): IO[Report] =
  for
    interactive <- Stdin.isTerminal
    _           <- IO.raiseUnless(interactive)(PolioError.Usage("init needs a terminal; run: polio bind <repo>"))
    layout      <- Layout.resolve
    current     <- Git.in(layout.repo).originUrl.redeem(_ => None, Some(_))
    url         <- askRemote(current)
    bound       <- bind(url)
    picked      <- suggest(style)
    startup     <- offerStartupSync(layout, url)
  yield Report(bound.rows ::: picked.rows, bound.notes ::: picked.notes ::: List(startup, quickStart))
