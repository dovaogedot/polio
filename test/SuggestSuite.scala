package polio

import cats.effect.IO
import cats.syntax.all.*

/** polio suggest: the picker over common config files, driven by keys on a pseudo-terminal. */
object SuggestSuite extends SandboxSuite {

  private val down = 27.toChar.toString + "[B"

  /** Binds and creates .bashrc and .gitconfig, the first two common configs in list order. */
  private def prepared(sb: Sandbox): IO[Unit] =
    sb.polio("bind", sb.remote)
      *> sb.host(".bashrc").writeText("b\n")
      *> sb.host(".gitconfig").writeText("g\n")

  sandboxed("space tracks the file under the cursor; the arrow moves the cursor") { sb =>
    for
      _       <- prepared(sb)
      out     <- sb.onTty("suggest", s"$down \r")
      bashrc  <- sb.repoCopy(".bashrc").isPresent
      gitconf <- sb.repoCopy(".gitconfig").isPresent
    yield
      out.report.hasRow("tracking", "~/.gitconfig")
        && check(gitconf, "~/.gitconfig was not tracked")
        && check(!bashrc, "~/.bashrc was tracked without being selected")
  }

  sandboxed("deselecting a tracked file untracks it; q changes nothing") { sb =>
    for
      _     <- prepared(sb)
      _     <- sb.polio("add", sb.host(".bashrc"))
      kept  <- sb.onTty("suggest", " q")
      still <- sb.repoCopy(".bashrc").isPresent
      out   <- sb.onTty("suggest", " \r")
      gone  <- sb.repoCopy(".bashrc").isPresent
    yield
      kept.has("nothing changed")
        && check(still, "q untracked ~/.bashrc")
        && out.report.hasRow("untracked", "~/.bashrc")
        && check(!gone, "~/.bashrc is still tracked")
  }

  sandboxed("marks: [ ] for untracked, [+] for selected, [-] for dropped, without color") { sb =>
    for
      _   <- prepared(sb)
      _   <- sb.polio("add", sb.host(".bashrc"))
      out <- sb.onTty("suggest --no-color", " q")
    yield
      out.has("[+] ~/.bashrc") && out.has("[ ] ~/.gitconfig") && out.has("[-] ~/.bashrc")
  }

  sandboxed("suggest without a terminal fails and names add") { sb =>
    sb.tryPolio("suggest").map: run =>
      check(run.code != 0, "suggest succeeded without a terminal") && run.err.has("polio add <path>")
  }

  extension (out: String) {

    /** The text after the last help line of the picker, where the report starts. */
    private def report: String = {
      val at = out.lastIndexOf("q cancel")
      if at < 0 then out else out.substring(at)
    }
  }
}
