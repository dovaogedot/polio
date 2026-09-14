package polio

import cats.effect.IO
import cats.syntax.all.*
import fs2.io.file.Path

/** The data layout without POLIO_HOME: the XDG base directories, and the copy of a clone left in ~/.polio. */
object LayoutSuite extends SandboxSuite {

  private val rc = ".bashrc"

  extension (sb: Sandbox) {

    /** The data root under the sandbox XDG_DATA_HOME. */
    private def xdgData: Path = sb.root / "xdg-data/polio"

    /** Binds and tracks .bashrc with POLIO_HOME at ~/.polio, so that directory holds a clone and a state file. */
    private def oldClone: IO[Unit] = {
      val env = Map("POLIO_HOME" -> (sb.home / ".polio").toString)
      sb.exec(sb.bin.toString, List("bind", sb.remote.toString), extra = env)
        *> sb.host(rc).writeText("x\n")
        *> sb.exec(sb.bin.toString, List("add", sb.host(rc).toString), extra = env).void
    }
  }

  sandboxed("without POLIO_HOME the clone goes to XDG_DATA_HOME and the state to XDG_STATE_HOME") { sb =>
    for
      _     <- sb.tryXdgPolio("bind", sb.remote)
      _     <- sb.host(rc).writeText("x\n")
      _     <- sb.tryXdgPolio("add", sb.host(rc))
      clone <- (sb.xdgData / "repo/.git").isPresent
      state <- (sb.root / "xdg-state/polio/state.json").isPresent
      home  <- (sb.home / ".polio").isPresent
    yield
      check(clone, "no clone under XDG_DATA_HOME")
        && check(state, "no state under XDG_STATE_HOME")
        && check(!home, "~/.polio was created")
  }

  sandboxed("a clone in ~/.polio is copied to the XDG directories with a warning; the old directory stays") { sb =>
    for
      _     <- sb.oldClone
      run   <- sb.tryXdgPolio("status")
      clone <- (sb.xdgData / "repo/.git").isPresent
      state <- (sb.root / "xdg-state/polio/state.json").isPresent
      stray <- (sb.xdgData / "state.json").isPresent
      old   <- (sb.home / ".polio/repo/.git").isPresent
      again <- sb.tryXdgPolio("status")
    yield
      check(run.code == 0, s"status failed:\n${run.err}")
        && run.err.has(s"${sb.home / ".polio"} is no longer used")
        && run.err.has(s"rm -rf ${sb.home / ".polio"}")
        && run.out.hasRow("up to date", s"~/$rc")
        && check(clone, "no clone under XDG_DATA_HOME")
        && check(state, "no state under XDG_STATE_HOME")
        && check(!stray, "state.json left in the data root")
        && check(old, "~/.polio was removed")
        && again.err.lacks("no longer used")
  }

  sandboxed("a ~/.polio without a clone is left alone") { sb =>
    for
      _   <- (sb.home / ".polio/repo").ensureDir
      run <- sb.tryXdgPolio("status")
    yield
      run.err.has("not bound") && run.err.lacks("mv ")
  }
}
