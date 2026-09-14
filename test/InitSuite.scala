package polio

import cats.effect.IO
import cats.syntax.all.*

/** polio init: the guided setup on a terminal. The sandbox shell is bash, so the profile is .bashrc. */
object InitSuite extends SandboxSuite {

  private val profile = ".bashrc"

  extension (out: String) {

    /** The text after the last prompt. On a pseudo-terminal the typed answers echo early, so the report starts on the prompt's line. */
    private def report: String = out.substring(out.lastIndexOf("[y/N] ") + "[y/N] ".length)
  }

  sandboxed("init binds the remote, adds the startup sync to the profile and prints the next steps") { sb =>
    for
      out   <- sb.onTty("init", s"${sb.remote}\ny\n")
      shell <- sb.host(profile).readText
      bound <- (sb.repo / ".git").isPresent
    yield
      check(bound, "no clone after init")
        && out.report.hasRow("bound", sb.remote.toString)
        && out.has("polio add ~/.bashrc")
        && shell.has("polio sync -q")
  }

  sandboxed("a second init keeps the remote and adds the profile line once") { sb =>
    for
      _     <- sb.onTty("init", s"${sb.remote}\ny\n")
      out   <- sb.onTty("init", "\nn\n")
      shell <- sb.host(profile).readText
    yield
      out.report.hasRow("bound", sb.remote.toString)
        && check(shell.split("\n").count(_.contains("polio sync")) == 1, s"profile:\n$shell")
  }

  sandboxed("init without a terminal fails and names bind") { sb =>
    sb.tryPolio("init").map: run =>
      check(run.code != 0, "init succeeded without a terminal") && run.err.has("polio bind <repo>")
  }
}
