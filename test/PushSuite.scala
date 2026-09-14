package polio

import cats.effect.IO
import cats.syntax.all.*

/** What a sync pushes: the local commits are folded into one, and cancelling changes push nothing. */
object PushSuite extends SandboxSuite {

  sandboxed("add then remove of the same file leaves nothing to push and the sync pushes nothing") { sb =>
    for
      _      <- sb.track(".bashrc", "b\n")
      _      <- sb.host(".vimrc").writeText("v\n")
      _      <- sb.polio("add", sb.host(".vimrc"))
      _      <- sb.polio("remove", sb.host(".vimrc"))
      status <- sb.polio("status")
      before <- sb.remoteCommits
      out    <- sb.polio("sync")
      after  <- sb.remoteCommits
    yield
      status.lacks("to push")
        && out.lacks("pushed")
        && check(after == before, s"remote went from $before to $after commits")
  }

  sandboxed("add x, add y, remove x pushes one commit that adds y") { sb =>
    for
      _       <- sb.track(".bashrc", "b\n")
      _       <- sb.host(".vimrc").writeText("v\n")
      _       <- sb.host(".gitconfig").writeText("g\n")
      _       <- sb.polio("add", sb.host(".vimrc"))
      _       <- sb.polio("add", sb.host(".gitconfig"))
      _       <- sb.polio("remove", sb.host(".vimrc"))
      status  <- sb.polio("status")
      before  <- sb.remoteCommits
      _       <- sb.polio("sync")
      after   <- sb.remoteCommits
      subject <- sb.git("log", "-1", "--format=%s")
    yield
      status.has("2 path(s) to push")
        && check(after == before + 1, s"remote went from $before to $after commits")
        && check(subject == "polio: add ~/.gitconfig", s"last commit: $subject")
  }
}
