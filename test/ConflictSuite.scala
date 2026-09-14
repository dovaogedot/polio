package polio

import cats.syntax.all.*

/** Conflict handling from end to end: the flags, the rule for a non-terminal stdin, and the interactive menu. */
object ConflictSuite extends SandboxSuite {

  private val rc = ".bashrc"

  sandboxed("bind and add commit locally; the first sync pushes") { sb =>
    for
      _        <- sb.polio("bind", sb.remote)
      _        <- sb.host(rc).writeText("original\n")
      _        <- sb.polio("add", sb.host(rc))
      unsynced <- sb.remoteCommits
      pending  <- sb.polio("status")
      _        <- sb.polio("sync")
      synced   <- sb.remoteCommits
      current  <- sb.polio("status")
    yield
      expect.same(0, unsynced)
        && pending.has("up to date    ~/.bashrc")
        && pending.has("to push")
        && expect(synced > 0)
        && current.lacks("to push")
  }

  sandboxed("adding a tracked file reports it and leaves the repo copy alone") { sb =>
    for
      _    <- sb.track(rc, "original\n")
      _    <- sb.host(rc).writeText("edited\n")
      out  <- sb.polio("add", sb.host(rc))
      repo <- sb.repoCopy(rc).readText
    yield
      out.has("already tracked: ~/.bashrc")
        && out.lacks("committed")
        && expect.same("original\n", repo)
  }

  sandboxed("status reports a host-side modification") { sb =>
    for
      _      <- sb.track(rc, "original\n")
      _      <- sb.host(rc).writeText("tweak\n")
      status <- sb.polio("status")
    yield status.has("modified      ~/.bashrc (polio sync: host -> repo)")
  }

  sandboxed("unknown flags are rejected") { sb =>
    List("-x", "-m", "--merge").traverse(rejected(sb)).map(_.combineAll)
  }

  /** Expects polio sync to reject the flag. */
  private def rejected(sb: Sandbox)(flag: String) =
    sb.tryPolio("sync", flag).map: run =>
      check(run.code != 0, s"sync $flag accepted")

  sandboxed("-q suppresses stdout; -s suppresses stderr as well") { sb =>
    for
      _        <- sb.track(rc, "original\n")
      leading  <- sb.polio("-q", "status")
      trailing <- sb.polio("status", "--quiet")
      quiet    <- sb.tryPolio("-q", "sync", "-x")
      shushed  <- sb.tryPolio("-s", "sync", "-x")
      status   <- sb.polio("-s", "status")
    yield
      expect.same("", leading)
        && expect.same("", trailing)
        && check(quiet.code != 0, "-q sync -x accepted")
        && check(quiet.err.nonEmpty, "-q silenced stderr")
        && check(shushed.code != 0, "-s sync -x accepted")
        && expect.same("", shushed.err)
        && expect.same("", status)
  }

  sandboxed("non-terminal stdin resolves a conflict like --force") { sb =>
    for
      _      <- sb.track(rc, "original\n")
      _      <- sb.diverge(rc, "host change one\n", "repo change one\n")
      status <- sb.polio("status")
      out    <- sb.polio("sync")
      repo   <- sb.repoCopy(rc).readText
    yield
      status.has("conflict      ~/.bashrc")
        && out.has("host copy kept")
        && expect.same("host change one\n", repo)
  }

  sandboxed("menu offers [l/r/s] only; d is rejected; r keeps the repo copy") { sb =>
    for
      _    <- sb.track(rc, "original\n")
      _    <- sb.diverge(rc, "host change two\n", "repo change two\n")
      out  <- sb.tty("d\nr\n")
      host <- sb.host(rc).readText
    yield
      out.has("[l] keep local")
        && out.lacks("[d]")
        && out.has("choose [l/r/s]: choose [l/r/s]:")
        && out.has("repo copy kept")
        && expect.same("repo change two\n", host)
  }

  sandboxed("remove commits locally; status names it; sync pushes it") { sb =>
    for
      _       <- sb.track(rc, "original\n")
      before  <- sb.remoteCommits
      _       <- sb.polio("remove", sb.host(rc))
      removed <- sb.remoteCommits
      pending <- sb.polio("status")
      _       <- sb.polio("sync")
      after   <- sb.remoteCommits
      settled <- sb.polio("status")
    yield
      expect.same(before, removed)
        && pending.has("removed       ~/.bashrc (untracked; polio sync pushes the removal)")
        && expect(after > before)
        && settled.lacks("removed")
  }

  sandboxed("a host copy deleted by hand reads as missing, not removed") { sb =>
    for
      _      <- sb.track(rc, "original\n")
      _      <- sb.host(rc).removeIfExists
      status <- sb.polio("status")
      out    <- sb.polio("sync")
      host   <- sb.host(rc).readText
    yield
      status.has("missing       ~/.bashrc (gone from the host; polio sync reinstalls it — polio remove to untrack)")
        && status.lacks("removed")
        && out.has("repo -> host  ~/.bashrc")
        && expect.same("original\n", host)
  }

  sandboxed("--yolo keeps the repo copy without asking") { sb =>
    val rc = ".bashrc"
    for
      _    <- sb.track(rc, "original\n")
      _    <- sb.diverge(rc, "host change\n", "repo change\n")
      out  <- sb.polio("sync", "-y")
      host <- sb.host(rc).readText
      repo <- sb.repoCopy(rc).readText
    yield
      check(host == "repo change\n", s"host copy:\n$host")
        && check(repo == "repo change\n", s"repo copy:\n$repo")
        && out.has(s"repo -> host  ~/$rc")
  }
}
