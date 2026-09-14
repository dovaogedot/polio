package polio

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import fs2.io.file.{Files, Path}
import fs2.io.process.ProcessBuilder
import fs2.{Stream, text}
import scala.util.control.NoStackTrace
import weaver.{Expectations, SimpleIOSuite, SourceLocation, TestName}

/** The exit code and the captured output of one finished process. */
final case class Run(code: Int, out: String, err: String)

/** Failures of the test harness itself, not failed expectations. */
enum HarnessError extends RuntimeException with NoStackTrace {

  /** There is no polio binary at path. The suites need an installed binary, or POLIO_BIN pointing at one. */
  case NoBinary(path: Path)

  /** A child process saw home instead of the sandbox home. */
  case Leaky(home: String)

  /** The command exited with a non-zero code. run holds its code and output. */
  case Exited(command: String, run: Run)

  /** The file expected at path does not exist. */
  case Missing(path: Path)

  override def getMessage: String = this match
    case NoBinary(path)       => s"no polio binary at $path: install it, or point POLIO_BIN at one"
    case Leaky(home)          => s"spawned processes see HOME=$home instead of the sandbox"
    case Exited(command, run) => s"$command exited ${run.code}\n${run.out}${run.err}"
    case Missing(path)        => s"no such file: $path"
}

extension (path: Path) {

  /** Reads a whole file. Fails if it is missing. */
  def readText: IO[String] =
    path.readTextIfExists.flatMap: content =>
      IO.fromOption(content)(HarnessError.Missing(path))
}

/**
 * An isolated home, polio data directory and bare remote for one test. Every process started through it
 * runs with that home and its own git identity, so the real files of the developer are never touched.
 */
final case class Sandbox(root: Path, bin: Path) {

  /** The sandbox home directory. */
  val home = root / "home"

  /** POLIO_HOME for the sandbox. */
  val polioHome = root / "poliohome"

  /** The bare repository used as the remote. */
  val remote = root / "remote.git"

  /** The clone polio syncs. */
  val repo = polioHome / "repo"

  /** Tracked file contents inside the clone. */
  val filesDir = repo / "files"

  /** Parked conflict copies. */
  val conflicts = polioHome / "conflicts"

  private val gitconfig = root / "gitconfig"

  /** The environment of every child process: the sandbox home, POLIO_HOME, an isolated git config, and bash as the shell. */
  private val env = Map(
    "HOME"              -> home.toString,
    "POLIO_HOME"        -> polioHome.toString,
    "GIT_CONFIG_GLOBAL" -> gitconfig.toString,
    "GIT_CONFIG_SYSTEM" -> "/dev/null",
    "SHELL"             -> "/bin/bash",
  )

  /** The host copy of a file tracked at rel under home. */
  def host(rel: String): Path = home / rel

  /** The repo copy of a file tracked at rel. */
  def repoCopy(rel: String): Path = filesDir / rel

  /** The parked conflict copy of a file tracked at rel. */
  def parked(rel: String): Path = conflicts / rel

  /** Runs a command in the sandbox environment, with input on its stdin, and waits for it to finish. */
  def exec(command: String, args: List[String], input: String = ""): IO[Run] =
    ProcessBuilder(command, args).withExtraEnv(env).spawn[IO].use { p =>
      val feed = Stream.emit(input).through(text.utf8.encode).through(p.stdin).compile.drain
      val out  = p.stdout.through(text.utf8.decode).compile.string
      val err  = p.stderr.through(text.utf8.decode).compile.string
      (feed, out, err).parTupled.flatMap: (_, stdout, stderr) =>
        p.exitValue.map(Run(_, stdout, stderr))
    }

  /** Runs the binary. A non-zero exit fails the test. Returns its stdout. */
  def polio(args: (String | Path)*): IO[String] = tryPolio(args*) >>= succeeded(s"polio ${args.mkString(" ")}")

  /** Runs the binary and returns the result, whatever the exit code. */
  def tryPolio(args: (String | Path)*): IO[Run] = exec(bin.toString, args.map(_.toString).toList)

  /** Runs polio with args on a pseudo-terminal and types keys at its prompts. Returns everything it printed. */
  def onTty(args: String, keys: String): IO[String] =
    exec("script", List("-qec", s"$bin $args", "/dev/null"), keys) >>= succeeded(s"polio $args on a tty")

  /** Runs polio sync on a pseudo-terminal and types keys at its prompts. Returns everything it printed. */
  def tty(keys: String): IO[String] = onTty("sync", keys)

  /** Runs git inside the clone, like another host that edits the remote. */
  def git(args: String*): IO[String] = gitIn(repo, args*)

  /** The number of commits on the remote. add and remove do not change it until a sync. */
  def remoteCommits: IO[Int] = gitIn(remote, "rev-list", "--count", "--all").map(_.toInt)

  /** Binds to the remote, writes content to the file at rel under home, tracks it, and syncs. */
  def track(rel: String, content: String): IO[Unit] =
    polio("bind", remote)
      *> host(rel).writeText(content)
      *> polio("add", host(rel))
      *> polio("sync").void

  /** Edits the tracked file on the host and puts a different edit on the remote, so the next sync has a conflict. */
  def diverge(rel: String, onHost: String, onRemote: String): IO[Unit] =
    host(rel).writeText(onHost)
      *> repoCopy(rel).writeText(onRemote)
      *> git("commit", "-qam", s"change $rel from another host")
      *> git("push", "-q", "origin", "main").void

  /**
   * Points pushes at a path that does not exist. A push that is tried gives a warning line. A push
   * that is skipped gives nothing.
   */
  def breakPushes: IO[Unit] = {
    val nowhere = root / "nowhere"
    git("config", "remote.origin.pushurl", nowhere.toString).void
  }

  /** Points pushes back at the remote. */
  def restorePushes: IO[Unit] = git("config", "--unset", "remote.origin.pushurl").void

  /** Creates the home, the git identity and the bare remote, then checks that child processes see them. */
  private def prepared: IO[Sandbox] =
    home.ensureDir
      *> gitconfig.writeText(Sandbox.gitIdentity)
      *> gitIn(root, "init", "--quiet", "--bare", remote.toString)
      *> probe.as(this)

  /** Fails if child processes do not get the sandbox environment. A leak would expose the real home. */
  private def probe: IO[Unit] =
    exec("sh", List("-c", "printf %s \"$HOME\"")).flatMap: run =>
      IO.raiseUnless(run.out == home.toString)(HarnessError.Leaky(run.out))

  /** Runs git inside dir. Returns its trimmed stdout. A non-zero exit fails the test. */
  private def gitIn(dir: Path, args: String*): IO[String] = {
    val out = exec("git", "-C" :: dir.toString :: args.toList) >>= succeeded(s"git ${args.mkString(" ")}")
    out.map(_.trim)
  }

  /** Returns the stdout of the run. If the exit code is not zero, fails the test and names the command. */
  private def succeeded(command: String)(run: Run): IO[String] =
    IO.raiseWhen(run.code != 0)(HarnessError.Exited(command, run)).as(run.out)
}

object Sandbox {

  /** The git config every sandbox process reads: a fixed identity and main as the default branch. */
  private val gitIdentity = "[user]\n\tname = smoke\n\temail = smoke@test\n[init]\n\tdefaultBranch = main\n"

  /** The binary under test: POLIO_BIN, or the installed binary in the real home. */
  private def binary: IO[Path] =
    IO.fromEither(homeDir).flatMap { home =>
      val bin    = envGet("POLIO_BIN").map(Path(_)).getOrElse(home / ".local/bin/polio")
      val absent = IO.raiseError(HarnessError.NoBinary(bin))
      bin.isPresent.ifM(IO.pure(bin), absent)
    }

  /** A new sandbox with an empty home, a git identity and a bare remote. The directory is deleted on release. */
  def make: Resource[IO, Sandbox] =
    for
      bin  <- Resource.eval(binary)
      root <- Files[IO].tempDirectory(None, "polio-test-", None)
      sb   <- Resource.eval(Sandbox(root, bin).prepared)
    yield sb
}

/** A suite where every test gets its own sandbox. */
trait SandboxSuite extends SimpleIOSuite {

  /** Registers a test. The body gets its own sandbox. */
  def sandboxed(name: String)(body: Sandbox => IO[Expectations])(using loc: SourceLocation): Unit = {
    val named = TestName(name, loc, Set.empty)
    test(named)(Sandbox.make.use(body))
  }
}
