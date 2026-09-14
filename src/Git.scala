package polio

import cats.effect.IO
import cats.syntax.all.*
import fs2.io.file.Path
import fs2.io.process.ProcessBuilder
import fs2.{Stream, text}
import java.io.IOException
import mouse.all.*

/** Git commands that run in a working directory. Git is the only external program polio needs. */

private final case class GitOutput(code: Int, stdout: String, stderr: String) {

  /** Whether git exited with status zero. */
  def succeeded: Boolean = code == 0

  /** The best text to describe the failure: stderr, else stdout, else the exit code. */
  def errorText: String = {
    val err   = stderr.trim.some.filter(_.nonEmpty)
    val out   = stdout.trim.some.filter(_.nonEmpty)
    val first = err <+> out
    first.getOrElse(s"exit code $code")
  }
}

/** The error for a git process that could not run. A missing git executable gets its own message. */
private def spawnError(args: List[String])(t: Throwable): PolioError = {
  val output = t match
    case io: IOException if String.valueOf(io.getMessage).contains("Cannot run program") =>
      "git executable not found on PATH"
    case other => describe(other)
  PolioError.Git(args, output)
}

/** One side of a three-way merge: the file, and the label used in its conflict markers. */
final case class MergeSide(path: Path, label: String)

/** A git handle. It holds the working directory where the commands run. */
opaque type Git = Option[Path]

object Git {

  /** A handle that runs commands inside repo. */
  def in(repo: Path): Git = Some(repo)

  /** A handle that runs commands in the current directory. Use it for commands that take every path as an argument. */
  val anywhere: Git = None
}

extension (git: Git) {

  /** Runs git with args and a closed stdin. Fails only if git cannot start. */
  private def raw(args: String*): IO[GitOutput] = {
    val argList = args.toList
    val base    = ProcessBuilder("git", argList)
    val builder = git.fold(base)(base.withWorkingDirectory)
    val run     = builder.spawn[IO].use { p =>
      val closeIn = Stream.empty.through(p.stdin).compile.drain
      val out     = p.stdout.through(text.utf8.decode).compile.string
      val err     = p.stderr.through(text.utf8.decode).compile.string
      closeIn *> (out, err).parTupled.flatMap: (stdout, stderr) =>
        p.exitValue.map(GitOutput(_, stdout, stderr))
    }
    run.adaptError:
      case t => spawnError(argList)(t)
  }

  /** Runs git and fails on a non-zero exit. Returns the trimmed stdout. */
  private def run(args: String*): IO[String] =
    raw(args*).flatMap: out =>
      IO.raiseUnless(out.succeeded)(PolioError.Git(args.toList, out.errorText)).as(out.stdout.trim)

  /** The branch that HEAD points at. */
  def currentBranch: IO[String] = run("symbolic-ref", "--short", "HEAD")

  /** The URL of the origin remote. Fails if there is none. */
  def originUrl: IO[String] = run("remote", "get-url", "origin")

  /** Points the origin remote at url. */
  def setOriginUrl(url: String): IO[Unit] = run("remote", "set-url", "origin", url).void

  /** Adds an origin remote with url. Fails if one already exists. */
  def addOrigin(url: String): IO[Unit] = run("remote", "add", "origin", url).void

  /** Fetches every branch of origin. */
  def fetchOrigin: IO[Unit] = run("fetch", "origin").void

  /** Clones the repository at url into the directory. */
  def clone(url: String, into: Path): IO[Unit] = run("clone", url, into.toString).void

  /** The short hash of the parent of HEAD. None if HEAD has no parent. */
  def parentOfHead: IO[Option[String]] = run("rev-parse", "--short", "HEAD^").redeem(_ => None, Some(_))

  /** The content of a repository file at ref. None if the ref or the file does not exist. */
  def show(ref: String, file: String): IO[Option[String]] =
    raw("show", s"$ref:$file").map: out =>
      out.succeeded.option(out.stdout)

  /**
   * Merges ours and theirs on top of base. Each conflict is marked with the labels of the sides.
   * Returns the merged text.
   */
  def mergeFile(ours: MergeSide, base: MergeSide, theirs: MergeSide): IO[String] = {
    val args = List(
      "merge-file",
      "-p",
      "-L",
      ours.label,
      "-L",
      base.label,
      "-L",
      theirs.label,
      ours.path.toString,
      base.path.toString,
      theirs.path.toString,
    )
    raw(args*).flatMap: out =>
      // merge-file exits with the conflict count (capped at 127); >127 is an error.
      IO.raiseWhen(out.code > 127)(PolioError.Git(args, out.errorText)).as(out.stdout)
  }

  /** Stages all changes and commits if there are any. Returns true if a commit was made. */
  def commitIfChanged(message: String): IO[Boolean] = {
    val add    = run("add", "-A")
    val commit = run("commit", "-m", message)
    val dirty  = run("status", "--porcelain").map(_.nonEmpty)
    add *> dirty.ifM(commit.as(true), IO.pure(false))
  }

  /** Pulls the branch with rebase. An empty remote, where nothing was pushed yet, counts as up to date. */
  def pull(branch: String): IO[Boolean] = {
    val args = List("pull", "--rebase", "--autostash", "origin", branch)
    raw(args*).flatMap: out =>
      if out.succeeded then
        IO.pure(true)
      else if out.stderr.contains("couldn't find remote ref") then
        IO.pure(false)
      else
        IO.raiseError(PolioError.Git(args, out.errorText))
  }

  /**
   * The number of commits on the branch that origin does not have. If origin has no such branch yet,
   * every commit counts.
   */
  def pendingPushes(branch: String): IO[Int] = {
    def count(text: String): Int = text.trim.toIntOption.getOrElse(0)
    raw("rev-list", "--count", s"origin/$branch..HEAD").flatMap: out =>
      if out.succeeded
      then IO.pure(count(out.stdout))
      else run("rev-list", "--count", "HEAD").map(count)
  }

  /** Whether origin has the branch, as far as the last fetch or pull knows. */
  def hasRemote(branch: String): IO[Boolean] = raw("rev-parse", "--verify", "-q", s"origin/$branch").map(_.succeeded)

  /**
   * The paths whose content differs between origin's branch and HEAD: what a push would carry. Every
   * tracked path if origin has no such branch yet.
   */
  def changedAgainstRemote(branch: String): IO[List[String]] = {
    val diff = run("diff", "--name-only", s"origin/$branch", "HEAD")
    val all  = run("ls-files")
    hasRemote(branch).ifM(diff, all).map(_.split("\n").toList.filter(_.nonEmpty))
  }

  /**
   * Replaces every commit after origin's branch with one commit that holds their net change, under
   * message. No commit is made when the net change is nothing. Returns whether one was made.
   */
  def squashOnto(branch: String, message: String): IO[Boolean] =
    run("reset", "--soft", s"origin/$branch") *> commitIfChanged(message)

  /** Pushes HEAD to origin. A failure returns a warning line instead of an error. */
  def pushBestEffort: IO[Option[String]] =
    run("push", "origin", "HEAD").as(none[String]).recover:
      case e: PolioError.Git =>
        val firstLine = e.output.split("\n", -1).headOption.getOrElse("")
        Some("warning: push failed, the commit stays local until the next sync: " + firstLine)
}
