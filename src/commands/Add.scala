package polio

import cats.effect.IO
import cats.syntax.all.*
import mouse.all.*
import fs2.io.file.Path

/** A file just copied into the repo, with the hash both copies now have. */
private final case class Added(target: Target, hash: String) {

  /** The manifest entry for the file. */
  def entry: (String, Target) = target.repoPath -> target

  /** The sync-state entry for the file. */
  def record: (String, String) = target.repoPath -> hash
}

private object Added {

  /** Copies the host file at path into the repo as target. Returns the addition with its hash. */
  def stage(layout: Layout, path: Path, target: Target): IO[Added] =
    path.copyTo(layout.repoFile(target.repoPath)) *> path.sha256.map(Added(target, _))

  /**
   * The report text: one line per new file, one line per file that was already tracked, and a note
   * about the commit.
   */
  def report(added: List[Added], skipped: List[Target]): String = {
    val tracking  = added.map(a => s"tracking ${a.target}")
    val already   = skipped.map(t => s"already tracked: $t")
    val committed = added.nonEmpty.option(s"committed ${added.length} file(s) — polio sync pushes them").toList
    val lines     = tracking ::: already ::: committed
    lines.mkString("\n")
  }
}

extension (layout: Layout) {

  /**
   * The files at a location: one regular file, or every file under a directory. The data directory of
   * polio itself is refused.
   */
  private def filesToTrack(location: Path): IO[List[Path]] = {
    val ownData = PolioError.Usage(s"cannot track polio's own data directory: $location")
    val files   = location.fileKind.flatMap:
      case FileKind.Missing     => IO.raiseError(PolioError.Usage(s"no such path: $location"))
      case FileKind.Directory   => location.walkFiles
      case FileKind.RegularFile => IO.pure(List(location))
    IO.raiseWhen(location.startsWith(layout.root) || location.startsWith(layout.statePath.parent.get))(ownData) *> files
  }
}

/**
 * polio add: starts tracking the file at the path the user typed, or every file inside a directory.
 * Returns the report.
 */
def add(raw: String): IO[String] =
  for
    layout   <- Layout.resolve
    manifest <- Manifest.load(layout)
    paths    <- layout.filesToTrack(layout.locate(raw))

    entries = paths.map: path =>
      path -> Target.contract(path, layout.home)

    (skipped, fresh) = entries.partition: (_, target) =>
      manifest.tracks(target)

    added <- fresh.traverse: (path, target) =>
      Added.stage(layout, path, target)

    known  = skipped.map(_._2)
    labels = added.map(_.target).mkString(", ")

    _     <- Manifest(manifest.files ++ added.map(_.entry)).save(layout)
    state <- SyncState.load(layout)
    _     <- SyncState(state.files ++ added.map(_.record)).save(layout)
    _     <- Git.in(layout.repo).commitIfChanged(s"polio: add $labels")
  yield Added.report(added, known)
