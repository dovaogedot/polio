package polio

import cats.effect.IO
import cats.syntax.all.*
import mouse.all.*

extension (facts: Facts) {

  /** The status row based only on the three-way comparison. */
  private def freshRow: Row = {
    val target = facts.target.value
    facts.decide match
      case Detected.Clean   => Row(Code.clean, "up to date", Tone.Good, target)
      case Detected.Missing =>
        Row(Code.bothGone, "missing", Tone.Bad, target, "gone on host and in repo; polio remove to untrack")
      case Detected.Conflict => Row(Code.conflict, "conflict", Tone.Bad, target, "both sides changed; polio sync asks")
      case Detected.ToRepo   =>
        if facts.repoHash.isEmpty
        then Row(Code.hostNew, "added", Tone.Change, target, "polio sync copies it to the repo")
        else Row(Code.hostChanged, "modified", Tone.Change, target, "polio sync: host -> repo")
      case Detected.ToHost =>
        if facts.hostHash.isEmpty
        then
          Row(
            Code.hostGone,
            "missing",
            Tone.Bad,
            target,
            "gone from the host; polio sync reinstalls it — polio remove to untrack",
          )
        else Row(Code.repoChanged, "modified", Tone.Change, target, "polio sync: repo -> host")
  }

  /** The status row. A parked copy, if there is one, decides it before the comparison does. */
  private def statusRow(layout: Layout): IO[Row] = {
    val conflictsDisplay = layout.display(layout.conflictsDir)
    val target           = facts.target.value
    layout.parkedFile(facts.repoPath).readTextIfExists.map:
      case None         => facts.freshRow
      case Some(parked) =>
        if parked.hasConflictMarkers
        then
          Row(Code.parked, "parked", Tone.Bad, target, s"resolve $conflictsDisplay/${facts.repoPath}, then polio sync")
        else Row(Code.resolved, "resolved", Tone.Change, target, "polio sync applies it to both sides")
  }
}

/** polio status: lists every tracked file and what polio sync would do with it. Reads local state only. */
def status: IO[Report] =
  for
    layout   <- Layout.resolve
    _        <- layout.requireBound
    manifest <- Manifest.load(layout)
    state    <- SyncState.load(layout)

    tracked <- manifest.files.toList.traverse: (repoPath, target) =>
      Facts.gather(layout, state, repoPath, target).flatMap: facts =>
        facts.statusRow(layout).map(target -> _)

    repo = Git.in(layout.repo)
    branch  <- repo.currentBranch
    pushed  <- Manifest.atOrigin(layout, branch)
    changed <- repo.changedAgainstRemote(branch)

    // polio remove drops the manifest entry and commits, so a target origin still
    // tracks and this manifest does not is an untracking waiting to be pushed.
    removed = manifest.droppedFrom(pushed).map: target =>
      target -> Row(Code.untracked, "removed", Tone.Change, target.value, "untracked; polio sync pushes the removal")

    shown   = tracked ::: removed
    rows    = shown.sortBy(_._1).map(_._2)
    nothing = manifest.files.isEmpty.option("nothing tracked — polio add <path>").toList
    pushes  = Option.when(changed.nonEmpty)(s"${changed.length} path(s) to push — polio sync pushes them").toList
  yield Report(rows, nothing ::: pushes)
