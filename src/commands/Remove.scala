package polio

import cats.effect.IO
import cats.syntax.all.*

/** polio remove: stops tracking the file or directory at the path the user typed. Host copies stay in place. */
def remove(raw: String): IO[Report] =
  for
    layout   <- Layout.resolve
    manifest <- Manifest.load(layout)

    portable = Target.contract(layout.locate(raw), layout.home)
    doomed   = manifest.files.toList.filter: (_, target) =>
      target.within(portable)

    _ <- IO.raiseWhen(doomed.isEmpty)(PolioError.Usage(s"not tracked: $portable"))

    dropped = doomed.map(_._1).toSet
    files   = manifest.files.removedAll(dropped)
    labels  = doomed.map(_._2).mkString(", ")

    _     <- dropped.toList.traverse_(layout.repoFile(_).removeIfExists)
    _     <- Manifest(files).save(layout)
    state <- SyncState.load(layout)

    kept = state.files.view.filterKeys(files.contains).toMap

    _ <- SyncState(kept).save(layout)
    _ <- Git.in(layout.repo).commitIfChanged(s"polio: remove $labels")

    untracked = doomed.map: (_, target) =>
      Row(Code.untracked, "untracked", Tone.Change, target.value, "host copy kept")
  yield Report(untracked, List("committed — polio sync pushes"))
