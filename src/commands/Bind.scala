package polio

import cats.effect.IO
import cats.syntax.all.*

/** Points an existing clone at url and fetches from it. */
private def rebind(layout: Layout, url: String): IO[Report] = {
  val repo      = Git.in(layout.repo)
  val setUrl    = repo.originUrl *> repo.setOriginUrl(url)
  val reconnect = setUrl <+> repo.addOrigin(url)
  reconnect *> repo.fetchOrigin.as(Report(layout.boundRows(url)))
}

/** Makes sure the cloned repo has a manifest. If the remote had none, an empty one is committed. */
private def ensureManifest(layout: Layout): IO[Unit] = {
  val init = Manifest.empty.save(layout)
    *> Git.in(layout.repo).commitIfChanged("polio: init manifest").void
  layout.manifestPath.isPresent >>= init.unlessA
}

private def cloneRepo(layout: Layout, url: String): IO[Report] = {
  val next = Row("", "run", Tone.Muted, "polio sync")
  layout.root.ensureDir
    *> Git.anywhere.clone(url, layout.repo)
    *> ensureManifest(layout).as(Report(layout.boundRows(url) :+ next))
}

extension (layout: Layout) {

  /** The rows that report a bound remote: the URL and the clone location. */
  private def boundRows(url: String): List[Row] =
    List(Row("", "bound", Tone.Good, url), Row("", "repo", Tone.Muted, layout.repo.toString))
}

/**
 * polio bind: connects this host to the git remote that stores the config files. Clones it if this host
 * has no clone yet.
 */
def bind(url: String): IO[Report] =
  Layout.resolve.flatMap { layout =>
    val fresh = cloneRepo(layout, url)
    val again = rebind(layout, url)
    layout.isBound.ifM(again, fresh)
  }
