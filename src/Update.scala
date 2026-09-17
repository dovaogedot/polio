package polio

import cats.effect.IO
import cats.syntax.all.*
import io.circe.syntax.*
import io.circe.{Codec, parser}
import mouse.all.*

/** The setting that says whether polio looks for a newer release of itself. */

/** The shape of update.json on disk. */
private final case class UpdateDoc(version: Int, enabled: Boolean) derives Codec.AsObject

/**
 * What this host remembers about the update check. Only the setting lives here: the npm shim does the
 * looking and keeps what it finds in a file of its own beside this one. It is never committed.
 */
final case class UpdateState(enabled: Boolean) {

  /** Writes the setting for this host. */
  def save(layout: Layout): IO[Unit] =
    UpdateDoc(1, enabled).asJson
      |> Doc.printer.print
      |> (text => layout.updatePath.writeText(text + "\n"))
}

object UpdateState {

  /** The setting before anyone changes it: the check is on. */
  val fresh: UpdateState = UpdateState(enabled = true)

  /** The setting of this host. A missing or invalid file gives the fresh setting. */
  def load(layout: Layout): IO[UpdateState] =
    layout.updatePath.readTextIfExists
      .map { text =>
        val decoded = text.flatMap: t =>
          parser.decode[UpdateDoc](t).toOption
        decoded.fold(fresh)(doc => UpdateState(doc.enabled))
      }
      .handleError(_ => fresh)
}
