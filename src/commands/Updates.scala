package polio

import cats.effect.IO
import cats.syntax.all.*

/** The words that turn the update check on and off. */
private val settings: Map[String, Boolean] = Map("on" -> true, "off" -> false)

/** The setting the word names. A word that names none is a Usage error. */
private def parseSetting(word: String): IO[Boolean] = {
  val unknown = PolioError.Usage(s"unknown setting: $word — use: polio updates [on|off]")
  IO.fromOption(settings.get(word.trim.toLowerCase))(unknown)
}

/**
 * polio updates: turns the check for a newer polio on or off, and reports the setting. Without a
 * setting it only reports. Returns the report.
 */
def updates(setting: Option[String]): IO[Report] =
  for
    layout <- Layout.resolve
    state  <- UpdateState.load(layout)
    wanted <- setting.traverse(parseSetting)
    _      <- wanted.traverse_(on => state.copy(enabled = on).save(layout))
    on = wanted.getOrElse(state.enabled)
  yield Report.notes(s"update check: ${if on then "on" else "off"}")
