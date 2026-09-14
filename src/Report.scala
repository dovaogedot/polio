package polio

/** How a report is printed: with colors, or as porcelain for scripts. */
final case class Style(color: Boolean, porcelain: Boolean)

/** The meaning of a row, shown as the color of its label. */
enum Tone {

  /** Nothing to do for this file. */
  case Good

  /** A change that sync makes, made, or would make. */
  case Change

  /** Something that waits for the user. */
  case Bad

  /** Plain information. */
  case Muted

  /** The ANSI color code for the tone. */
  def code: String = this match
    case Good   => "32"
    case Change => "33"
    case Bad    => "31"
    case Muted  => "2"
}

/**
 * The two-letter porcelain codes. The first letter is the repo side and the second the host side, like
 * the index and work tree columns of git status --short.
 */
object Code {

  /** Both copies are the same. Porcelain leaves these rows out. */
  val clean = "OK"

  /** The host copy changed; sync copies it to the repo. */
  val hostChanged = " M"

  /** The repo copy changed; sync copies it to the host. */
  val repoChanged = "M "

  /** New on the host; the repo has no copy yet. */
  val hostNew = " A"

  /** Gone from the host; sync reinstalls it. */
  val hostGone = " D"

  /** Gone on both sides. */
  val bothGone = "DD"

  /** Both sides changed. */
  val conflict = "UU"

  /** Both sides changed and the host copy was kept. */
  val conflictHostKept = "UH"

  /** Both sides changed and the repo copy was kept. */
  val conflictRepoKept = "UR"

  /** A parked conflict copy waits for a manual fix. */
  val parked = "PP"

  /** A parked copy is fixed and goes to both sides on the next sync. */
  val resolved = "RR"

  /** Not tracked any more. */
  val untracked = "--"

  /** Tracked from now on. */
  val tracking = "++"

  /** Was tracked already. */
  val alreadyTracked = "=="
}

/**
 * One line of the table in a report: the porcelain code, the state label, the target it is about, a
 * short note, and extra lines shown under it. An empty code marks plain information without a state.
 */
final case class Row(
  code: String,
  label: String,
  tone: Tone,
  target: String,
  note: String = "",
  details: List[String] = Nil,
)

/** What a command reports: rows about files, rendered as a table, followed by free-text notes. */
final case class Report(rows: List[Row] = Nil, notes: List[String] = Nil) {

  /**
   * The text to print. Porcelain gives one line per file as the two-letter code, a space and the
   * target, leaves out files that are up to date, and puts information rows and notes after them with
   * a leading "# ". Otherwise the rows form a table with aligned columns, colored when asked.
   */
  def render(style: Style): String =
    if style.porcelain then porcelain else table(style.color)

  private def porcelain: String = {
    val states = rows.filter(r => r.code.nonEmpty && r.code != Code.clean).map(r => s"${r.code} ${r.target}")
    val info   = rows.filter(_.code.isEmpty).map(r => s"# ${r.label} ${r.target}")
    val lines  = states ::: info ::: notes.map("# " + _)
    lines.mkString("\n")
  }

  private def table(color: Boolean): String = {
    val labelWidth  = rows.map(_.label.length).maxOption.getOrElse(0)
    val targetWidth = rows.filter(_.note.nonEmpty).map(_.target.length).maxOption.getOrElse(0)
    val lines       = rows.flatMap { row =>
      val label  = Ansi.paint(row.label.padTo(labelWidth, ' '), row.tone.code, color)
      val target = row.target.padTo(targetWidth, ' ')
      val note   = Ansi.paint(row.note, Ansi.dim, color)
      val line   = s"$label  $target  $note".stripTrailing
      line :: row.details.map(" " * (labelWidth + 2) + _)
    }
    (lines ::: notes).mkString("\n")
  }
}

object Report {

  /** A report of notes only. */
  def notes(lines: String*): Report = Report(Nil, lines.toList)
}

/** ANSI escape sequences for the table. */
object Ansi {

  /** The code for dim text. */
  val dim = "2"

  /** The text wrapped in the color code and a reset, when color is on. Empty text stays empty. */
  def paint(text: String, code: String, on: Boolean): String =
    if on && text.nonEmpty then s"\u001b[${code}m$text\u001b[0m" else text
}
