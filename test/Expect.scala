package polio

import weaver.Expectations.Helpers.{failure, success}
import weaver.{Expectations, SourceLocation}

/** Passes if the condition is true. The hint is shown on failure. */
def check(cond: Boolean, hint: => String)(using SourceLocation): Expectations =
  if cond then success else failure(hint)

extension (text: String) {

  /** Passes if the text contains needle. A failure shows the whole text. */
  def has(needle: String)(using SourceLocation): Expectations =
    check(text.contains(needle), s"«$needle» missing from:\n$text")

  /** Passes if some line is a table row with the label, then the target, in columns split by two or more spaces. Colors are ignored. */
  def hasRow(label: String, target: String)(using SourceLocation): Expectations = {
    val plain = text.replaceAll("\\u001b\\[[0-9;?]*[A-Za-z]", "")
    val rows  = plain.linesIterator.map(_.split("  +", 3).toList)
    val found = rows.exists:
      case first :: second :: _ => first == label && second == target
      case _                    => false
    check(found, s"row «$label  $target» missing from:\n$text")
  }

  /** Passes if the text does not contain needle. A failure shows the whole text. */
  def lacks(needle: String)(using SourceLocation): Expectations =
    check(!text.contains(needle), s"«$needle» present in:\n$text")
}
