package polio

import cats.effect.IO
import cats.syntax.all.*
import java.io.{FileDescriptor, FileInputStream}
import java.lang.ProcessBuilder.Redirect
import java.nio.charset.StandardCharsets.UTF_8

/** The standard input of the process, read without the JVM buffer, so a read takes only the bytes it returns. */
object Stdin {

  /** Standard input with no buffer in front of it. */
  private val raw = FileInputStream(FileDescriptor.in)

  /** Reads one byte. -1 at the end of input. */
  def readByte: Int = raw.read

  /**
   * Reads one line, one byte at a time, so nothing after the newline is taken. A later prompt still
   * sees lines that were typed or pasted ahead. None at the end of input.
   */
  def readLine: Option[String] = {
    val bytes = scala.collection.mutable.ArrayBuffer.empty[Byte]
    var done = false
    var eof  = false
    while !done do
      val b = raw.read
      if b < 0 then
        if bytes.isEmpty then eof = true
        done = true
      else if b == 10 then
        done = true
      else if b != 13 then
        bytes += b.toByte
    if eof then None else Some(String(bytes.toArray, UTF_8))
  }

  /** Prints question on stderr and reads one trimmed line. None at the end of input. */
  def ask(question: String): IO[Option[String]] =
    IO.blocking {
      System.err.print(question)
      System.err.flush
      readLine.map(_.trim)
    }.orIoError("prompt", "stdin")

  /**
   * Asks a yes/no question. y and yes are true, n and no are false, an empty answer is the default.
   * Any other answer asks again; the end of input is false.
   */
  def confirm(question: String, default: Boolean): IO[Boolean] =
    ask(question).flatMap:
      case None                                      => IO.pure(false)
      case Some("")                                  => IO.pure(default)
      case Some(a) if Set("y", "yes")(a.toLowerCase) => IO.pure(true)
      case Some(a) if Set("n", "no")(a.toLowerCase)  => IO.pure(false)
      case Some(_)                                   => confirm(question, default)

  /** Whether stdin is a terminal. */
  val isTerminal: IO[Boolean] = isTty(0)

  /**
   * Whether the file descriptor is a terminal: 0 for stdin, 1 for stdout, 2 for stderr. A child process
   * that inherits the streams checks it. If that check cannot run, the JVM console check is used.
   */
  def isTty(fd: Int): IO[Boolean] = IO.blocking {
    try
      val pb = new ProcessBuilder("test", "-t", fd.toString)
      pb.redirectInput(Redirect.INHERIT)
      pb.redirectOutput(Redirect.INHERIT)
      pb.redirectError(Redirect.INHERIT)
      pb.start.waitFor == 0
    catch
      case _: Exception => System.console != null
  }
}
