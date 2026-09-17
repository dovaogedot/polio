package polio

import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream
import fs2.hashing.{Hash, HashAlgorithm, Hashing}
import fs2.io.file.{CopyFlag, CopyFlags, Files, Path, PosixPermissions}
import java.nio.file.{AccessDeniedException, NoSuchFileException}

/** File system operations built on fs2 Files. Failures are reported as PolioError.Io. */

enum FileKind {

  /** Nothing exists at the path. */
  case Missing

  /** A regular file, or a symlink to one. */
  case RegularFile

  /** A directory, or a symlink to one. */
  case Directory
}

extension (hash: Hash) {

  /** The hash bytes as a lowercase hex string. */
  private def hex: String = hash.bytes.toArray.map(b => f"${b & 0xff}%02x").mkString
}

extension (path: Path) {

  /** What exists at the path: nothing, a regular file, or a directory. */
  def fileKind: IO[FileKind] = {
    val present = Files[IO].isDirectory(path).ifF(FileKind.Directory, FileKind.RegularFile)
    Files[IO].exists(path).ifM(present, IO.pure(FileKind.Missing)).orIoError("stat", path.toString)
  }

  /** Whether anything exists at the path. */
  def isPresent: IO[Boolean] = Files[IO].exists(path).orIoError("stat", path.toString)

  /** The content of the file as UTF-8 text. None if the file is missing. */
  def readTextIfExists: IO[Option[String]] =
    Files[IO]
      .readUtf8(path)
      .compile
      .string
      .map(Some(_))
      .recover { case _: NoSuchFileException => None }
      .orIoError("read", path.toString)

  /** Creates the directory and any missing parents. */
  def ensureDir: IO[Unit] = Files[IO].createDirectories(path).orIoError("mkdir", path.toString)

  /** Writes the text as UTF-8 and creates missing parent directories. An existing file is replaced. */
  def writeText(text: String): IO[Unit] = {
    val write = Stream.emit(text).through(Files[IO].writeUtf8(path)).compile.drain
    path.parent.traverse_(_.ensureDir)
      *> write.orIoError("write", path.toString)
  }

  /** Moves the file or directory to dst. The parent of dst is created. Fails if dst exists. */
  def moveTo(dst: Path): IO[Unit] = {
    val move = Files[IO].move(path, dst)
    dst.parent.traverse_(_.ensureDir)
      *> move.orIoError("move", path.toString)
  }

  /** Copies the directory and everything inside it to dst. Files that already exist in dst are replaced. */
  def copyTreeTo(dst: Path): IO[Unit] = {
    def copyEntry(entry: Path): IO[Unit] = {
      val target = dst / path.relativize(entry)
      Files[IO].isDirectory(entry).ifM(target.ensureDir, entry.copyTo(target))
    }
    val entries = Files[IO].walk(path).compile.toList.orIoError("walk", path.toString)
    entries.flatMap(_.traverse_(copyEntry))
  }

  /** Copies the file content. On POSIX systems it also copies the file attributes. */
  def copyTo(dst: Path): IO[Unit] = {
    val flags = CopyFlags(CopyFlag.ReplaceExisting, CopyFlag.CopyAttributes)
    val copy  = Files[IO]
      .copy(path, dst, flags)
      .adaptError { case _: AccessDeniedException => PolioError.Io("copy", s"$path -> $dst", "permission denied") }
      .orIoError("copy", s"$path -> $dst")
    dst.parent.traverse_(_.ensureDir)
      *> copy
  }

  /** The permissions of the file. None if nothing is there, or if the file system has no POSIX permissions. */
  def permissionsIfExists: IO[Option[PosixPermissions]] =
    Files[IO]
      .getPosixPermissions(path)
      .map(_.some)
      .recover { case _: NoSuchFileException | _: UnsupportedOperationException => None }
      .orIoError("stat", path.toString)

  /** Sets the permissions of the file. */
  def setPermissions(mode: PosixPermissions): IO[Unit] =
    Files[IO].setPosixPermissions(path, mode).orIoError("chmod", path.toString)

  /** Deletes the file if it exists. */
  def removeIfExists: IO[Unit] = Files[IO].deleteIfExists(path).void.orIoError("remove", path.toString)

  /** Deletes the directory and everything inside it, if it exists. */
  def removeTreeIfExists: IO[Unit] =
    Files[IO]
      .deleteRecursively(path)
      .recover { case _: NoSuchFileException => () }
      .orIoError("remove", path.toString)

  /** Lists every regular file under this directory, sorted. Symlinks and ".git" directories are skipped. */
  def walkFiles: IO[List[Path]] = {
    def outsideGitTree(p: Path): Boolean =
      !path.relativize(p).names.dropRight(1).exists(_.toString == ".git")
    Files[IO]
      .walkWithAttributes(path)
      .filter(info => info.attributes.isRegularFile && !info.attributes.isSymbolicLink)
      .map(_.path)
      .filter(outsideGitTree)
      .compile
      .toList
      .map(_.sortBy(_.toString))
      .orIoError("walk", path.toString)
  }

  /** The SHA-256 hash of the file as a hex string. None if the file is missing. */
  def sha256IfExists: IO[Option[String]] =
    Files[IO]
      .readAll(path)
      .through(Hashing[IO].hash(HashAlgorithm.SHA256))
      .compile
      .lastOrError
      .map(_.hex.some)
      .recover { case _: NoSuchFileException => None }
      .orIoError("hash", path.toString)

  /** The SHA-256 hash of the file as a hex string. A missing file is an Io error. */
  def sha256: IO[String] = {
    val missing = PolioError.Io("hash", path.toString, "no such file")
    sha256IfExists.flatMap(IO.fromOption(_)(missing))
  }
}
