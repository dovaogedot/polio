package polio

import cats.effect.IO
import cats.syntax.all.*
import mouse.all.*
import fs2.io.file.Path
import io.circe.syntax.*
import io.circe.{Codec, Printer, parser}
import scala.collection.immutable.SortedMap

/** The data directory layout, the committed manifest, and the sync state of this host. */

/** Reads an environment variable. None if it is not set. */
def envGet(name: String): Option[String] = sys.env.get(name)

/** The home directory of the user. A Config error if the environment does not name one. */
def homeDir: Either[PolioError, Path] = {
  val raw  = envGet("HOME") <+> envGet("USERPROFILE")
  val home = raw.filter(_.nonEmpty).map(Path(_).normalize)
  home.toRight(PolioError.Config("cannot locate the home directory: HOME / USERPROFILE is unset or unreadable"))
}

/** Where polio keeps its data on this host. */
final case class Layout(
  /** User home directory. */
  home: Path,
  /** Data root that holds the clone and the parked copies: POLIO_HOME, or the polio directory under XDG_DATA_HOME. */
  root: Path,
  /** The git clone that holds the manifest and the files. */
  repo: Path,
  /** The tracked file contents, stored by repo path. */
  filesDir: Path,
  /** The committed manifest inside the repo. */
  manifestPath: Path,
  /** The state of this host: POLIO_HOME, or the polio directory under XDG_STATE_HOME. It is never committed. */
  statePath: Path,
  /** Parked conflict copies that wait for a manual fix, stored by repo path. */
  conflictsDir: Path,
  /** Whether this host looks for a newer polio. It sits beside the state and is never committed. */
  updatePath: Path,
) {

  /** The repo copy of the file tracked at repoPath. */
  def repoFile(repoPath: String): Path = filesDir / repoPath

  /** The parked conflict copy of the file tracked at repoPath. */
  def parkedFile(repoPath: String): Path = conflictsDir / repoPath

  /** The .git directory of the repo. If it exists, the repo is cloned. */
  def gitDir: Path = repo / ".git"

  /** Whether the repo is cloned. */
  def isBound: IO[Boolean] = gitDir.isPresent

  /** Fails if the repo is not cloned or has no origin remote. */
  def requireBound: IO[Unit] = {
    val checkRemote = Git.in(repo).originUrl.void.adaptError:
      case _ => PolioError.Config("no remote configured — run: polio bind <repo>")
    isBound.flatMap: bound =>
      IO.raiseUnless(bound)(PolioError.Config("not bound — run: polio bind <repo>"))
        *> checkRemote
  }

  /** The absolute location for a path the user typed. A leading "~" means the home directory. */
  def locate(raw: String): Path =
    if raw == "~" then
      home
    else if raw.startsWith("~/") then
      home / raw.drop(2)
    else
      Path(raw).absolute.normalize

  /** A location as shown to the user. A location under home is shown as "~/...". */
  def display(location: Path): String = Target.contract(location, home).value
}

object Layout {

  /**
   * The layout of this host. POLIO_HOME puts everything under one directory. Otherwise the clone and the
   * parked copies live in XDG_DATA_HOME/polio and the host state in XDG_STATE_HOME/polio, with the XDG
   * defaults under home. A clone left in ~/.polio is copied over once, with a warning.
   */
  def resolve: IO[Layout] =
    IO.fromEither(homeDir).flatMap { home =>
      envGet("POLIO_HOME").filter(_.nonEmpty) match
        case Some(raw) =>
          val root = Path(raw).absolute.normalize
          IO.pure(under(home, root, root))
        case None =>
          val layout =
            under(home, xdg(home, "XDG_DATA_HOME", ".local/share"), xdg(home, "XDG_STATE_HOME", ".local/state"))
          layout.adoptOldData.as(layout)
    }

  /** The polio directory inside the base directory that the XDG variable names, or inside its default under home. */
  private def xdg(home: Path, variable: String, default: String): Path = {
    val base = envGet(variable).filter(_.nonEmpty).fold(home / default)(Path(_).absolute.normalize)
    base / "polio"
  }

  /** The layout with the clone and the parked copies under root and the host state under stateDir. */
  private def under(home: Path, root: Path, stateDir: Path): Layout =
    Layout(
      home = home,
      root = root,
      repo = root / "repo",
      filesDir = root / "repo/files",
      manifestPath = root / "repo/polio.json",
      statePath = stateDir / "state.json",
      conflictsDir = root / "conflicts",
      updatePath = stateDir / "update.json",
    )
}

extension (layout: Layout) {

  /**
   * Copies a clone found in ~/.polio into the data and state directories and warns on stderr that ~/.polio
   * is not used. A ~/.polio without a clone, or an existing data root, leaves everything as it is.
   */
  private def adoptOldData: IO[Unit] = {
    val old         = layout.home / ".polio"
    val copiedState = layout.root / "state.json"
    val warning     =
      s"polio: $old is no longer used; its data was copied to ${layout.root}"
        + s" and ${layout.statePath.parent.get}. Remove it with: rm -rf $old"
    val moveState = copiedState.isPresent >>= copiedState.moveTo(layout.statePath).whenA
    val adopt     = old.copyTreeTo(layout.root)
      *> moveState
      *> IO.blocking(System.err.println(warning))
    val pending = ((old / "repo/.git").isPresent, layout.root.isPresent).mapN(_ && !_)
    pending >>= adopt.whenA
  }
}

/** The shape of polio.json and state.json on disk. */
private final case class Doc(version: Int, files: Map[String, String]) derives Codec.AsObject

private object Doc {

  /** Prints JSON with a two-space indent and no space before the colon. */
  val printer = Printer.spaces2.copy(colonLeft = "")

  /**
   * The files listed in the document text. A broken document or an unsupported version is a Config
   * error that names what.
   */
  def decode(text: String, what: String): Either[PolioError, Map[String, String]] =
    for
      doc <- parser.decode[Doc](text).leftMap: e =>
        PolioError.Config(s"$what: ${e.getMessage}")

      files <- Either.cond(doc.version == 1, doc.files, PolioError.Config(s"$what: unsupported version ${doc.version}"))
    yield files

  /** The document text for the files. Keys are sorted, and the text ends with a newline. */
  def render(files: Map[String, String]): String =
    SortedMap.from(files)
      |> (Doc(1, _).asJson)
      |> printer.print
      |> (_ + "\n")
}

/**
 * The manifest. It is committed at the repo root as polio.json and shared by every host. It maps each
 * repo path under files/ to the target where the file is installed.
 */
final case class Manifest(files: Map[String, Target]) {

  /** Whether the target is tracked. */
  def tracks(target: Target): Boolean = files.contains(target.repoPath)

  /** Writes the manifest into the repo. */
  def save(layout: Layout): IO[Unit] =
    files.view.mapValues(_.value).toMap
      |> Doc.render
      |> layout.manifestPath.writeText

  /** The targets that the other manifest tracks and this one does not. */
  def droppedFrom(other: Manifest): List[Target] =
    other.files.toList.collect:
      case (repoPath, target) if !files.contains(repoPath) => target
}

object Manifest {

  /** A manifest tracking nothing. */
  val empty: Manifest = Manifest(Map.empty)

  /** Decodes manifest text. A broken document or an unsupported version is a Config error. */
  def parse(text: String): Either[PolioError, Manifest] =
    Doc.decode(text, "polio.json").map: files =>
      Manifest(files.view.mapValues(Target(_)).toMap)

  /** The manifest in the repo. A Config error if there is none. */
  def load(layout: Layout): IO[Manifest] = {
    val missing = PolioError.Config(s"no manifest at ${layout.manifestPath} — run: polio bind <repo>")
    layout.manifestPath.readTextIfExists.flatMap:
      case None       => IO.raiseError(missing)
      case Some(text) => IO.fromEither(parse(text))
  }

  /** The manifest on the origin branch. Empty if the branch was never pushed or the text does not decode. */
  def atOrigin(layout: Layout, branch: String): IO[Manifest] =
    Git.in(layout.repo).show(s"origin/$branch", "polio.json").map: text =>
      text.flatMap(parse(_).toOption).getOrElse(empty)
}

/** The record of this host: for each file, the content hash both sides had after the last sync. */
final case class SyncState(files: Map[String, String]) {

  /** Writes the state for this host. */
  def save(layout: Layout): IO[Unit] =
    Doc.render(files)
      |> layout.statePath.writeText
}

object SyncState {

  /** The state before any sync. */
  val empty: SyncState = SyncState(Map.empty)

  /**
   * The state of this host. A missing or invalid state file gives the empty state, because the state
   * is only a cache.
   */
  def load(layout: Layout): IO[SyncState] =
    layout.statePath.readTextIfExists
      .map { text =>
        val decoded = text.flatMap: t =>
          Doc.decode(t, "state.json").toOption
        decoded.fold(empty)(SyncState(_))
      }
      .handleError(_ => empty)
}

/** The host name used in sync commit messages. */
def hostLabel: String =
  try
    java.net.InetAddress.getLocalHost.getHostName
  catch
    case _: Exception =>
      val fallback = envGet("HOSTNAME") <+> envGet("COMPUTERNAME")
      fallback.getOrElse("unknown-host")
