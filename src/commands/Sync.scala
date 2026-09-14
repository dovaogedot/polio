package polio

import cats.effect.IO
import cats.syntax.all.*
import fs2.io.file.Path
import mouse.all.*

/** The result of the three-way comparison for one tracked file. */
private enum Detected {

  /** Both copies hold the same content. */
  case Clean

  /** The host copy changed since the last sync, or the repo has no copy yet. */
  case ToRepo

  /** The repo copy changed since the last sync, or the host has no copy. */
  case ToHost

  /** Both copies changed since the last sync. */
  case Conflict

  /** Neither side has the file. */
  case Missing
}

/** How one tracked file was handled. */
private enum Plan {

  /** Nothing to do: both copies already match. */
  case Clean

  /** The host copy was copied into the repo. */
  case ToRepo

  /** The repo copy was installed on the host. */
  case ToHost

  /** Both sides had changed. The host copy was kept and replaced the repo copy. */
  case Conflict

  /** Both sides had changed. The repo copy was kept and replaced the host copy. */
  case ConflictRepo

  /** Both sides had changed. A copy with conflict markers waits in conflictsDir, and both sides stay as they are. */
  case Parked

  /** A parked copy, fixed by hand, was applied to both sides. */
  case Resolved

  /** Neither side has the file. The manifest entry is stale. */
  case Missing
}

/** How a file that changed on both sides is resolved. */
enum ConflictMode {

  /** Ask for each file if stdin is a terminal. Otherwise keep the host copy. */
  case Ask

  /** Keep the host copy without asking. */
  case Force

  /** Keep the repo copy without asking. The host copy is replaced. */
  case Yolo
}

/** What is known about one tracked file: the hashes of both copies, and the hash recorded at the last sync. */
private final case class Facts(
  repoPath: String,
  target: Target,
  hostPath: Path,
  hostHash: Option[String],
  repoHash: Option[String],
  baseHash: Option[String],
) {

  /**
   * Three-way comparison with the hash recorded at the last sync. The side that did not change takes
   * the content of the side that changed. A change on both sides is a conflict.
   */
  def decide: Detected = (repoHash, hostHash) match
    case (None, None)                               => Detected.Missing
    case (None, Some(_))                            => Detected.ToRepo
    case (Some(_), None)                            => Detected.ToHost
    case (Some(repo), Some(host)) if repo == host   => Detected.Clean
    case (Some(repo), _) if baseHash.contains(repo) => Detected.ToRepo
    case (_, Some(host)) if baseHash.contains(host) => Detected.ToHost
    case _                                          => Detected.Conflict

  /** The outcome for this file after plan, with hash as the content both sides now have. */
  def outcome(plan: Plan, hash: Option[String]): Outcome = Outcome(plan, repoPath, target, hash)
}

private object Facts {

  /** Reads the hashes of both copies for one manifest entry, and the hash recorded at the last sync. */
  def gather(layout: Layout, state: SyncState, repoPath: String, target: Target): IO[Facts] = {
    val hostPath = target.expand(layout.home)
    for
      hostHash <- hostPath.sha256IfExists
      repoHash <- layout.repoFile(repoPath).sha256IfExists
    yield Facts(repoPath, target, hostPath, hostHash, repoHash, state.files.get(repoPath))
  }
}

/** What sync did to one tracked file. */
private final case class Outcome(
  plan: Plan,
  repoPath: String,
  target: Target,
  /** The hash both sides have after the action. None removes the state entry. */
  hash: Option[String],
) {

  /** The report line for this file. None if nothing changed. */
  def line(recover: Option[String], parkedAt: String): Option[String] = plan match
    case Plan.Clean    => None
    case Plan.ToRepo   => Some(s"host -> repo  $target")
    case Plan.ToHost   => Some(s"repo -> host  $target")
    case Plan.Conflict =>
      Some(
        s"host -> repo  $target (both sides changed: host copy kept)"
          + recover.map(r => s"\n  overwritten repo copy: $r").getOrElse(""),
      )
    case Plan.ConflictRepo =>
      Some(s"repo -> host  $target (both sides changed: repo copy kept, host copy overwritten)")
    case Plan.Parked =>
      Some(s"parked        $target (resolve $parkedAt, then polio sync; or polio sync --abort)")
    case Plan.Resolved =>
      Some(s"resolved      $target (parked copy applied to both sides)")
    case Plan.Missing =>
      Some(s"missing       $target (gone on host and in repo; polio remove to untrack)")
}

/** The choices the conflict menu offers. */
private enum Choice {

  /** Keep the host copy. The repo copy stays in git history. */
  case Local

  /** Keep the repo copy. The host copy is replaced. */
  case Repo

  /** Park a copy with conflict markers, to fix by hand. Both sides stay as they are. */
  case Skip
}

private object Choice {

  /** The choice for each accepted menu input. */
  val byInput: Map[String, Choice] = Map(
    "l"     -> Local,
    "local" -> Local,
    "r"     -> Repo,
    "repo"  -> Repo,
    "s"     -> Skip,
    "skip"  -> Skip,
  )

  /** The prompt text with the choices for target. */
  def menu(target: Target): String =
    s"conflict: $target changed both on this host and in the repo\n"
      + "  [l] keep local — the host copy wins; the repo copy stays in git history\n"
      + "  [r] keep repo  — overwrites the host copy\n"
      + "  [s] skip — park a conflict-marked copy to resolve by hand; both sides stay"

  /** Reads one choice from the terminal. End of input counts as skip. */
  def ask(target: Target): IO[Choice] =
    IO.blocking {
      System.err.println(menu(target))
      var chosen: Option[Choice] = None
      var eof                    = false
      while chosen.isEmpty && !eof do
        System.err.print("choose [l/r/s]: ")
        System.err.flush
        Stdin.readLine match
          case None        => eof = true
          case Some(input) => chosen = byInput.get(input.trim.toLowerCase)
      chosen.getOrElse(Skip)
    }.orIoError("prompt", target.value)
}

extension (text: String) {
  private def hasConflictMarkers: Boolean = text.split("\n", -1).exists(_.startsWith("<<<<<<<"))
}

extension (src: Path) {

  /** Copies to a host path. On a permission error, the message names the sudo command that does the copy by hand. */
  private def copyToHost(hostPath: Path): IO[Unit] =
    src.copyTo(hostPath).adaptError:
      case e: PolioError.Io if e.cause == "permission denied" =>
        PolioError.Io(e.op, e.path, s"permission denied — run: sudo cp $src $hostPath")
}

/** The values that stay the same during one sync run and that reconciling a file needs. */
private final case class SyncCtx(layout: Layout, mode: ConflictMode, interactive: Boolean) {

  /**
   * Writes a merge of the host and repo copies, with conflict markers, to the parked path. The merge
   * base is empty, so equal lines pass through and different regions become marked conflicts. Both
   * originals stay as they are.
   */
  def park(facts: Facts): IO[Unit] = {
    val base   = layout.root / "tmp-merge-base"
    val merged = Git.anywhere.mergeFile(
      ours = MergeSide(facts.hostPath, s"host: ${facts.target}"),
      base = MergeSide(base, "base: empty"),
      theirs = MergeSide(layout.repoFile(facts.repoPath), s"repo: files/${facts.repoPath}"),
    )
    for
      _    <- base.writeText("")
      text <- merged
      _    <- layout.parkedFile(facts.repoPath).writeText(text)
      _    <- base.removeIfExists
    yield ()
  }

  /**
   * Resolves a file that changed on both sides. Force keeps the host copy, Yolo the repo copy. Ask lets
   * the user choose for each file, but keeps the host copy if stdin is not a terminal. The host copy is
   * the only side git history cannot restore, so it is discarded only by an explicit choice.
   */
  def resolveConflict(facts: Facts): IO[Outcome] = {
    val repoFile  = layout.repoFile(facts.repoPath)
    val keepLocal = facts.hostPath.copyTo(repoFile).as(facts.outcome(Plan.Conflict, facts.hostHash))
    val keepRepo  = repoFile.copyToHost(facts.hostPath).as(facts.outcome(Plan.ConflictRepo, facts.repoHash))
    mode match
      case ConflictMode.Force => keepLocal
      case ConflictMode.Yolo  => keepRepo
      case ConflictMode.Ask   =>
        if !interactive then
          keepLocal
        else
          Choice.ask(facts.target).flatMap:
            case Choice.Local => keepLocal
            case Choice.Repo  => keepRepo
            case Choice.Skip  =>
              park(facts).as(facts.outcome(Plan.Parked, facts.baseHash))
  }

  /**
   * Reconciles one file. A parked copy comes first, in every mode. If its conflict markers are gone,
   * the user fixed it, and the content goes to both sides. If the markers are still there, the
   * conflict stays parked and the user is not asked again. A parked copy is dropped if its conflict no
   * longer exists.
   */
  def reconcile(facts: Facts): IO[Outcome] = {
    val repoFile   = layout.repoFile(facts.repoPath)
    val parkedFile = layout.parkedFile(facts.repoPath)
    def fresh: IO[Outcome] = facts.decide match
      case Detected.Clean    => IO.pure(facts.outcome(Plan.Clean, facts.hostHash))
      case Detected.Missing  => IO.pure(facts.outcome(Plan.Missing, None))
      case Detected.Conflict => resolveConflict(facts)
      case Detected.ToHost   =>
        repoFile.copyToHost(facts.hostPath).as(facts.outcome(Plan.ToHost, facts.repoHash))
      case Detected.ToRepo =>
        facts.hostPath.copyTo(repoFile).as(facts.outcome(Plan.ToRepo, facts.hostHash))
    parkedFile.readTextIfExists.flatMap:
      case None         => fresh
      case Some(parked) =>
        if !parked.hasConflictMarkers then
          for
            hash <- parkedFile.sha256IfExists

            _ <- parkedFile.copyToHost(facts.hostPath)
            _ <- parkedFile.copyTo(repoFile)
            _ <- parkedFile.removeIfExists
          yield facts.outcome(Plan.Resolved, hash)
        else if facts.decide == Detected.Conflict then
          IO.pure(facts.outcome(Plan.Parked, facts.baseHash))
        else
          parkedFile.removeIfExists *> fresh
  }
}

/** The report for one run: one line per changed file, the count of files that are up to date, and the push status. */
private def summarize(
  layout: Layout,
  outcomes: List[Outcome],
  pushed: Boolean,
  pushWarning: Option[String],
  preSync: Option[String],
): String = {
  val repoDisplay      = layout.display(layout.repo)
  val conflictsDisplay = layout.display(layout.conflictsDir)
  val fileLines        = outcomes.flatMap { outcome =>
    val recover = preSync.filter(_ => outcome.plan == Plan.Conflict).map { ref =>
      val spec   = s"$ref:files/${outcome.repoPath}"
      val quoted = if spec.exists(_.isWhitespace) then s"\"$spec\"" else spec
      s"git -C $repoDisplay show $quoted"
    }
    outcome.line(recover, conflictsDisplay + "/" + outcome.repoPath)
  }
  val clean          = outcomes.count(_.plan == Plan.Clean)
  val cleanLine      = Option.when(clean > 0)(s"up to date: $clean file(s)").toList
  val nothingTracked = outcomes.isEmpty.option("nothing tracked — polio add <path>").toList
  val pushLine       = pushWarning.orElse(pushed.option("pushed")).toList
  val lines          =
    fileLines
      ::: cleanLine
      ::: nothingTracked
      ::: pushLine
  lines.mkString("\n")
}

/** polio sync: pulls, reconciles every tracked file with the host using mode, commits, and pushes. Returns the report. */
def sync(mode: ConflictMode): IO[String] =
  for
    layout      <- Layout.resolve
    _           <- layout.requireBound
    interactive <- Stdin.isTerminal
    repo = Git.in(layout.repo)
    branch   <- repo.currentBranch
    _        <- repo.pull(branch)
    manifest <- Manifest.load(layout)
    state    <- SyncState.load(layout)

    ctx = SyncCtx(layout, mode, interactive)

    outcomes <- manifest.files.toList.sortBy(_._2).traverse: (repoPath, target) =>
      Facts.gather(layout, state, repoPath, target) >>= ctx.reconcile

    hashes = outcomes.flatMap: o =>
      o.hash.map(o.repoPath -> _)

    _         <- SyncState(hashes.toMap).save(layout)
    committed <- repo.commitIfChanged(s"polio: sync from $hostLabel")

    // The sync commit's parent holds the repo copies that conflicts
    // overwrote; its hash pins the printed retrieval command.
    preSync <-
      if committed && outcomes.exists(_.plan == Plan.Conflict)
      then repo.parentOfHead
      else IO.pure(None)

    // Push only when the remote lacks commits: a new one from this sync, or
    // one stranded by an earlier failed push.
    pending <-
      if committed
      then IO.pure(true)
      else repo.pendingPushes(branch).map(_ > 0)
    warning <-
      if pending
      then repo.pushBestEffort
      else IO.pure(None)
  yield summarize(layout, outcomes, pending && warning.isEmpty, warning, preSync)

/** polio sync --abort: discards every parked conflict. Host and repo copies stay as they are. */
def syncAbort: IO[String] =
  for
    layout <- Layout.resolve
    there  <- layout.conflictsDir.isPresent

    message <-
      if !there then
        IO.pure("no parked conflicts")
      else
        layout.conflictsDir.walkFiles.flatMap: files =>
          layout.conflictsDir.removeTreeIfExists.as:
            if files.isEmpty
            then "no parked conflicts"
            else s"discarded ${files.length} parked conflict(s)"
  yield message
