# polio

Config-file sync through a git repository. Everything is named `polio`: the
executable, the Scala package, the data directory `~/.local/share/polio` and the state directory
`~/.local/state/polio` (XDG base directories; `POLIO_HOME` puts both in one place),
the manifest `polio.json` in the clone, the AUR package and the platform npm
packages. The main npm package is `@dovaogedot/polio` because npm rejects the
bare name.

## Toolchain

Scala 3 with the `scala` runner (scala-cli) from a plain `project.scala`; no
sbt. GraalVM native-image builds the binary; the runner downloads GraalVM
itself. When the tools come from SDKMAN and are not on `PATH`:

```sh
export PATH="$HOME/.sdkman/candidates/scala/current/bin:$HOME/.sdkman/candidates/java/current/bin:$PATH"
export JAVA_HOME="$HOME/.sdkman/candidates/java/current"
```

## Commands

- `scala compile --test .` type-checks the sources and the tests.
- `scala fmt .` formats with `.scalafmt.conf`. Run it before committing.
- `scala run -q . -M polio.Install` builds the native binary into
  `~/.local/bin/polio`. The build takes minutes and needs about 3 GB of memory.
- `scala test .` runs the weaver suites. They run the installed binary, so
  rebuild it after every code change before testing. `POLIO_BIN=<path>` points
  them at another binary; `--test-only polio.ParkSuite` runs one suite.
- `polio version` prints `VERSION` from `Main.scala`.

## Layout

- `Main.scala`: the Decline command line, the global `-q` and `-s` flags, and
  the dispatch to the commands.
- `src/Config.scala`: `Layout` (the paths), `Manifest` (`polio.json`),
  `SyncState` (the per-host hashes), `Doc` (the JSON shape of both files).
- `src/Report.scala`: `Report`, what a command returns: `Row`s that render as a
  table, colored by `Tone`, with `Code` as the porcelain letters, and notes; `Style` carries the global output flags.
- `src/Errors.scala`: `PolioError`, the only failures the CLI reports, and
  `orIoError`, which wraps raw exceptions at the effect boundary.
- `src/Fs.scala`: file operations as extension methods on `fs2.io.file.Path`.
- `src/Stdin.scala`: unbuffered line reads from stdin and the terminal check the prompts use. Prompts print on stderr, so `-q` hides only results.
- `src/Git.scala`: the `Git` handle. Every git command is a named operation
  there; no other file spells git arguments.
- `src/Target.scala`: `Target`, the portable spelling of a tracked file's
  location (`~/...` or an absolute slash-separated path).
- `src/commands/`: one file per command. `Sync.scala` holds the three-way
  engine: `Facts`, `Detected`, `Plan`, `Outcome`, `SyncCtx`.
- `scripts/Install.scala`: the installer.
- `test/`: weaver suites and the `Sandbox` harness. A sandbox is a temporary
  home with its own polio data directory and bare remote; every process spawned
  through it sees that home. Terminal tests run the binary under `script`.
- `npm/`, `packaging/aur/`, `.github/workflows/release.yml`: publishing. A
  `v*` tag runs the workflow: native binaries for Linux and macOS, a GitHub
  release with checksums and a rendered `PKGBUILD`, and the npm packages through
  trusted publishing. `npm/publish-local.sh <version>` publishes a release's
  packages from a developer machine.

## Invariants

- Only `sync` talks to the remote, plus `bind` (and `init`, which calls it) for the clone. `add` and
  `remove` commit locally.
- A sync folds the local commits into one before it pulls, so a push carries the
  net change and `status` reports the paths that differ from the remote.
- A sync pushes only when the remote is missing commits.
- The host copy is the only side git history cannot restore. Every path that
  discards it is an explicit choice: `-y`, the menu, or a resolved parked copy.
- A conflict is parked under `~/.local/share/polio/conflicts/<repo path>` as a file with
  conflict markers. A parked copy wins over every mode until its markers are
  gone.
- A permission failure on a host copy prints the `sudo cp` command that
  applies the file by hand.
