# polio

Sync config files between machines through a git repository. One binary, one
command per action, and git is the only thing it needs at run time.

## Install

With npm, on Linux (x64, arm64) or macOS (arm64, x64):

```sh
npm install -g @dovaogedot/polio    # or run it without installing: npx @dovaogedot/polio status
```

Without Node, download the binary for your platform from the GitHub releases
and put it on your `PATH`.

## Use

```sh
polio init                                  # guided setup: remote, common config files to track, sync on shell start
polio bind git@github.com:you/dotfiles.git  # the same without questions; clones into ~/.local/share/polio/repo
polio add ~/.bashrc                         # track a file (a directory tracks every file inside)
polio suggest                               # pick common config files to track from a list
polio sync                                  # pull, reconcile, push (-f: conflicts keep the host copy, -y: the repo copy)
polio sync --abort                          # discard parked conflicts; both sides stay as they are
polio status                                # every tracked file and what sync would do; reads local state only
polio -q <command>                          # --quiet: suppress stdout; -s / --shush suppresses stderr too
polio remove ~/.bashrc                      # untrack (the host copy stays)
polio updates off                           # stop the check for a newer polio (on / no argument shows the setting)
```

With two machines:

```sh
# machine A
polio init
polio add ~/.bashrc
polio sync                          # pushes ~/.bashrc

# machine B
polio init                          # answer with the same remote
polio sync                          # ~/.bashrc arrives
vim ~/.bashrc                       # edit the tracked file
vim ~/.config/git/config
polio add ~/.config/git/config
polio sync                          # pushes both files

# machine A
polio sync                          # both files arrive
```

## How it works

Your config files stay where they are. polio keeps a clone of your git
repository in `~/.local/share/polio` (`XDG_DATA_HOME`) with a copy of every tracked file, and a
manifest, `polio.json`, that says where each file lives on a host. Paths under
the home directory are stored as `~/...`, so hosts with different user names
share one manifest.

`polio sync` pulls the repository, then compares each tracked file on the host
with the copy in the repository, using the content hash saved at the last sync
to tell which side changed:

- One side changed: that side wins, and the other copy is updated.
- Both sides changed: polio asks for each file. Keep the local copy, keep the
  repository copy, or skip. `-f` keeps the local copy without asking, and so
  does a run without a terminal. `-y` keeps the repository copy without
  asking. A replaced repository copy stays in git history; sync prints the
  command that shows it.
- Skip parks a copy with conflict markers under `~/.local/share/polio/conflicts` and leaves
  both sides alone. Edit it until the markers are gone; the next sync applies
  it to both sides. `polio sync --abort` throws the parked copies away.

A sync also looks for a newer polio, once a day, and prints a line when one is
out:

```
polio 0.5.0 is out — run: npm install -g @dovaogedot/polio@latest
```

The line appears on every sync until you upgrade. The check comes with the npm
package and asks the npm registry, so an install from the AUR or a release
binary never sees it. `polio updates off` stops it and this host remembers that,
`polio updates on` starts it again, and `polio updates` shows the setting.

The check sends nothing about you and never holds a sync up: it runs while polio
works, and an answer that has not arrived by the time polio is done is dropped
and asked for again an hour later. A very short sync can outrun the answer, so a
new release sometimes shows up a sync or two after it appears.

It asks whichever registry npm would install from, so a `registry` line in your
`.npmrc` is followed. The first sync after an install only starts the clock:
what you just installed is the newest there is.

polio syncs content, not permissions. Git records only whether a file is
executable, so a mode like `600` never travels between hosts. A host file that
is already there keeps its permissions. A host file that polio creates takes the
permissions of the repository copy, which git wrote under the umask of that
host.

Changes are committed after each sync, and pushed only when the remote is
behind. `add` and `remove` commit locally; the next sync folds those commits
into one and pushes it, so an add and a remove of the same file push nothing.
`sync` is the
only command that talks to the remote, besides the clone made by `bind` and
`init`.

`init` can add a line to your shell profile that runs `polio sync -q` when a
shell starts. The sync runs before the prompt appears, so every new shell waits
for the pull. Use it with an ssh remote and a key; an https remote asks for a
login on every sync.

## Yet another?

Ordered by similarity.

| Why not | Because |
|---|---|
| chezmoi | Edits go to chezmoi's source directory, and `chezmoi apply` writes them to your home. An edit made directly to `~/.bashrc` is drift: the next `apply` wants to overwrite it, and it survives only if you `chezmoi add` it first. With polio you edit `~/.bashrc` itself and `polio sync` carries it to the other machines. |
| yadm, vcsh | Home becomes a git worktree; conflicts land in live files. polio keeps its clone in `~/.local/share/polio`. |
| dotr, dotdrop, dotter | Deploy from a repository, with profiles and templates to learn; edits come back as a separate step, or not at all. polio syncs in one command. |
| stow, dotbot, rcm | They link files into place; syncing between machines stays your job. |
