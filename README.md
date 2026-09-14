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
polio init                                  # guided setup: asks for the remote, offers a sync on shell start
polio bind git@github.com:you/dotfiles.git  # the same without questions; clones into ~/.local/share/polio/repo
polio add ~/.bashrc                         # track a file (a directory tracks every file inside)
polio sync                                  # pull, reconcile, push (-f: conflicts keep the host copy, -y: the repo copy)
polio sync --abort                          # discard parked conflicts; both sides stay as they are
polio status                                # every tracked file and what sync would do; reads local state only
polio -q <command>                          # --quiet: suppress stdout; -s / --shush suppresses stderr too
polio remove ~/.bashrc                      # untrack (the host copy stays)
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
