# gyre

A modal text editor in the spirit of Helix, written in Common Lisp, with a
Common Lisp plugin system and an Emacs-style client/server model.

This is a single-user project. There are no external users, no released
versions and no compatibility promises.

## Ground rules

- **Backwards compatibility is not a concern.** Rename, restructure and delete
  freely. When an interface changes, update every caller in the same change.
  No deprecation shims, compatibility aliases, feature flags, config
  migrations or "old behavior" switches.
- **Delete dead code.** Don't comment it out or leave it "for later"; git has it.
- **The plugin API can break.** Plugins are the user's own code. Change the API
  when a better shape appears and fix the plugins in the same change.
- **SBCL only.** Use `sb-ext`, `sb-thread`, `sb-posix`, `sb-alien`,
  `sb-bsd-sockets`, `sb-concurrency` and `sb-sprof` directly. Don't write
  portability layers or `#+`/`#-` branches for other implementations.
- **Platforms:** macOS on Apple Silicon (`aarch64-apple-darwin`) and Linux
  x86_64. Unix-only APIs are fine. No Windows.
- **Few dependencies.** Prefer the SBCL contribs and code written here. Add a
  third-party library only when it saves substantial work, and ask before
  adding one. No Quicklisp/qlot workflow. Third-party code is vendored under
  `vendor/` (git submodules at pinned commits) and found through the ASDF
  source registry.

## Build and run

- SBCL is installed via Homebrew.
- Build an executable: `sbcl --non-interactive --eval '(asdf:make "gyre")'`
- Run tests: `sbcl --non-interactive --eval '(asdf:test-system "gyre")'`
- Development is REPL-driven: load the system into a running image and
  redefine functions in place rather than doing full rebuilds.

## Project layout

- `gyre.asd` uses `:class :package-inferred-system`. Each file is its own
  package named after its path (`gyre/term/input` lives in
  `src/term/input.lisp`), and ASDF derives dependencies from `defpackage`.
- `gyre.asd` contains only system definitions: no other code and no changes to
  ASDF's configuration.
- Tests live in `t/` and mirror the `src/` layout, defined by a separate
  `gyre/tests` system.

## Architecture

### Client/server

- One server process owns all editor state: buffers, language servers,
  registers and plugins. `gyre` starts the server if none is running, then
  connects to it as a client.
- The client passes its terminal file descriptor to the server over a Unix
  socket (`SCM_RIGHTS`), and the server draws directly to that terminal.
  There is no rendering protocol; the client is a thin launcher.
- The client forwards `SIGWINCH`, `SIGTSTP` and `SIGCONT` to the server over
  the socket. The server suspends the client's process group for ctrl-z.
- The socket lives in `$XDG_RUNTIME_DIR` (Linux) or `$TMPDIR` (macOS). Keep
  the socket path short: macOS limits it to 104 bytes.
- **Version handshake:** the client sends a build identifier first. On a
  mismatch the server refuses the connection, and the client tells the user
  to restart the server or restarts it itself. There is no protocol version
  negotiation.
- `MSG_CMSG_CLOEXEC` doesn't exist on macOS. After receiving a descriptor
  there, set `FD_CLOEXEC` with `fcntl`.
- Client lifecycle: `:q` closes only the current client. Stopping the server
  is a separate command.

### Shared vs per-client state

- **Per client:** window/view layout, mode, count prefix, selected register,
  last motion, working directory, terminal size and capabilities.
- **Shared:** documents, undo history (it belongs to the document, so undo
  can revert another client's edit), registers, language servers, plugins and
  config.
- When adding state, decide which side it belongs on and put it there
  explicitly. Don't add globals that silently become shared.

### Threads

- One editor thread owns all editor state and runs the event loop.
- Socket I/O, language server I/O, file watching and similar work run on
  other threads. They communicate with the editor thread only by sending
  messages through an `sb-concurrency` mailbox, never by touching editor
  state.
- Editor state is never protected by locks.

### Fault isolation

- A server crash loses every client's unsaved work, so the server must
  survive errors in commands and plugins.
- Each command and plugin hook runs inside a `handler-case` on `error` at the
  dispatch boundary. The error is reported in that client's status line and
  logged. Don't catch `serious-condition` or `t`.
- Never use `(safety 0)` in code that plugins can reach.

### Terminal layer

- Written in-house, with no ncurses. It covers raw mode via termios, input
  decoding (kitty keyboard protocol / `CSI u`, legacy sequences, SGR mouse,
  bracketed paste) and a cell-grid renderer that
  diffs the previous frame against the new one and writes only the changes.
- Every terminal object wraps a specific file descriptor, not
  `/dev/tty` or stdin. That is what lets one server drive many clients'
  terminals.
- libghostty-vt (via `sb-alien`) is a candidate for an embedded terminal pane
  and for a test harness that checks the actual bytes on screen. Don't use it
  for the editor's own UI.

### Terminal support

gyre must work in **tmux** (the usual setup), **Ghostty** and **kitty**, with
no special tmux configuration. Helix is the reference here
(`helix-tui/src/backend/termina.rs` in `~/helix`).

- **Detect features by querying the terminal, never from `TERM`.** Detection
  happens per client, when it connects, and the results are per-client state.
- **Keyboard:** query for the kitty keyboard protocol and enable it if
  supported; otherwise use legacy encoding, with a short timeout to tell Esc
  apart from the start of a sequence. Default tmux delivers legacy keys, so
  **default keybindings must work with legacy encoding.** Also decode `CSI u`
  keys whenever they arrive (tmux sends them with `extended-keys` on).
- **Synchronized output** (mode 2026): use it when the terminal reports
  support. Rendering must look correct without it.
- **Resize:** the client forwards `SIGWINCH` (see Client/server).
- **Clipboard:** use the system clipboard tools (`pbcopy`, `wl-copy`, etc.),
  falling back to OSC 52.

### Editing model

- Helix/Kakoune style: select first, then act. When it's unclear how a
  command should behave, Helix's behavior is the reference. The user's Helix
  fork is at `~/helix` (commands are in `helix-term/src/commands.rs`).

### Plugins

- Plugins are Lisp code loaded into the server image.
- Extension points (commands, keymaps, hooks, modes) are generic functions or
  explicit registries with documented contracts. Plugins don't reach into
  core internals with `::`.
- Plugin code has to be reloadable like the rest of the system (see the next
  section).

## Code that survives reloading

The server runs for a long time and code is redefined in place, so every
top-level form must be safe to evaluate again.

- Long-lived state goes in `defvar` or `sb-ext:defglobal`, never in
  `defparameter`, because reloading a file must not reset running state.
- Loading a file never starts threads, opens sockets or does I/O. That happens
  only in explicit entry functions such as `start-server` or `main`.
- Registries are keyed by name, so re-registering replaces the entry instead
  of adding a duplicate.
- Prefer `defstruct` for hot data but remember that redefining a struct's
  slots invalidates existing instances. Plan for a server restart when struct
  layouts change.

## Style

Based on the Google Common Lisp Style Guide and lisp-lang.org, adjusted for
this project.

- **Naming:** lowercase-with-hyphens. `*special*`, `+constant+`. Predicates
  end in `p` for one word (`emptyp`) and `-p` for several (`buffer-modified-p`).
  Name things by intent, not by type (`lines`, not `line-list`). No package
  prefixes in symbol names.
- **Packages:** one per file. Use `:import-from` instead of `:use` (except
  `:cl`). Use `:local-nicknames` for long package names. No `::` into another
  package outside tests.
- **Formatting:** spaces, 2-space bodies, standard SLIME-style indentation,
  lines up to 100 columns, no closing parens on their own lines. Keep
  top-level forms short; split anything much over 40 lines.
- **Comments:** `;;;;` file header, `;;;` section, `;;` inside a form, `;`
  end of line. Docstrings on exported functions, macros, classes and
  generic functions describe the contract, not the implementation.
- **Control flow:** `when`/`unless` for one branch, `if` for two, `cond` for
  more. `ecase`/`etypecase` over `case`/`typecase`.
- **Data:** `defstruct` for core data, CLOS at extension points. Lists only
  for small or sequential data; use vectors for random access and hash tables
  for lookups. Don't pass heterogeneous data as ad hoc lists or plists.
- **Macros:** only for syntax a function can't express. Never for
  performance. Use the call-with pattern: `with-foo` expands to a call to
  `call-with-foo` with a lambda. Evaluate arguments once, and use gensyms.
- **Conditions:** signal specific condition types, never bare strings. Use
  `assert`/`check-type` for internal bugs, and `error` with a condition type
  for bad input. No `ignore-errors`.
- **No runtime `eval` or `intern`**, except where evaluating user Lisp is the
  feature (the eval command, loading plugins).
- **No output to `*standard-output*`** in the server; it has no terminal. Use
  the log.

## Performance

The goal is low keystroke-to-frame latency on every client. Measure before
optimizing.

- **Profile with `sb-sprof`** (CPU and `:alloc` mode). Use `sb-profile` for
  exact call counts on a few functions.
- **Hot paths:** input decoding, the render diff, text storage operations and
  syntax highlighting. The render loop should not allocate per frame; reuse
  buffers.
- **Declarations are assertions.** At default safety, SBCL checks declared
  types at runtime. They're still worth adding at hot function boundaries
  because they let the compiler pick fast paths.
- **To optimize a function:** add `(declare (optimize speed))` locally,
  compile, and fix the compiler notes. Don't add file-level or global
  `declaim optimize`.
- **`(safety 0)`** only inside a profiled, tested hot loop that plugins can't
  reach, with a comment saying why. No `sb-ext:truly-the`.
- **Types:** use `fixnum` for positions and counts, specialized
  `simple-array`s (`(simple-array character (*))`,
  `(simple-array (unsigned-byte 8) (*))`), and typed struct slots. Declare
  types on variables that are assigned with `setf`; SBCL infers them poorly.
- **Inlining:** `declaim inline` small hot helpers, and `declaim ftype` for
  hot functions. Struct accessors must stay inlined: don't call them before
  the `defstruct` or pass them as function arguments in hot code.
- **CLOS:** generic dispatch and CLOS slot access are slower than struct
  accessors. Keep them out of inner loops.
- **Stack allocation:** `dynamic-extent` for short-lived lists, small vectors
  and closures in hot code. SBCL trusts these declarations, so a value that
  escapes its extent corrupts memory.
- **Other SBCL tools:** `sb-ext:freeze-type` on struct types that won't
  change, `sb-ext:defglobal` for global state that's never rebound. Avoid
  assigning to closed-over variables, which forces them onto the heap.
- **Fixed-width arithmetic:** wrap it in `(logand ... #xFFFFFFFFFFFFFFFF)`
  so SBCL compiles machine arithmetic instead of bignums.

## Code health

- **No warnings.** Full warnings and style warnings fail the build. Compiler
  notes in hot code are either fixed or muffled locally with a comment.
- **Tests:** every bug fix gets a regression test. Test the terminal layer
  against byte streams and the renderer against cell grids, not a real
  terminal.
- **CI:** GitHub Actions on `ubuntu-latest` and `macos-latest`, building and
  running the tests.
- **Scope:** keep changes focused. Do unrelated cleanup in a separate commit.

## Open decisions

- Test framework (FiveAM or Parachute).
- Text representation (rope or piece table).
- Syntax highlighting (tree-sitter through `sb-alien`, or our own).
- License.
