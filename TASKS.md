# tui — working checklist (issue #1)

Deleted in the final commit; its contents go into the PR description.

## Repo
- [x] Branch, `.gitignore`, root `Makefile` (`make test`, ASDF configured per invocation, strict warnings)
- [x] `tui/tui.asd`: `tui` (package-inferred), `tui/tests`, `tui/examples`
- [x] Public package `tui` re-exporting every module (`src/all.lisp`)
- [x] Minimal test harness (`deftest`, `is`, `signals`, runner, exit status)
- [x] GitHub Actions workflow (ubuntu-latest, macos-latest) running `make test` and `make bench`
- [x] Update `CLAUDE.md` for the monorepo layout and build/test commands

## Modules
- [x] `tui/style`: colors (reset, 16, 256, RGB) as fixnums, modifier bitmask, underline styles/color, patch
- [x] `tui/width`: grapheme segmentation (non-allocating), display width
- [x] `tui/text`: span/line/text, truncation, wrapping (word, then grapheme), alignment
- [x] `tui/layout`: rect ops, constraints, flex, spacing, documented direct algorithm
- [x] `tui/buffer`: struct-of-arrays cells, wide chars + continuation, side table, diff options, operations
- [x] `tui/input`: pure decoder, chunk-safe, pending/flush, legacy, kitty, SGR mouse, paste, focus, replies, unknown, resize event
- [x] `tui/output`: reusable per-terminal byte buffer, escape writers without `format`
- [x] `tui/posix`: termios raw mode, TIOCGWINSZ, poll, read/write
- [x] `tui/terminal`: terminal object (in/out fds), with-terminal, suspend/resume, size, one write per frame, wait helper
- [x] `tui/caps`: query kitty keyboard + sync output, DA1 sentinel, blocking + non-blocking API, per-terminal results
- [x] `tui/render`: draw/frame, double buffering, minimal diff, wide/VS16 handling, sync output, cursor, invalidate/resize, zero allocation
- [ ] `tui/widgets`: render/render-stateful generics; block, paragraph, list, table, tabs, scrollbar, clear

## Examples
- [x] Event inspector
- [ ] Widget demo

## Verification
- [x] Decoder tables (all encodings, Alt, every modifier, kitty event types, mouse, paste)
- [x] Decoder chunking (every split point, byte at a time)
- [x] Decoder Esc handling
- [x] Render round trip against a screen model (random pairs + hand-written wide/narrow cases)
- [x] Minimal output goldens
- [x] Zero allocation after warmup
- [x] Width tests
- [x] Layout tests + coverage property tests
- [x] Two terminals over ptys in one process
- [x] Benchmark (250×80 full redraw, 10% change)

## Discovered along the way
- [x] Exclude `.claude/` (tool worktrees with their own .asd files) from the ASDF tree
- [x] Ignore warnings SBCL muffles itself (macro redefinition on fasl load) in the strict build
- [x] ASDF 3.3 package inference rejects `:local-nicknames` — documented in CLAUDE.md
- [x] `make test` also compiles the examples strictly; pty smoke test for the inspector
- [x] Buffer: keep a continuation cell's style equal to its wide grapheme's
- [x] Cursor position tracked across frames (no move when the cursor didn't change)
- [x] Rename one-word struct predicates (`rectp`, `layoutp`, `eventp`)
- [x] Split buffer.lisp (cell reading in `tui/buffer/read`, styled text in `tui/buffer/text`)
- [x] ASCII fast path in set-string, hoisted arrays in the diff
- [ ] Merge widgets; widget demo; demo smoke test
- [ ] CI green on Linux and macOS

## Finish
- [ ] Review whole diff against CLAUDE.md and the issue; fix blockers
- [ ] CI green on the PR
- [ ] PR description (Blocked on me / Changed / Found / Manual checks / Benchmark)
