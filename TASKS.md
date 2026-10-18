# tui — working checklist (issue #1)

Deleted in the final commit; its contents go into the PR description.

## Repo
- [x] Branch, `.gitignore`, root `Makefile` (`make test`, ASDF configured per invocation, strict warnings)
- [x] `tui/tui.asd`: `tui` (package-inferred), `tui/tests`, `tui/examples`
- [x] Public package `tui` re-exporting every module (`src/all.lisp`)
- [x] Minimal test harness (`deftest`, `is`, `signals`, runner, exit status)
- [x] GitHub Actions workflow (ubuntu-latest, macos-latest) running `make test` and `make bench`
- [ ] Update `CLAUDE.md` for the monorepo layout and build/test commands

## Modules
- [ ] `tui/style`: colors (reset, 16, 256, RGB) as fixnums, modifier bitmask, underline styles/color, patch
- [ ] `tui/width`: grapheme segmentation (non-allocating), display width
- [ ] `tui/text`: span/line/text, truncation, wrapping (word, then grapheme), alignment
- [ ] `tui/layout`: rect ops, constraints, flex, spacing, documented direct algorithm
- [ ] `tui/buffer`: struct-of-arrays cells, wide chars + continuation, side table, diff options, operations
- [ ] `tui/input`: pure decoder, chunk-safe, pending/flush, legacy, kitty, SGR mouse, paste, focus, replies, unknown, resize event
- [ ] `tui/output`: reusable per-terminal byte buffer, escape writers without `format`
- [ ] `tui/posix`: termios raw mode, TIOCGWINSZ, poll, read/write
- [ ] `tui/terminal`: terminal object (in/out fds), with-terminal, suspend/resume, size, one write per frame, wait helper
- [ ] `tui/caps`: query kitty keyboard + sync output, DA1 sentinel, blocking + non-blocking API, per-terminal results
- [ ] `tui/render`: draw/frame, double buffering, minimal diff, wide/VS16 handling, sync output, cursor, invalidate/resize, zero allocation
- [ ] `tui/widgets`: render/render-stateful generics; block, paragraph, list, table, tabs, scrollbar, clear

## Examples
- [ ] Event inspector
- [ ] Widget demo

## Verification
- [ ] Decoder tables (all encodings, Alt, every modifier, kitty event types, mouse, paste)
- [ ] Decoder chunking (every split point, byte at a time)
- [ ] Decoder Esc handling
- [ ] Render round trip against a screen model (random pairs + hand-written wide/narrow cases)
- [ ] Minimal output goldens
- [ ] Zero allocation after warmup
- [ ] Width tests
- [ ] Layout tests + coverage property tests
- [ ] Two terminals over ptys in one process
- [ ] Benchmark (250×80 full redraw, 10% change)

## Finish
- [ ] Review whole diff against CLAUDE.md and the issue; fix blockers
- [ ] CI green on the PR
- [ ] PR description (Blocked on me / Changed / Found / Manual checks / Benchmark)
