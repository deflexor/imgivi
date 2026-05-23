# imgivi — TUI Image Viewer

A terminal-based image viewer for modern terminals, written in Haskell with
[Brick](https://github.com/jtdaugherty/brick) and
[Vty](https://github.com/jtdaugherty/vty).

Inspired by [img2ascii](https://github.com/edzdez/img2ascii).

## Features

- **Dual-pane layout**: file browser (left) + image preview (right)
- **24‑bit TrueColor**: forces `FullColor` mode via vty configuration
- **img2ascii‑inspired rendering**: block‑averaged colours mapped through a
  character density ramp (`@#8&|o:*_. `) — both foreground and background
  set to the averaged cell colour
- **Aspect‑ratio‑preserving scaling**: images fit within the available pane
  and are centred when empty space remains
- **Keyboard navigation**: arrow keys, Enter to enter directories,
  `s`/`Space` for selection, `d` for delete, `r` for rename, `q`/`Esc` to quit

## Requirements

- GHC 9.6.7+
- Cabal
- libtinfo (development headers)
- A terminal emulator with TrueColor support (e.g. Windows Terminal, Kitty,
  iTerm2, modern GNOME Terminal)

## Build

```sh
cabal build
```

## Run

```sh
cabal run
```

## Project structure

```
app/Main.hs              — entry point
src/ImgVi/
  App.hs                 — Brick App wiring, vty initialisation
  Events.hs              — keyboard event handling
  FileBrowser.hs         — file list widget
  HelpBar.hs             — bottom status/help bar
  ImageCache.hs          — image decoding and caching
  ImageViewer.hs         — image preview pane
  TerminalImage.hs       — img2ascii‑based vty Image renderer
  Types.hs               — domain types, AppState
imgivi.cabal             — package descriptor
```

## Licence

MIT
