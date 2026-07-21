# Chess Piece Sets

Each subfolder here is a selectable piece set in **Settings → Board Theme → Piece Set**.

## Adding your own set

1. Create a folder under `wwwroot/pieces/` — the folder name becomes the set name.
2. Put 12 image files in it, named exactly:
   `wK`, `wQ`, `wR`, `wB`, `wN`, `wP`, `bK`, `bQ`, `bR`, `bB`, `bN`, `bP`
   as `.svg` (recommended — crisp at any board size) or `.png`.
   If both exist, SVG is preferred. SVGs should have a `viewBox` attribute.
3. Reload the Settings page — the folder is scanned on page load, no restart needed.

Piece sets from the Lichess ecosystem use this exact file naming and can be dropped in
directly (respect each set's license and attribution requirements).

## Bundled sets

- **wikipedia** — the cburnett piece set (see LICENSE.md in that folder)
- **kosal** — by Kosal Sen
