# Changelog

All notable changes to this app. Versions follow [Semantic Versioning](https://semver.org/):
MAJOR for a change to the model or its notation, MINOR for new features
(a stage, a worked example, a figure), PATCH for fixes and wording.
Each release is tagged in git as `vX.Y.Z` and shown in the app footer.

## [1.0.6] - 2026-09-28

### App
- Card headers in the blue used for headings, not body grey.
- Save PNG only under each figure; the PDF button is gone.

## [1.0.5] - 2026-09-28

### App
- Cards have no border or header rule: figures, equations and stories sit
  on the page separated by whitespace alone.

## [1.0.4] - 2026-09-28

### App
- Cards, panels, tiles and buttons are square with no shadow: they organise
  the page rather than decorate it.

## [1.0.3] - 2026-09-28

### App
- The QR code returns to the foot of the sidebar, with the name and site
  address, alongside the small one in the title bar.

## [1.0.2] - 2026-09-28

### App
- No figure carries a title or subtitle inside the image; the card header
  and the caption under it name and explain the figure (CONVENTIONS.md 6).
- Figures are drawn on a white ground, so the image sits flat in its card
  instead of showing as a tinted tile.

## [1.0.1] - 2026-09-28

### App
- The In Words tab lays out its three columns at fixed widths, so an
  equation no longer collapses to one term per line beside its note.
- The preset card no longer doubles the word "Stage" in front of a stage
  name that already carries it.

## [1.0.0] - 2026-09-28

First public release as a standalone repository.

### Model
- The Stiglitz and Weiss (1981) model as Whelan's MA Advanced
  Macroeconomics part 12 teaches it: the pricing rule, the borrower's
  convex and the bank's concave payoff, the cut-off type, the pool average
  and the turning point r-bar.
- Lognormal project returns with a common mean, a pool of types with a
  safest project and a heavy right tail, and a loan supply curve that
  follows the bank's expected return; all closed form or quadrature.
- The Value at Risk cut point and the capital rule K = 3 x VaR, RWA = 37.5
  x VaR, from part 13.

### App
- Four stages (pricing, who applies, the rationing equilibrium, capital and
  the tail) that add one layer of the model at a time.
- Nine worked examples in the main window, with a ghost of the loaded
  example drawn behind the live sliders.
- Six figures in 2:1 cards with Save PNG and Save PDF buttons, exported at
  1600x800 or 1440x720 under {app}-{stage}-{figure} names.
- Equations, Notation and In Words tabs that track the model at each stage,
  readout tiles, and notes under the figures on the selection effect, the
  backward bend and what Value at Risk does not measure.
- A test suite (tests/verify_model.R) of 44 checks on the model's
  properties, the displayed equations and the app's exports.
