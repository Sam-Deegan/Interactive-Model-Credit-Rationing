# Interactive Model: Credit Rationing

A Shiny app for teaching the Stiglitz and Weiss (1981) model of credit
rationing: how a loan is priced, why a borrower with limited liability
likes risk, why raising the loan rate worsens the pool of applicants, and
why banks ration rather than raise the rate. A fourth stage adds the Value
at Risk capital rule. Built by [Sam Deegan](https://sam-deegan.com) for
ECON42240 Advanced Macroeconomics, University College Dublin.

**Try it in the browser (nothing to install):**
https://sam-deegan.com/toy-models/credit-rationing/

Current version: **1.0.1** (see [CHANGELOG.md](CHANGELOG.md)). The version
is shown in the app footer; releases are tagged `vX.Y.Z`.

## What it does

The stage selector adds one layer of the model at a time:

| Stage | What is added |
|---|---|
| 1. Pricing the Loan | The pricing rule `r = r^f + (1 − c)p`, and the borrower's convex payoff read against a mean-preserving spread of project returns |
| 2. Who Applies and What the Bank Earns | The bank's concave payoff, the cut-off type, and the bank's expected return against the loan rate: a step with two types, a single peak at r̄ with a continuum |
| 3. The Rationing Equilibrium | Loan supply that follows the bank's expected return and bends backwards above r̄, against two levels of loan demand: one clears, one is rationed |
| 4. Capital and the Tail | A fat-tailed loss distribution, the Value at Risk cut point, provisions against the mean, and `K = 3 × VaR` with risk-weighted assets of `37.5 × VaR` |

Each stage opens on a worked example (a well-collateralised loan, an
unsecured loan, a wider spread at the same mean, a pool with a heavier
tail, low demand that clears, high demand that is rationed, a fat-tailed
loss book, a quiet sample). Every slider has a box beside it for an exact
value, readout tiles give the loan rate, the turning point, the excess
demand and required capital, and each figure has Save PNG and Save PDF
buttons under it. The Equations, Notation and In Words tabs show the model
as it stands at the chosen stage and flag what that stage added. The loan
is `B = 100` throughout, so every amount reads as a figure per 100 lent.

## Run it locally

1. Install [R](https://cran.r-project.org/) (4.1 or later) and, ideally,
   [RStudio](https://posit.co/download/rstudio-desktop/).
2. Install the three packages once:

   ```r
   install.packages(c("shiny", "bslib", "ggplot2"))
   ```

3. Open `app.R` in RStudio and click **Run App**, or from R in this folder:

   ```r
   shiny::runApp()
   ```

Equations are typeset with MathJax from a CDN, so they need an internet
connection; everything else runs offline.

## Files

```
app.R                  the app: settings and text (section B), figures (D),
                       interface (E), server (F)
R/model.R              the model: pricing, the two payoffs, the cut-off, the
                       pool average, the turning point, supply and demand,
                       Value at Risk. Sources on its own, so slides can
                       reuse it.
R/toolkit.R            layout and helpers shared with the other toy-model
                       apps
tests/verify_model.R   the checks (see below)
www/                   logo and QR code
README.md              this file
CHANGELOG.md           version history
CONVENTIONS.md         how the figures and worked examples are laid out
LICENSE                CC BY-NC-ND 4.0
```

All text on screen (worked examples, prompts, equations, notation) is in
section `B_03` of `app.R`, so it can be edited without touching the rest.

## Checks

From the repo root:

```
Rscript tests/verify_model.R
```

It needs shiny, bslib, ggplot2 and htmltools, and uses pdftools if it is
installed. It exits 1 on any failure, so it can gate a deploy. There is no
published replication to compare against, because the four exhibits are
drawn objects rather than data, so the suite checks the model's own
properties: that the mean-preserving spread raises the borrower's expected
payoff and lowers the bank's, that the two payoffs add to the project
return, that the cut-off rises with the loan rate, that the bank's
expected-return curve is single-peaked with r̄ at the peak (also at a
heavy-tailed calibration), that high demand is rationed and low demand
clears, that lower collateral widens the gap, that every equation on the
Equations panel matches the solver (including the turning-point derivative
against a finite difference), that the Value at Risk tail carries one per
cent and the capital arithmetic gives `RWA = 37.5 × VaR`, and that the
shipped calibration is clean. The later checks load `app.R` and drive it:
every figure exports at its declared 2:1 size, the equations and notation
lists close over each other and carry every symbol on the lecture's
Notation frame, every stage builds its three panels, and every download
handler writes a correctly named file.

## The model

Stiglitz and Weiss (1981) is the standard model of credit rationing as an
equilibrium rather than a disequilibrium: because the interest rate a bank
charges changes who applies, the bank's expected return can fall as the
rate rises, so it may stop short of the rate that clears the market and
refuse applicants it cannot tell apart from the ones it serves. The app
follows the model as Whelan's *MA Advanced Macroeconomics* (part 12,
Default Risk, Collateral and Credit Rationing) teaches it, and adds the
Value at Risk material of part 13 (Banking: Crises and Regulation) as a
fourth stage. Rates are in per cent on the sliders and decimals in the
equations; `r` is the loan rate, `R` the project return.

```
Pricing:    r = r^f + (1 − c) p
Borrower:   P(R, r) = max( R − (1 + r)B, −C )
Bank:       ρ(θ, r) = ∫ min( R + C, (1 + r)B ) f(R, θ) dR
Returns:    E[R | θ] = μ,   log R ~ N( log μ − θ²/2, θ² )
Cut-off:    E[P(R, r)] > 0  ⟺  θ > θ̂(r),   θ̂(r) = max{ θ_min, θ : E[P] = 0 }
Pool:       E[ρ(θ, r)] = ∫_{θ̂(r)}^∞ ρ(θ, r) g(θ) dθ / (1 − G(θ̂(r)))
Types:      log(θ − θ_min) ~ N( log θ_med, σ_θ² )
Supply:     L^s(r) = L_max Φ( (E[ρ(θ, r)] − (1 + r^f)B) / σ_c )
Demand:     L^d(r) = D_0 e^{−η r}
Turning:    r̄ = argmax_r E[ρ(θ, r)];  rationing when L^d(r̄) > L^s(r̄)
VaR:        Pr(L > VaR) = 1 − q,   log L ~ N( log E[L] − σ_L²/2, σ_L² )
Capital:    K = 3 × VaR,   0.08 × RWA = K  ⟹  RWA = 37.5 × VaR
```

**The pricing rule** says a lender that needs the risk-free rate `r^f` in
expectation charges a spread of `(1 − c)p`: `p` is the probability of
default and `c` the fraction of the principal collateral recovers. The
exact rate is `(r^f + (1 − c)p) / (1 − p)`; the rule drops the small product
`pr`, as Whelan does with the word "approximately".

**The borrower's payoff** is flat at `−C` (the collateral pledged) until the
project return plus the collateral covers the amount owed `(1 + r)B`, then
rises one for one: convex in `R`, so limited liability makes a borrower
like risk. Every type has the same mean return `μ` and a higher `θ` is a
mean-preserving spread; the app's family is the lognormal with that mean
and log spread `θ`, which is a mean-preserving spread in `θ` exactly.

**The bank's payoff** is the mirror image: capped at principal and
interest, floored by the collateral, concave in `R`, so a higher `θ` lowers
the bank's return. State by state the two payoffs add to `R`, so
`ρ(θ, r) = μ − E[P]` exactly, and that identity is how the model file
computes the bank's side.

**The cut-off** follows from `E[P]` rising in `θ`: the applicants are the
types above the one that is exactly indifferent, and that type rises with
the rate, so the safest borrowers withdraw first. The bank cannot tell the
types apart and averages over those who apply; the pool of types has its
safest project at `θ_min` and a heavy right tail set by `σ_θ`. Raising the
rate then has two effects, more revenue from every performing loan and a
worse pool, and the bank's expected return peaks at `r̄` where the two are
equal.

**Supply and demand.** Banks' funding costs are spread around `(1 + r^f)B`
with dispersion `σ_c`, so the share of banks lending is a rising function
of the pool average and supply, `L_max` times that share, inherits the
turn at `r̄` and bends backwards above it. Demand falls in the rate from a
level `D_0` the student moves. Low demand crosses supply below `r̄` and the
market clears; high demand exceeds supply at `r̄`, no bank posts a rate
above `r̄`, and the difference is the rationing.

**Value at Risk** is the loss the book exceeds with probability `1 − q`:
a cut point on the loss distribution, not the size of the loss beyond it.
Provisions cover the mean `E[L]`; capital covers the tail, with `K = 3 ×
VaR` and the 8 per cent rule giving risk-weighted assets of `37.5 × VaR`.

The model is solved in closed form wherever it can be: the loan rate, the
payoffs and `E[P]` are closed-form lognormal expressions, the cut-off and
the withdrawal rate are roots of `E[P] = 0`, the pool average is a Simpson
quadrature in the probability `u = G(θ)`, the turning point is bracketed on
a grid and then optimised, and the clearing rate is a root of demand less
supply below `r̄`. Nothing is simulated.

**What the four stages show with it**

- *Stage 1* The pricing rule, and the borrower's convex payoff read against
  two densities with one mean: the left tail lands on the flat stretch and
  the right tail on the sloped one, so a wider spread raises the riskier
  type's expected payoff while the safer type's stands still.
- *Stage 2* The bank's expected return against the loan rate. With two
  types it drops the moment the safer one withdraws; with a continuum the
  safest types leave a few at a time, the drop becomes a turn, and no bank
  posts a rate above `r̄`. A heavier tail of types drops the whole curve
  while barely moving the turning point.
- *Stage 3* The same bank against two levels of demand. Only the level of
  demand separates the clearing case from the rationed one, and the
  refused applicants are observationally identical to those served. Lower
  collateral lowers the peak return, so peak supply, and widens the gap.
- *Stage 4* Provisions against the mean, capital against the tail. Value at
  Risk is the cut point; the average loss beyond it enters no rule, and a
  quiet sample lowers required capital while the loan book has not changed.

**Where it departs from the textbook.** Two symbols differ from Whelan's:
`r` for the loan rate (he writes `R`, which clashes with the project return)
and `r̄` for the turning point (he writes `r*`, the natural real rate
elsewhere in the course). The lognormal return family, the pool of types
`log(θ − θ_min) ~ N(log θ_med, σ_θ²)`, and the supply block (banks' cost
dispersion `σ_c`, the scale `L_max`, the demand curve `D_0 e^{−ηr}`) are the
app's own: Whelan and Stiglitz and Weiss leave `f`, `g` and the supply of
loanable funds as general functions, and the app needs something it can
draw. The app's notes say why the pool has to be unbounded above and vanish
at `θ_min`: with a bounded support the cut-off reaches the top and the
curve has no interior peak, and the interior peak is what Stiglitz and
Weiss's rationing result (their Theorem 5) requires. The app keeps Whelan's
own qualification that the backward-bending supply curve "is a bit
misleading", because banks post the rate and no auctioneer clears the
market, and it notes that Stiglitz and Weiss show raising collateral can
also lower the bank's return by drawing in wealthier, less risk-averse
borrowers, an effect the model here leaves out. The Value at Risk stage
follows Whelan's part 13 example (an expected loss of 10 and a VaR of about
50) but calls the horizon ten-day rather than weekly, after the 1996
market-risk rule the `3 × VaR` multiplier comes from. There is no moral
hazard (the incentive effect of Stiglitz and Weiss's Section II), no
multi-period lending, no deposit market and no bank net worth.

## References

- Whelan, K. *MA Advanced Macroeconomics*, part 12 (Default Risk,
  Collateral and Credit Rationing) and part 13 (Banking: Crises and
  Regulation). https://www.karlwhelan.com/ma-advanced-macroeconomics/
- Stiglitz, J. E. and Weiss, A. (1981). Credit rationing in markets with
  imperfect information. *American Economic Review* 71(3), 393-410.
- Basel Committee on Banking Supervision (1996). Amendment to the Capital
  Accord to Incorporate Market Risks.

## Licence

© Sam Deegan. Released under
[CC BY-NC-ND 4.0](https://creativecommons.org/licenses/by-nc-nd/4.0/):
free to use and share for teaching with attribution; not for commercial use
or redistribution in modified form.
