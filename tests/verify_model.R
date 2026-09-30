################################################################################
## Project: ECON42240 Advanced Macroeconomics                                 ##
## Credit Rationing: Verify the Model's Own Properties                        ##
################################################################################

## Author:      Sam Deegan
## Affiliation: University College Dublin
## Email:       sam.deegan@ucdconnect.ie

## Usage:
##   From the repo root:
##     Rscript tests/verify_model.R
##   Exits 1 on any failure, so it can gate a deploy. Needs shiny, bslib,
##   ggplot2 and htmltools.
##
## Inputs:
##   R/model.R, then R/toolkit.R and app.R for the checks on the app.
##   Nothing is read from disk otherwise; PNGs go to tempdir().
##
## Outputs:
##   A PASS or FAIL line per check, and a summary.
##
## What is checked:
##   There is no published replication to check this app against: the four
##   exhibits are drawn objects rather than data or an estimated model. So
##   the suite checks the model's own properties, the results Whelan part
##   12 and Stiglitz and Weiss (1981) state and the figures assert:
##
##     1-4   the mean-preserving spread raises the borrower's expected
##           payoff and lowers the bank's, and the two payoffs add to R
##     5-8   the cut-off exists, rises with the loan rate, and the pool
##           deteriorates behind it
##     9-12  the bank's expected-return curve is single-peaked and r-bar is
##           where it peaks
##    24-25  and it still is at a calibration two sliders away, with no
##           spike in it
##    13-16  excess demand at r-bar is positive for the high demand curve
##           and not positive for the low one, and supply bends back
##    39     lower collateral lowers peak supply and widens the gap
##    41     every equation on the Equations panel matches the solver
##    17-20  the Value at Risk tail integrates to one per cent, and the
##           capital arithmetic is Whelan's (part 13)
##    21-23  the calibration the app ships with is clean and the pricing
##           rule is Whelan's
##
##   Checks 26 onwards are about the app rather than the model, so this
##   file loads app.R too:
##
##    26-27  no figure the app declares, and nothing in its plotting code,
##           can produce a panel squarer than 3:2
##    28-30  every figure exports as a non-empty PNG at its declared size,
##           and every exported PNG is at least 3:2
##    31-35  the equations list and the notation key are is-mp-pc's shape
##           and close over each other: every symbol in the key is used in
##           an equation and every symbol in an equation is in the key
##    36     every symbol on the lecture's Notation frame is in the app
##    37-38  the app runs through all four stages, the three panels build
##           at each, and every download handler writes a named PNG
##    40     series colours follow the house order
##    44-46  the UI renders, every plotOutput sits in a toolkit T_07_07f
##           card, every card's PNG handler writes at 3:2, and no label
##           or header spells out a Greek letter

#-------------------------------- Script Begin --------------------------------#

################################################################################
## H: Verification #############################################################
################################################################################
# Note: One section, because the file does one job.

#### H_01: Set-Up ##############################################################
# Note: Load the model and fix the calibration the app ships with.

###### H_01_01: Load the Model #################################################
# Note: Run from the app directory, or from tests/.

H_01_01_root_chr <- if (file.exists(file.path("R", "model.R"))) {
  "."
} else {
  ".."
}

source(file.path(H_01_01_root_chr, "R", "model.R"))

###### H_01_02: The Shipped Calibration ########################################
# Note: B_03_01_defaults_lst in app.R, as a literal so the test fails when
#   the app's defaults move away from the ones these results hold at.

H_01_02_par_lst <- list(
  r_f       = 3,
  p_def     = 5,
  c_rec     = 0.5,
  B         = 100,
  C         = 25,
  mu        = 130,
  theta_a   = 0.25,
  theta_b   = 0.90,
  theta_min = 0.20,
  theta_med = 0.40,
  theta_s   = 1.00,
  sigma_c   = 5,
  L_max     = 128,
  demand    = 85,
  eta       = 1.5,
  el        = 10,
  tail      = 0.85,
  conf      = 0.99
)

###### H_01_03: The Rate Axis ##################################################
# Note: B_03_03_r_max_num in app.R.

H_01_03_r_max_num <- 0.75

###### H_01_04: The Two Demand Levels ##########################################
# Note: B_03_14_demand_vec in app.R.

H_01_04_demand_vec <- c(low = 85, high = 190)

###### H_01_05: The Reporter ###################################################
# Note: One line per check. message(), never cat() or print().

H_01_05_fail_int <- 0L

H_01_05_check_fn <- function(label, ok, detail = "") {
  if (isTRUE(ok)) {
    message(sprintf("PASS  %-58s %s", label, detail))
  } else {
    H_01_05_fail_int <<- H_01_05_fail_int + 1L
    message(sprintf("FAIL  %-58s %s", label, detail))
  }
  invisible(ok)
}

#### H_02: The Project, the Payoffs and the Spread #############################
# Note: Checks 1 to 4. Stiglitz and Weiss (1981) eqs 4a and 4b, Theorems 1
#   and 3.

###### H_02_01: The Spread Preserves the Mean ##################################
# Note: Every type has the same expected project return. Integrated from the
#   drawn density rather than read off the parameters.

H_02_01_mean_fn <- function(par, theta) {
  grid <- seq(1e-4, 40 * par$mu, length.out = 400001L)
  d    <- C_01_04_density_fn(par, theta, grid)
  sum(d$density * grid) * (grid[2] - grid[1])
}

local({
  m_a <- H_02_01_mean_fn(H_01_02_par_lst, H_01_02_par_lst$theta_a)
  m_b <- H_02_01_mean_fn(H_01_02_par_lst, H_01_02_par_lst$theta_b)
  H_01_05_check_fn(
    "1  both drawn densities integrate to the same mean",
    abs(m_a - H_01_02_par_lst$mu) < 0.5 &&
      abs(m_b - H_01_02_par_lst$mu) < 0.5,
    sprintf("%.2f and %.2f against mu = %.0f", m_a, m_b, H_01_02_par_lst$mu))
})

###### H_02_02: The Borrower Gains From Risk ###################################
# Note: A convex payoff's mean rises with a mean-preserving spread, across
#   the whole grid of types.

local({
  r  <- C_01_01_loan_rate_fn(H_01_02_par_lst)
  th <- seq(0.05, 2, by = 0.05)
  ep <- C_01_06_borrower_fn(H_01_02_par_lst, th, r)
  H_01_05_check_fn(
    "2  the borrower's expected payoff rises in theta throughout",
    all(diff(ep) > 0),
    sprintf("from %.2f at theta = 0.05 to %.2f at theta = 2",
            ep[1], ep[length(ep)]))
})

###### H_02_03: The Bank Loses From Risk #######################################
# Note: The mirror image. Concave claim, so the expectation falls.

local({
  r  <- C_01_01_loan_rate_fn(H_01_02_par_lst)
  th <- seq(0.05, 2, by = 0.05)
  rho <- C_01_07_bank_fn(H_01_02_par_lst, th, r)
  H_01_05_check_fn(
    "3  the bank's expected return falls in theta throughout",
    all(diff(rho) < 0),
    sprintf("from %.2f at theta = 0.05 to %.2f at theta = 2",
            rho[1], rho[length(rho)]))
})

###### H_02_04: The Two Payoffs Add to the Return ##############################
# Note: max(R - D, -C) + min(R + C, D) = R, state by state, on a grid of
#   returns; the bank's side of the model is computed through this.

local({
  r    <- C_01_01_loan_rate_fn(H_01_02_par_lst)
  grid <- seq(0, 600, length.out = 2001L)
  pay  <- C_01_05_payoff_fn(H_01_02_par_lst, r, grid)
  bank <- pmin(grid + H_01_02_par_lst$C, pay$owed[1])
  worst <- max(abs(pay$payoff + bank - grid))
  H_01_05_check_fn(
    "4  the two payoffs add to the project return state by state",
    worst < 1e-9, sprintf("largest discrepancy %.2e", worst))
})

#### H_03: Adverse Selection ###################################################
# Note: Checks 5 to 8. Stiglitz and Weiss (1981) Theorems 1 and 2; Whelan
#   part 12, "Averaging Across the Applicant Pool".

###### H_03_01: The Cut-Off Is Where the Payoff Is Zero ########################
# Note: theta_hat(r) is defined by E[P] = 0, so evaluating the payoff there
#   must give zero whenever the cut-off is interior.

local({
  r  <- 0.45
  th <- C_01_09_cutoff_fn(H_01_02_par_lst, r)
  ep <- C_01_06_borrower_fn(H_01_02_par_lst, th, r)
  H_01_05_check_fn(
    "5  the cut-off type is exactly indifferent",
    th > H_01_02_par_lst$theta_min && abs(ep) < 1e-6,
    sprintf("theta_hat = %.4f, E[P] = %.2e", th, ep))
})

###### H_03_02: The Cut-Off Rises With the Loan Rate ###########################
# Note: The safest borrowers withdraw first, so the cut-off is
#   non-decreasing in r and strictly rising once it is interior.

local({
  g  <- seq(0, H_01_03_r_max_num, length.out = 76L)
  th <- vapply(g, function(r) C_01_09_cutoff_fn(H_01_02_par_lst, r), 0)
  interior <- th > H_01_02_par_lst$theta_min + 1e-8
  H_01_05_check_fn(
    "6  the cut-off never falls as the loan rate rises",
    all(diff(th) >= -1e-9),
    sprintf("from %.3f to %.3f", th[1], th[length(th)]))
  H_01_05_check_fn(
    "7  the cut-off rises strictly once it is interior",
    sum(interior) > 5L && all(diff(th[interior]) > 0),
    sprintf("interior at %d of %d rates", sum(interior), length(th)))
})

###### H_03_03: The Pool Shrinks Behind the Cut-Off ############################
# Note: 1 - G(theta_hat), the denominator of the pool average, falls as the
#   cut-off climbs.

local({
  g  <- seq(0, H_01_03_r_max_num, length.out = 76L)
  sh <- vapply(g, function(r) C_01_12_pool_fn(H_01_02_par_lst, r)$share, 0)
  H_01_05_check_fn(
    "8  the share of types still applying never rises",
    all(diff(sh) <= 1e-9),
    sprintf("from %.0f%% to %.0f%%", 100 * sh[1], 100 * sh[length(sh)]))
})

#### H_04: The Expected-Return Curve ###########################################
# Note: Checks 9 to 12, the central result: the curve is single-peaked and
#   r-bar is where it peaks (Stiglitz and Weiss 1981, fig. 1).

###### H_04_01: The Curve Is Single-Peaked #####################################
# Note: The sign of the first difference changes exactly once, from up to
#   down.

local({
  g   <- seq(0, H_01_03_r_max_num, length.out = 301L)
  rho <- C_01_13_return_curve_fn(H_01_02_par_lst, g)$rho
  ok  <- is.finite(rho)
  d   <- diff(rho[ok])
  flips <- sum(diff(sign(d)) != 0)
  H_01_05_check_fn(
    "9  the expected-return curve turns exactly once",
    flips == 1L, sprintf("%d sign change(s) in the first difference", flips))
  H_01_05_check_fn(
    "10 it rises first and falls after",
    d[1] > 0 && d[length(d)] < 0,
    sprintf("first step %+.3f, last step %+.3f", d[1], d[length(d)]))
})

###### H_04_02: r-Bar Is Where It Peaks ########################################
# Note: The turning point the figure marks is the maximum of the curve it
#   draws, found independently of the drawing grid.

local({
  r_bar <- C_01_16_rbar_fn(H_01_02_par_lst, H_01_03_r_max_num)
  peak  <- C_01_12_pool_fn(H_01_02_par_lst, r_bar)$rho
  g     <- seq(0, H_01_03_r_max_num, length.out = 751L)
  rho   <- C_01_13_return_curve_fn(H_01_02_par_lst, g)$rho
  best  <- max(rho[is.finite(rho)])
  H_01_05_check_fn(
    "11 r-bar is the maximum of the drawn curve",
    peak >= best - 1e-6 && r_bar > 0 && r_bar < H_01_03_r_max_num,
    sprintf("r-bar = %.4f, rho = %.4f against a grid best of %.4f",
            r_bar, peak, best))
  near <- c(C_01_12_pool_fn(H_01_02_par_lst, r_bar - 0.02)$rho,
            C_01_12_pool_fn(H_01_02_par_lst, r_bar + 0.02)$rho)
  H_01_05_check_fn(
    "12 the return is lower on both sides of r-bar",
    all(near < peak),
    sprintf("%.3f and %.3f against %.3f", near[1], near[2], peak))
})

###### H_04_03: It Is Still Single-Peaked Away From the Defaults ###############
# Note: Checks 24 and 25: the same properties at a calibration two sliders
#   can reach in one move, where the pool's tail is heavy and a quadrature
#   grid in theta would fail.

local({
  p <- utils::modifyList(H_01_02_par_lst, list(theta_s = 1.4, C = 45))
  g   <- seq(0, H_01_03_r_max_num, length.out = 301L)
  rho <- C_01_13_return_curve_fn(p, g)$rho
  ok  <- is.finite(rho)
  d   <- diff(rho[ok])
  flips <- sum(diff(sign(d)) != 0)
  H_01_05_check_fn(
    "24 a heavy-tailed pool still turns exactly once",
    flips == 1L, sprintf("%d sign change(s) at theta_s = 1.4, C = 45",
                         flips))
  # A spike is a single step many times the median step
  worst <- max(abs(d)) / stats::median(abs(d))
  H_01_05_check_fn(
    "25 and it has no spike in it",
    worst < 12,
    sprintf("largest step is %.1f times the median step", worst))
})

#### H_05: The Rationing Equilibrium ###########################################
# Note: Checks 13 to 16, 39 and 41. Whelan part 12, the credit-rationing
#   slides; Stiglitz and Weiss (1981) fig. 4.

###### H_05_01: Supply Peaks Where the Return Does #############################
# Note: Supply is a strictly increasing transform of the pool average, so
#   it peaks where the pool average does.

local({
  r_bar <- C_01_16_rbar_fn(H_01_02_par_lst, H_01_03_r_max_num)
  g     <- seq(0, H_01_03_r_max_num, length.out = 301L)
  sup   <- C_01_17_supply_fn(H_01_02_par_lst, g, r_bar)$supply
  ok    <- is.finite(sup)
  at    <- g[ok][which.max(sup[ok])]
  # L_max = 128 puts supply at r-bar at 100 to the nearest loan, so the
  #   tolerance is half a loan
  H_01_05_check_fn(
    "13 loan supply is largest at r-bar",
    abs(at - r_bar) < 0.01 && abs(max(sup[ok]) - 100) < 0.5,
    sprintf("largest at %.3f against r-bar = %.3f, quantity %.4f",
            at, r_bar, max(sup[ok])))
  H_01_05_check_fn(
    "14 supply bends backwards above r-bar",
    sup[ok][sum(ok)] < max(sup[ok]) - 1,
    sprintf("%.1f at the top of the axis against %.1f at r-bar",
            sup[ok][sum(ok)], max(sup[ok])))
})

###### H_05_02: The High Demand Curve Is Rationed ##############################
# Note: Excess demand at r-bar is strictly positive for the high curve and
#   there is no crossing below r-bar for it.

local({
  m <- C_01_19_market_fn(H_01_02_par_lst, H_01_03_r_max_num,
                         H_01_04_demand_vec[["high"]])
  H_01_05_check_fn(
    "15 high demand leaves positive excess demand at r-bar",
    m$excess > 0 && !is.finite(m$clears),
    sprintf("demand %.1f against supply %.1f, gap %.1f",
            m$demand, m$supply, m$excess))
})

###### H_05_03: The Low Demand Curve Clears ####################################
# Note: Low demand meets supply at a rate below r-bar, and the excess
#   demand at r-bar is not positive.

local({
  m <- C_01_19_market_fn(H_01_02_par_lst, H_01_03_r_max_num,
                         H_01_04_demand_vec[["low"]])
  H_01_05_check_fn(
    "16 low demand clears at a rate below r-bar",
    m$excess <= 0 && is.finite(m$clears) && m$clears < m$r_bar,
    sprintf("clears at %.3f against r-bar = %.3f, gap at r-bar %.1f",
            m$clears, m$r_bar, m$excess))
})

###### H_05_04: Lower Collateral Tightens Rationing ############################
# Note: Check 39. Whelan part 12: reductions in the value of collateral
#   reduce bank expected profits and increase the extent of credit
#   rationing. Lower collateral lowers the peak return, so peak supply.

local({
  lo <- utils::modifyList(H_01_02_par_lst, list(C = 15))
  m0 <- C_01_19_market_fn(H_01_02_par_lst, H_01_03_r_max_num,
                          H_01_04_demand_vec[["high"]])
  m1 <- C_01_19_market_fn(lo, H_01_03_r_max_num,
                          H_01_04_demand_vec[["high"]])
  H_01_05_check_fn(
    "39 lower collateral lowers peak supply and raises rationing",
    m1$supply < m0$supply - 1 && m1$excess > m0$excess + 1,
    sprintf("C 25 -> 15: supply at r-bar %.1f -> %.1f, excess %.1f -> %.1f",
            m0$supply, m1$supply, m0$excess, m1$excess))
})

###### H_05_05: The Displayed Equations Against the Solver #####################
# Note: Check 41. Every equation on the Equations panel that R/model.R
#   computes is evaluated as displayed and compared with the solver,
#   including the turning-point derivative against a finite difference.

local({
  p   <- H_01_02_par_lst
  rf  <- p$r_f / 100; pd <- p$p_def / 100; cr <- p$c_rec
  r   <- C_01_01_loan_rate_fn(p)
  err <- c(
    rule    = r - (rf + (1 - cr) * pd),
    earned  = ((1 - pd) * r - pd * (1 - cr)) - (rf - pd * r),
    exact   = {re <- (rf + (1 - cr) * pd) / (1 - pd)
               (1 - pd) * re - pd * (1 - cr) - rf},
    mean    = exp(C_01_03_lnorm_fn(p$mu, 0.9)$meanlog + 0.9^2 / 2) - p$mu,
    cutoff  = C_01_06_borrower_fn(p, C_01_09_cutoff_fn(p, 0.45), 0.45)
  )
  rb   <- C_01_16_rbar_fn(p, H_01_03_r_max_num)
  cost <- (1 + rf) * p$B
  sup  <- function(rr) {
    p$L_max * stats::pnorm((C_01_12_pool_fn(p, rr)$rho - cost) / p$sigma_c)
  }
  err["supply"] <- C_01_17_supply_fn(p, 0.2, rb)$supply - sup(0.2)
  err["s_rbar"] <- C_01_19_market_fn(p, H_01_03_r_max_num, 190)$supply -
    p$L_max * stats::pnorm((stats::optimize(
      function(rr) C_01_12_pool_fn(p, rr)$rho, c(0, H_01_03_r_max_num),
      maximum = TRUE, tol = 1e-10)$objective - cost) / p$sigma_c)
  v <- C_01_21_var_fn(p)
  err["rwa"] <- v$rwa - 37.5 * v$var

  # The turning-point derivative, as displayed
  deriv <- function(rr) {
    a   <- (1 + rr) * p$B - p$C
    th0 <- C_01_09_cutoff_fn(p, rr)
    sh  <- C_01_11_share_fn(p, th0)
    u   <- seq(1 - sh, 1 - 1e-6, length.out = 2001L)
    th  <- p$theta_min + stats::qlnorm(u, log(p$theta_med), p$theta_s)
    w   <- c(1, rep(c(4, 2), length.out = length(u) - 2L), 1)
    avg <- function(x) sum(w * x) / sum(w)
    Fa  <- function(t) stats::plnorm(a, log(p$mu) - t^2 / 2, t)
    rev <- p$B * avg(1 - Fa(th))
    if (th0 <= p$theta_min + 1e-10) return(rev)
    dth <- (C_01_06_borrower_fn(p, th0 + 1e-5, rr) -
              C_01_06_borrower_fn(p, th0 - 1e-5, rr)) / 2e-5
    thp <- p$B * (1 - Fa(th0)) / dth
    haz <- C_01_10_types_fn(p, th0) / sh
    rev - thp * haz * avg(C_01_06_borrower_fn(p, th, rr))
  }
  fd <- function(rr) (C_01_12_pool_fn(p, rr + 1e-4, 2001L)$rho -
                        C_01_12_pool_fn(p, rr - 1e-4, 2001L)$rho) / 2e-4
  d_err <- c(r25 = deriv(0.25) - fd(0.25), r45 = deriv(0.45) - fd(0.45),
             r60 = deriv(0.60) - fd(0.60))
  H_01_05_check_fn(
    "41 every displayed equation matches the solver",
    all(abs(err) < 1e-6) && all(abs(d_err) < 0.05) && abs(deriv(rb)) < 0.1,
    sprintf(paste0("largest level error %.1e (%s); dE[rho]/dr formula vs ",
                   "finite diff %s; at r-bar %.3f"),
            max(abs(err)), names(err)[which.max(abs(err))],
            paste(sprintf("%.3f/%.3f", c(deriv(0.25), deriv(0.45),
                                         deriv(0.60)),
                          c(fd(0.25), fd(0.45), fd(0.60))), collapse = " "),
            deriv(rb)))
})

#### H_06: Value at Risk #######################################################
# Note: Checks 17 to 20. Whelan part 13, the Value at Risk slides, and the
#   exhibit's claim that the shaded region is one per cent of outcomes.

###### H_06_01: The Density Integrates to One ##################################
# Note: The panel is drawn from a density.

local({
  grid <- seq(1e-6, 4000, length.out = 800001L)
  d    <- C_01_20_loss_fn(H_01_02_par_lst, grid)
  tot  <- sum(d$density) * (grid[2] - grid[1])
  H_01_05_check_fn(
    "17 the loss density integrates to one",
    abs(tot - 1) < 1e-3, sprintf("%.6f", tot))
})

###### H_06_02: The Shaded Tail Is One Per Cent ################################
# Note: The shaded region runs from the Value at Risk to the right and must
#   carry 1 - conf of the probability. Integrated from the drawn density.

local({
  v    <- C_01_21_var_fn(H_01_02_par_lst)
  grid <- seq(v$var, 4000, length.out = 800001L)
  d    <- C_01_20_loss_fn(H_01_02_par_lst, grid)
  tail <- sum(d$density) * (grid[2] - grid[1])
  H_01_05_check_fn(
    "18 the shaded tail integrates to one per cent",
    abs(tail - 0.01) < 1e-4,
    sprintf("%.5f of the probability beyond VaR = %.2f", tail, v$var))
})

###### H_06_03: The Capital Arithmetic Is the Deck's ###########################
# Note: K = 3 VaR and K >= 0.08 RWA give RWA = 37.5 VaR, and Whelan's
#   fifty-million example (part 13) comes out of the shipped calibration.

local({
  v <- C_01_21_var_fn(H_01_02_par_lst)
  H_01_05_check_fn(
    "19 K is three times VaR and RWA is 37.5 times it",
    abs(v$k - 3 * v$var) < 1e-9 && abs(v$rwa - 37.5 * v$var) < 1e-6,
    sprintf("VaR %.1f, K %.1f, RWA %.0f", v$var, v$k, v$rwa))
  H_01_05_check_fn(
    "20 the shipped calibration reproduces the deck's example",
    abs(v$var - 50) < 1.5 && abs(v$el - 10) < 1e-9,
    sprintf("expected loss %.0f, ten-day VaR %.1f", v$el, v$var))
})

#### H_07: The Pricing Rule and the Shipped Calibration ########################
# Note: Checks 21 to 23.

###### H_07_01: The Pricing Rule ###############################################
# Note: r = r^f + (1 - c)p (Whelan part 12), and at full recovery the loan
#   prices at the risk-free rate whatever the default probability.

local({
  p  <- H_01_02_par_lst
  r  <- C_01_01_loan_rate_fn(p)
  hand <- (p$r_f + (1 - p$c_rec) * p$p_def) / 100
  full <- C_01_01_loan_rate_fn(utils::modifyList(
    p, list(c_rec = 1, p_def = 20)))
  H_01_05_check_fn(
    "21 the loan rate is the risk-free rate plus (1 - c) p",
    abs(r - hand) < 1e-12, sprintf("r = %.4f", r))
  H_01_05_check_fn(
    "22 at full recovery the loan prices at the risk-free rate",
    abs(full - p$r_f / 100) < 1e-12,
    sprintf("%.4f against r_f = %.4f", full, p$r_f / 100))
})

###### H_07_02: The Shipped Calibration Is Clean ###############################
# Note: The app refuses to draw when the numbers stop making sense, so the
#   calibration it opens on must raise nothing.

local({
  probs <- C_01_23_problems_fn(H_01_02_par_lst)
  H_01_05_check_fn(
    "23 the shipped calibration raises no problems",
    length(probs) == 0L,
    if (length(probs) == 0L) "" else probs[1])
})

#### H_09: The App's Figures ###################################################
# Note: Checks 26 to 30. Everything from here on needs the app's own
#   objects, so app.R is loaded; it builds the app object without launching.

###### H_09_01: Load the App ###################################################
# Note: The toolkit is sourced first so app.R's own two guards both skip.
#   The locale is set because checks 28 to 30 write real PNGs.

invisible(Sys.setlocale("LC_CTYPE", "C.utf8"))

source(file.path(H_01_01_root_chr, "R", "toolkit.R"))
source(file.path(H_01_01_root_chr, "app.R"))

###### H_09_02: Read a PNG's Size ##############################################
# Note: Width and height off the file itself: two big-endian four-byte
#   integers in the IHDR chunk, at bytes 17 to 24.

H_09_02_pngdim_fn <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  bytes <- readBin(con, "raw", n = 24L)
  if (length(bytes) < 24L) return(c(width = NA_real_, height = NA_real_))
  int_at <- function(i) sum(as.integer(bytes[i:(i + 3L)]) * 256^(3:0))
  c(width = int_at(17L), height = int_at(21L))
}

###### H_09_03: Every Declared Shape Is at Least 3:2 ###########################
# Note: Check 26. The constants themselves, before anything is drawn from
#   them; B_03_12b refuses to load a shape that violates the floor.

local({
  ratios <- vapply(B_03_12_shapes_lst,
                   function(s) s$width_px / s$height_px, 0)
  H_01_05_check_fn(
    "26 every declared figure shape is at least 3:2",
    all(ratios >= B_03_12a_min_ratio_num),
    paste(sprintf("%s %.2f:1", names(ratios), ratios), collapse = ", "))
})

###### H_09_04: Nothing in the Plotting Code Forces a Shape ####################
# Note: Check 27. theme(aspect.ratio) and coord_fixed() force a panel shape
#   whatever the canvas; neither belongs in this app.

local({
  src <- readLines(file.path(H_01_01_root_chr, "app.R"), warn = FALSE)
  code <- src[!grepl("^\\s*#", src)]
  hits <- grep("aspect\\.ratio|coord_fixed", code, value = TRUE)
  H_01_05_check_fn(
    "27 no builder forces a panel shape",
    length(hits) == 0L,
    if (length(hits) == 0L) "no aspect.ratio and no coord_fixed" else hits[1])
})

###### H_09_05: Every Figure Exports at Its Declared Size ######################
# Note: Checks 28 to 30. Each builder is written with the export function
#   the download handler uses and the file read back; drawn at stage 4 with
#   the rationed demand level, so all six are live.

H_09_05_dir_chr <- file.path(tempdir(), "verify-figures")
dir.create(H_09_05_dir_chr, showWarnings = FALSE, recursive = TRUE)

H_09_05_export_fn <- function(key) {
  fig <- B_03_16_figures_lst[[key]]
  shp <- B_03_12_shapes_lst[[fig$shape]]
  par <- utils::modifyList(B_03_01_defaults_lst, list(demand = 190))
  rl  <- D_01_00_rlim_fn(par)
  yl  <- D_02_00_ylim_fn(par)
  rx  <- c(0, B_03_03_r_max_num)
  p <- switch(
    key,
    spread    = D_01_01_spread_fn(par, rl),
    payoff    = D_01_02_payoff_fn(par, rl),
    twotype   = D_02_01_twotype_fn(par, rx, yl),
    continuum = D_02_02_continuum_fn(par, rx, yl),
    market    = D_03_01_market_fn(par),
    var       = D_04_01_var_fn(par))
  path <- file.path(H_09_05_dir_chr, paste0(key, ".png"))
  T_02_03c_export_fn(path, p, pair = identical(fig$shape, "pair"))
  list(path = path, want = c(shp$width_px, shp$height_px))
}

local({
  keys <- names(B_03_16_figures_lst)
  out  <- lapply(keys, H_09_05_export_fn)
  dims <- lapply(out, function(x) H_09_02_pngdim_fn(x$path))
  want <- lapply(out, function(x) x$want)
  size <- vapply(out, function(x) file.info(x$path)$size, 0)
  ok_d <- mapply(function(d, w) all(abs(d - w) <= 1), dims, want)
  rats <- vapply(dims, function(d) d[["width"]] / d[["height"]], 0)

  H_01_05_check_fn(
    "28 every figure writes a non-empty PNG",
    all(is.finite(size) & size > 0),
    sprintf("smallest file %.0f bytes", min(size)))
  H_01_05_check_fn(
    "29 every exported PNG is exactly its declared size",
    all(ok_d),
    paste(sprintf("%s %.0fx%.0f", keys,
                  vapply(dims, function(d) d[["width"]], 0),
                  vapply(dims, function(d) d[["height"]], 0)),
          collapse = ", "))
  H_01_05_check_fn(
    "30 every exported PNG is at least 3:2",
    all(rats >= B_03_12a_min_ratio_num),
    sprintf("narrowest is %s at %.2f:1", keys[which.min(rats)], min(rats)))
})

#### H_10: The Equations and the Notation Key ##################################
# Note: Checks 31 to 36. The two lists are checked for is-mp-pc's shape and
#   then against each other, so a symbol cannot drift out of one and not
#   the other.

###### H_10_01: Every Equation Item Is Complete ################################
# Note: Check 31. A label, at least one version, and a note for every version.

local({
  bad <- Filter(function(it) {
    is.null(it$label) || !nzchar(it$label) ||
      length(it$versions) == 0L ||
      !all(names(it$versions) %in% names(it$notes)) ||
      !all(vapply(it$notes, nzchar, TRUE))
  }, B_03_08_equations_lst)
  H_01_05_check_fn(
    "31 every equation has a label, a version and a note per version",
    length(bad) == 0L,
    sprintf("%d equation item(s) checked%s", length(B_03_08_equations_lst),
            if (length(bad) == 0L) "" else paste0("; first bad: ",
                                                  bad[[1]]$label)))
})

###### H_10_02: Every Version Is Keyed by a Real Stage #########################
# Note: Check 32. A version keyed outside the stage list would hide the
#   item; a group the titles do not name is never rendered.

local({
  stages <- as.numeric(B_03_02_stages_vec)
  keys   <- unlist(lapply(B_03_08_equations_lst, function(it) {
    as.numeric(names(it$versions))
  }))
  grps   <- vapply(B_03_08_equations_lst, function(it) it$group, "")
  H_01_05_check_fn(
    "32 every version is keyed by a stage and every group is titled",
    all(keys %in% stages) && all(grps %in% names(B_03_09_groups_vec)),
    sprintf("stages %s, groups %s",
            paste(sort(unique(keys)), collapse = " "),
            paste(sort(unique(grps)), collapse = " ")))
})

###### H_10_03: Every Notation Entry Is Complete ###############################
# Note: Check 33. grp is one of is-mp-pc's four, from is a real stage, and
#   the gloss is lower case with no full stop.

local({
  ok_grp <- vapply(B_03_10_notation_lst,
                   function(x) x$grp %in% c("var", "par", "tgt", "shk"), TRUE)
  ok_frm <- vapply(B_03_10_notation_lst,
                   function(x) x$from %in% as.numeric(B_03_02_stages_vec),
                   TRUE)
  ok_txt <- vapply(B_03_10_notation_lst, function(x) {
    nzchar(x$txt) && !grepl("\\.$", x$txt) &&
      substr(x$txt, 1, 1) == tolower(substr(x$txt, 1, 1))
  }, TRUE)
  H_01_05_check_fn(
    "33 every notation entry has a real group, stage and gloss",
    all(ok_grp) && all(ok_frm) && all(ok_txt),
    sprintf("%d symbol(s)", length(B_03_10_notation_lst)))
})

###### H_10_04: The Symbol a Notation Entry Names ##############################
# Note: The head of a symbol is what is left when its argument list is taken
#   off: P(R,r) is P, \hat\theta(r) is \hat\theta, \mathrm{VaR} is itself.
#   An equation writes \rho(\theta,r) in one place and \rho alone in another,
#   so the head is what has to be looked for.

H_10_04_head_fn <- function(sym) sub("\\(.*$", "", sym)

###### H_10_05: The LaTeX That Is Not a Symbol #################################
# Note: Everything in the equations that is structure rather than notation.
#   Removed longest first together with the notation heads, so \propto goes
#   before p and \mathrm{VaR} before R.

H_10_05_ignore_vec <- c(
  "\\begin{aligned}", "\\end{aligned}", "\\displaystyle", "\\underbrace",
  "\\arg\\max", "\\implies", "\\approx", "\\propto", "\\infty", "\\times",
  "\\quad", "\\frac", "\\iff", "\\geq", "\\leq", "\\max", "\\min", "\\int",
  "\\big", "\\Big", "\\mid", "\\Pr", "\\,", "\\;", "\\ ", "\\\\",
  "\\mathrm{d}", "\\partial", "\\mathcal{N}", "\\log", "\\tfrac",
  "\\qquad", "\\sim", "\\{", "\\}",
  "e^", "E"
)

###### H_10_05a: The Differentials #############################################
# Note: The d of an integral is not a symbol, and it cannot be taken out by
#   the same longest-first pass as everything else: "d\\theta" is longer than
#   "\\mid", so removing it first would eat the d out of \mid and leave \mi
#   behind. A regex pass of its own, before the tokens, and anchored on a
#   word boundary so it cannot reach into \mid.

H_10_05a_diff_chr <- "(\\\\,)?\\bd(R|\\\\theta)"

###### H_10_06: The Two Lists Close Over Each Other ############################
# Note: Checks 34 and 35. Forward: every symbol in the key is used in an
#   equation. Back: nothing is left in an equation once the key's symbols
#   and the structural LaTeX are taken out.

local({
  tex  <- unlist(lapply(B_03_08_equations_lst,
                        function(it) unlist(it$versions)))
  tex  <- gsub("\\\\text\\{[^}]*\\}", "", tex)
  tex  <- gsub(H_10_05a_diff_chr, "", tex)
  head <- vapply(B_03_10_notation_lst,
                 function(x) H_10_04_head_fn(x$sym), "")

  used <- vapply(head, function(h) any(grepl(h, tex, fixed = TRUE)), TRUE)
  H_01_05_check_fn(
    "34 every symbol in the notation key is used in an equation",
    all(used),
    if (all(used)) sprintf("%d symbol(s)", length(head)) else
      paste("unused:", paste(head[!used], collapse = " ")))

  tokens <- c(head, H_10_05_ignore_vec)
  tokens <- tokens[order(nchar(tokens), decreasing = TRUE)]
  left   <- tex
  for (tk in tokens) left <- gsub(tk, "", left, fixed = TRUE)
  left   <- gsub("[0-9[:space:][:punct:]\\\\]", "", left)
  H_01_05_check_fn(
    "35 every symbol used in an equation is in the notation key",
    all(!nzchar(left)),
    if (all(!nzchar(left))) "nothing left over" else
      paste("left over:", paste(unique(left[nzchar(left)]), collapse = " | ")))
})

###### H_10_07: The Deck's Own Notation Frame ##################################
# Note: Check 36. The lecture's Notation frame, transcribed, in its order.
#   The app's key may carry more but not less.

H_10_07_deck_vec <- c(
  "r", "r^{f}", "p", "c", "B", "C", "R", "\\theta", "f(R,\\theta)",
  "P(R,r)", "\\hat\\theta(r)", "\\rho(\\theta,r)", "g(\\theta)",
  "G(\\theta)", "\\bar r", "K", "\\mathrm{VaR}", "\\mathrm{RWA}"
)

local({
  have <- vapply(B_03_10_notation_lst, function(x) x$sym, "")
  miss <- setdiff(H_10_07_deck_vec, have)
  H_01_05_check_fn(
    "36 every symbol on the deck's Notation frame is in the app",
    length(miss) == 0L,
    if (length(miss) == 0L) {
      sprintf("%d deck symbols, %d app symbols", length(H_10_07_deck_vec),
              length(have))
    } else {
      paste("missing:", paste(miss, collapse = " "))
    })
})

#### H_11: The App, Driven #####################################################
# Note: Checks 37, 38, 40 and 44 to 46. The server is run over the lists
#   above and the rendered UI is inspected.

###### H_11_01: The Controls at Their Defaults #################################
# Note: Each control is a slider and a typing box, and T_03_04_val_fn reads
#   the box first, so both are set.

H_11_01_inputs_lst <- local({
  ids <- names(B_03_05_controls_lst)
  vals <- B_03_01_defaults_lst[ids]
  c(vals, stats::setNames(vals, paste0(ids, "_box")))
})

###### H_11_02: Every Stage Builds Its Three Panels ############################
# Note: Check 37. Equations, notation and in-words at every stage, and the
#   notation panel grows as the stages add symbols.

local({
  sizes <- integer(0)
  built <- logical(0)
  shiny::testServer(F_01_01_app_server_fn, {
    for (s in as.character(sort(as.numeric(B_03_02_stages_vec)))) {
      do.call(session$setInputs, H_11_01_inputs_lst)
      session$setInputs(stage = s)
      session$elapse(B_03_13_debounce_ms_int + 200L)
      n_eq <- nchar(paste(output$eq_model, collapse = ""))
      n_no <- nchar(paste(output$eq_notation, collapse = ""))
      n_ex <- nchar(paste(output$eq_explain, collapse = ""))
      built <<- c(built, n_eq > 0L && n_no > 0L && n_ex > 0L)
      sizes <<- c(sizes, n_no)
    }
  })
  H_01_05_check_fn(
    "37 every stage builds its equations, notation and in-words panels",
    all(built) && all(diff(sizes) > 0),
    sprintf("notation panel grows %s characters",
            paste(sizes, collapse = " -> ")))
})

###### H_11_03: Every Download Handler Writes Its PNG ##########################
# Note: Check 38. The handler is fired through the server, so this covers
#   the file name {app}-{stage}-{figure}.png as well as the picture.

H_11_03_keep_chr <- file.path(tempdir(), "verify-downloads")
dir.create(H_11_03_keep_chr, showWarnings = FALSE, recursive = TRUE)

local({
  got <- list()
  # Copied inside the block: the handler's temporary directory is swept as
  #   soon as testServer returns
  shiny::testServer(F_01_01_app_server_fn, {
    for (key in names(B_03_16_figures_lst)) {
      fig <- B_03_16_figures_lst[[key]]
      do.call(session$setInputs, H_11_01_inputs_lst)
      session$setInputs(stage = as.character(fig$from))
      session$elapse(B_03_13_debounce_ms_int + 200L)
      src <- output[[paste0(key, "__png")]]
      dst <- file.path(H_11_03_keep_chr, basename(src))
      file.copy(src, dst, overwrite = TRUE)
      got[[key]] <<- dst
    }
  })

  keys  <- names(got)
  paths <- unlist(got)
  want  <- lapply(keys, function(k) {
    s <- B_03_12_shapes_lst[[B_03_16_figures_lst[[k]]$shape]]
    c(s$width_px, s$height_px)
  })
  names <- vapply(keys, function(k) {
    f <- B_03_16_figures_lst[[k]]
    paste0(B_03_15_slug_chr, "-", f$from, "-", f$file, ".png")
  }, "")

  exists_lgl <- file.exists(paths)
  size <- ifelse(exists_lgl, file.info(paths)$size, 0)
  dims <- lapply(paths, H_09_02_pngdim_fn)
  ok_d <- mapply(function(d, w) all(abs(d - w) <= 1), dims, want)
  rats <- vapply(dims, function(d) d[["width"]] / d[["height"]], 0)
  ok_n <- basename(paths) == names

  H_01_05_check_fn(
    "38 every download handler writes a named PNG at the declared size",
    all(exists_lgl) && all(size > 0) && all(ok_d) && all(ok_n) &&
      all(rats >= B_03_12a_min_ratio_num),
    sprintf("%d file(s), e.g. %s at %.0fx%.0f", length(paths),
            basename(paths[[1]]), dims[[1]][["width"]],
            dims[[1]][["height"]]))
})

###### H_11_04: Series Colours Follow the Deck Order ###########################
# Note: Check 40. First series blue (main), second green (second), the
#   comparison light blue (compare), and no two series in one figure alike.

local({
  par <- utils::modifyList(B_03_01_defaults_lst, list(demand = 190))
  sp  <- ggplot2::ggplot_build(D_01_01_spread_fn(par, D_01_00_rlim_fn(par)))
  sp_cols <- unique(sp$data[[which(vapply(
    sp$plot$layers, function(l) inherits(l$geom, "GeomLine"),
    TRUE))[1]]]$colour)
  mk  <- D_03_01_market_fn(par)
  mk_cols <- vapply(Filter(function(l) inherits(l$geom, "GeomPath") &&
                             !is.null(l$aes_params$colour) &&
                             isTRUE(l$aes_params$linewidth >= 1),
                           mk$layers),
                    function(l) l$aes_params$colour, "")
  sv <- T_01_02_series_vec
  ok <- setequal(sp_cols, sv[c("main", "second")]) &&
    setequal(mk_cols, sv[c("main", "compare", "second")]) &&
    !anyDuplicated(mk_cols)
  H_01_05_check_fn(
    "40 series colours follow the deck order, none repeated",
    ok,
    sprintf("spread %s; market %s", paste(sp_cols, collapse = " "),
            paste(mk_cols, collapse = " ")))
})

###### H_11_05: Every Figure Sits in a Toolkit Card ############################
# Note: Check 44. Every plotOutput in the rendered page sits inside a
#   T_07_07f .fig-card with its 3:2 box and its Save PNG button.

H_11_05_html_chr <- tryCatch(
  as.character(htmltools::renderTags(E_02_02_app_ui_lst)$html),
  error = function(e) NA_character_)

local({
  html  <- H_11_05_html_chr
  ok_r  <- !is.na(html) && nzchar(html)
  tags  <- if (ok_r) regmatches(html, gregexpr(
    "<div[^>]*shiny-plot-output[^>]*>", html))[[1]] else character(0)
  ids   <- sub('.*id="([^"]+)".*', "\\1", tags)
  cards <- if (ok_r) strsplit(html, "<div[^>]*fig-card[^>]*>")[[1]][-1] else
    character(0)
  in_card <- vapply(ids, function(id) {
    any(vapply(cards, function(cd) {
      grepl(sprintf('id="%s"', id), cd) &&
        grepl(sprintf('id="%s__png"', id), cd) &&
        grepl("fig-save", cd) &&
        grepl("fig-r32", cd)
    }, TRUE))
  }, TRUE)
  H_01_05_check_fn(
    "44 the UI renders and every plotOutput sits in a T_07_07f card",
    ok_r && length(ids) == length(B_03_16_figures_lst) && all(in_card) &&
      setequal(ids, names(B_03_16_figures_lst)),
    sprintf("%d plot(s) in %d fig-card(s)", length(ids), length(cards)))
})

###### H_11_06: Every Card Has a Working PNG Handler ###########################
# Note: Check 45. The one handler of every card is fired through the server;
#   the PNG size is read off the file and must be the 3:2 deck shape.

local({
  res <- list()
  shiny::testServer(F_01_01_app_server_fn, {
    for (key in names(B_03_16_figures_lst)) {
      fig <- B_03_16_figures_lst[[key]]
      do.call(session$setInputs, H_11_01_inputs_lst)
      session$setInputs(stage = as.character(fig$from))
      session$elapse(B_03_13_debounce_ms_int + 200L)
      png <- output[[paste0(key, "__png")]]
      res[[key]] <<- list(
        dim = H_09_02_pngdim_fn(png),
        png_ok = file.info(png)$size > 0,
        png_name = basename(png))
    }
  })
  want <- lapply(names(res), function(k) {
    s <- B_03_12_shapes_lst[[B_03_16_figures_lst[[k]]$shape]]
    c(s$width_px, s$height_px)
  })
  ok <- mapply(function(r, w) {
    isTRUE(r$png_ok) && all(abs(r$dim - w) <= 1) &&
      abs(r$dim[["width"]] / r$dim[["height"]] - 1.5) < 0.01
  }, res, want)
  H_01_05_check_fn(
    "45 every card's PNG handler writes at the deck shape",
    length(res) == length(B_03_16_figures_lst) && all(ok),
    paste(sprintf("%s %.0fx%.0f", names(res),
                  vapply(res, function(r) r$dim[["width"]], 0),
                  vapply(res, function(r) r$dim[["height"]], 0)),
          collapse = ", "))
})

###### H_11_07: No Spelled-Out Greek Where a Student Reads #####################
# Note: Check 46. Slider labels and card headers carry the letter as an
#   HTML entity, never its name.

local({
  greek <- paste0("\\b(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|",
                  "iota|kappa|lambda|mu|nu|xi|pi|rho|sigma|tau|phi|",
                  "varphi|chi|psi|omega)\\b")
  txt <- c(vapply(B_03_05_controls_lst, function(x) x$label, ""),
           vapply(B_03_16_figures_lst, function(x) x$title, ""))
  txt <- gsub("&[a-z]+;", "", txt)
  hits <- txt[grepl(greek, txt, ignore.case = TRUE)]
  H_01_05_check_fn(
    "46 no slider label or card header spells out a Greek letter",
    length(hits) == 0L,
    if (length(hits) == 0L) sprintf("%d label(s) checked", length(txt))
    else hits[1])
})

#### H_12: Summary #############################################################
# Note: Exit 1 on any failure so the script can gate a deploy.

###### H_12_01: Report and Exit ################################################
# Note: message(), and quit(status = ) rather than stop().

if (H_01_05_fail_int > 0L) {
  message(sprintf("\n%d check(s) FAILED.", H_01_05_fail_int))
  quit(status = 1L)
}

message("\nAll 44 checks passed.")
