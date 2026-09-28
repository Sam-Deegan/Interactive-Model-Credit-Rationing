################################################################################
## Project: ECON42240 Advanced Macroeconomics                                 ##
## Credit Rationing (Stiglitz-Weiss): Model                                   ##
################################################################################

## Author:      Sam Deegan
## Affiliation: University College Dublin
## Email:       sam.deegan@ucdconnect.ie

## Usage:
##   Sourced automatically by app.R. Can be sourced alone from a lecture
##   .qmd so slide figures come from the same model:
##     source("R/model.R")
##
## Inputs:
##   None. Every function is a pure function of a parameter list "par"
##   built by the app.
##
## Outputs:
##   C_01_* functions: the loan rate, the two payoffs, the cut-off, the pool
##   average, the turning point, supply and demand, Value at Risk.
##
## References:
##   Whelan, K. MA Advanced Macroeconomics, part 12 (Default Risk,
##     Collateral and Credit Rationing), part 13 (Banking: Crises and
##     Regulation) for Value at Risk.
##   Stiglitz, J. and Weiss, A. (1981). Credit Rationing in Markets with
##     Imperfect Information. American Economic Review 71(3), 393-410.
##
## Notation (the lecture's Notation frame, symbol for symbol):
##   Whelan part 12 writes R for the loan rate on the pricing slides and R
##   again for the project return in the Stiglitz-Weiss slides; this file
##   writes r for the loan rate and keeps R for the project return. Whelan
##   writes r* for the rate that maximises the bank's expected return; this
##   file writes r_bar, since r* is the natural real rate elsewhere in the
##   course. Stiglitz and Weiss (1981) write r-hat for the loan rate, pi for
##   the firm's payoff and r* for the bank-optimal rate.
##
##   r        the interest rate on a loan
##   r^f      the rate on a risk-free government bond
##   p        the probability the borrower defaults
##   c        the fraction of principal collateral recovers
##   B        the size of the loan applied for
##   C        the collateral the borrower pledges
##   R        the project's return, a random variable
##   theta    the borrower's risk type
##   f(R,th)  the density of returns for type theta
##   P(R,r)   the borrower's payoff
##   th_hat   the safest type still willing to borrow
##   rho      the bank's expected return from a type-theta borrower
##   g, G     the density and cumulative distribution of risk types
##   r_bar    the loan rate maximising the bank's expected return
##   K        the regulatory capital required
##   VaR      the one per cent tail loss
##   RWA      risk-weighted assets
##
## The model (Stiglitz and Weiss 1981 as Whelan part 12 teaches it):
##
##   1. Pricing (Whelan part 12, the default-risk slides). A lender that
##      requires the risk-free rate in expectation charges
##          r = r^f + (1 - c) p.
##      Exactly, r^f = r - pr - p(1-c); Whelan drops pr as small and writes
##      "approximately".
##
##   2. The project. Every borrower seeks B and pledges C. The project
##      returns R >= 0 with density f(R, theta). All types share the mean
##      E[R] = mu and a higher theta is a mean-preserving spread (Stiglitz
##      and Weiss eqs 1-2). The family used here is the lognormal with mean
##      mu and log standard deviation theta,
##          log R ~ N(log mu - theta^2/2, theta^2).
##      Lognormals with a common mean are ordered in the convex order by
##      theta, so the expectation of any convex function of R rises in
##      theta and of any concave function falls in it.
##
##   3. The borrower (Stiglitz and Weiss eq. 4a). Limited liability caps
##      the loss at the collateral:
##          P(R, r) = max(R - (1+r)B, -C),
##      convex in R, with the kink at R = (1+r)B - C and a zero at the
##      amount owed D = (1+r)B.
##
##   4. The bank (Stiglitz and Weiss eq. 4b). Its claim is capped at
##      principal and interest and floored by the collateral:
##          rho(theta, r) = E[min(R + C, (1+r)B)],
##      concave in R, so falling in theta (their Theorem 3). The two
##      payoffs add to the project return state by state,
##          max(R - D, -C) + min(R + C, D) = R,
##      so rho(theta, r) = mu - E[P(theta, r)] exactly. Checked in
##      tests/verify_model.R.
##
##   5. Adverse selection (Theorems 1 and 2). A type borrows when E[P] > 0.
##      E[P] rises in theta, so the applicants are the types above a
##      cut-off th_hat(r), and th_hat rises in r: the safest withdraw first.
##
##   6. The pool (Stiglitz and Weiss eq. 7; Whelan part 12, "Averaging
##      Across the Applicant Pool"). The bank averages over those who apply:
##          E[rho] = int_{th_hat}^{inf} rho(theta,r) g(theta) dtheta
##                   / (1 - G(th_hat)).
##      The support is unbounded above, as the source's integral is. With a
##      bounded support the cut-off reaches the top, the survivors are all
##      near indifference and the return climbs back to mu, so there is no
##      interior peak. g is the density of theta_min plus a lognormal draw:
##      no project is riskless, so below the rate at which the safest type
##      withdraws only the revenue effect acts; g(theta_min) = 0, so the
##      selection effect switches on smoothly, which turns the two-type
##      step into the continuum's single peak; and the tail is heavy, so a
##      few very risky types remain at high rates.
##
##   7. The turning point (Stiglitz and Weiss eqs 6 and 8, per loan rather
##      than per dollar). With a = (1+r)B - C and h = g/(1-G) at th_hat,
##          dE[rho]/dr = B E[1 - F(a, theta) | applies]
##                       - th_hat'(r) h(th_hat) E[E[P] | applies],
##          th_hat'(r) = B [1 - F(a, th_hat)] / (dE[P]/dtheta at th_hat):
##      revenue from every applicant who repays in full, against the
##      applicants' average surplus lost at the hazard rate as the cut-off
##      rises. The maximum is at r_bar and no bank posts a rate above it.
##      tests/verify_model.R check 41 compares this derivative with a
##      finite difference of the solver.
##
##   8. Supply and rationing (Whelan part 12, the credit-rationing slides;
##      Stiglitz and Weiss Theorem 5 and fig. 4). Banks differ in what it
##      costs them to fund a loan, around (1 + r^f) B. A bank lends when
##      the pool's expected return covers its own cost, so the share lending
##      is the distribution function of those costs at E[rho], times a fixed
##      pool of loanable funds L_max. Supply is then a monotone transform of
##      a single-peaked curve and bends backwards above r_bar. Demand falls
##      in r. Low demand crosses supply below r_bar; high demand exceeds
##      supply at r_bar, and the difference is the rationing. Whelan's own
##      qualification is kept: the picture is a bit misleading, because the
##      rate is set by banks, not by an auctioneer. The supply block (banks'
##      cost dispersion, L_max, the demand curve) is the app's own.
##
##   9. Value at Risk (Whelan part 13). The loss on the book is right-
##      skewed. Provisions cover the mean; capital covers the tail. VaR is
##      the cut point at the confidence level, K = 3 VaR, and K >= 0.08 RWA
##      gives RWA = 3 VaR / 0.08 = 37.5 VaR.

#-------------------------------- Script Begin --------------------------------#

################################################################################
## C: Model ####################################################################
################################################################################
# Note: Pure functions. Nothing here touches Shiny and nothing is random:
#   every curve comes from a closed form or from quadrature.

#### C_01: Pricing, Selection, Rationing and Capital ###########################
# Note: The loan rate first, then the two payoffs, then the pool, then the
#   market, then the regulatory tail.

###### C_01_01: The Loan Rate ##################################################
# Note: r = r^f + (1 - c) p, Whelan's pricing rule. Sliders are in per
#   cent; this returns a decimal, because (1 + r) B needs one.

C_01_01_loan_rate_fn <- function(par) {
  (par$r_f + (1 - par$c_rec) * par$p_def) / 100
}

###### C_01_02: The Risk Spread ################################################
# Note: The second term of the pricing rule on its own, in per cent. At full
#   recovery (c = 1) it is zero and the loan prices at the risk-free rate.

C_01_02_spread_fn <- function(par) {
  (1 - par$c_rec) * par$p_def
}

###### C_01_03: The Log Parameters of the Project Return #######################
# Note: The lognormal with mean mu and log standard deviation theta. Holding
#   mu and moving theta is the mean-preserving spread.

C_01_03_lnorm_fn <- function(mu, theta) {
  list(meanlog = log(mu) - theta^2 / 2, sdlog = theta)
}

###### C_01_04: The Density of Project Returns #################################
# Note: f(R, theta) over a grid of R, for the left-hand panel of the first
#   exhibit. Evaluated from the closed form, not simulated.

C_01_04_density_fn <- function(par, theta, grid) {
  ln <- C_01_03_lnorm_fn(par$mu, theta)
  data.frame(
    R       = grid,
    density = stats::dlnorm(grid, meanlog = ln$meanlog, sdlog = ln$sdlog),
    theta   = theta
  )
}

###### C_01_05: The Borrower's Payoff ##########################################
# Note: P(R, r) = max(R - (1+r)B, -C), one value per return. Convex, with
#   the kink at (1+r)B - C and a zero at (1+r)B.

C_01_05_payoff_fn <- function(par, r, grid) {
  owed <- (1 + r) * par$B
  data.frame(
    R      = grid,
    payoff = pmax(grid - owed, -par$C),
    owed   = owed,
    kink   = owed - par$C
  )
}

###### C_01_06: The Borrower's Expected Payoff #################################
# Note: E[P] for a type-theta borrower at loan rate r, in closed form. With
#   a = D - C, E[P] = E[R 1{R > a}] - D (1 - F(a)) - C F(a), and for the
#   lognormal E[R 1{R > a}] = mu Phi(s - z) with z = (log a - m)/s.

C_01_06_borrower_fn <- function(par, theta, r) {
  owed <- (1 + r) * par$B
  cut  <- owed - par$C
  if (cut <= 0) return(rep(par$mu - owed, length(theta)))
  # Vectorised in theta: the pool average calls this on a grid of types
  th   <- pmax(theta, 1e-8)
  ml   <- log(par$mu) - th^2 / 2
  z    <- (log(cut) - ml) / th
  f_cut <- stats::pnorm(z)
  out  <- par$mu * stats::pnorm(th - z) - owed * (1 - f_cut) - par$C * f_cut
  # A riskless type is the limit case, written down rather than approached
  out[theta < 1e-8] <- max(par$mu - owed, -par$C)
  out
}

###### C_01_07: The Bank's Expected Return From One Type #######################
# Note: rho(theta, r) = E[min(R + C, (1+r)B)]. The two payoffs add to R
#   state by state, so rho = mu - E[P] exactly (point 4 of the model note).

C_01_07_bank_fn <- function(par, theta, r) {
  par$mu - C_01_06_borrower_fn(par, theta, r)
}

###### C_01_08: The Top of the Type Grid #######################################
# Note: A practical ceiling for the root search for the cut-off, at the far
#   end of the pool's own distribution. Not an integration limit: see C_01_12.

C_01_08_top_fn <- function(par) {
  par$theta_min + stats::qlnorm(0.9999995, log(par$theta_med), par$theta_s)
}

###### C_01_09: The Cut-Off Type ###############################################
# Note: th_hat(r), the safest type still willing to borrow: the theta at
#   which E[P] = 0. E[P] rises in theta, so every type above it applies.
#   Returns theta_min when even the safest project in the economy wants the
#   loan, and the top of the grid when no type does.

C_01_09_cutoff_fn <- function(par, r) {
  top <- C_01_08_top_fn(par)
  if (C_01_06_borrower_fn(par, par$theta_min, r) >= 0) return(par$theta_min)
  if (C_01_06_borrower_fn(par, top, r) <= 0) return(top)
  stats::uniroot(function(th) C_01_06_borrower_fn(par, th, r),
                 lower = par$theta_min, upper = top, tol = 1e-10)$root
}

###### C_01_10: The Density of Risk Types ######################################
# Note: g(theta). The safest project has spread theta_min and the excess
#   risk over it is lognormal, so the density vanishes at theta_min and the
#   right tail is heavy; see point 6 of the model note.

C_01_10_types_fn <- function(par, theta) {
  stats::dlnorm(theta - par$theta_min, log(par$theta_med), par$theta_s)
}

###### C_01_11: The Share of Types Who Still Apply #############################
# Note: 1 - G(th_hat), the denominator of the pool average, and the place
#   adverse selection enters the bank's return.

C_01_11_share_fn <- function(par, cutoff) {
  1 - stats::plnorm(cutoff - par$theta_min, log(par$theta_med), par$theta_s)
}

###### C_01_12: The Pool Average ###############################################
# Note: E[rho] over the types who apply (point 6 of the model note), by
#   Simpson quadrature in the probability u = G(theta) rather than in theta:
#       E[rho | theta >= th_hat] = int_{G(th_hat)}^{1} rho(theta(u)) du
#                                  / (1 - G(th_hat)).
#   Equal spacing in u puts the nodes where the mass is, which a grid in
#   theta cannot do with a heavy right tail. The top is one part in a
#   million short of 1, and rho is bounded below by the collateral, so what
#   is left out is of that order.

C_01_12_pool_fn <- function(par, r, nodes = 201L) {
  cutoff <- C_01_09_cutoff_fn(par, r)
  share  <- C_01_11_share_fn(par, cutoff)
  if (share <= 1e-8) {
    return(list(rho = NA_real_, cutoff = cutoff, share = 0))
  }
  u_lo <- 1 - share
  u_hi <- 1 - 1e-6
  if (u_hi <= u_lo) {
    return(list(rho = NA_real_, cutoff = cutoff, share = 0))
  }
  u    <- seq(u_lo, u_hi, length.out = nodes)
  th   <- par$theta_min +
    stats::qlnorm(u, log(par$theta_med), par$theta_s)
  rho  <- C_01_07_bank_fn(par, th, r)
  simp <- c(1, rep(c(4, 2), length.out = nodes - 2L), 1)
  list(rho = sum(simp * rho) / sum(simp), cutoff = cutoff, share = share)
}

###### C_01_13: The Expected Return Against the Loan Rate ######################
# Note: The right-hand panel of the second exhibit. One pool average per
#   loan rate; NA once no type will borrow, so the curve stops.

C_01_13_return_curve_fn <- function(par, r_grid) {
  out <- lapply(r_grid, function(r) C_01_12_pool_fn(par, r))
  data.frame(
    r      = r_grid,
    rho    = vapply(out, function(x) x$rho, numeric(1)),
    cutoff = vapply(out, function(x) x$cutoff, numeric(1)),
    share  = vapply(out, function(x) x$share, numeric(1))
  )
}

###### C_01_14: The Same Object With Two Types #################################
# Note: The left-hand panel of the second exhibit (Stiglitz and Weiss 1981,
#   fig. 3). Two types in equal numbers; the safe type withdraws where its
#   E[P] reaches zero. Which side of the drop each point is on is returned.

C_01_14_two_type_fn <- function(par, r_grid) {
  ep_a  <- vapply(r_grid, function(r) C_01_06_borrower_fn(par, par$theta_a, r),
                  numeric(1))
  ep_b  <- vapply(r_grid, function(r) C_01_06_borrower_fn(par, par$theta_b, r),
                  numeric(1))
  rho_a <- par$mu - ep_a
  rho_b <- par$mu - ep_b
  in_a  <- ep_a > 0
  in_b  <- ep_b > 0
  data.frame(
    r     = r_grid,
    rho   = ifelse(in_a & in_b, (rho_a + rho_b) / 2,
                   ifelse(in_b, rho_b, ifelse(in_a, rho_a, NA_real_))),
    rho_a = rho_a, rho_b = rho_b, both = in_a & in_b
  )
}

###### C_01_15: The Rate Where a Type Withdraws ################################
# Note: The step on the two-type panel: the root of E[P] = 0 in r. NA when
#   the type is out at every rate drawn, or in at all of them.

C_01_15_withdraw_fn <- function(par, theta, r_max) {
  lo <- C_01_06_borrower_fn(par, theta, 0)
  hi <- C_01_06_borrower_fn(par, theta, r_max)
  if (lo <= 0 || hi >= 0) return(NA_real_)
  stats::uniroot(function(r) C_01_06_borrower_fn(par, theta, r),
                 lower = 0, upper = r_max, tol = 1e-10)$root
}

###### C_01_16: The Turning Point ##############################################
# Note: r_bar, the loan rate at which the bank's expected return peaks. A
#   coarse grid to bracket it, then optimise inside the bracket.

C_01_16_rbar_fn <- function(par, r_max, n = 81L) {
  grid <- seq(0, r_max, length.out = n)
  df   <- C_01_13_return_curve_fn(par, grid)
  ok   <- which(is.finite(df$rho))
  if (length(ok) == 0L) return(NA_real_)
  i  <- ok[which.max(df$rho[ok])]
  lo <- grid[max(1L, i - 1L)]
  hi <- grid[min(n, i + 1L)]
  if (hi <= lo) return(grid[i])
  stats::optimize(function(r) {
    v <- C_01_12_pool_fn(par, r)$rho
    if (is.finite(v)) v else -Inf
  }, interval = c(lo, hi), maximum = TRUE, tol = 1e-9)$maximum
}

###### C_01_17: The Supply of Loans ############################################
# Note: Point 8 of the model note. Banks' funding costs are spread around
#   (1 + r^f) B with dispersion sigma_c, so the share lending is the
#   distribution function of those costs at E[rho]: supply is a strictly
#   increasing transform of the pool average and peaks where it does.
#   Scaled by a fixed L_max rather than normalised to its own peak, so that
#   anything lowering the bank's return lowers supply; this is what carries
#   Whelan's claim (part 12) that a fall in collateral values raises the
#   extent of rationing. L_max = 128 puts supply at r_bar at 100 to the
#   nearest loan at the defaults. r_bar is an argument for the callers.

C_01_17_supply_fn <- function(par, r_grid, r_bar = NULL) {
  cost <- (1 + par$r_f / 100) * par$B
  df   <- C_01_13_return_curve_fn(par, r_grid)
  data.frame(r = r_grid,
             supply = par$L_max *
               stats::pnorm((df$rho - cost) / par$sigma_c),
             rho = df$rho)
}

###### C_01_18: The Demand for Loans ###########################################
# Note: Falling in the loan rate, from a level the student moves. Written
#   D_0 exp(-eta r) so that it is positive at every rate the axis draws.

C_01_18_demand_fn <- function(par, r_grid, level) {
  data.frame(r = r_grid, demand = level * exp(-par$eta * r_grid),
             level = level)
}

###### C_01_19: The Market at the Turning Point ################################
# Note: Supply and demand at r_bar and the excess demand between them.
#   Positive excess demand at r_bar is the rationing; a demand curve that
#   meets supply below r_bar clears, and its crossing rate is returned too.

C_01_19_market_fn <- function(par, r_max, level, n = 161L, r_bar = NULL) {
  grid  <- seq(0, r_max, length.out = n)
  # r_bar is passed in where the caller has already found it
  if (is.null(r_bar)) r_bar <- C_01_16_rbar_fn(par, r_max)
  cost  <- (1 + par$r_f / 100) * par$B

  # One closure for supply, shared by the quantity at r_bar and the root
  #   finder below
  s_at <- function(r) {
    par$L_max *
      stats::pnorm((C_01_12_pool_fn(par, r)$rho - cost) / par$sigma_c)
  }
  d_at <- function(r) level * exp(-par$eta * r)

  s_bar <- s_at(r_bar)
  d_bar <- d_at(r_bar)

  # The clearing rate is the rate at or below r_bar at which demand has
  #   fallen to supply; below r_bar there is at most one, so a sign change
  #   locates it
  sup_grid <- par$L_max * stats::pnorm(
    (C_01_13_return_curve_fn(par, grid)$rho - cost) / par$sigma_c)
  sup_grid[!is.finite(sup_grid)] <- 0
  excess <- d_at(grid) - sup_grid
  cross  <- NA_real_
  idx    <- which(grid <= r_bar & c(FALSE, diff(sign(excess)) != 0))
  if (length(idx) > 0L) {
    i <- idx[1L]
    cross <- stats::uniroot(function(r) d_at(r) - s_at(r),
                            lower = grid[i - 1L], upper = grid[i],
                            tol = 1e-9)$root
  }

  list(r_bar = r_bar, supply = s_bar, demand = d_bar,
       excess = d_bar - s_bar, clears = cross)
}

###### C_01_20: The Loss Distribution ##########################################
# Note: The fourth exhibit. Losses are right-skewed, so the density is
#   lognormal in the loss with mean "el" and log spread "tail", in millions
#   as Whelan's fifty-million example is (part 13).

C_01_20_loss_fn <- function(par, grid) {
  ln <- C_01_03_lnorm_fn(par$el, par$tail)
  data.frame(loss = grid,
             density = stats::dlnorm(grid, meanlog = ln$meanlog,
                                     sdlog = ln$sdlog))
}

###### C_01_21: Value at Risk and the Capital It Implies #######################
# Note: Whelan part 13. VaR is the cut point at the confidence level, not
#   the size of the tail; K = 3 VaR and K >= 0.08 RWA give RWA = 37.5 VaR.

C_01_21_var_fn <- function(par) {
  ln  <- C_01_03_lnorm_fn(par$el, par$tail)
  var <- stats::qlnorm(par$conf, meanlog = ln$meanlog, sdlog = ln$sdlog)
  list(
    el = par$el, var = var, k = 3 * var, rwa = 3 * var / 0.08,
    tail_prob = 1 - par$conf,
    # The mean loss given that the cut point is exceeded; a readout only,
    #   since no rule uses it
    beyond = par$el *
      stats::pnorm(ln$sdlog - (log(var) - ln$meanlog) / ln$sdlog) /
      (1 - par$conf)
  )
}

###### C_01_22: Readouts #######################################################
# Note: The numbers shown in the tiles above the figures.

C_01_22_diagnostics_fn <- function(par, r_max) {
  r    <- C_01_01_loan_rate_fn(par)
  pool <- C_01_12_pool_fn(par, r)
  var  <- C_01_21_var_fn(par)
  mkt  <- C_01_19_market_fn(par, r_max, par$demand)

  list(
    r        = r,
    spread   = C_01_02_spread_fn(par),
    owed     = (1 + r) * par$B,
    kink     = (1 + r) * par$B - par$C,
    ep_a     = C_01_06_borrower_fn(par, par$theta_a, r),
    ep_b     = C_01_06_borrower_fn(par, par$theta_b, r),
    rho_a    = C_01_07_bank_fn(par, par$theta_a, r),
    rho_b    = C_01_07_bank_fn(par, par$theta_b, r),
    quit_a   = C_01_15_withdraw_fn(par, par$theta_a, r_max),
    quit_b   = C_01_15_withdraw_fn(par, par$theta_b, r_max),
    r_bar    = mkt$r_bar,
    rho_bar  = C_01_12_pool_fn(par, mkt$r_bar)$rho,
    cutoff   = pool$cutoff,
    share    = pool$share,
    rho      = pool$rho,
    supply   = mkt$supply,
    demand   = mkt$demand,
    excess   = mkt$excess,
    clears   = mkt$clears,
    rationed = isTRUE(mkt$excess > 1e-6),
    var      = var$var,
    capital  = var$k,
    rwa      = var$rwa,
    beyond   = var$beyond,
    problems = C_01_23_problems_fn(par)
  )
}

###### C_01_23: Problems with the Calibration ##################################
# Note: Warnings shown above the figures when the numbers stop making sense.
#   Strings are plain ASCII; see CONVENTIONS.md 6.

C_01_23_problems_fn <- function(par) {
  out <- character(0)

  if (isTRUE(par$mu <= par$B)) {
    out <- c(out, paste(
      "The mean project return is no larger than the loan, so no borrower",
      "would apply at any rate. Raise the mean return."
    ))
  }
  # The figures have nothing to draw only when no type in the pool applies
  #   at the rate the pricing rule sets
  if (isTRUE(par$mu > par$B)) {
    r_now <- C_01_01_loan_rate_fn(par)
    if (isTRUE(C_01_11_share_fn(par, C_01_09_cutoff_fn(par, r_now)) < 1e-6)) {
      out <- c(out, paste(
        "At this loan rate no borrower in the pool would apply, so there is",
        "no pool for the bank to average over. Lower the default",
        "probability, raise the recovery rate, or lower the collateral."
      ))
    }
  }
  if (isTRUE(par$theta_a >= par$theta_b)) {
    out <- c(out, paste(
      "The safer of the two drawn types is not safer than the riskier one.",
      "Lower the first risk type or raise the second."
    ))
  }
  if (isTRUE(par$theta_a < par$theta_min)) {
    out <- c(out, paste(
      "The safer drawn type is safer than the safest project in the pool.",
      "Raise it, or lower the safest project."
    ))
  }
  if (isTRUE(par$tail <= 0) || isTRUE(par$el <= 0)) {
    out <- c(out, paste(
      "A loss distribution with no spread has no tail for capital to",
      "cover. Raise the expected loss and the tail thickness."
    ))
  }
  out
}
