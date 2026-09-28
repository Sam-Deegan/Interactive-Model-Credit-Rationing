################################################################################
## Project: ECON42240 Advanced Macroeconomics                                 ##
## Credit Rationing (Stiglitz-Weiss): Interactive Shiny App                   ##
################################################################################

## Author:      Sam Deegan
## Affiliation: University College Dublin
## Email:       sam.deegan@ucdconnect.ie

## Usage:
##   Open app.R in RStudio and click Run App, or from this folder:
##     shiny::runApp()
##   Needs R 4.1 or later with shiny, bslib and ggplot2 installed. A hosted
##   copy runs in the browser at
##   https://sam-deegan.com/toy-models/credit-rationing/
##   The stage selector adds one layer of the model at a time:
##     1  pricing the loan, and why a borrower likes risk
##     2  who applies, and what the bank earns
##     3  the rationing equilibrium
##     4  capital, and the tail the rule is written against
##   The loan is B = 100 throughout, so every money figure reads as an
##   amount per 100 lent. Rates are per cent. All text (scenarios, prompts,
##   equations, notation) lives in B_03.
##
## Inputs:
##   R/model.R (the model) and R/toolkit.R (shared layout and helpers),
##   both sourced automatically by Shiny.
##
## Outputs:
##   Nothing on start-up. Each figure carries a Save PNG button
##   under it, which write that one figure through the toolkit's
##   T_02_03c_export_fn with no title and no legend: 1600x800 for a
##   full-width figure and 1440x720 for one panel of a pair, named
##   credit-rationing-{stage}-{figure}.png.
##
## Packages:
##   shiny, bslib, ggplot2.
##
## Version:
##   B_03_17_version_chr; history in CHANGELOG.md; git tag vX.Y.Z.
##
## References:
##   Whelan, K. MA Advanced Macroeconomics, part 12 (Default Risk,
##     Collateral and Credit Rationing) and part 13 (Banking: Crises and
##     Regulation, for Value at Risk and the capital rule).
##   Stiglitz, J. and Weiss, A. (1981). Credit Rationing in Markets with
##     Imperfect Information. American Economic Review 71(3).
##   Basel Committee (1996). Amendment to the Capital Accord to Incorporate
##     Market Risks, for the 3 x VaR multiplier.

#-------------------------------- Script Begin --------------------------------#

################################################################################
## A: Table of Contents ########################################################
################################################################################
# Note: C (the model) is in R/model.R and T (the toolkit) in R/toolkit.R.
#
#   B: Setup
#   C: Model (R/model.R)
#   T: Toolkit (R/toolkit.R)
#   D: Plots
#     D_00  Shared figure furniture
#     D_01  The loan rate and the borrower's payoff
#     D_02  The bank's expected return
#     D_03  The rationing equilibrium
#     D_04  Capital and the tail
#   E: User Interface
#   F: Server
#   G: Run

################################################################################
## B: Setup ####################################################################
################################################################################
# Note: Packages, options and every soft-coded value.

#### B_01: Packages ############################################################
# Note: Shiny for the app, bslib for the look, ggplot2 for the figures.

###### B_01_01: Load Packages ##################################################
# Note: All three run under shinylive.

library(shiny)
library(bslib)
library(ggplot2)

###### B_01_02: Load the Model #################################################
# Note: Shiny sources R/ itself; this covers sourcing app.R by hand.

if (!exists("C_01_01_loan_rate_fn")) {
  source(file.path("R", "model.R"))
}

###### B_01_03: Load the Toolkit ###############################################
# Note: The shared palette, plot theme, CSS and builders.

if (!exists("T_01_01_palette_vec")) {
  source(file.path("R", "toolkit.R"))
}

#### B_02: Settings ############################################################
# Note: Standard options.

###### B_02_01: Global Options #################################################
# Note: No scientific notation; three significant digits in the console.

options(scipen = 999, digits = 3)

###### B_02_02: Seed ###########################################################
# Note: Nothing here is random; kept for consistency with the other apps.

set.seed(42)

#### B_03: Soft-Coded Objects ##################################################
# Note: Calibration, stages, scenarios, controls, equations, figures, text.

###### B_03_01: Input Defaults #################################################
# Note: Starting value of every control; Reset returns here. The loan is
#   B = 100, so every amount reads as a figure per 100 lent; mu = 130 makes
#   the project worth doing at the risk-free rate. The pool (theta_min,
#   theta_med, theta_s) puts the turning point at a rate a student can read
#   off the panel; point 6 of the model note in R/model.R says why the
#   support is unbounded and the density vanishes at the safest type. The
#   loss distribution is in millions and reproduces the Value at Risk
#   example in Whelan, MA Advanced Macroeconomics, part 13: an expected loss
#   of 10 and a VaR of about 50 at 99 per cent. He calls it weekly; the 1996
#   market-risk rule behind 3 x VaR uses a ten-day VaR, which the on-screen
#   notes follow.

B_03_01_defaults_lst <- list(
  r_f       = 3,     # the risk-free rate, per cent
  p_def     = 5,     # the probability the borrower defaults, per cent
  c_rec     = 0.5,   # the fraction of principal collateral recovers
  B         = 100,   # the loan applied for. Fixed: it is the unit of account
  C         = 25,    # the collateral the borrower pledges
  mu        = 130,   # the mean project return, common to every type
  theta_a   = 0.25,  # the safer of the two drawn types
  theta_b   = 0.90,  # the riskier of the two drawn types
  theta_min = 0.20,  # the safest project in the applicant pool
  theta_med = 0.40,  # median excess risk over the safest project. Fixed
  theta_s   = 1.00,  # how spread out the pool of types is
  sigma_c   = 5,     # dispersion of banks' own funding costs. Fixed
  L_max     = 128,   # loanable funds if every bank lends. Fixed: supply
                     #   at r-bar is 100 at these defaults
  demand    = 85,    # the level of loan demand
  eta       = 1.5,   # sensitivity of demand to the loan rate. Fixed
  el        = 10,    # expected loss on the book, millions
  tail      = 0.85,  # how fat the loss tail is
  conf      = 0.99   # the confidence level Value at Risk is cut at
)

###### B_03_02: Stages #########################################################
# Note: The lecture's order: the pricing problem, then adverse selection
#   and rationing, then the regulatory material.

B_03_02_stages_vec <- c(
  "Stage 1: Pricing the Loan"          = "1",
  "Stage 2: Who Applies and What the Bank Earns" = "2",
  "Stage 3: The Rationing Equilibrium" = "3",
  "Stage 4: Capital and the Tail"      = "4"
)

###### B_03_03: The Rate Axis ##################################################
# Note: How far up the loan rate the second and third exhibits run; the
#   turning point at the defaults is near 33 per cent.

B_03_03_r_max_num <- 0.75

###### B_03_04: Scenarios ######################################################
# Note: Worked examples. Each belongs to one stage and overrides some
#   defaults; unlisted controls return to B_03_01. Story order and wording
#   follow CONVENTIONS.md 3 to 5.

B_03_04_scenarios_lst <- list(
  secured = list(
    label  = "A Well-Collateralised Loan",
    stage  = "1",
    values = list(c_rec = 0.9, p_def = 5, C = 45),
    story  = paste(
      "Collateral recovers nine tenths of the principal (c = 0.9), so the",
      "risk spread (1 − c)p in the pricing rule almost vanishes and the loan",
      "rate (r) prices barely above the risk-free rate (r<sup>f</sup>). The",
      "borrower pledges C = 45 against it. On the payoff panel both axes",
      "move together and in the same direction: the amount owed (1+r)B falls",
      "a little along the project-return axis, but the floor drops from −25",
      "to −45 down the payoff axis and the kink at (1+r)B − C slides left",
      "with it, so much more of the distribution lands on the sloped part.",
      "What decides the size of the effect is c in the rule and C on the",
      "panel, and they are different objects: c is what the lender recovers",
      "and C is what the borrower loses. Watch how much less of the left",
      "tail now sits on the flat."
    ),
    prompt = paste(
      "Better collateral does two things at once. It lowers the rate, and it",
      "raises what the borrower loses on default. Which of the two moves the",
      "payoff picture more?"
    )
  ),
  unsecured = list(
    label  = "An Unsecured Loan",
    stage  = "1",
    values = list(c_rec = 0, p_def = 8, C = 5),
    story  = paste(
      "Nothing is recovered on default (c = 0) and the default probability",
      "is p = 8 per cent, so the whole of that probability is priced into",
      "the spread and the loan rate (r) rises with it. The borrower pledges",
      "almost nothing, C = 5. The two axes of the payoff panel come apart:",
      "along the project-return axis the kink moves right, because the",
      "amount owed (1+r)B is larger and almost none of it is offset by",
      "collateral, while down the payoff axis the floor rises to −5. The",
      "flat stretch is now long and shallow, which is the whole of limited",
      "liability: a borrower with nothing to lose is indifferent between a",
      "bad outcome and a catastrophic one. Watch what that does to the",
      "riskier type's expected payoff in the readouts."
    ),
    prompt = paste(
      "With almost no collateral the floor is nearly flat at zero. Compare",
      "the expected payoff of the two drawn types. Which one gains more from",
      "the unsecured loan?"
    )
  ),
  spread = list(
    label  = "A Wider Spread, Same Mean",
    stage  = "1",
    values = list(theta_a = 0.25, theta_b = 1.30),
    story  = paste(
      "The riskier type's risk (&theta;) is raised to 1.30 while the mean",
      "project return (&mu;) is held at 130: a mean-preserving spread, and",
      "nothing else about the loan changes. On the density panel the two",
      "axes move in",
      "opposite directions — mass slides out along the project-return axis",
      "into both tails, and the peak of the density falls. The payoff panel",
      "does not move at all, because the payoff P(R,r) is a function of the",
      "return, not of the type. That is the point of the pair: the same",
      "convex payoff, read against a wider distribution. The left tail lands",
      "on the flat stretch where it costs the borrower nothing extra, and",
      "the right tail lands on the sloped stretch where it pays one for one.",
      "Watch the riskier type's expected payoff rise while the safer type's",
      "stands still."
    ),
    prompt = paste(
      "Both types have the same mean return. Raise the spread and watch the",
      "expected payoff of the risky type rise. Now say which half of the",
      "distribution the gain came from."
    )
  ),
  pool = list(
    label  = "Two Types, Then a Pool",
    stage  = "2",
    values = list(theta_min = 0.20, theta_s = 1.00, C = 25),
    story  = paste(
      "Nothing is changed from the defaults: this is the base case the two",
      "panels are read against. On the left there are two types and the",
      "bank's expected return drops the moment the safer one's expected",
      "payoff reaches zero and it withdraws. On the right the same object is",
      "computed over a whole pool of types, drawn from the safest project",
      "(&theta;<sub>min</sub> = 0.20) out along a heavy tail. The two axes of",
      "the right panel turn together and then apart: as the loan rate (r)",
      "rises the bank's expected return (&rho;) rises with it while every type",
      "still applies, and falls once the cut-off type &theta;&#770;(r) starts",
      "climbing",
      "through the pool. Where the turn comes is decided by the rate at",
      "which the safest project withdraws; how deep the fall is, by how much",
      "of the pool sits above the cut-off. Watch the share still applying in",
      "the readouts."
    ),
    prompt = paste(
      "Read the two panels as one sequence, not as a comparison. The step on",
      "the left is what the smooth turn on the right is made of. Find the",
      "rate at which the safe type leaves, then find r&#772;."
    )
  ),
  wider_pool = list(
    label  = "A Pool With a Heavier Tail",
    stage  = "2",
    values = list(theta_s = 1.40, theta_min = 0.20),
    story  = paste(
      "The spread of risk types in the pool is widened to 1.40, so there are",
      "many more very risky applicants behind the safe ones while the safest",
      "project (&theta;<sub>min</sub>) is unchanged. Nothing on the borrower's",
      "side moves. The two axes of the right panel come apart: the loan rate",
      "at which the turn happens hardly moves along the horizontal axis,",
      "because that is set by when the safest type withdraws, but the bank's",
      "expected return (&rho;) falls further down the vertical axis on both",
      "sides of it, because the pool the bank is averaging over is worse at",
      "every rate. How deep the fall is, is set by the weight in the tail:",
      "the heavier it is, the more the cut-off leaves behind. Watch the peak",
      "drop and the fall past r&#772; steepen."
    ),
    prompt = paste(
      "The turning point barely moves, but the whole curve drops. Say which",
      "of the two effects — revenue or selection — the heavier tail changed,",
      "and why the other one did not move."
    )
  ),
  clears = list(
    label  = "Low Demand: The Market Clears",
    stage  = "3",
    values = list(demand = 85),
    story  = paste(
      "Loan demand is set to 85, below the largest quantity any bank will",
      "supply. Demand falls in the loan rate (r) and supply follows the",
      "bank's expected return (&rho;), so supply rises up to r&#772; and bends",
      "back above it. The two axes meet: demand comes down the rate axis and",
      "supply comes up it, and they cross at a quantity along the horizontal",
      "axis with the rate below r&#772;, so every applicant who wants a loan",
      "at that rate gets one. What decides whether this is the case is only",
      "the level of demand — nothing about the bank has changed. Watch",
      "the crossing point sit on the rising part of the supply curve, well",
      "short of the bend."
    ),
    prompt = paste(
      "Nothing is rationed here. Raise demand until the crossing point",
      "reaches the bend, and then a little further. What happens to the",
      "loan rate after that?"
    )
  ),
  rationed = list(
    label  = "High Demand: Rationing",
    stage  = "3",
    values = list(demand = 190),
    story  = paste(
      "Loan demand is raised to 190, above the largest quantity supplied.",
      "Demand still falls in the loan rate (r), but it no longer meets",
      "supply anywhere below r&#772;, and no bank posts a rate above r&#772;",
      "because doing so would lower its expected return (&rho;). The two axes",
      "stop moving together: the rate is stuck at r&#772; on the vertical axis",
      "while the horizontal axis carries a gap between what is demanded and",
      "what is supplied. That gap is the rationing, and the applicants inside",
      "it",
      "are refused rather than charged more — the bank cannot tell them from",
      "the ones it accepted. How large the gap is, is set by the level of",
      "demand and by how much the pool deteriorates above r&#772;. Watch the",
      "brace at r&#772;."
    ),
    prompt = paste(
      "The gap at r&#772; is the rationing. Now lower the level of demand",
      "until the gap closes. At what level does the market start clearing",
      "again?"
    )
  ),
  fat = list(
    label  = "A Fat-Tailed Loss Book",
    stage  = "4",
    values = list(el = 10, tail = 0.85, conf = 0.99),
    story  = paste(
      "The bank's own model says losses have an expected value (E[L]) of 10",
      "million and a fat right tail, and the rule cuts the distribution at",
      "the 99th percentile to get Value at Risk (VaR). Provisions cover the",
      "mean; capital covers the tail. The two axes of the panel work against",
      "each other: almost all the probability sits at small losses on the",
      "left of the loss axis and rises high up the density axis, while the",
      "one per cent that matters is spread out flat and far to the right.",
      "What decides the requirement is the cut point alone — K = 3 × VaR and",
      "risk-weighted assets are 37.5 times it — and nothing in the method",
      "says how bad the shaded region is. Watch the average loss beyond the",
      "line in the readouts, and note that no rule uses it."
    ),
    prompt = paste(
      "Value at Risk is the cut point, not the size of the tail. Read the",
      "average loss beyond the line and compare it with VaR itself."
    )
  ),
  quiet = list(
    label  = "A Quiet Sample",
    stage  = "4",
    values = list(el = 10, tail = 0.45, conf = 0.99),
    story  = paste(
      "The bank estimates the tail from a quiet three years, so the fitted",
      "spread of losses falls to 0.45 while the expected loss (E[L]) is left",
      "at 10 million. The two axes move in opposite directions: the density",
      "piles up around the mean and rises steeply up the vertical axis,",
      "while the one per cent cut point slides a long way left along the",
      "loss axis. Value at Risk (VaR) falls, and required capital",
      "(K = 3 × VaR) falls with it, one for one. What decides the whole",
      "requirement is a sample the bank chose. Watch capital fall while",
      "nothing about the loan book has changed at all."
    ),
    prompt = paste(
      "Compare required capital here with the fat-tailed case. The book is",
      "the same book. Who chose the sample, and who checks it?"
    )
  )
)

###### B_03_05: Controls #######################################################
# Note: One entry per numeric control: label (HTML), slider range and step,
#   and the stage from which it appears. B, theta_med, sigma_c, eta and
#   L_max are not controls: the unit of account and four scale parameters,
#   set once in B_03_01.

B_03_05_controls_lst <- list(
  r_f       = list(label = "Risk-Free Rate (r<sup>f</sup>, %)",
                   min = 0, max = 10, step = 0.25, from = 1),
  p_def     = list(label = "Default Probability (p, %)",
                   min = 0, max = 20, step = 0.5, from = 1),
  c_rec     = list(label = "Collateral Recovery (c)",
                   min = 0, max = 1, step = 0.05, from = 1),
  C         = list(label = "Collateral Pledged (C)",
                   min = 0, max = 60, step = 2.5, from = 1),
  mu        = list(label = "Mean Project Return (&mu;)",
                   min = 105, max = 180, step = 2.5, from = 1),
  theta_a   = list(label = "The Safer Type (&theta;)",
                   min = 0.05, max = 1, step = 0.05, from = 1),
  theta_b   = list(label = "The Riskier Type (&theta;)",
                   min = 0.2, max = 2, step = 0.05, from = 1),
  theta_min = list(label = paste0("The Safest Project in the Pool ",
                                  "(&theta;<sub>min</sub>)"),
                   min = 0.05, max = 0.6, step = 0.05, from = 2),
  theta_s   = list(label = "How Spread the Pool Is (&sigma;<sub>&theta;</sub>)",
                   min = 0.4, max = 1.8, step = 0.1, from = 2),
  demand    = list(label = "Loan Demand (D<sub>0</sub>)",
                   min = 40, max = 260, step = 5, from = 3),
  el        = list(label = "Expected Loss (E[L], millions)",
                   min = 2, max = 30, step = 1, from = 4),
  tail      = list(label = "How Fat the Loss Tail Is (&sigma;<sub>L</sub>)",
                   min = 0.2, max = 1.6, step = 0.05, from = 4),
  conf      = list(label = "Confidence Level (q)",
                   min = 0.9, max = 0.999, step = 0.005, from = 4)
)

###### B_03_06: Parameter Explanations #########################################
# Note: What each control is and what raising it does.

B_03_06_help_lst <- list(
  r_f = paste(
    "The rate on a risk-free government bond. It is what a lender must earn",
    "in expectation, so it is the floor under every loan rate, and it is",
    "also what a bank pays to fund itself."
  ),
  p_def = paste(
    "How likely the borrower is to default. In the pricing rule it enters",
    "only through the spread (1 − c)p, so it costs nothing when collateral",
    "recovers the whole principal."
  ),
  c_rec = paste(
    "The fraction of the PRINCIPAL the lender recovers on default. At c = 1",
    "the loan prices at the risk-free rate however likely default is. Not",
    "the same object as the collateral the borrower pledges."
  ),
  C = paste(
    "What the borrower loses if the project cannot repay. It is the floor",
    "under the borrower's payoff and the cushion under the bank's, and it",
    "is what makes a safe borrower drop out when the rate rises."
  ),
  mu = paste(
    "The mean return on the project, the same for every risk type. Raising",
    "it makes the project worth doing at a higher loan rate, so it moves",
    "the turning point to the right."
  ),
  theta_a = paste(
    "The risk of the safer of the two types drawn on the figures. Risk here",
    "is spread, not expected return: every type has the same mean."
  ),
  theta_b = paste(
    "The risk of the riskier of the two drawn types. Raising it is a",
    "mean-preserving spread: both tails get heavier and the mean does not",
    "move."
  ),
  theta_min = paste(
    "The spread of the safest project anybody has. Nothing is riskless, and",
    "it matters: the loan rate at which THIS type withdraws is where",
    "adverse selection starts."
  ),
  theta_s = paste(
    "How widely risk types are spread through the pool. A wider pool has a",
    "heavier tail of very risky applicants, so the bank's average return is",
    "lower at every rate."
  ),
  demand = paste(
    "The level of loan demand. It is the only thing separating the clearing",
    "case from the rationed one, which is why rationing is not a special",
    "assumption."
  ),
  el = paste(
    "The mean of the loss distribution. This is the number provisions are",
    "set against: the bank writes down part of the book each year in",
    "anticipation of it."
  ),
  tail = paste(
    "How much weight the loss distribution puts far out to the right. It is",
    "estimated from the assets' own past returns, so the sample the bank",
    "chooses sets it."
  ),
  conf = paste(
    "Where the tail is cut. At 0.99 the Value at Risk is exceeded one",
    "ten-day period in a hundred. The method says nothing at all about how",
    "bad those periods are."
  )
)

###### B_03_07: Prompts ########################################################
# Note: One "what to try" prompt per stage, shown above the figures.

B_03_07_prompts_lst <- list(
  "1" = paste(
    "A lender that needs the risk-free rate in expectation charges",
    "r = r<sup>f</sup> + (1 − c)p. Then look at what the borrower faces:",
    "limited liability caps the loss at the collateral, so the payoff is",
    "convex, and a wider spread at the same mean raises its expectation."
  ),
  "2" = paste(
    "Raising the loan rate does two things at once. Every performing loan",
    "pays more, and the pool of applicants gets worse. The left panel makes",
    "the second effect a step; the right panel smooths it into a peak at",
    "r&#772;."
  ),
  "3" = paste(
    "No bank posts a rate above r&#772;, so supply bends backwards there.",
    "Whether the market clears is then a question about demand and nothing",
    "else.",
    "Move the level of demand and watch the gap open."
  ),
  "4" = paste(
    "Provisions cover the mean of the loss distribution and capital covers",
    "the tail. Value at Risk is where the tail is cut, not how bad it is.",
    "Move the fatness of the tail and watch required capital follow."
  )
)

###### B_03_08: The Model, Stage by Stage ######################################
# Note: The equations panel, in the shape of is-mp-pc's B_03_16: one item
#   per equation. "label" is the object's full name, "versions" maps the
#   stage a form first applies to its LaTeX, "notes" is the in-words gloss
#   for that stage; the symbols are B_03_10's job. The expressions follow
#   Whelan, MA Advanced Macroeconomics, part 12 (the Stiglitz-Weiss frames)
#   and part 13 (Value at Risk), in the lecture's own symbols. No item has a
#   second version: each stage adds objects (the pool at 2, the market at 3,
#   the capital rule at 4) and revises none, so the panel's "changed" flag
#   never fires here.

B_03_08_equations_lst <- list(

  # --- The model's equations --------------------------------------------------
  list(
    group = "model", label = "The Pricing Rule",
    versions = list("1" = paste0("r = r^{f} + (1-c)\\,p,\\quad",
                                 " (1-p)\\,r - p\\,(1-c) = r^{f} - p\\,r")),
    notes = list(
      "1" = paste("The rate the app charges is the deck's rule, and the second",
                  "equation is exactly what it earns: repaid with probability",
                  "1&minus;p, losing 1&minus;c of the principal otherwise,",
                  "the lender's expected return is r<sup>f</sup> &minus; pr.",
                  "That shortfall pr is what Whelan's",
                  "&ldquo;approximately&rdquo; drops as small (0.3 percentage",
                  "points at the defaults).")
    )
  ),
  list(
    group = "model", label = "The Borrower's Payoff",
    versions = list("1" = "P(R,r) = \\max\\big(R - (1+r)B,\\; -C\\big)"),
    notes = list(
      "1" = paste("Limited liability caps the loss at the collateral. The",
                  "payoff is flat at &minus;C up to a return of",
                  "(1+r)B &minus; C, then rises one for one, so it is",
                  "convex in R and crosses zero at (1+r)B.")
    )
  ),
  list(
    group = "model", label = "The Bank's Payoff",
    versions = list("2" = paste0("\\rho(\\theta,r) = \\int_{0}^{\\infty}",
                                 "\\min\\big(R+C,\\;(1+r)B\\big)",
                                 " f(R,\\theta)\\,dR")),
    notes = list(
      "2" = paste("In good states the bank receives principal and interest",
                  "and no more; in bad states it receives the collateral.",
                  "The upside is truncated and the downside is not, which is",
                  "concavity, so a higher &theta; LOWERS the bank's return.")
    )
  ),
  list(
    group = "model", label = "The Cut-Off",
    versions = list("2" = paste0("E\\big[P(R,r)\\big] > 0",
                                 " \\iff \\theta > \\hat\\theta(r),",
                                 "\\quad \\hat\\theta(r) = \\max\\big\\{",
                                 "\\theta_{\\min},\\ \\theta :",
                                 " E\\big[P(R,r)\\big] = 0\\big\\}")),
    notes = list(
      "2" = paste("A firm borrows when its expected payoff is positive. That",
                  "expectation rises in &theta;, so the applicants are the",
                  "types above the one that is exactly indifferent, and that",
                  "type rises with the rate: the safest borrowers withdraw",
                  "first. Until even the safest project in the pool",
                  "(&theta;<sub>min</sub>) would rather not borrow, everyone",
                  "applies and the cut-off sits at &theta;<sub>min</sub>.")
    )
  ),
  list(
    group = "model", label = "The Pool Average",
    versions = list("2" = paste0("E\\big[\\rho(\\theta,r)\\big] = ",
                                 "\\frac{\\displaystyle\\int_{\\hat\\theta(r)}",
                                 "^{\\infty}\\rho(\\theta,r)g(\\theta)",
                                 "d\\theta}{1 - G\\big(\\hat\\theta(r)",
                                 "\\big)}")),
    notes = list(
      "2" = paste("The bank cannot tell the types apart, so it averages over",
                  "those who apply. The denominator is the share of types",
                  "above the cut-off, and it is where adverse selection",
                  "enters the bank's return.")
    )
  ),
  list(
    group = "model", label = "Loan Supply and Loan Demand",
    versions = list("3" = paste0("L^{s}(r) = L_{\\max}\\,\\Phi\\Big(",
                                 "\\frac{E\\big[\\rho(\\theta,r)\\big]",
                                 " - (1+r^{f})B}{\\sigma_{c}}\\Big),",
                                 "\\quad L^{d}(r) = D_{0}\\,e^{-\\eta r}")),
    notes = list(
      "3" = paste("What the third exhibit draws. Banks' funding costs are",
                  "spread around (1+r<sup>f</sup>)B, and a bank lends when",
                  "the pool's expected return covers its own cost, so supply",
                  "is the funds available (L<sub>max</sub>) times the share",
                  "of banks lending. It is a rising function of the pool",
                  "average, so it inherits its turn at r&#772;: the backward",
                  "bend is the selection effect and not an assumption laid on",
                  "top of it. Anything that lowers the bank's expected return",
                  "lowers supply. Demand falls in the rate from a level the",
                  "student moves.")
    )
  ),
  list(
    group = "model", label = "The Value at Risk (VaR) Cut Point",
    versions = list("4" = paste0("\\Pr\\big(L > \\mathrm{VaR}\\big) = 1 - q,",
                                 "\\quad \\log L \\sim \\mathcal{N}\\big(",
                                 "\\log E[L] - \\tfrac{1}{2}\\sigma_{L}^{2},",
                                 "\\ \\sigma_{L}^{2}\\big)")),
    notes = list(
      "4" = paste("Value at Risk is the loss the book exceeds with",
                  "probability 1 &minus; q, so it is a CUT POINT on the",
                  "loss distribution and not the size of the loss beyond",
                  "it. The book's losses are lognormal with mean E[L] and",
                  "log spread &sigma;<sub>L</sub>, which is what makes the",
                  "tail fat. Whelan's example, on the rule's own ten-day",
                  "horizon: a ten-day, 99 per cent VaR of fifty million is",
                  "exceeded one ten-day period in a hundred.")
    )
  ),
  list(
    group = "model",
    label = "From Value at Risk (VaR) to Risk-Weighted Assets (RWA)",
    versions = list("4" = paste0("0.08\\times\\mathrm{RWA} = K = ",
                                 "3\\times\\mathrm{VaR}",
                                 " \\implies \\mathrm{RWA} = ",
                                 "37.5\\times\\mathrm{VaR}")),
    notes = list(
      "4" = paste("Under an internal model the bank's own estimate sets the",
                  "denominator of its own ratio. With the capital rule met",
                  "exactly, three divided by 0.08 is 37.5, so risk-weighted",
                  "assets are 37.5 times the modelled tail loss. The deck",
                  "writes the chain the same way. The 3 &times; VaR rule is",
                  "the Basel Committee's 1996 market-risk amendment: a",
                  "ten-day, 99 per cent VaR times a multiplier of at least",
                  "three. Whelan presents it under the IRB approach and says",
                  "&ldquo;usually three&rdquo;.")
    )
  ),

  # --- Assumptions ------------------------------------------------------------
  list(
    group = "assumption", label = "One Mean, Many Spreads",
    versions = list("1" = paste0("E[R\\mid\\theta] = \\mu,\\quad",
                                 " \\log R \\sim \\mathcal{N}\\big(",
                                 "\\log\\mu - \\tfrac{1}{2}\\theta^{2},",
                                 "\\ \\theta^{2}\\big)")),
    notes = list(
      "1" = paste("Every type has the same expected project return. A higher",
                  "&theta; is a mean-preserving spread: both tails get",
                  "heavier and the mean does not move. The app's density is",
                  "the lognormal with that mean and log spread &theta;, which",
                  "is a mean-preserving spread in &theta; exactly. Without",
                  "this the model would just be about good and bad borrowers.")
    )
  ),
  list(
    group = "assumption", label = "The Pool of Types",
    versions = list("2" = paste0("\\log\\big(\\theta - \\theta_{\\min}\\big)",
                                 " \\sim \\mathcal{N}\\big(",
                                 "\\log\\theta_{\\mathrm{med}},",
                                 "\\ \\sigma_{\\theta}^{2}\\big)")),
    notes = list(
      "2" = paste("What g(&theta;) is in the app. No project is riskless:",
                  "the safest has spread &theta;<sub>min</sub>, and the",
                  "excess risk over it has a long right tail whose weight",
                  "&sigma;<sub>&theta;</sub> sets. The tail is what keeps the",
                  "selection effect alive at high rates.")
    )
  ),
  list(
    group = "assumption", label = "Observationally Identical",
    versions = list("1" = "\\theta \\text{ is not observed by the bank}"),
    notes = list(
      "1" = paste("The bank sees nothing that separates the types. That is",
                  "what makes the loan rate the only instrument it has, and",
                  "it is why the applicants who are refused look exactly",
                  "like the ones who are served.")
    )
  ),
  list(
    group = "assumption", label = "Banks Post the Rate",
    versions = list("3" = "\\text{no auctioneer clears the loan market}"),
    notes = list(
      "3" = paste("Banks choose the rate; nothing forces it up until the",
                  "market clears. Whelan's own qualification, kept: the",
                  "backward-bending supply curve &ldquo;is a bit",
                  "misleading&rdquo; for exactly this reason. Draw it",
                  "because it is what the literature draws, and say out loud",
                  "that banks set the rate.")
    )
  ),

  # --- Solved forms -----------------------------------------------------------
  list(
    group = "solved", label = "Why the Loan Rate Is That",
    versions = list("1" = paste0("(1-p)\\,r - p\\,(1-c) = r^{f}",
                                 "\\implies r = \\frac{r^{f} + (1-c)\\,p}",
                                 "{1-p} \\approx r^{f} + (1-c)\\,p")),
    notes = list(
      "1" = paste("The appendix derivation, in one line. A loan of principal",
                  "1 repays in full with probability 1&minus;p and loses",
                  "1&minus;c on default. Setting the expected return equal",
                  "to the risk-free rate gives the exact rate on the left;",
                  "dropping the small product pr gives the rule the app",
                  "charges (5.8 against 5.5 per cent at the defaults).")
    )
  ),
  list(
    group = "solved", label = "The Two Payoffs Add to the Return",
    versions = list("2" = paste0("\\max\\big(R-(1+r)B,\\,-C\\big) + ",
                                 "\\min\\big(R+C,\\,(1+r)B\\big) = R")),
    notes = list(
      "2" = paste("State by state, whatever the borrower keeps and whatever",
                  "the bank receives add to the project return. So the",
                  "bank's expected return is the mean project return less",
                  "the borrower's expected payoff, and the concavity result",
                  "is the arithmetic complement of the convexity one rather",
                  "than a second derivation.")
    )
  ),
  list(
    group = "solved", label = "The Turning Point",
    versions = list("2" = paste0(
      "\\begin{aligned}",
      "\\frac{\\mathrm{d}\\,E[\\rho]}{\\mathrm{d}r} &= ",
      "B\\,E\\big[1 - F\\big((1+r)B - C,\\theta\\big)",
      "\\mid \\theta > \\hat\\theta\\big]",
      " - \\hat\\theta'(r)\\,\\frac{g(\\hat\\theta)}{1 - G(\\hat\\theta)}",
      "\\,E\\big[E[P] \\mid \\theta > \\hat\\theta\\big],\\\\",
      "\\hat\\theta'(r) &= \\frac{B\\,\\big[1 - F\\big((1+r)B - C,",
      "\\hat\\theta\\big)\\big]}{\\partial E[P]/\\partial\\theta},",
      "\\qquad \\bar r = \\arg\\max_{r} E\\big[\\rho(\\theta,r)\\big]",
      "\\end{aligned}")),
    notes = list(
      "2" = paste("The first term is the revenue effect: every applicant who",
                  "repays in full pays B more per point on the rate. The",
                  "second is the selection effect: the cut-off rises, and",
                  "each type lost takes the applicants' average surplus",
                  "with it at the hazard rate of the type density. Until the",
                  "safest project withdraws the cut-off does not move and",
                  "only revenue acts. r&#772; is where the two are equal.")
    )
  ),
  list(
    group = "solved", label = "Rationing",
    versions = list("3" = paste0(
      "L^{d}(\\bar r) > L^{s}(\\bar r) = L_{\\max}\\,\\Phi\\Big(",
      "\\frac{\\max_{r} E\\big[\\rho(\\theta,r)\\big] - (1+r^{f})B}",
      "{\\sigma_{c}}\\Big)\\ \\text{ and no bank posts } r>\\bar r")),
    notes = list(
      "3" = paste("Excess demand persists at the rate the banks post,",
                  "because raising the rate would lower the expected return.",
                  "The most banks will ever lend is set by the PEAK of the",
                  "bank's expected return, so in this model lower collateral,",
                  "which lowers that peak, lowers the most supplied and widens",
                  "the gap (Whelan's recession claim; Stiglitz and Weiss show",
                  "raising collateral can also lower the return).",
                  "The frame's own wording is kept: the loan market",
                  "&ldquo;need not clear&rdquo;, not that it does not.")
    )
  ),
  list(
    group = "solved",
    label = "Risk-Weighted Assets (RWA) Under the Original Schedule",
    versions = list("4" = paste0("\\mathrm{RWA} = 100(0) + 300(0.2)",
                                 " + 600(0.5) = 360,\\quad",
                                 " K = 0.08(360) = 28.8")),
    notes = list(
      "4" = paste("The appendix drill, under the ORIGINAL schedule of",
                  "weights rather than an internal model. Required capital",
                  "of 28.8 is 2.9 per cent of total assets of 1000, which is",
                  "the gap between a capital ratio and a leverage ratio in",
                  "one line.")
    )
  ),

  # --- Descriptors ------------------------------------------------------------
  list(
    group = "descriptor", label = "Two Effects of a Higher Rate",
    versions = list("2" = paste0("\\underbrace{\\text{revenue}}_{+}",
                                 "\\ \\text{ against }\\ ",
                                 "\\underbrace{\\text{selection}}_{-}")),
    notes = list(
      "2" = paste("Every performing loan pays more, which raises the return;",
                  "the pool grows riskier, which lowers it. Beyond",
                  "r&#772; the second dominates. This is the examined step,",
                  "and a Section C answer that states adverse selection",
                  "without deriving the cut-off from E[P] &gt; 0 has skipped",
                  "the model.")
    )
  ),
  list(
    group = "descriptor", label = "Who Is Refused",
    versions = list("3" = "\\text{applicants identical to those served}"),
    notes = list(
      "3" = paste("The refused borrowers are not the ones the bank has",
                  "identified as bad. It cannot identify them at all. Whelan",
                  "keeps the hedge that rationing &ldquo;can often be quite",
                  "severe&rdquo; and that borrowers who &ldquo;appear to be",
                  "good credits&rdquo; are turned down.")
    )
  ),
  list(
    group = "descriptor", label = "What the Method Does Not Say",
    versions = list("4" = paste0("E\\big[L \\mid L > \\mathrm{VaR}\\big]",
                                 "\\ \\text{ enters no rule}")),
    notes = list(
      "4" = paste("Value at Risk is the cut point, not the size of the tail.",
                  "What lies past the line is unmeasured, and selling",
                  "insurance against rare events puts the payout outside the",
                  "window entirely, so it never enters the reported number.")
    )
  )
)

###### B_03_09: Equation Group Titles ##########################################
# Note: Group headings for the equations tabs.

B_03_09_groups_vec <- c(
  model      = "Model Equations",
  assumption = "Assumptions",
  solved     = "Solved Forms",
  descriptor = "Descriptors"
)

###### B_03_10: Notation Key ###################################################
# Note: Notation tab, one entry per symbol, in the shape of is-mp-pc's
#   B_03_23: grp is "var", "par" or "tgt" (nothing in Stiglitz-Weiss is a
#   shock), sym the LaTeX, txt a lower-case gloss, from the stage it first
#   appears. Eighteen rows are the lecture's Notation frame; the rest are
#   objects the app draws or computes (mu, L^s, L^d, D_0, eta, L, q and the
#   pool, supply and loss parameters). Two departures from Whelan, MA
#   Advanced Macroeconomics, part 12: r for the loan rate (he writes R,
#   which clashes with the project return) and r-bar for the turning point
#   (he writes r*, the natural real rate elsewhere in the course). Bare L
#   is the loss on the book at stage 4; loan quantities are L^s and L^d.

B_03_10_notation_lst <- list(
  list(grp = "var", sym = "r", txt = "the interest rate on a loan", from = 1),
  list(grp = "par", sym = "r^{f}",
       txt = "the rate on a risk-free government bond", from = 1),
  list(grp = "par", sym = "p", txt = "the probability the borrower defaults",
       from = 1),
  list(grp = "par", sym = "c",
       txt = "the fraction of principal collateral recovers", from = 1),
  list(grp = "par", sym = "B", txt = "the size of the loan applied for",
       from = 1),
  list(grp = "par", sym = "C", txt = "the collateral the borrower pledges",
       from = 1),
  list(grp = "var", sym = "R", txt = "the project's return, a random variable",
       from = 1),
  list(grp = "par", sym = "\\theta", txt = "the borrower's risk type",
       from = 1),
  list(grp = "par", sym = "\\mu",
       txt = "the mean project return, common to every type", from = 1),
  list(grp = "var", sym = "f(R,\\theta)",
       txt = "the density of returns for type \u03b8", from = 1),
  list(grp = "var", sym = "F(R,\\theta)",
       txt = "the cumulative distribution of returns for type \u03b8",
       from = 2),
  list(grp = "var", sym = "P(R,r)", txt = "the borrower's payoff", from = 1),
  list(grp = "var", sym = "\\hat\\theta(r)",
       txt = "the safest type still willing to borrow", from = 2),
  list(grp = "var", sym = "\\rho(\\theta,r)",
       txt = "the bank's expected return from type \u03b8", from = 2),
  list(grp = "var", sym = "g(\\theta)", txt = "the density of risk types",
       from = 2),
  list(grp = "var", sym = "G(\\theta)",
       txt = "the cumulative distribution of types", from = 2),
  list(grp = "par", sym = "\\theta_{\\min}",
       txt = "the risk of the safest project in the pool", from = 2),
  list(grp = "par", sym = "\\theta_{\\mathrm{med}}",
       txt = "the median excess risk over the safest project", from = 2),
  list(grp = "par", sym = "\\sigma_{\\theta}",
       txt = "the log spread of excess risk across the pool", from = 2),
  list(grp = "tgt", sym = "\\bar r",
       txt = "the loan rate maximising the bank's return", from = 2),
  list(grp = "var", sym = "L^{s}", txt = "loans supplied", from = 3),
  list(grp = "var", sym = "L^{d}", txt = "loans demanded", from = 3),
  list(grp = "par", sym = "D_{0}",
       txt = "the level of loan demand", from = 3),
  list(grp = "par", sym = "\\eta",
       txt = "the sensitivity of demand to the loan rate", from = 3),
  list(grp = "par", sym = "L_{\\max}",
       txt = "the loans supplied if every bank lends", from = 3),
  list(grp = "par", sym = "\\sigma_{c}",
       txt = "the dispersion of banks' funding costs", from = 3),
  list(grp = "var", sym = "\\Phi",
       txt = "the standard normal cumulative distribution", from = 3),
  list(grp = "var", sym = "L",
       txt = "the loss on the bank's book (millions)", from = 4),
  list(grp = "par", sym = "q",
       txt = "the confidence level the tail is cut at", from = 4),
  list(grp = "par", sym = "\\sigma_{L}",
       txt = "the log spread of losses, how fat the tail is", from = 4),
  list(grp = "tgt", sym = "\\mathrm{VaR}",
       txt = "the loss exceeded with probability 1 - q",
       from = 4),
  list(grp = "tgt", sym = "K", txt = "the regulatory capital required",
       from = 4),
  list(grp = "var", sym = "\\mathrm{RWA}", txt = "risk-weighted assets",
       from = 4)
)

###### B_03_11: Notation Columns ###############################################
# Note: How the notation tab is split into columns, one column per group.

B_03_11_nota_cols_lst <- list(
  "Variables"  = "var",
  "Parameters" = "par",
  "Thresholds and Requirements" = "tgt"
)

###### B_03_12: Figure Shapes ##################################################
# Note: Export size of each figure shape, both 2:1 as T_02_03c_export_fn
#   writes them: "pair" is one panel of a half-width pair, "wide" a
#   full-width figure. "title_chr" is how many characters of title fit on
#   one line of that canvas.

B_03_12_shapes_lst <- list(
  wide = list(width_px = 1600L, height_px = 800L, title_chr = 46L),
  pair = list(width_px = 1440L, height_px = 720L, title_chr = 38L)
)

###### B_03_12a: The Floor on the Aspect Ratio #################################
# Note: Width over height; every figure drawn or exported is at least this.

B_03_12a_min_ratio_num <- 1.5

###### B_03_12b: Enforce the Floor at Load #####################################
# Note: Stops the app at load if a declared shape is squarer than the
#   floor.

B_03_12b_check_fn <- function(shapes, floor) {
  for (nm in names(shapes)) {
    ratio <- shapes[[nm]]$width_px / shapes[[nm]]$height_px
    if (ratio < floor) {
      stop(sprintf("Figure shape '%s' is %.2f:1, squarer than the %.2f:1 ",
                   nm, ratio, floor), "floor.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

B_03_12b_ratios_ok_lgl <- B_03_12b_check_fn(B_03_12_shapes_lst,
                                            B_03_12a_min_ratio_num)

###### B_03_13: Recalculation Delay ############################################
# Note: Milliseconds to wait before recalculating after a change; longer
#   than the other apps', as the pool average is a quadrature at every rate.

B_03_13_debounce_ms_int <- 400L

###### B_03_14: The Two Demand Levels ##########################################
# Note: The clearing case and the rationed case. The third exhibit always
#   draws both: low demand clears below r-bar and high demand does not.

B_03_14_demand_vec <- c(low = 85, high = 190)

###### B_03_15: The App's Own Name #############################################
# Note: The first part of every exported file name.

B_03_15_slug_chr <- "credit-rationing"

###### B_03_16: The Figure Register ############################################
# Note: One entry per figure: title, shape and export file name. The UI
#   builds the card from this and the server the plot and its download
#   handlers. "file" is the middle of {app}-{stage}-{figure}.png.

B_03_16_figures_lst <- list(
  spread = list(
    title = "A Mean-Preserving Spread",
    shape = "pair", file = "spread", from = 1),
  payoff = list(
    title = "The Borrower's Convex Payoff",
    shape = "pair", file = "payoff", from = 1),
  twotype = list(
    title = "Two Borrower Types",
    shape = "pair", file = "two-types", from = 2),
  continuum = list(
    title = "A Continuum of Types",
    shape = "pair", file = "continuum", from = 2),
  market = list(
    title = "Rationed, or Not, Depending on Demand",
    shape = "wide", file = "market", from = 3),
  var = list(
    title = "The Value at Risk Loss Distribution",
    shape = "wide", file = "loss-distribution", from = 4)
)

###### B_03_17: Version ########################################################
# Note: Semantic version, shown in the footer; CHANGELOG.md has the history.

B_03_17_version_chr <- "1.0.6"

###### B_03_18: Source Repository ##############################################
# Note: The GitHub repo, linked from the footer.

B_03_18_repo_chr <- paste0("https://github.com/Sam-Deegan/",
                        "Interactive-Model-Credit-Rationing")

#### B_04: Paths ###############################################################
# Note: The QR code only.

###### B_04_01: QR Code Source #################################################
# Note: Found by the toolkit; www/ first.

B_04_01_qr_src_chr <- T_07_04_qr_fn()

################################################################################
## D: Plots ####################################################################
################################################################################
# Note: Builders only; each returns a ggplot for the server to draw.

#### D_00: Shared Figure Furniture #############################################
# Note: Pieces more than one figure needs.

###### D_00_01: A Curly Brace ##################################################
# Note: A horizontal brace from x0 to x1 at height y, tip "depth" away from
#   the baseline; four quarter arcs scaled onto the panel. See CONVENTIONS.md
#   6: a distance between two curves is a brace, not a band.

D_00_01_brace_fn <- function(x0, x1, y, depth, n = 40L) {
  phi <- seq(0, pi / 2, length.out = n)
  u   <- c(0.25 * sin(phi), 0.25 + 0.25 * (1 - cos(phi)))
  o   <- c(0.5 * (1 - cos(phi)), 0.5 + 0.5 * sin(phi))
  u   <- c(u, 1 - rev(u))
  o   <- c(o, rev(o))
  data.frame(x = x0 + (x1 - x0) * u, y = y + depth * o)
}

###### D_00_02: A Label at the End of a Line ###################################
# Note: A curve's name at the end of its line, nudged clear of the last
#   point; no legend. See CONVENTIONS.md 6.

D_00_02_endlab_fn <- function(x, y, label, colour, hjust = 0, nudge_x = 0,
                              nudge_y = 0, size = 3.4, parse = FALSE) {
  ggplot2::annotate("text", x = x + nudge_x, y = y + nudge_y, label = label,
                    hjust = hjust, vjust = 0.5, size = size, colour = colour,
                    parse = parse)
}

###### D_00_02a: A Curve's Name, as Plotmath ###################################
# Note: Name plus the parameter that separates two states of one curve, as
#   two lines of plotmath. Every word on a panel is ASCII or plotmath, since
#   shinylive draws under a C locale; see CONVENTIONS.md 6.

D_00_02a_name_fn <- function(name, sym, value, digits = 2) {
  paste0("atop('", name, "', (", sym, " == ",
         T_02_05_num_fn(value, digits), "))")
}

###### D_00_02b: Keep a Label Inside the Panel #################################
# Note: Clamps a label's height inside the panel with a margin, so a name
#   hung off a curve that ends near an edge is not lost.

D_00_02b_inside_fn <- function(y, lim, pad = 0.06) {
  min(max(y, lim[1] + pad * diff(lim)), lim[2] - pad * diff(lim))
}

###### D_00_03: The Rate Grid ##################################################
# Note: The loan rates the second and third exhibits are drawn over; one
#   grid, so the exhibits and the readouts agree on where a curve sits.

D_00_03_rgrid_fn <- function(n = 151L) {
  seq(0, B_03_03_r_max_num, length.out = n)
}

###### D_00_04: Per Cent on an Axis ############################################
# Note: A rate axis carries per cent, not a decimal. Passed through the
#   marking helpers' "labels" argument.

D_00_04_pct_fn <- function(x) {
  paste0(formatC(x * 100, format = "f", digits = 0), "%")
}

#### D_01: The Loan Rate and the Borrower's Payoff #############################
# Note: The first exhibit as two panels sharing the project-return axis, so
#   the kink on the right sits under the densities on the left.

###### D_01_00: The Shared Return Axis #########################################
# Note: Both panels take these limits as an argument, with no default, so
#   the pair shares one return axis.

D_01_00_rlim_fn <- function(par) {
  # Holds the mean, the amount owed and the bulk of both densities; the far
  #   tail of the wider one runs off the axis
  c(0, max(2.4 * par$mu, 1.5 * (1 + C_01_01_loan_rate_fn(par)) * par$B))
}

###### D_01_01: A Mean-Preserving Spread #######################################
# Note: Two densities over the project return with one mean, one visibly
#   wider. The mean is dotted and named on the top axis; arrows show which
#   way the mass went.

D_01_01_spread_fn <- function(par, xlim, ref = NULL) {
  grid <- seq(0.5, xlim[2], length.out = 400)
  a    <- C_01_04_density_fn(par, par$theta_a, grid)
  b    <- C_01_04_density_fn(par, par$theta_b, grid)
  a$line <- "safer"
  b$line <- "riskier"
  lab_a  <- D_00_02a_name_fn("Project return", "theta", par$theta_a)
  lab_b  <- D_00_02a_name_fn("Project return", "theta", par$theta_b)
  long <- rbind(a, b)
  long$line <- factor(long$line, levels = c("safer", "riskier"))

  # Headroom for the safer density's two-line name above its peak
  y_hi <- max(long$density) * 1.5

  # The safer density is named at its peak, the riskier one out along its
  #   tail, where it is the outer of the two curves
  pk_a <- a[which.max(a$density), ]
  pk_b <- b[which.min(abs(b$R - 0.84 * xlim[2])), ]

  # Arrows low in the panel, away from the mean in both directions
  arrow_y <- y_hi * 0.055
  span    <- diff(xlim)

  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    list(
      T_02_03a_ghost_line_fn(C_01_04_density_fn(ref, ref$theta_a, grid),
                             aes(x = R, y = density),
                             colour = T_01_02_series_vec[["main"]],
                             linewidth = 1),
      T_02_03a_ghost_line_fn(C_01_04_density_fn(ref, ref$theta_b, grid),
                             aes(x = R, y = density),
                             colour = T_01_02_series_vec[["second"]],
                             linewidth = 1)
    )
  }

  ggplot(long, aes(x = R, y = density, colour = line)) +
    T_02_02_rest_fn(v = par$mu) +
    ghost_lyr +
    geom_line(linewidth = 1.1) +
    annotate("segment", x = par$mu - 0.04 * span, xend = par$mu - 0.22 * span,
             y = arrow_y, yend = arrow_y,
             arrow = arrow(length = unit(0.16, "cm"), type = "closed"),
             colour = T_01_01_palette_vec[["muted"]], linewidth = 0.4) +
    annotate("segment", x = par$mu + 0.04 * span, xend = par$mu + 0.22 * span,
             y = arrow_y, yend = arrow_y,
             arrow = arrow(length = unit(0.16, "cm"), type = "closed"),
             colour = T_01_01_palette_vec[["muted"]], linewidth = 0.4) +
    D_00_02_endlab_fn(pk_a$R, pk_a$density, lab_a,
                      T_01_02_series_vec[["main"]], hjust = 0,
                      nudge_x = 0.05 * span, nudge_y = y_hi * 0.05,
                      parse = TRUE) +
    D_00_02_endlab_fn(pk_b$R, pk_b$density, lab_b,
                      T_01_02_series_vec[["second"]], hjust = 1,
                      nudge_x = -0.01 * span, nudge_y = y_hi * 0.26,
                      parse = TRUE) +
    scale_colour_manual(values = stats::setNames(
      c(T_01_02_series_vec[["main"]], T_01_02_series_vec[["second"]]),
      levels(long$line))) +
    T_02_02_mark_x_fn(par$mu, expression(E * group("[", R, "]"))) +
    coord_cartesian(xlim = xlim, ylim = c(0, y_hi), expand = FALSE) +
    labs(
      x = expression(bold("Project Return (" * R * ")")),
      y = expression(bold("Density (" * f * ")"))
    ) +
    T_02_01_theme_fn(grid = "none") +
    theme(legend.position = "none")
}

###### D_01_02: The Convex Payoff ##############################################
# Note: P(R,r) against R: flat at -C while the project plus the collateral
#   cannot cover what is owed, then rising one for one. The flat stretch is
#   a region, so it is shaded and named inside itself.

D_01_02_payoff_fn <- function(par, xlim, ref = NULL) {
  r    <- C_01_01_loan_rate_fn(par)
  grid <- seq(xlim[1], xlim[2], length.out = 400)
  df   <- C_01_05_payoff_fn(par, r, grid)
  kink <- df$kink[1]
  owed <- df$owed[1]
  # Headroom so the name can sit above the end of the line
  y_lo <- -par$C - 0.12 * (max(df$payoff) + par$C)
  y_hi <- max(df$payoff) * 1.32

  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    T_02_03a_ghost_line_fn(
      C_01_05_payoff_fn(ref, C_01_01_loan_rate_fn(ref), grid),
      aes(x = R, y = payoff),
      colour = T_01_02_series_vec[["main"]], linewidth = 1.2)
  }

  ggplot(df, aes(x = R, y = payoff)) +
    annotate("rect", xmin = xlim[1], xmax = kink, ymin = y_lo, ymax = y_hi,
             fill = T_01_02_series_vec[["annot"]], alpha = 0.15) +
    T_02_02_zero_fn(h = TRUE, v = FALSE) +
    T_02_02_rest_fn(v = kink) +
    ghost_lyr +
    geom_line(colour = T_01_02_series_vec[["main"]], linewidth = 1.2) +
    annotate("text", x = kink / 2, y = y_hi * 0.62,
             label = T_02_01b_fold_fn("limited liability", 12),
             size = 3.4, colour = T_01_02_series_vec[["annot"]]) +
    # Above the end of the line; a label under a 45-degree line crosses it
    D_00_02_endlab_fn(xlim[2], max(df$payoff),
                      "Borrower payoff", T_01_02_series_vec[["main"]],
                      hjust = 1, nudge_x = -0.01 * xlim[2],
                      nudge_y = 0.075 * (y_hi - y_lo)) +
    # One mark on the top axis: the kink. The amount owed is where the
    #   payoff crosses the zero line, which the panel already draws
    T_02_02_mark_x_fn(kink, expression((1 + r) * B - C)) +
    # -C is named only when it sits clear of the zero label
    (if (par$C > 0.06 * (y_hi - y_lo)) {
      T_02_02_mark_y_fn(c(-par$C, 0), expression(-C, 0))
    } else {
      T_02_02_mark_y_fn(0, expression(0))
    }) +
    coord_cartesian(xlim = xlim, ylim = c(y_lo, y_hi), expand = FALSE) +
    labs(
      x = expression(bold("Project Return (" * R * ")")),
      y = expression(bold("Payoff (" * P * ")"))
    ) +
    T_02_01_theme_fn(grid = "none")
}

#### D_02: The Bank's Expected Return ##########################################
# Note: The second exhibit. The two panels are a sequence, not a
#   comparison, so they share a vertical scale and the second reads as the
#   first smoothed.

###### D_02_00: The Shared Return Scale ########################################
# Note: Both panels take these limits as an argument, with no default; set
#   from whichever panel needs the most room.

D_02_00_ylim_fn <- function(par) {
  grid <- D_00_03_rgrid_fn(61L)
  vals <- c(C_01_13_return_curve_fn(par, grid)$rho,
            C_01_14_two_type_fn(par, grid)$rho)
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0L) return(c(0, 1))
  pad <- 0.10 * diff(range(vals))
  c(min(vals) - pad, max(vals) + pad)
}

###### D_02_01: Two Borrower Types #############################################
# Note: The step (Stiglitz and Weiss 1981, fig. 3). While both types apply
#   the bank earns their average; where the safer type withdraws its return
#   drops to the risky type's alone. Two segments, as it is a discontinuity.

D_02_01_twotype_fn <- function(par, xlim, ylim, ref = NULL) {
  grid <- D_00_03_rgrid_fn()
  df   <- C_01_14_two_type_fn(par, grid)
  df   <- df[is.finite(df$rho), ]
  df$seg <- ifelse(df$both, "both", "risky")
  quit <- C_01_15_withdraw_fn(par, par$theta_a, B_03_03_r_max_num)
  last <- df[nrow(df), ]

  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    g <- C_01_14_two_type_fn(ref, grid)
    g <- g[is.finite(g$rho), ]
    g$seg <- ifelse(g$both, "both", "risky")
    T_02_03a_ghost_path_fn(g, aes(x = r, y = rho, group = seg),
                           colour = T_01_02_series_vec[["main"]],
                           linewidth = 1.2)
  }

  ggplot(df, aes(x = r, y = rho, group = seg)) +
    (if (is.finite(quit)) T_02_02_rest_fn(v = quit)) +
    ghost_lyr +
    geom_line(colour = T_01_02_series_vec[["main"]], linewidth = 1.2) +
    # Well below the line, which rises to the right
    D_00_02_endlab_fn(last$r,
                      D_00_02b_inside_fn(last$rho - 0.40 * diff(ylim), ylim,
                                         0.10),
                      "atop('Expected return', '(two types)')",
                      T_01_02_series_vec[["main"]], hjust = 1,
                      nudge_x = -0.015 * diff(xlim), parse = TRUE) +
    (if (is.finite(quit)) {
      T_02_02_mark_x_fn(quit, "safe type leaves",
                        labels = D_00_04_pct_fn, breaks = seq(0, 1, 0.1))
    } else {
      scale_x_continuous(labels = D_00_04_pct_fn, breaks = seq(0, 1, 0.1))
    }) +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(
      x = expression(bold("Loan Rate (" * r * ")")),
      y = expression(bold("Return (" * rho * ")"))
    ) +
    T_02_01_theme_fn(grid = "h")
}

###### D_02_02: A Continuum of Types ###########################################
# Note: The same object over a whole pool (Stiglitz and Weiss 1981, fig.
#   1). Single-peaked, with r-bar marked on the horizontal axis.

D_02_02_continuum_fn <- function(par, xlim, ylim, ref = NULL) {
  grid  <- D_00_03_rgrid_fn()
  df    <- C_01_13_return_curve_fn(par, grid)
  df    <- df[is.finite(df$rho), ]
  r_bar <- C_01_16_rbar_fn(par, B_03_03_r_max_num)
  peak  <- C_01_12_pool_fn(par, r_bar)$rho
  last  <- df[nrow(df), ]
  # With almost no collateral the curve is still climbing at the right-hand
  #   edge, so no turning point is marked
  on_view <- r_bar < B_03_03_r_max_num - 1e-4

  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    g <- C_01_13_return_curve_fn(ref, grid)
    list(
      T_02_03a_ghost_line_fn(g[is.finite(g$rho), ], aes(x = r, y = rho),
                             colour = T_01_02_series_vec[["main"]],
                             linewidth = 1.2),
      T_02_03a_ghost_point_fn(C_01_16_rbar_fn(ref, B_03_03_r_max_num),
                              C_01_12_pool_fn(
                                ref, C_01_16_rbar_fn(ref, B_03_03_r_max_num)
                              )$rho,
                              colour = T_01_02_series_vec[["main"]])
    )
  }

  ggplot(df, aes(x = r, y = rho)) +
    (if (on_view) T_02_02_rest_fn(v = r_bar, h = peak)) +
    ghost_lyr +
    geom_line(colour = T_01_02_series_vec[["main"]], linewidth = 1.2) +
    (if (on_view) T_02_03_point_fn(r_bar, peak)) +
    D_00_02_endlab_fn(last$r,
                      # Below the end of the curve where there is room for
                      #   two lines, above it where the end sits low
                      if (last$rho - 0.13 * diff(ylim) >
                          ylim[1] + 0.16 * diff(ylim)) {
                        last$rho - 0.13 * diff(ylim)
                      } else {
                        D_00_02b_inside_fn(last$rho + 0.26 * diff(ylim),
                                           ylim, 0.10)
                      },
                      "atop('Expected return', '(a continuum)')",
                      T_01_02_series_vec[["main"]], hjust = 1,
                      nudge_x = -0.015 * diff(xlim), parse = TRUE) +
    (if (on_view) {
      T_02_02_mark_x_fn(r_bar, expression(bar(r)),
                        labels = D_00_04_pct_fn, breaks = seq(0, 1, 0.1))
    } else {
      scale_x_continuous(labels = D_00_04_pct_fn, breaks = seq(0, 1, 0.1))
    }) +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(
      x = expression(bold("Loan Rate (" * r * ")")),
      y = expression(bold("Return (" * rho * ")"))
    ) +
    T_02_01_theme_fn(grid = "h")
}

#### D_03: The Rationing Equilibrium ###########################################
# Note: The third exhibit.

###### D_03_01: Supply, Two Demands and the Gap ################################
# Note: Loan quantity across, loan rate up. Supply follows the bank's
#   expected return, so it bends backwards above r-bar. Both demand levels
#   are always drawn; the gap at r-bar is a brace (CONVENTIONS.md 6).

D_03_01_market_fn <- function(par, ref = NULL) {
  grid  <- D_00_03_rgrid_fn()
  r_bar <- C_01_16_rbar_fn(par, B_03_03_r_max_num)
  sup   <- C_01_17_supply_fn(par, grid, r_bar)
  sup   <- sup[is.finite(sup$supply), ]

  # The pair is the live level and the other of the two the presets carry
  mid   <- mean(B_03_14_demand_vec)
  other <- if (par$demand >= mid) B_03_14_demand_vec[["low"]] else
    B_03_14_demand_vec[["high"]]
  d_now <- C_01_18_demand_fn(par, grid, par$demand)
  d_alt <- C_01_18_demand_fn(par, grid, other)
  m_now <- C_01_19_market_fn(par, B_03_03_r_max_num, par$demand,
                             r_bar = r_bar)
  m_alt <- C_01_19_market_fn(par, B_03_03_r_max_num, other, r_bar = r_bar)

  x_hi <- max(d_now$demand, d_alt$demand, sup$supply) * 1.10
  y_hi <- B_03_03_r_max_num

  # The excess demand at r-bar, braced and named, for whichever of the two
  #   curves is rationed
  m_gap <- if (isTRUE(m_now$excess > 0)) m_now else
    if (isTRUE(m_alt$excess > 0)) m_alt else NULL
  m_cut <- if (is.finite(m_now$clears)) m_now else
    if (is.finite(m_alt$clears)) m_alt else NULL
  cut_level <- if (is.finite(m_now$clears)) par$demand else other
  # The crossing marker takes the colour of the curve that crosses, which
  #   is not always the live one
  cut_col <- if (is.finite(m_now$clears)) T_01_02_series_vec[["second"]] else
    T_01_02_series_vec[["compare"]]

  brace_lyr <- if (is.null(m_gap)) NULL else {
    # Below the r-bar line, tip down, where the two curves open away from
    #   each other
    br <- D_00_01_brace_fn(m_gap$supply, m_gap$demand, r_bar - 0.015 * y_hi,
                           -0.040 * y_hi)
    list(
      geom_path(data = br, aes(x = x, y = y), inherit.aes = FALSE,
                colour = T_01_02_series_vec[["annot"]], linewidth = 0.5),
      annotate("text", x = (m_gap$supply + m_gap$demand) / 2,
               y = r_bar - 0.090 * y_hi, label = "rationed", size = 3.6,
               colour = T_01_02_series_vec[["annot"]])
    )
  }

  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    g_bar <- C_01_16_rbar_fn(ref, B_03_03_r_max_num)
    g_sup <- C_01_17_supply_fn(ref, grid, g_bar)
    T_02_03a_ghost_path_fn(g_sup[is.finite(g_sup$supply), ],
                           aes(x = supply, y = r),
                           colour = T_01_02_series_vec[["main"]],
                           linewidth = 1.2)
  }

  # The supply curve's name goes on its rising branch, clear of the demand
  #   curves arriving at the top of the axis
  i_lab <- which.min(abs(sup$r - 0.2 * y_hi))
  y_lab <- 0.10 * y_hi
  # The lower demand curve is named low down and the higher one further up
  y_at  <- function(level) {
    if (level <= min(par$demand, other)) y_lab else 0.30 * y_hi
  }

  ggplot() +
    T_02_02_rest_fn(h = r_bar) +
    ghost_lyr +
    geom_path(data = sup, aes(x = supply, y = r),
              colour = T_01_02_series_vec[["main"]], linewidth = 1.2) +
    geom_path(data = d_alt, aes(x = demand, y = r),
              colour = T_01_02_series_vec[["compare"]], linewidth = 1) +
    geom_path(data = d_now, aes(x = demand, y = r),
              colour = T_01_02_series_vec[["second"]], linewidth = 1.2) +
    brace_lyr +
    (if (!is.null(m_cut)) {
      T_02_03_point_fn(cut_level * exp(-par$eta * m_cut$clears),
                       m_cut$clears, colour = cut_col)
    }) +
    D_00_02_endlab_fn(sup$supply[i_lab], sup$r[i_lab],
                      "Loan supply", T_01_02_series_vec[["main"]],
                      hjust = 0, nudge_x = 0.02 * x_hi) +
    # Each demand curve's name sits to the right of the curve at the
    #   label's own height (y_at)
    D_00_02_endlab_fn(par$demand * exp(-par$eta * y_at(par$demand)),
                      y_at(par$demand),
                      D_00_02a_name_fn("Loan demand", "D[0]", par$demand, 0),
                      T_01_02_series_vec[["second"]], hjust = 0,
                      nudge_x = 0.04 * x_hi, parse = TRUE) +
    D_00_02_endlab_fn(other * exp(-par$eta * y_at(other)), y_at(other),
                      D_00_02a_name_fn("Loan demand", "D[0]", other, 0),
                      T_01_02_series_vec[["compare"]], hjust = 0,
                      nudge_x = 0.04 * x_hi, parse = TRUE) +
    T_02_02_mark_y_fn(r_bar, expression(bar(r)),
                      labels = D_00_04_pct_fn, breaks = seq(0, 1, 0.1)) +
    coord_cartesian(xlim = c(0, x_hi), ylim = c(0, y_hi), expand = FALSE) +
    labs(
      # Both superscripts: bare L is the loss on the book at stage 4
      x = expression(bold("Loans (" * L^s * ", " * L^d * ")")),
      y = expression(bold("Loan Rate (" * r * ")"))
    ) +
    T_02_01_theme_fn(grid = "none")
}

#### D_04: Capital and the Tail ################################################
# Note: The fourth exhibit, beside the Value at Risk frame of the lecture.

###### D_04_01: The Loss Distribution ##########################################
# Note: A right-skewed loss density with the expected loss and the cut point
#   marked and the tail beyond the cut point shaded (Whelan, MA Advanced
#   Macroeconomics, part 13, "Illustrating Value at Risk").

D_04_01_var_fn <- function(par, ref = NULL) {
  v    <- C_01_21_var_fn(par)
  # The axis stops not far past the cut point, so the shaded region is
  #   readable
  x_hi <- v$var * 1.65
  grid <- seq(0.01, x_hi, length.out = 600)
  df   <- C_01_20_loss_fn(par, grid)
  tail_df <- df[df$loss >= v$var, ]
  y_hi <- max(df$density) * 1.18
  # The shoulder, not the mode, which sits against the expected-loss rule
  pk   <- df[which.min(abs(df$loss - 0.22 * x_hi)), ]

  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    T_02_03a_ghost_line_fn(C_01_20_loss_fn(ref, grid),
                           aes(x = loss, y = density),
                           colour = T_01_02_series_vec[["main"]],
                           linewidth = 1.2)
  }

  ggplot(df, aes(x = loss, y = density)) +
    # Two shades: the pale band is the region past the cut point, named
    #   inside itself; the darker fill under the curve is the one per cent
    #   of probability it carries
    annotate("rect", xmin = v$var, xmax = x_hi, ymin = 0, ymax = y_hi,
             fill = T_01_02_series_vec[["annot"]], alpha = 0.15) +
    geom_area(data = tail_df, aes(x = loss, y = density), inherit.aes = FALSE,
              fill = T_01_01_palette_vec[["blue"]], alpha = 0.75) +
    T_02_02_rest_fn(v = c(v$el, v$var)) +
    ghost_lyr +
    geom_line(colour = T_01_02_series_vec[["main"]], linewidth = 1.2) +
    annotate("text", x = (v$var + x_hi) / 2, y = y_hi * 0.55,
             hjust = 0.5, size = 3.5, lineheight = 1.05,
             colour = T_01_02_series_vec[["annot"]],
             label = T_02_01b_fold_fn(
               paste0("beyond the cut point: ",
                      T_02_06_pct_fn(v$tail_prob, 0),
                      " of outcomes, size unmeasured"), 18)) +
    D_00_02_endlab_fn(pk$loss, pk$density, "Credit losses",
                      T_01_02_series_vec[["main"]], hjust = 0,
                      nudge_x = 0.03 * x_hi, nudge_y = y_hi * 0.09) +
    T_02_02_mark_x_fn(c(v$el, v$var),
                      expression(E * group("[", L, "]"), VaR)) +
    coord_cartesian(xlim = c(0, x_hi), ylim = c(0, y_hi), expand = FALSE) +
    labs(
      x = expression(bold("Loss on the Book (" * L * ")")),
      y = expression(bold("Density (" * f(L) * ")"))
    ) +
    T_02_01_theme_fn(grid = "none")
}

################################################################################
## E: User Interface ###########################################################
################################################################################
# Note: The sidebar of controls and the page itself.

#### E_01: Sidebar #############################################################
# Note: Stage selector, then the controls. The scenario menu and its story
#   live in the main window: see E_02_01.

###### E_01_01: Control Shorthand ##############################################
# Note: Saves passing the same three lists at every call.

E_01_01_ctl_fn <- function(id) {
  T_03_01_control_fn(id, B_03_05_controls_lst, B_03_06_help_lst,
                     B_03_01_defaults_lst)
}

###### E_01_02: Sidebar ########################################################
# Note: conditionalPanel reveals controls as the stages add layers.

E_01_02_sidebar_lst <- sidebar(
  width = 380,
  radioButtons("stage", "Stage of the Model",
               choices = B_03_02_stages_vec, selected = "1"),
  T_03_05_note_fn(paste(
    "Each stage adds one piece to the model and leaves the rest",
    "alone. Start at the top; the equations panel marks what is new.")),
  accordion(
    open = c("The Loan"),
    accordion_panel(
      "The Loan",
      E_01_01_ctl_fn("r_f"),
      E_01_01_ctl_fn("p_def"),
      E_01_01_ctl_fn("c_rec"),
      E_01_01_ctl_fn("C")
    ),
    accordion_panel(
      "The Project",
      E_01_01_ctl_fn("mu"),
      E_01_01_ctl_fn("theta_a"),
      E_01_01_ctl_fn("theta_b")
    ),
    accordion_panel(
      "The Applicant Pool",
      conditionalPanel(
        "parseFloat(input.stage) >= 2",
        E_01_01_ctl_fn("theta_min"),
        E_01_01_ctl_fn("theta_s")
      ),
      conditionalPanel("parseFloat(input.stage) < 2",
                       tags$p(class = "stat-caption",
                              "The pool appears at stage 2."))
    ),
    accordion_panel(
      "The Loan Market",
      conditionalPanel(
        "parseFloat(input.stage) >= 3",
        E_01_01_ctl_fn("demand")
      ),
      conditionalPanel("parseFloat(input.stage) < 3",
                       tags$p(class = "stat-caption",
                              "Loan demand appears at stage 3."))
    ),
    accordion_panel(
      "Capital and the Tail",
      conditionalPanel(
        "parseFloat(input.stage) >= 4",
        E_01_01_ctl_fn("el"),
        E_01_01_ctl_fn("tail"),
        E_01_01_ctl_fn("conf")
      ),
      conditionalPanel("parseFloat(input.stage) < 4",
                       tags$p(class = "stat-caption",
                              "The capital rules appear at stage 4."))
    )
  ),
  actionButton("reset", "Reset Everything",
               class = "btn-outline-secondary btn-sm w-100"),
  T_07_10b_sidebarqr_fn(B_04_01_qr_src_chr)
)

#### E_02: Main Panel ##########################################################
# Note: Equations, worked examples, prompt, readouts, then the figures for
#   this stage.

###### E_02_00: One Figure Card ################################################
# Note: The toolkit's figure card (T_07_07f): header, plot held at 2:1,
#   caption, and Save PNG under it. The title comes from B_03_16.

E_02_00_figcard_fn <- function(id) {
  T_07_07f_figcard_fn(id, B_03_16_figures_lst[[id]]$title)
}

###### E_02_01: Worked-Example Presets #########################################
# Note: The scenarios of the stage on screen, in the main window (see
#   CONVENTIONS.md 1). The machinery is T_05_04 to T_05_07; this is the
#   wiring.

E_02_01_presets_lst <- T_05_04_presets_fn(
  B_03_04_scenarios_lst, B_03_02_stages_vec, stage_word = ""
)

###### E_02_02: Page ###########################################################
# Note: The full UI object passed to shinyApp().

E_02_02_app_ui_lst <- tagList(
  T_07_08b_nav_fn(),
  page_sidebar(
  title        = T_07_09_title_fn("Credit Rationing",
                                  B_04_01_qr_src_chr),
  window_title = paste("Credit Rationing ·", T_07_01_author_chr),
  fillable     = FALSE,
  theme        = T_07_05_theme_fn(),
  sidebar      = E_01_02_sidebar_lst,
  T_07_08_head_fn(),
  tags$head(
    tags$style(HTML(T_05_07_preset_css_chr)),
    tags$script(HTML(T_05_05_preset_js_chr))
  ),
  T_07_07j_eqtabs_fn(
    title = textOutput("eq_title", inline = TRUE),
    nav_panel("Equations", uiOutput("eq_model")),
    nav_panel("Notation", uiOutput("eq_notation")),
    nav_panel("In Words", uiOutput("eq_explain"))
  ),
  E_02_01_presets_lst,
  uiOutput("prompt"),
  uiOutput("problems"),
  uiOutput("tiles"),
  # The two paired exhibits, each one row of two
  T_07_07g_pair_fn(
    E_02_00_figcard_fn("spread"),
    E_02_00_figcard_fn("payoff")
  ),
  conditionalPanel(
    "parseFloat(input.stage) >= 2",
    T_07_07g_pair_fn(
      E_02_00_figcard_fn("twotype"),
      E_02_00_figcard_fn("continuum")
    ),
    uiOutput("selection_note")
  ),
  # The market and the loss distribution share a row: at stage 3 the market
  #   sits alone at half width, and the loss distribution joins it at stage 4
  conditionalPanel(
    "parseFloat(input.stage) >= 3",
    T_07_07g_pair_fn(
      E_02_00_figcard_fn("market"),
      conditionalPanel("parseFloat(input.stage) >= 4",
                       E_02_00_figcard_fn("var"))
    ),
    uiOutput("market_note"),
    conditionalPanel("parseFloat(input.stage) >= 4",
                     uiOutput("capital_note"))
  ),
  T_07_11_footer_fn(paste(
    "Lecture 3.1. Notation follows the deck's own Notation frame:",
    "r is the loan rate, R the project return, and r&#772; the rate that",
    "maximises the bank's expected return.",
    "Version", paste0(B_03_17_version_chr, ".")), repo = B_03_18_repo_chr),
))

################################################################################
## F: Server ###################################################################
################################################################################
# Note: Builds parameters for the chosen stage, solves the model, draws.

#### F_01: Server Function #####################################################
# Note: Everything reactive lives inside this function.

###### F_01_01: Server #########################################################
# Note: Local objects use plain snake_case.

F_01_01_app_server_fn <- function(input, output, session) {

  # --- Figure captions --------------------------------------------------------
  # T_02_01c_draw_fn lifts each plot's caption out of the graphics device;
  #   this prints it under the figure

  T_07_07d_cap_fn(output)

  # --- Stage as a number ------------------------------------------------------
  stage_num <- reactive(as.numeric(input$stage))

  # --- Controls ---------------------------------------------------------------
  val <- function(id) T_03_04_val_fn(input, id)
  T_03_02_sync_fn(input, session, B_03_05_controls_lst)

  set_control <- function(id, value) {
    T_03_03_set_fn(session, B_03_05_controls_lst, id, value)
  }

  apply_values <- function(values) {
    for (id in names(values)) {
      if (!is.null(B_03_05_controls_lst[[id]])) set_control(id, values[[id]])
    }
    invisible(NULL)
  }

  # --- Worked-example presets -------------------------------------------------
  # One observer per preset. The buttons exist for every stage's scenarios
  # from the start and conditionalPanel decides which are on screen, so the
  # ids are stable. A preset never moves the stage.
  scenario <- reactiveVal(names(B_03_04_scenarios_lst)[1])

  scn_now <- reactive({
    k <- scenario()
    if (is.null(k) || !k %in% names(B_03_04_scenarios_lst)) NULL
    else B_03_04_scenarios_lst[[k]]
  })

  set_scenario_fn <- function(key) {
    scenario(if (is.null(key)) "custom" else key)
    session$sendCustomMessage("dgPreset", if (is.null(key)) "" else key)
    invisible(NULL)
  }

  load_preset_fn <- function(key) {
    if (is.null(key) || !key %in% names(B_03_04_scenarios_lst)) {
      return(invisible(NULL))
    }
    scn <- B_03_04_scenarios_lst[[key]]
    set_scenario_fn(key)
    apply_values(utils::modifyList(B_03_01_defaults_lst, scn$values))
    invisible(NULL)
  }

  lapply(names(B_03_04_scenarios_lst), function(key) {
    observeEvent(input[[paste0("preset_", key)]],
                 load_preset_fn(key), ignoreInit = TRUE)
  })

  # The first scenario of a stage, or NULL for a stage with none
  first_preset_fn <- function(stage) {
    hits <- names(B_03_04_scenarios_lst)[vapply(
      B_03_04_scenarios_lst, function(x) identical(x$stage, stage), TRUE)]
    if (length(hits) == 0L) NULL else hits[[1L]]
  }

  observeEvent(input$stage, {
    first <- first_preset_fn(input$stage)
    if (!is.null(first)) {
      load_preset_fn(first)
      return()
    }
    set_scenario_fn(NULL)
  })

  output$preset_title <- renderUI({
    T_05_06_preset_title_fn(scn_now(), input$stage, B_03_02_stages_vec,
                            stage_word = "")
  })

  # --- Reset ------------------------------------------------------------------
  observeEvent(input$reset, {
    apply_values(B_03_01_defaults_lst)
    set_scenario_fn(NULL)
  })

  # --- Parameters in force at this stage --------------------------------------
  # A pure function of the control values and the stage, so it runs once
  # over the sliders and once over the loaded worked example (the ghost).
  # Controls a stage has not reached yet are held at their defaults.
  assemble_fn <- function(v, s) {
    d <- B_03_01_defaults_lst
    list(
      r_f       = v$r_f,
      p_def     = v$p_def,
      c_rec     = v$c_rec,
      B         = d$B,
      C         = v$C,
      mu        = v$mu,
      theta_a   = v$theta_a,
      theta_b   = v$theta_b,
      theta_min = if (s >= 2) v$theta_min else d$theta_min,
      theta_med = d$theta_med,
      theta_s   = if (s >= 2) v$theta_s else d$theta_s,
      sigma_c   = d$sigma_c,
      L_max     = d$L_max,
      demand    = if (s >= 3) v$demand else d$demand,
      eta       = d$eta,
      el        = if (s >= 4) v$el else d$el,
      tail      = if (s >= 4) v$tail else d$tail,
      conf      = if (s >= 4) v$conf else d$conf
    )
  }

  par_raw <- reactive({
    req(!is.null(input$mu))
    ids  <- names(B_03_05_controls_lst)
    vals <- stats::setNames(lapply(ids, val), ids)
    assemble_fn(vals, stage_num())
  })

  par_now  <- debounce(par_raw, B_03_13_debounce_ms_int)
  diag_now <- reactive(C_01_22_diagnostics_fn(par_now(), B_03_03_r_max_num))
  ok_now   <- reactive(length(diag_now()$problems) == 0)

  # --- The ghost: every figure at the reference settings ----------------------
  # Every figure draws itself at the loaded worked example's settings as
  # well as at the sliders; skipped while the two agree. See CONVENTIONS.md 8.
  ref_vals <- reactive({
    key <- scenario()
    if (is.null(key) || !key %in% names(B_03_04_scenarios_lst)) {
      return(B_03_01_defaults_lst)
    }
    utils::modifyList(B_03_01_defaults_lst,
                      B_03_04_scenarios_lst[[key]]$values)
  })

  ghost_par <- reactive({
    ref <- assemble_fn(ref_vals(), stage_num())
    if (T_02_03b_ghost_off_fn(par_now(), ref)) return(NULL)
    if (length(C_01_23_problems_fn(ref)) > 0) return(NULL)
    ref
  })

  # --- Shared panel limits ----------------------------------------------------
  # Computed once from the sliders and the ghost together and passed into
  # both builders of a pair.
  rlim_now <- reactive({
    g <- ghost_par()
    l <- D_01_00_rlim_fn(par_now())
    if (is.null(g)) l else c(0, max(l[2], D_01_00_rlim_fn(g)[2]))
  })

  ylim_now <- reactive({
    g <- ghost_par()
    l <- D_02_00_ylim_fn(par_now())
    if (is.null(g)) return(l)
    h <- D_02_00_ylim_fn(g)
    c(min(l[1], h[1]), max(l[2], h[2]))
  })

  # --- Scenario story ---------------------------------------------------------
  output$scenario_story <- renderUI({
    T_05_02_story_fn(scn_now(), B_03_05_controls_lst, B_03_06_help_lst)
  })

  # --- The model so far -------------------------------------------------------
  output$eq_title <- renderText({
    T_05_04_stage_name_fn(B_03_02_stages_vec, input$stage)
  })

  eq_items <- reactive(T_06_03_items_fn(B_03_08_equations_lst, stage_num()))

  output$eq_model <- renderUI({
    T_06_04_model_fn(eq_items(), B_03_09_groups_vec,
                     "These appear as the later stages add to the model.")
  })

  output$eq_notation <- renderUI({
    T_06_05_notation_fn(B_03_10_notation_lst, stage_num(),
                        B_03_11_nota_cols_lst, first_stage = 1)
  })

  output$eq_explain <- renderUI({
    T_06_06_explain_fn(eq_items(), B_03_09_groups_vec)
  })

  # --- Prompt and problems ----------------------------------------------------
  output$prompt <- renderUI({
    # The prompts carry HTML entities, so they are marked as HTML here
    scn <- scn_now()
    if (!is.null(scn)) scn$prompt <- HTML(scn$prompt)
    T_07_12_prompt_fn(scn, input$stage, lapply(B_03_07_prompts_lst, HTML))
  })

  output$problems <- renderUI(T_07_13_problems_fn(diag_now()$problems))

  # --- Readouts ---------------------------------------------------------------
  output$tiles <- renderUI({
    d <- diag_now()
    s <- stage_num()
    T_04_03_row_fn(
      T_04_01_tile_fn(
        "The loan rate, r", T_02_06_pct_fn(d$r, 2),
        paste0("Risk-free ", T_02_06_pct_fn(par_now()$r_f / 100, 2),
               " plus a spread of ", T_02_06_pct_fn(d$spread / 100, 2))
      ),
      T_04_01_tile_fn(
        "Owed at repayment", T_02_05_num_fn(d$owed, 1),
        paste0("The payoff kinks at ", T_02_05_num_fn(d$kink, 1),
               ", where the collateral runs out")
      ),
      T_04_01_tile_fn(
        "Expected payoff, riskier type", T_02_05_num_fn(d$ep_b, 1),
        paste0("Safer type: ", T_02_05_num_fn(d$ep_a, 1),
               ". Same mean return, more spread"),
        class = if (isTRUE(d$ep_b > d$ep_a)) "good" else ""
      ),
      if (s >= 2) {
        local({
          on_view <- isTRUE(d$r_bar < B_03_03_r_max_num - 1e-4)
          T_04_01_tile_fn(
            "The turning point, r&#772;",
            if (on_view) T_02_06_pct_fn(d$r_bar, 1) else "\u2014",
            if (on_view) {
              "Above it, the bank's expected return falls"
            } else {
              "Still rising at the top of the rate axis"
            }
          )
        })
      },
      if (s >= 2) {
        T_04_01_tile_fn(
          "Cut-off type at r", T_02_05_num_fn(d$cutoff, 2),
          paste0(T_02_06_pct_fn(d$share, 0), " of the pool still applies")
        )
      },
      if (s >= 2) {
        T_04_01_tile_fn(
          "The safe type leaves at",
          if (is.finite(d$quit_a)) T_02_06_pct_fn(d$quit_a, 1) else "—",
          "Where the step on the left-hand panel falls"
        )
      },
      if (s >= 3) {
        T_04_01_tile_fn(
          "Excess demand at r&#772;",
          T_02_05_num_fn(d$excess, 1),
          if (d$rationed) {
            "Rationed: some applicants are refused"
          } else {
            paste0("The market clears at ",
                   if (is.finite(d$clears)) T_02_06_pct_fn(d$clears, 1)
                   else "no rate on the panel")
          },
          class = if (d$rationed) "bad" else "good"
        )
      },
      if (s >= 4) {
        T_04_01_tile_fn(
          "Value at Risk", T_02_05_num_fn(d$var, 1),
          paste0("Exceeded ", T_02_06_pct_fn(1 - par_now()$conf, 1),
                 " of the time")
        )
      },
      if (s >= 4) {
        T_04_01_tile_fn(
          "Capital required, K", T_02_05_num_fn(d$capital, 1),
          paste0("Three times Value at Risk. Risk-weighted assets: ",
                 T_02_05_num_fn(d$rwa, 0))
        )
      },
      if (s >= 4) {
        T_04_01_tile_fn(
          "Average loss beyond the line", T_02_05_num_fn(d$beyond, 1),
          "No rule uses this number", class = "bad"
        )
      }
    )
  })

  # --- Figures ----------------------------------------------------------------
  # One call site per figure, used by renderPlot for the screen and by the
  # download handlers for the file.

  build_fig_fn <- function(key) {
    switch(
      key,
      spread    = D_01_01_spread_fn(par_now(), rlim_now(),
                                    ref = ghost_par()),
      payoff    = D_01_02_payoff_fn(par_now(), rlim_now(),
                                    ref = ghost_par()),
      twotype   = D_02_01_twotype_fn(par_now(), c(0, B_03_03_r_max_num),
                                     ylim_now(), ref = ghost_par()),
      continuum = D_02_02_continuum_fn(par_now(), c(0, B_03_03_r_max_num),
                                       ylim_now(), ref = ghost_par()),
      market    = D_03_01_market_fn(par_now(), ref = ghost_par()),
      var       = D_04_01_var_fn(par_now(), ref = ghost_par())
    )
  }

  for (fig_key in names(B_03_16_figures_lst)) {
    local({
      key <- fig_key
      fig <- B_03_16_figures_lst[[key]]

      # res = 96 so the text reads at half width. A title that only repeats
      #   the card header is dropped; one carrying a number or a warning stays
      output[[key]] <- renderPlot({
        req(ok_now(), stage_num() >= fig$from)
        p <- build_fig_fn(key)
        if (identical(p$labels$title, fig$title)) p$labels$title <- NULL
        T_02_01c_draw_fn(p, title_width = 34)
      }, res = 96)

      # T_07_07h registers <id>__png through
      #   T_02_03c_export_fn, named {app}-{stage}-{figure}
      T_07_07h_exports_fn(
        output, key,
        plot_fn = function() {
          req(ok_now(), stage_num() >= fig$from)
          build_fig_fn(key)
        },
        stem = function() {
          paste0(B_03_15_slug_chr, "-", input$stage, "-", fig$file)
        },
        pair = identical(fig$shape, "pair")
      )
    })
  }

  output$selection_note <- renderUI({
    req(stage_num() >= 2)
    tags$div(
      class = "narrative",
      tags$div(class = "nar-head", "Reading the Two Panels as One Sequence"),
      tags$p(HTML(paste(
        "<strong>The step is the mechanism.</strong> With two types the",
        "bank's expected return drops discontinuously at the rate where the",
        "low-risk type withdraws. Nothing about the loans changed at that",
        "rate: the applicants did."
      ))),
      tags$p(HTML(paste(
        "<strong>The peak is the step smoothed.</strong> With a continuum",
        "the safest types leave a few at a time, so the drop becomes a turn.",
        "The rate at which it turns is r&#772;, and no bank posts a rate",
        "above it, because doing so would lower what it earns. The interior",
        "peak is an assumption of Stiglitz and Weiss (1981), not a theorem:",
        "their rationing result holds when the return has an interior mode,",
        "and this pool's heavy tail of risky types is what gives it one."
      ))),
      tags$p(HTML(paste(
        "<strong>Deriving the cut-off is the examined step.</strong> A",
        "Section C answer that states adverse selection without getting",
        "&theta;&#770;(r) out of E[P] &gt; 0, and without dividing the pool",
        "average by the share of types above it, has skipped the model."
      ))),
      tags$p(HTML(paste(
        "<strong>The two payoffs are mirror images.</strong> The borrower",
        "holds a convex claim on the project and the bank a concave one, and",
        "state by state they add to the project return. That is why a",
        "mean-preserving spread is good for one and bad for the other, and",
        "it is worth putting the two side by side on the board."
      )))
    )
  })

  output$market_note <- renderUI({
    req(stage_num() >= 3)
    tags$div(
      class = "narrative",
      tags$div(class = "nar-head",
               "What the Backward Bend Does and Does Not Say"),
      tags$p(HTML(paste(
        "<strong>Rationing is the high-demand case, not a special",
        "assumption.</strong> The bank is the same bank in both cases. Only",
        "the level of demand differs: one curve meets supply below r&#772;",
        "and the other does not."
      ))),
      tags$p(HTML(paste(
        "<strong>The picture is a bit misleading, and Whelan says so.</strong>",
        "A backward-bending supply curve suggests a market being cleared by",
        "a price that happens to run out of room. There is no auctioneer",
        "here: banks post the rate, and they stop at r&#772; because raising",
        "it further lowers what they expect to earn. The curve is drawn",
        "because it is what the literature draws."
      ))),
      tags$p(HTML(paste(
        "<strong>Who is refused.</strong> Not the applicants the bank has",
        "identified as bad. It cannot identify them at all: the refused",
        "borrowers are observationally identical to the ones it served."
      ))),
      tags$p(HTML(paste(
        "<strong>Why a recession can tighten it.</strong> Whelan argues",
        "that demand for credit may be high exactly when collateral is worth",
        "less, and both can push toward the rationed case. In this model",
        "lower collateral lowers the bank's expected return up to r&#772;,",
        "and so its peak and the most banks will lend; lower the collateral",
        "pledged and watch the supply curve fall away from the demand curves.",
        "Stiglitz and Weiss (1981) show the opposite can also happen: raising",
        "collateral can lower the bank's return, because it draws in",
        "wealthier, less risk-averse borrowers with riskier projects."
      )))
    )
  })

  output$capital_note <- renderUI({
    req(stage_num() >= 4)
    tags$div(
      class = "narrative",
      tags$div(class = "nar-head",
               "Provisions, Capital and What Is Not Measured"),
      tags$p(HTML(paste(
        "<strong>Two different numbers do two different jobs.</strong> The",
        "mean of the loss distribution is provisioned for: the bank writes",
        "down part of the book each year in anticipation. Capital is held",
        "against the tail beyond the cut point."
      ))),
      tags$p(HTML(paste(
        "<strong>Value at Risk is the cut point, not the size of the",
        "tail.</strong> The shaded region is one per cent of outcomes, and",
        "the method says nothing at all about how bad they are. The average",
        "loss inside that region is in the readouts above, and no rule uses",
        "it."
      ))),
      tags$p(HTML(paste(
        "<strong>The bank's own model sets the denominator of its own",
        "ratio.</strong> K = 3 &times; VaR and K &ge; 0.08 &times; RWA give",
        "risk-weighted assets of 37.5 times the modelled tail loss. A lower",
        "Value at Risk is a lower requirement, and the estimate depends on",
        "the sample the bank chose."
      ))),
      tags$p(HTML(paste(
        "<strong>How it was gamed.</strong> Returns from 2005 to 2007 made",
        "risk look low in 2008. Selling insurance against rare events shows",
        "small steady gains and puts the payout outside the window",
        "altogether, so it never enters the reported number. Lower the",
        "fatness of the tail and watch required capital fall while the loan",
        "book does not change at all."
      )))
    )
  })
}

################################################################################
## G: Run ######################################################################
################################################################################
# Note: Launch.

#### G_01: Launch ##############################################################
# Note: Returns the app object.

###### G_01_01: The App ########################################################
# Note: The object Shiny runs.

G_01_01_app_lst <- shinyApp(E_02_02_app_ui_lst, F_01_01_app_server_fn)

G_01_01_app_lst
