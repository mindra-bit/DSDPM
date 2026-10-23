# FINAL PAPER CODE PACKAGE: main Monte Carlo is run with R=300 via RUN_MAIN_R300.R.
# DSDPM v3.4: generated from MASTER_SOURCE.R module order.
# See README.md and VALIDATION_STATUS.md for scope and evidence.

# >>> BEGIN R/00_setup.R >>>
# ======================================================================
# DSDPM Full Estimator Suite v3.4 reviewer revision
# 00_setup.R
#
# Structural target: manuscript "Bayesian Estimation of the Dynamic
# Spatial Durbin Panel Model" (Re-Submitted Main version supplied by user).
#
# Main estimators:
#   FULL_QMLE
#   FULL_GMM
#   FULL_INLA  (single joint latent-Gaussian model via rgeneric)
#   FULL_MCMC  (collapsed full-posterior reference using the same model/prior)
#
# The code is written so the structural model is identical across methods.
# ======================================================================

`%||%` <- function(x, y) if (is.null(x)) y else x

dsdpm_required_packages <- function(include_inla = TRUE,
                                    include_plot = TRUE,
                                    include_parallel = TRUE,
                                    include_mcmc_diag = TRUE) {
  pkgs <- c("Matrix", "spdep", "MASS")
  if (include_inla) pkgs <- c(pkgs, "INLA")
  if (include_plot) pkgs <- c(pkgs, "ggplot2")
  if (include_parallel) pkgs <- c(pkgs, "parallel")
  if (include_mcmc_diag) pkgs <- c(pkgs, "coda")
  unique(pkgs)
}

dsdpm_require <- function(include_inla = FALSE,
                          include_plot = FALSE,
                          include_parallel = FALSE,
                          include_mcmc_diag = FALSE) {
  pkgs <- dsdpm_required_packages(
    include_inla = include_inla,
    include_plot = include_plot,
    include_parallel = include_parallel,
    include_mcmc_diag = include_mcmc_diag
  )
  missing <- pkgs[
    !vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    stop(
      "Missing package(s): ", paste(missing, collapse = ", "),
      "\nRun INSTALL_PACKAGES.R first."
    )
  }
  invisible(TRUE)
}

dsdpm_assert_scalar <- function(x, name) {
  if (is.null(x) || length(x) != 1L || !is.finite(as.numeric(x))) {
    stop(name, " must be one finite numeric scalar.")
  }
  as.numeric(x)
}

dsdpm_safe_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

dsdpm_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
}

dsdpm_session_info <- function(file = NULL) {
  x <- capture.output(sessionInfo())
  if (!is.null(file)) writeLines(x, file)
  invisible(x)
}

# Local deterministic random stream; never reset a caller's Monte Carlo RNG.
dsdpm_with_seed <- function(seed, expr) {
  had <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
  old <- if (had) get(".Random.seed", .GlobalEnv) else NULL
  on.exit({
    if (had) assign(".Random.seed", old, .GlobalEnv)
    else if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir=.GlobalEnv)
  }, add=TRUE)
  set.seed(seed)
  eval(substitute(expr), envir=parent.frame())
}

# <<< END R/00_setup.R <<<

# >>> BEGIN R/01_model_core.R >>>
# ======================================================================
# 01_model_core.R
# Exact DSDPM structural definitions, parameter space, stability, priors.
# Reviewer revision v3.4.
# ======================================================================

dsdpm_main_parameter_names <- function() {
  c(
    "alpha", "beta1", "beta2", "beta3", "beta4",
    "tau", "delta", "eta", "lambda", "rho", "kappa"
  )
}

dsdpm_variance_parameter_names <- function() {
  c("sigma2_epsilon", "sigma2_xi", "sigma2_zeta")
}

dsdpm_all_parameter_names <- function() {
  c(dsdpm_main_parameter_names(), dsdpm_variance_parameter_names())
}

dsdpm_true_parameters <- function(delta, tau, eta,
                                  lambda = 0.3,
                                  rho = 0.5,
                                  kappa = 0.6,
                                  alpha = 0.5,
                                  beta1 = 0.5,
                                  beta2 = 0.5,
                                  beta3 = 0.5,
                                  beta4 = 0.5,
                                  sigma2_epsilon = 1.0,
                                  sigma2_xi = 0.5,
                                  sigma2_zeta = 0.5) {
  list(
    alpha = alpha,
    beta1 = beta1,
    beta2 = beta2,
    beta3 = beta3,
    beta4 = beta4,
    tau = tau,
    delta = delta,
    eta = eta,
    lambda = lambda,
    rho = rho,
    kappa = kappa,
    sigma2_epsilon = sigma2_epsilon,
    sigma2_xi = sigma2_xi,
    sigma2_zeta = sigma2_zeta
  )
}

dsdpm_truth_vector <- function(par) {
  out <- c(
    alpha = par$alpha,
    beta1 = par$beta1,
    beta2 = par$beta2,
    beta3 = par$beta3,
    beta4 = par$beta4,
    tau = par$tau,
    delta = par$delta,
    eta = par$eta,
    lambda = par$lambda,
    rho = par$rho,
    kappa = par$kappa,
    sigma2_epsilon = par$sigma2_epsilon,
    sigma2_xi = par$sigma2_xi,
    sigma2_zeta = par$sigma2_zeta
  )
  out[dsdpm_all_parameter_names()]
}

dsdpm_make_queen_W <- function(N) {
  dsdpm_require()
  N <- as.integer(N)
  if (N < 4L) stop("N must be >= 4.")

  nr <- floor(sqrt(N))
  nc <- ceiling(N / nr)

  nb_full <- spdep::cell2nb(nrow = nr, ncol = nc, type = "queen")
  keep <- seq_len(N)
  nb <- lapply(nb_full[keep], function(nei) intersect(nei, keep))
  class(nb) <- "nb"
  attr(nb, "region.id") <- as.character(keep)
  attr(nb, "type") <- "queen"
  attr(nb, "sym") <- TRUE

  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  W <- Matrix::Matrix(spdep::listw2mat(lw), sparse = TRUE)
  ev <- eigen(as.matrix(W), only.values = TRUE)$values

  list(
    W = W,
    nb = nb,
    eigenvalues = ev,
    spectral_radius = max(Mod(ev))
  )
}

dsdpm_logabsdet <- function(A) {
  dd <- determinant(as.matrix(A), logarithm = TRUE)
  val <- as.numeric(dd$modulus)
  if (!is.finite(val)) -Inf else val
}

dsdpm_spatial_interval <- function(W, margin = 1e-4) {
  ev <- eigen(as.matrix(W), only.values = TRUE)$values
  if (max(abs(Im(ev))) > 1e-7) {
    stop(
      "The manuscript-based interval requires real eigenvalues of W. ",
      "For the row-standardised matrix generated from an undirected ",
      "contiguity graph, eigenvalues should be real up to tolerance."
    )
  }
  ev <- Re(ev)
  emin <- min(ev)
  emax <- max(ev)
  lo <- if (emin < 0) 1 / emin else -10
  hi <- if (emax > 0) 1 / emax else 10
  span <- hi - lo
  lo <- lo + margin * span
  hi <- hi - margin * span
  if (!is.finite(lo) || !is.finite(hi) || lo >= hi) {
    stop("Could not build a finite nonsingularity interval from W.")
  }
  c(lo = lo, hi = hi)
}

# tau and eta retain the manuscript Gaussian priors on the real line.
# Their *joint posterior/optimizer support* is restricted by the dynamic
# spectral-radius condition, rather than by an arbitrary marginal box.
dsdpm_default_bounds <- function(W,
                                 rho_bound = 0.999,
                                 spatial_margin = 1e-4,
                                 stability_tol = 0.999) {
  iv <- dsdpm_spatial_interval(W, margin = spatial_margin)
  list(
    delta = iv,
    tau = c(lo = -Inf, hi = Inf),
    eta = c(lo = -Inf, hi = Inf),
    lambda = iv,
    rho = c(lo = -rho_bound, hi = rho_bound),
    kappa = iv,
    eigen_range = range(Re(eigen(as.matrix(W), only.values=TRUE)$values)),
    stability_tol = stability_tol
  )
}

dsdpm_check_structure_direct <- function(W, par, tol = 0.999) {
  Wd <- as.matrix(W)
  storage.mode(Wd) <- "double"
  N <- nrow(Wd)
  if (is.null(N) || N < 1L || ncol(Wd) != N) stop("W must be square.")

  scalar <- function(x, name) {
    if (is.null(x) || !length(x) || !is.finite(as.numeric(x)[1])) {
      stop("Invalid scalar parameter: ", name)
    }
    as.numeric(x)[1]
  }

  delta <- scalar(par$delta, "delta")
  tau <- scalar(par$tau, "tau")
  eta <- scalar(par$eta, "eta")
  lambda <- scalar(par$lambda, "lambda")
  rho <- scalar(par$rho, "rho")
  kappa <- scalar(par$kappa, "kappa")

  I_N <- diag(N)
  S <- I_N - delta * Wd
  L <- I_N - lambda * Wd
  M <- I_N - kappa * Wd

  min_mod <- function(A) min(Mod(eigen(A, only.values = TRUE)$values))
  ns_delta <- min_mod(S) > 1e-8
  ns_lambda <- min_mod(L) > 1e-8
  ns_kappa <- min_mod(M) > 1e-8

  spr_y <- NA_real_
  spr_v <- NA_real_
  if (ns_delta) {
    A_y <- solve(S, tau * I_N + eta * Wd)
    spr_y <- max(Mod(eigen(A_y, only.values = TRUE)$values))
  }
  if (ns_lambda) {
    A_v <- rho * solve(L, I_N)
    spr_v <- max(Mod(eigen(A_v, only.values = TRUE)$values))
  }

  data.frame(
    nonsingular_delta = ns_delta,
    nonsingular_lambda = ns_lambda,
    nonsingular_kappa = ns_kappa,
    outcome_spectral_radius = spr_y,
    disturbance_spectral_radius = spr_v,
    outcome_stable = is.finite(spr_y) && spr_y < tol,
    disturbance_stable = is.finite(spr_v) && spr_v < tol,
    admissible = (
      ns_delta && ns_lambda && ns_kappa &&
      is.finite(spr_y) && spr_y < tol &&
      is.finite(spr_v) && spr_v < tol
    )
  )
}

# Fast equivalent stability formula when W has real eigenvalues.
# Used only as a diagnostic/candidate-search accelerator; the authoritative
# check above uses the transition matrices directly.
dsdpm_check_structure_eigen <- function(W, par, tol = 0.999) {
  ev <- eigen(as.matrix(W), only.values = TRUE)$values
  if (max(abs(Im(ev))) > 1e-7) return(dsdpm_check_structure_direct(W, par, tol))
  w <- Re(ev)
  den_d <- 1 - par$delta * w
  den_l <- 1 - par$lambda * w
  den_k <- 1 - par$kappa * w
  ns_d <- min(abs(den_d)) > 1e-8
  ns_l <- min(abs(den_l)) > 1e-8
  ns_k <- min(abs(den_k)) > 1e-8
  spr_y <- if (ns_d) max(abs((par$tau + par$eta * w) / den_d)) else Inf
  spr_v <- if (ns_l) max(abs(par$rho / den_l)) else Inf
  data.frame(
    nonsingular_delta = ns_d,
    nonsingular_lambda = ns_l,
    nonsingular_kappa = ns_k,
    outcome_spectral_radius = spr_y,
    disturbance_spectral_radius = spr_v,
    outcome_stable = is.finite(spr_y) && spr_y < tol,
    disturbance_stable = is.finite(spr_v) && spr_v < tol,
    admissible = ns_d && ns_l && ns_k && spr_y < tol && spr_v < tol
  )
}

# ----------------------------------------------------------------------
# Internal transforms for nine nonlinear/hyper quantities:
# delta, tau, eta, lambda, rho, kappa, sigma_e, sigma_xi, sigma_zeta.
# The internal scale is R^9, as required for INLA rgeneric.
# ----------------------------------------------------------------------

dsdpm_invlogit <- function(z) stats::plogis(z)
dsdpm_logit <- function(p) log(p / (1 - p))

dsdpm_interval_from_z <- function(z, interval) {
  if (!all(is.finite(interval))) stop("Finite interval required.")
  u <- dsdpm_invlogit(z)
  unname(interval["lo"] + diff(interval) * u)
}

dsdpm_z_from_interval <- function(x, interval) {
  if (!all(is.finite(interval))) stop("Finite interval required.")
  u <- (x - interval["lo"]) / diff(interval)
  u <- min(max(u, 1e-10), 1 - 1e-10)
  dsdpm_logit(u)
}

dsdpm_log_jac_interval <- function(z, interval) {
  u <- dsdpm_invlogit(z)
  log(diff(interval)) + log(u) + log1p(-u)
}

# Smooth one-to-one map onto joint stability support. Endpoint transition
# eigenvalues determine the entire real spectrum of a row-standardized
# undirected W. This changes coordinates, NOT the Gaussian natural priors.
dsdpm_decode_theta9 <- function(z, bounds) {
  if (length(z) != 9L || any(!is.finite(z))) stop("theta must contain 9 finite coordinates")
  delta <- dsdpm_interval_from_z(z[1], bounds$delta)
  lambda <- dsdpm_interval_from_z(z[4], bounds$lambda)
  w <- bounds$eigen_range; cst <- bounds$stability_tol
  endpoint <- cst*tanh(z[2:3])*(1-delta*w)
  eta <- diff(endpoint)/diff(w)
  tau <- endpoint[1]-eta*w[1]
  rho_scale <- min(max(abs(bounds$rho)), cst*min(1-lambda*w))
  list(delta=delta, tau=unname(tau), eta=unname(eta), lambda=lambda,
       rho=unname(rho_scale*tanh(z[5])),
       kappa=dsdpm_interval_from_z(z[6],bounds$kappa),
       sigma_epsilon=exp(z[7]), sigma_xi=exp(z[8]), sigma_zeta=exp(z[9]))
}

dsdpm_encode_theta9 <- function(par, bounds) {
  w <- bounds$eigen_range; cst <- bounds$stability_tol
  trans <- (par$tau+par$eta*w)/(1-par$delta*w)/cst
  rho_scale <- min(max(abs(bounds$rho)), cst*min(1-par$lambda*w))
  if (any(abs(trans)>=1) || abs(par$rho)>=rho_scale) stop("Start outside joint support")
  c(dsdpm_z_from_interval(par$delta,bounds$delta), atanh(trans),
    dsdpm_z_from_interval(par$lambda,bounds$lambda), atanh(par$rho/rho_scale),
    dsdpm_z_from_interval(par$kappa,bounds$kappa),
    log(par$sigma_epsilon),log(par$sigma_xi),log(par$sigma_zeta))
}

dsdpm_log_dynamic_jacobian <- function(z, bounds, p) {
  w <- bounds$eigen_range; cst <- bounds$stability_tol
  rho_scale <- min(max(abs(bounds$rho)), cst*min(1-p$lambda*w))
  sum(log(1-p$delta*w)) - log(diff(w)) + 2*log(cst) +
    sum(log1p(-tanh(z[2:3])^2)) + log(rho_scale) + log1p(-tanh(z[5])^2)
}

dsdpm_pc_sigma_logdens <- function(sigma, u = 1, alpha = 0.5) {
  if (!is.finite(sigma) || sigma <= 0) return(-Inf)
  rate <- -log(alpha) / u
  log(rate) - rate * sigma
}

dsdpm_pc_cor0_logdens <- function(rho, u = 0.5, alpha = 0.5) {
  if (!is.finite(rho) || abs(rho) >= 1) return(-Inf)
  d <- sqrt(-log1p(-rho^2))
  du <- sqrt(-log1p(-u^2))
  rate <- -log(alpha) / du
  if (abs(rho) < 1e-10) return(log(rate) - log(2))
  log(rate) - log(2) - rate * d + log(abs(rho)) - log(d) - log1p(-rho^2)
}

dsdpm_default_prior_spec <- function() {
  list(
    beta_mean = rep(0, 5),
    beta_var = rep(1e6, 5),
    tau_sd = sqrt(0.5),
    eta_sd = sqrt(0.5),
    rho_pc_u = 0.5,
    rho_pc_alpha = 0.5,
    sigma_pc_u = 1.0,
    sigma_pc_alpha = 0.5,
    stability_tol = 0.999
  )
}

dsdpm_logprior_theta9_internal <- function(z, W, bounds,
                                           prior = dsdpm_default_prior_spec()) {
  if (length(z) != 9L || any(!is.finite(z))) return(-Inf)
  p <- dsdpm_decode_theta9(z, bounds)
  st <- try(dsdpm_check_structure(W, p, tol = prior$stability_tol), silent = TRUE)
  if (inherits(st, "try-error") || !isTRUE(st$admissible[1])) return(-Inf)

  lp <- 0
  # delta, lambda, kappa: uniform in natural space over nonsingularity interval.
  lp <- lp + dsdpm_log_jac_interval(z[1], bounds$delta)

  # tau, eta: exact manuscript N(0,0.5) on R; joint stability truncates support.
  lp <- lp + stats::dnorm(p$tau, 0, prior$tau_sd, log = TRUE)
  lp <- lp + stats::dnorm(p$eta, 0, prior$eta_sd, log = TRUE)

  lp <- lp + dsdpm_log_jac_interval(z[4], bounds$lambda)

  # rho: PC-cor0 prior in natural space plus tanh Jacobian.
  lp <- lp + dsdpm_pc_cor0_logdens(
    p$rho, u = prior$rho_pc_u, alpha = prior$rho_pc_alpha
  )
  lp <- lp + dsdpm_log_dynamic_jacobian(z, bounds, p)

  lp <- lp + dsdpm_log_jac_interval(z[6], bounds$kappa)

  # PC priors on standard deviations, with log-sigma transform Jacobians.
  for (jj in 7:9) {
    sig <- exp(z[jj])
    lp <- lp + dsdpm_pc_sigma_logdens(
      sig, u = prior$sigma_pc_u, alpha = prior$sigma_pc_alpha
    ) + z[jj]
  }

  as.numeric(lp)
}

dsdpm_theta9_to_output <- function(p) {
  c(
    tau = p$tau,
    delta = p$delta,
    eta = p$eta,
    lambda = p$lambda,
    rho = p$rho,
    kappa = p$kappa,
    sigma2_epsilon = p$sigma_epsilon^2,
    sigma2_xi = p$sigma_xi^2,
    sigma2_zeta = p$sigma_zeta^2
  )
}

# Same spectral condition, using one eigendecomposition instead of five.
# The original matrix implementation remains an independent test oracle.
dsdpm_check_structure <- function(W, par, tol = 0.999) {
  required <- c("delta","tau","eta","lambda","rho","kappa")
  for (nm in required) par[[nm]] <- dsdpm_assert_scalar(par[[nm]], nm)
  if (nrow(W) != ncol(W) || any(!is.finite(W))) stop("W must be finite and square")
  dsdpm_check_structure_eigen(W, par, tol)
}

# <<< END R/01_model_core.R <<<

# >>> BEGIN R/02_dgp.R >>>
# ======================================================================
# 02_dgp.R
# Exact manuscript data-generating process and algebraic validation.
# ======================================================================

dsdpm_simulate <- function(N, Tt, W, par, seed = NULL,
                           keep_innovations = TRUE) {
  dsdpm_require()
  if (!is.null(seed)) set.seed(seed)

  N <- as.integer(N)
  Tt <- as.integer(Tt)
  if (nrow(W) != N || ncol(W) != N) stop("W dimension mismatch.")

  st <- dsdpm_check_structure(W, par, tol = 0.999999)
  if (!isTRUE(st$admissible[1])) {
    stop(
      "DGP parameter configuration is not jointly admissible: ",
      "all three spatial filters must be nonsingular and both dynamic ",
      "transition spectral radii must be < 1."
    )
  }

  I_N <- Matrix::Diagonal(N)
  one_N <- rep(1, N)
  S <- I_N - par$delta * W
  L <- I_N - par$lambda * W
  M <- I_N - par$kappa * W

  zeta <- stats::rnorm(N, sd = sqrt(par$sigma2_zeta))
  mu <- as.numeric(Matrix::solve(M, zeta))

  Y <- matrix(0, N, Tt)
  V <- matrix(0, N, Tt)
  X <- matrix(0, N, Tt)
  WX <- matrix(0, N, Tt)
  Xlag <- matrix(0, N, Tt)
  WXlag <- matrix(0, N, Tt)
  WY <- matrix(0, N, Tt)

  xi <- numeric(Tt)
  EPS <- matrix(NA_real_, N, Tt)

  y_prev <- rep(0, N)
  v_prev <- rep(0, N)
  x_prev <- rep(0, N)

  for (tt in seq_len(Tt)) {
    x_t <- stats::rnorm(N)
    wx_t <- as.numeric(W %*% x_t)

    x_lag <- x_prev
    wx_lag <- as.numeric(W %*% x_lag)

    xi_t <- stats::rnorm(1, sd = sqrt(par$sigma2_xi))
    eps_t <- stats::rnorm(N, sd = sqrt(par$sigma2_epsilon))

    # Manuscript state equation:
    # (I - lambda W) v_t = rho v_{t-1} + mu + xi_t 1_N + epsilon_t
    rhs_v <- par$rho * v_prev + mu + xi_t * one_N + eps_t
    v_t <- as.numeric(Matrix::solve(L, rhs_v))

    wy_prev <- as.numeric(W %*% y_prev)

    # Manuscript outcome equation:
    # (I - delta W)Y_t =
    # alpha 1 + tau Y_{t-1} + eta W Y_{t-1}
    # + X_t beta1 + W X_t beta2
    # + X_{t-1} beta3 + W X_{t-1} beta4 + v_t
    rhs_y <- (
      par$alpha * one_N +
      par$tau * y_prev +
      par$eta * wy_prev +
      par$beta1 * x_t +
      par$beta2 * wx_t +
      par$beta3 * x_lag +
      par$beta4 * wx_lag +
      v_t
    )
    y_t <- as.numeric(Matrix::solve(S, rhs_y))

    Y[, tt] <- y_t
    V[, tt] <- v_t
    X[, tt] <- x_t
    WX[, tt] <- wx_t
    Xlag[, tt] <- x_lag
    WXlag[, tt] <- wx_lag
    WY[, tt] <- as.numeric(W %*% y_t)
    xi[tt] <- xi_t
    EPS[, tt] <- eps_t

    y_prev <- y_t
    v_prev <- v_t
    x_prev <- x_t
  }

  ans <- list(
    Y = Y,
    V = V,
    mu = mu,
    xi = xi,
    X = X,
    WX = WX,
    Xlag = Xlag,
    WXlag = WXlag,
    WY = WY,
    W = W,
    N = N,
    Tt = Tt,
    true = par
  )

  if (keep_innovations) {
    ans$epsilon <- EPS
    ans$zeta <- zeta
  }

  ans
}

dsdpm_validate_dgp <- function(sim,
                               relative_tolerance = 1e-8,
                               absolute_tolerance = 1e-7) {
  N <- sim$N
  Tt <- sim$Tt
  W <- sim$W
  par <- sim$true

  I_N <- Matrix::Diagonal(N)
  one_N <- rep(1, N)
  S <- I_N - par$delta * W
  L <- I_N - par$lambda * W
  M <- I_N - par$kappa * W

  max_abs_obs <- 0
  max_abs_state <- 0
  max_rel_obs <- 0
  max_rel_state <- 0

  for (tt in seq_len(Tt)) {
    y_prev <- if (tt == 1L) rep(0, N) else sim$Y[, tt - 1L]
    v_prev <- if (tt == 1L) rep(0, N) else sim$V[, tt - 1L]

    obs_lhs <- as.numeric(S %*% sim$Y[, tt])
    obs_rhs <- (
      par$alpha * one_N +
      par$tau * y_prev +
      par$eta * as.numeric(W %*% y_prev) +
      par$beta1 * sim$X[, tt] +
      par$beta2 * sim$WX[, tt] +
      par$beta3 * sim$Xlag[, tt] +
      par$beta4 * sim$WXlag[, tt] +
      sim$V[, tt]
    )

    obs_err <- obs_lhs - obs_rhs
    obs_scale <- max(1, max(abs(obs_lhs)), max(abs(obs_rhs)))

    state_lhs <- as.numeric(L %*% sim$V[, tt])
    state_rhs <- par$rho * v_prev + sim$mu + sim$xi[tt] * one_N
    if (!is.null(sim$epsilon)) state_rhs <- state_rhs + sim$epsilon[, tt]

    state_err <- state_lhs - state_rhs
    state_scale <- max(1, max(abs(state_lhs)), max(abs(state_rhs)))

    max_abs_obs <- max(max_abs_obs, max(abs(obs_err)))
    max_abs_state <- max(max_abs_state, max(abs(state_err)))
    max_rel_obs <- max(max_rel_obs, max(abs(obs_err)) / obs_scale)
    max_rel_state <- max(max_rel_state, max(abs(state_err)) / state_scale)
  }

  spatial_abs <- NA_real_
  spatial_rel <- NA_real_

  if (!is.null(sim$zeta)) {
    lhs <- as.numeric(M %*% sim$mu)
    rhs <- sim$zeta
    err <- lhs - rhs
    scale <- max(1, max(abs(lhs)), max(abs(rhs)))
    spatial_abs <- max(abs(err))
    spatial_rel <- spatial_abs / scale
  }

  component_pass <- function(a, r) {
    is.finite(a) && is.finite(r) &&
      (a <= absolute_tolerance || r <= relative_tolerance)
  }

  obs_ok <- component_pass(max_abs_obs, max_rel_obs)
  state_ok <- component_pass(max_abs_state, max_rel_state)
  spatial_ok <- is.na(spatial_abs) || component_pass(spatial_abs, spatial_rel)

  data.frame(
    max_abs_observation_equation_error = max_abs_obs,
    max_rel_observation_equation_error = max_rel_obs,
    max_abs_state_equation_error = max_abs_state,
    max_rel_state_equation_error = max_rel_state,
    max_abs_spatial_effect_equation_error = spatial_abs,
    max_rel_spatial_effect_equation_error = spatial_rel,
    observation_passed = obs_ok,
    state_passed = state_ok,
    spatial_effect_passed = spatial_ok,
    passed = obs_ok && state_ok && spatial_ok
  )
}

# <<< END R/02_dgp.R <<<

# >>> BEGIN R/03_innovation_likelihood.R >>>
# ======================================================================
# 03_innovation_likelihood.R
#
# Exact Gaussian innovation representation used by FULL_QMLE and FULL_MCMC.
# It is algebraically equivalent to integrating out v_{1:T}.
#
# Define:
#   S = I - delta W
#   L = I - lambda W
#   F = tau I + eta W
#
# v_t = S Y_t - F Y_{t-1} - D_t b
# e_t = L v_t - rho v_{t-1} - mu
#     = xi_t 1_N + epsilon_t
#
# b = (alpha, beta1, beta2, beta3, beta4)'.
#
# After integrating mu:
#   Cov(vec(e)) = I_T kron Omega + J_T kron Sigma_mu,
#   Omega = sigma_xi^2 11' + sigma_epsilon^2 I,
#   Sigma_mu = sigma_zeta^2 M^{-1} M^{-T}.
# ======================================================================

dsdpm_beta_names <- function() {
  c("alpha", "beta1", "beta2", "beta3", "beta4")
}

dsdpm_design_beta_list <- function(sim) {
  D <- vector("list", sim$Tt)
  for (tt in seq_len(sim$Tt)) {
    D[[tt]] <- cbind(
      alpha = rep(1, sim$N),
      beta1 = sim$X[, tt],
      beta2 = sim$WX[, tt],
      beta3 = sim$Xlag[, tt],
      beta4 = sim$WXlag[, tt]
    )
  }
  D
}

dsdpm_build_aG5 <- function(sim,theta_nat) {
  N<-sim$N;Tt<-sim$Tt;W<-as.matrix(sim$W);I<-diag(N)
  S<-I-theta_nat$delta*W;L<-I-theta_nat$lambda*W
  F<-theta_nat$tau*I+theta_nat$eta*W
  lagmat<-function(x) cbind(0,x[,-ncol(x),drop=FALSE])
  base<-S%*%sim$Y-F%*%lagmat(sim$Y)
  a<-L%*%base-theta_nat$rho*lagmat(base)
  DD<-list(matrix(1,N,Tt),sim$X,sim$WX,sim$Xlag,sim$WXlag)
  G<-array(0,c(N,5L,Tt),dimnames=list(NULL,dsdpm_beta_names(),NULL))
  for(j in 1:5) G[,j,]<-L%*%DD[[j]]-theta_nat$rho*lagmat(DD[[j]])
  list(a=a,G=G,S=S,L=L,F=F)
}

dsdpm_chol_inv_logdet <- function(A) {
  A <- as.matrix(A)
  A <- 0.5 * (A + t(A))
  R <- chol(A)
  list(inv = chol2inv(R), logdet = 2 * sum(log(diag(R))), ridge = 0)
}

dsdpm_covariance_components <- function(W, Tt, theta_nat) {
  N <- nrow(W)
  I_N <- diag(N)
  one <- rep(1, N)

  M <- I_N - theta_nat$kappa * as.matrix(W)
  Minv <- solve(M)

  Sigma_mu <- theta_nat$sigma_zeta^2 * tcrossprod(Minv)
  Omega <- (
    theta_nat$sigma_epsilon^2 * I_N +
    theta_nat$sigma_xi^2 * tcrossprod(one)
  )

  Cmean <- Omega + Tt * Sigma_mu
  oo <- dsdpm_chol_inv_logdet(Omega)
  cc <- dsdpm_chol_inv_logdet(Cmean)

  list(
    Omega = Omega,
    Sigma_mu = Sigma_mu,
    Cmean = Cmean,
    Omega_inv = oo$inv,
    Cmean_inv = cc$inv,
    logdet_V = (Tt - 1) * oo$logdet + cc$logdet,
    ridge_Omega = oo$ridge,
    ridge_Cmean = cc$ridge
  )
}

dsdpm_apply_Vinv <- function(X, N, Tt, Omega_inv, Cmean_inv) {
  X <- as.matrix(X)
  if (nrow(X) != N * Tt) {
    stop("X has incompatible row dimension in dsdpm_apply_Vinv().")
  }

  out <- matrix(0, nrow(X), ncol(X))

  for (jj in seq_len(ncol(X))) {
    E <- matrix(X[, jj], nrow = N, ncol = Tt)
    ebar <- rowMeans(E)
    D <- sweep(E, 1, ebar, "-")

    R <- (
      Omega_inv %*% D +
      matrix(Cmean_inv %*% ebar, nrow = N, ncol = Tt)
    )

    out[, jj] <- as.numeric(R)
  }

  out
}

dsdpm_likelihood_components <- function(sim, theta_nat,
                                         beta_prior = NULL,
                                         stability_tol = 0.999) {
  st <- dsdpm_check_structure(
    sim$W,
    list(
      delta = theta_nat$delta,
      tau = theta_nat$tau,
      eta = theta_nat$eta,
      lambda = theta_nat$lambda,
      rho = theta_nat$rho,
      kappa = theta_nat$kappa
    ),
    tol = stability_tol
  )

  if (!isTRUE(st$admissible[1])) {
    stop("Non-admissible structural parameter configuration.")
  }

  if (any(c(
    theta_nat$sigma_epsilon,
    theta_nat$sigma_xi,
    theta_nat$sigma_zeta
  ) <= 0)) {
    stop("All standard deviations must be positive.")
  }

  ag <- dsdpm_build_aG5(sim, theta_nat)
  covs <- dsdpm_covariance_components(sim$W, sim$Tt, theta_nat)

  n <- sim$N * sim$Tt
  a_vec <- as.numeric(ag$a)

  G_mat <- matrix(
    0,
    nrow = n,
    ncol = 5,
    dimnames = list(NULL, dsdpm_beta_names())
  )
  for (jj in seq_len(5L)) {
    G_mat[, jj] <- as.numeric(ag$G[, jj, ])
  }

  Vinv_a <- dsdpm_apply_Vinv(
    matrix(a_vec, ncol = 1),
    sim$N,
    sim$Tt,
    covs$Omega_inv,
    covs$Cmean_inv
  )[, 1L]

  Vinv_G <- dsdpm_apply_Vinv(
    G_mat,
    sim$N,
    sim$Tt,
    covs$Omega_inv,
    covs$Cmean_inv
  )

  GVG <- crossprod(G_mat, Vinv_G)
  GVa <- crossprod(G_mat, Vinv_a)
  aVa <- sum(a_vec * Vinv_a)

  jacobian <- (
    sim$Tt * dsdpm_logabsdet(ag$S) +
    sim$Tt * dsdpm_logabsdet(ag$L)
  )

  if (!is.finite(jacobian)) stop("Non-finite spatial Jacobian.")

  if (is.null(beta_prior)) {
    beta_hat <- as.numeric(solve(GVG, GVa))
    names(beta_hat) <- dsdpm_beta_names()

    resid <- a_vec - as.numeric(G_mat %*% beta_hat)
    Vinv_resid <- dsdpm_apply_Vinv(
      matrix(resid, ncol = 1),
      sim$N,
      sim$Tt,
      covs$Omega_inv,
      covs$Cmean_inv
    )[, 1L]

    quad <- sum(resid * Vinv_resid)
    loglik <- jacobian - 0.5 * (
      n * log(2 * pi) +
      covs$logdet_V +
      quad
    )

    return(list(
      beta_hat = beta_hat,
      loglik = loglik,
      quad = quad,
      GVG = GVG,
      GVa = GVa,
      G_mat = G_mat,
      a_vec = a_vec,
      covs = covs,
      ag = ag,
      structure = st
    ))
  }

  m0 <- as.numeric(beta_prior$mean)
  V0 <- as.matrix(beta_prior$cov)

  if (length(m0) != 5L || any(dim(V0) != c(5L, 5L))) {
    stop("beta_prior must contain mean length 5 and covariance 5x5.")
  }

  P0 <- solve(V0)
  P <- P0 + GVG
  h <- as.numeric(P0 %*% m0 + GVa)

  Pch <- dsdpm_chol_inv_logdet(P)
  P_inv <- Pch$inv
  beta_mean <- as.numeric(P_inv %*% h)
  names(beta_mean) <- dsdpm_beta_names()

  logdet_V0 <- dsdpm_chol_inv_logdet(V0)$logdet
  const_quad <- (
    aVa +
    as.numeric(crossprod(m0, P0 %*% m0)) -
    as.numeric(crossprod(h, P_inv %*% h))
  )

  log_marginal <- jacobian - 0.5 * (
    n * log(2 * pi) +
    covs$logdet_V +
    logdet_V0 +
    Pch$logdet +
    const_quad
  )

  list(
    beta_mean = beta_mean,
    beta_cov = P_inv,
    log_marginal = log_marginal,
    GVG = GVG,
    GVa = GVa,
    G_mat = G_mat,
    a_vec = a_vec,
    covs = covs,
    ag = ag,
    structure = st
  )
}

dsdpm_default_beta_prior <- function(prior = dsdpm_default_prior_spec()) {
  list(
    mean = prior$beta_mean,
    cov = diag(prior$beta_var, 5)
  )
}

# ----------------------------------------------------------------------
# Reduced Gaussian kernel used by FULL_INLA.
#
# The manuscript analytically eliminates the time-varying disturbance v_t
# through the innovation representation. Conditional on
# theta=(delta,tau,eta,lambda,rho,kappa,sigma_e,sigma_xi,sigma_zeta),
# define
#
#   e_t = a_t - G_t b - mu,
#   b=(alpha,beta1,beta2,beta3,beta4)'.
#
# with e_t ~ N(0, Omega),
# Omega = sigma_e^2 I + sigma_xi^2 11'.
# The prior is b~N(m0,V0) and
# mu~N(0, Q_mu^{-1}), Q_mu=sigma_zeta^{-2}(I-kappa W)'(I-kappa W).
# Hence x=(b,mu) is Gaussian conditional on theta and Y.  The function
# below returns its exact posterior precision/mean and the exact marginal
# innovation likelihood p(Y|theta), retaining the two spatial Jacobians
# T log|I-delta W| + T log|I-lambda W|.
# ----------------------------------------------------------------------

dsdpm_beta_mu_posterior_kernel <- function(
    sim,
    theta_nat,
    prior = dsdpm_default_prior_spec(),
    stability_tol = prior$stability_tol
) {
  st <- dsdpm_check_structure(sim$W, theta_nat, tol = stability_tol)
  if (!isTRUE(st$admissible[1])) stop("Non-admissible structural parameter configuration.")
  if (any(c(theta_nat$sigma_epsilon, theta_nat$sigma_xi, theta_nat$sigma_zeta) <= 0)) {
    stop("All standard deviations must be positive.")
  }

  N <- sim$N
  Tt <- sim$Tt
  ag <- dsdpm_build_aG5(sim, theta_nat)

  I_N <- diag(N)
  one <- rep(1, N)
  Omega <- theta_nat$sigma_epsilon^2 * I_N +
    theta_nat$sigma_xi^2 * tcrossprod(one)
  Om <- dsdpm_chol_inv_logdet(Omega)
  Oinv <- Om$inv

  m0 <- as.numeric(prior$beta_mean)
  V0 <- diag(as.numeric(prior$beta_var), 5L)
  P0 <- diag(1 / as.numeric(prior$beta_var), 5L)

  M <- diag(N) - theta_nat$kappa * as.matrix(sim$W)
  Qmu <- (1 / theta_nat$sigma_zeta^2) * crossprod(M)

  Qbb <- P0
  Qbmu <- matrix(0, 5L, N)
  Qmm <- Qmu
  hb <- as.numeric(P0 %*% m0)
  hm <- numeric(N)
  aOa <- 0

  for (tt in seq_len(Tt)) {
    a <- as.numeric(ag$a[, tt])
    G <- as.matrix(ag$G[, , tt])
    Oa <- as.numeric(Oinv %*% a)
    OG <- Oinv %*% G

    Qbb <- Qbb + crossprod(G, OG)
    Qbmu <- Qbmu + crossprod(G, Oinv)
    Qmm <- Qmm + Oinv
    hb <- hb + as.numeric(crossprod(G, Oa))
    hm <- hm + Oa
    aOa <- aOa + sum(a * Oa)
  }

  Qpost <- rbind(
    cbind(Qbb, Qbmu),
    cbind(t(Qbmu), Qmm)
  )
  Qpost <- 0.5 * (Qpost + t(Qpost))
  h <- c(hb, hm)

  chQ <- dsdpm_chol_inv_logdet(Qpost)
  Vpost <- chQ$inv
  mpost <- as.numeric(Vpost %*% h)

  logdet_P0 <- sum(log(diag(P0)))
  logdet_Qmu <- dsdpm_chol_inv_logdet(Qmu)$logdet
  m0P0m0 <- as.numeric(crossprod(m0, P0 %*% m0))
  hQh <- as.numeric(crossprod(h, mpost))

  jacobian <- sim$Tt * dsdpm_logabsdet(ag$S) +
    sim$Tt * dsdpm_logabsdet(ag$L)

  log_marginal <- jacobian - 0.5 * (
    N * Tt * log(2 * pi) +
      Tt * Om$logdet -
      logdet_P0 -
      logdet_Qmu +
      chQ$logdet +
      aOa +
      m0P0m0 -
      hQh
  )

  idx_b <- seq_len(5L)
  idx_mu <- 5L + seq_len(N)
  names(mpost) <- c(dsdpm_beta_names(), paste0("mu_", seq_len(N)))

  list(
    Q = Matrix::Matrix(Qpost, sparse = TRUE),
    mean = mpost,
    covariance = Vpost,
    h = h,
    log_marginal = as.numeric(log_marginal),
    jacobian = jacobian,
    Omega = Omega,
    Q_mu = Qmu,
    structure = st,
    index = list(beta = idx_b, mu = idx_mu),
    ag = ag
  )
}

# Independent algebraic identity used by runtime self-test: integrating
# x=(b,mu) jointly must give the same marginal likelihood as the earlier
# covariance representation that first integrates mu and then b.
dsdpm_validate_collapsed_kernel_identity <- function(
    sim,
    theta_nat,
    prior = dsdpm_default_prior_spec(),
    tolerance = 1e-6
) {
  a <- dsdpm_beta_mu_posterior_kernel(sim, theta_nat, prior)
  b <- dsdpm_likelihood_components(
    sim = sim,
    theta_nat = theta_nat,
    beta_prior = dsdpm_default_beta_prior(prior),
    stability_tol = prior$stability_tol
  )
  diff <- abs(a$log_marginal - b$log_marginal)
  data.frame(
    log_marginal_beta_mu_kernel = a$log_marginal,
    log_marginal_covariance_form = b$log_marginal,
    absolute_difference = diff,
    passed = is.finite(diff) && diff < tolerance
  )
}

# <<< END R/03_innovation_likelihood.R <<<

# >>> BEGIN R/04_full_qmle.R >>>
# ======================================================================
# 04_full_qmle.R
#
# FULL QMLE for the exact manuscript DSDPM.
#
# Outer nonlinear/constrained block:
#   delta, tau, eta, lambda, rho, kappa,
#   sigma_epsilon, sigma_xi, sigma_zeta
#
# Inner linear block concentrated analytically by GLS:
#   alpha, beta1, beta2, beta3, beta4
#
# This differs from the earlier implementation in one important way:
# tau and eta are NOT left unconstrained in the inner GLS block. They are
# part of the outer block so joint dynamic stability is imposed during
# optimization, not checked only after optimization.
# ======================================================================

dsdpm_qmle_default_start <- function(W) {
  list(
    delta = 0.10,
    tau = 0.10,
    eta = 0.05,
    lambda = 0.20,
    rho = 0.30,
    kappa = 0.40,
    sigma_epsilon = 1.0,
    sigma_xi = sqrt(0.5),
    sigma_zeta = sqrt(0.5)
  )
}

dsdpm_jitter_theta_start <- function(z0, scale = 0.25) {
  z0 + stats::rnorm(length(z0), sd = scale)
}

fit_dsdpm_qmle_full <- function(sim,
                                start = NULL,
                                maxit = 600,
                                n_starts = 5,
                                stability_tol = 0.999,
                                trace = FALSE) {
  dsdpm_require()
  bounds <- dsdpm_default_bounds(sim$W, stability_tol=stability_tol)

  start <- start %||% dsdpm_qmle_default_start(sim$W)
  z0 <- dsdpm_encode_theta9(start, bounds)

  objective <- function(z) {
    p <- try(dsdpm_decode_theta9(z, bounds), silent = TRUE)
    if (inherits(p, "try-error")) return(1e100)

    st <- try(
      dsdpm_check_structure(
        sim$W,
        list(
          delta = p$delta,
          tau = p$tau,
          eta = p$eta,
          lambda = p$lambda,
          rho = p$rho,
          kappa = p$kappa
        ),
        tol = stability_tol
      ),
      silent = TRUE
    )

    if (inherits(st, "try-error") || !isTRUE(st$admissible[1])) {
      return(1e100)
    }

    ans <- try(
      dsdpm_likelihood_components(
        sim = sim,
        theta_nat = p,
        beta_prior = NULL,
        stability_tol = stability_tol
      ),
      silent = TRUE
    )

    if (inherits(ans, "try-error") || !is.finite(ans$loglik)) {
      return(1e100)
    }

    -ans$loglik
  }

  starts <- vector("list", max(1L, n_starts))
  starts[[1L]] <- z0
  if (length(starts) > 1L) {
    starts <- dsdpm_with_seed(30101, {
      for (ss in 2:length(starts)) starts[[ss]] <- dsdpm_jitter_theta_start(z0, scale = 0.30)
      starts
    })
  }

  fits <- lapply(starts, function(zz) {
    try(
      stats::optim(
        par = zz,
        fn = objective,
        method = "BFGS",
        hessian = FALSE,
        control = list(
          maxit = maxit,
          reltol = 1e-9,
          trace = if (trace) 1 else 0
        )
      ),
      silent = TRUE
    )
  })

  good <- vapply(
    fits,
    function(x) !inherits(x, "try-error") && is.finite(x$value) && x$value < 1e90,
    logical(1)
  )

  if (!any(good)) stop("FULL_QMLE failed for all starts.")

  vals <- vapply(
    fits[good],
    function(x) x$value,
    numeric(1)
  )
  # Prefer converged candidates; retain a finite failed fit only for failure reporting.
  eligible <- which(good)
  converged <- eligible[vapply(fits[eligible], function(x) identical(x$convergence, 0L), logical(1))]
  if (length(converged)) eligible <- converged
  fit <- fits[[eligible[which.min(vapply(fits[eligible], function(x) x$value, numeric(1)))]]]


  p <- dsdpm_decode_theta9(fit$par, bounds)
  comp <- dsdpm_likelihood_components(
    sim = sim,
    theta_nat = p,
    beta_prior = NULL,
    stability_tol = stability_tol
  )

  b <- comp$beta_hat

  main <- c(
    alpha = unname(b["alpha"]),
    beta1 = unname(b["beta1"]),
    beta2 = unname(b["beta2"]),
    beta3 = unname(b["beta3"]),
    beta4 = unname(b["beta4"]),
    tau = unname(p$tau),
    delta = unname(p$delta),
    eta = unname(p$eta),
    lambda = unname(p$lambda),
    rho = unname(p$rho),
    kappa = unname(p$kappa)
  )

  variance <- c(
    sigma2_epsilon = p$sigma_epsilon^2,
    sigma2_xi = p$sigma_xi^2,
    sigma2_zeta = p$sigma_zeta^2
  )

  hess <- try(stats::optimHess(fit$par, objective), silent = TRUE)
  hess_diag <- list(
    rank = NA_integer_,
    condition_number = NA_real_,
    min_abs_eigenvalue = NA_real_
  )

  if (!inherits(hess, "try-error") && all(is.finite(hess))) {
    ev <- eigen(0.5 * (hess + t(hess)), symmetric = TRUE, only.values = TRUE)$values
    hess_diag <- list(
      rank = qr(hess)$rank,
      condition_number = tryCatch(kappa(hess), error = function(e) Inf),
      min_abs_eigenvalue = min(abs(ev))
    )
  }

  list(
    method = "FULL_QMLE",
    main = main[dsdpm_main_parameter_names()],
    variance = variance[dsdpm_variance_parameter_names()],
    loglik = comp$loglik,
    objective = fit$value,
    convergence = fit$convergence,
    message = fit$message %||% "",
    optimizer = fit,
    structure = comp$structure,
    beta_information = comp$GVG,
    theta_hessian = hess,
    hessian_diagnostics = hess_diag,
    bounds = bounds
  )
}

# <<< END R/04_full_qmle.R <<<

# >>> BEGIN R/08_identification.R >>>
# ======================================================================
# 08_identification.R
# Reviewer 1/3 diagnostics: rank, conditioning, Hessian, moment Jacobian.
# ======================================================================

dsdpm_numeric_jacobian <- function(fun, x, rel_step = 1e-5) {
  x <- as.numeric(x)
  f0 <- fun(x)
  m <- length(f0)
  p <- length(x)
  J <- matrix(NA_real_, m, p)

  for (j in seq_len(p)) {
    h <- rel_step * max(1, abs(x[j]))
    xp <- x
    xm <- x
    xp[j] <- xp[j] + h
    xm[j] <- xm[j] - h

    fp <- fun(xp)
    fm <- fun(xm)
    J[, j] <- (fp - fm) / (2 * h)
  }

  J
}

dsdpm_matrix_diagnostics <- function(A, tol = 1e-8) {
  A <- as.matrix(A)
  sv <- svd(A, nu = 0, nv = 0)$d
  data.frame(
    nrow = nrow(A),
    ncol = ncol(A),
    rank = qr(A, tol = tol)$rank,
    condition_number = if (length(sv) && min(sv) > 0) max(sv) / min(sv) else Inf,
    min_singular_value = if (length(sv)) min(sv) else NA_real_,
    max_singular_value = if (length(sv)) max(sv) else NA_real_
  )
}

dsdpm_outcome_identification <- function(sim) {
  N <- sim$N
  Tt <- sim$Tt
  W <- as.matrix(sim$W)

  Z <- matrix(
    NA_real_,
    nrow = N * Tt,
    ncol = 8L,
    dimnames = list(
      NULL,
      c(
        "intercept", "WY_t", "Y_t1", "WY_t1",
        "X_t", "WX_t", "X_t1", "WX_t1"
      )
    )
  )

  rr <- 1L
  for (tt in seq_len(Tt)) {
    yprev <- if (tt == 1L) rep(0, N) else sim$Y[, tt - 1L]
    wyprev <- as.numeric(W %*% yprev)
    id <- rr:(rr + N - 1L)

    Z[id, ] <- cbind(
      1,
      sim$WY[, tt],
      yprev,
      wyprev,
      sim$X[, tt],
      sim$WX[, tt],
      sim$Xlag[, tt],
      sim$WXlag[, tt]
    )
    rr <- rr + N
  }

  # Standardize non-intercept columns for conditioning diagnostics only.
  Zs <- Z
  for (j in 2:ncol(Zs)) {
    s <- stats::sd(Zs[, j])
    if (!is.finite(s) || s < 1e-12) s <- 1
    Zs[, j] <- (Zs[, j] - mean(Zs[, j])) / s
  }

  md <- dsdpm_matrix_diagnostics(Zs)

  C <- stats::cor(Zs[, -1, drop = FALSE])
  diag(C) <- NA_real_

  data.frame(
    n_rows = nrow(Zs),
    n_columns = ncol(Zs),
    design_rank = md$rank,
    design_condition_number = md$condition_number,
    design_min_singular_value = md$min_singular_value,
    max_abs_pairwise_correlation = max(abs(C), na.rm = TRUE),
    cor_WYt_Ylag = stats::cor(Zs[, "WY_t"], Zs[, "Y_t1"]),
    cor_WYt_WYlag = stats::cor(Zs[, "WY_t"], Zs[, "WY_t1"]),
    cor_Ylag_WYlag = stats::cor(Zs[, "Y_t1"], Zs[, "WY_t1"]),
    cor_X_WX = stats::cor(Zs[, "X_t"], Zs[, "WX_t"]),
    cor_Xlag_WXlag = stats::cor(Zs[, "X_t1"], Zs[, "WX_t1"])
  )
}

dsdpm_qmle_local_identification <- function(sim, par,
                                             stability_tol = 0.999) {
  bounds <- dsdpm_default_bounds(sim$W, stability_tol=stability_tol)
  theta <- list(
    delta = par$delta,
    tau = par$tau,
    eta = par$eta,
    lambda = par$lambda,
    rho = par$rho,
    kappa = par$kappa,
    sigma_epsilon = sqrt(par$sigma2_epsilon),
    sigma_xi = sqrt(par$sigma2_xi),
    sigma_zeta = sqrt(par$sigma2_zeta)
  )
  z0 <- dsdpm_encode_theta9(theta, bounds)

  obj <- function(z) {
    p <- dsdpm_decode_theta9(z, bounds)
    st <- dsdpm_check_structure(sim$W, p, tol = stability_tol)
    if (!isTRUE(st$admissible[1])) return(1e100)

    cc <- try(
      dsdpm_likelihood_components(
        sim, p, beta_prior = NULL, stability_tol = stability_tol
      ),
      silent = TRUE
    )
    if (inherits(cc, "try-error") || !is.finite(cc$loglik)) return(1e100)
    -cc$loglik
  }

  H <- try(stats::optimHess(z0, obj), silent = TRUE)
  if (inherits(H, "try-error") || any(!is.finite(H))) {
    return(data.frame(
      qml_theta_hessian_rank = NA_integer_,
      qml_theta_hessian_condition_number = NA_real_,
      qml_theta_hessian_min_abs_eigenvalue = NA_real_,
      qml_beta_information_rank = NA_integer_,
      qml_beta_information_condition_number = NA_real_
    ))
  }

  ev <- eigen(0.5 * (H + t(H)), symmetric = TRUE, only.values = TRUE)$values

  cc0 <- dsdpm_likelihood_components(
    sim, theta, beta_prior = NULL, stability_tol = stability_tol
  )
  bi <- dsdpm_matrix_diagnostics(cc0$GVG)

  data.frame(
    qml_theta_hessian_rank = qr(H)$rank,
    qml_theta_hessian_condition_number = tryCatch(kappa(H), error = function(e) Inf),
    qml_theta_hessian_min_abs_eigenvalue = min(abs(ev)),
    qml_beta_information_rank = bi$rank,
    qml_beta_information_condition_number = bi$condition_number
  )
}

dsdpm_gmm_local_identification <- function(sim, par,
                                            stability_tol = 0.999) {
  bounds <- dsdpm_default_bounds(sim$W, stability_tol=stability_tol)

  main <- dsdpm_truth_vector(par)[dsdpm_main_parameter_names()]
  variance <- dsdpm_truth_vector(par)[dsdpm_variance_parameter_names()]
  z0 <- dsdpm_gmm_encode14(main, variance, bounds)

  J <- dsdpm_numeric_jacobian(
    function(z) dsdpm_gmm_moments(z, sim, bounds, stability_tol),
    z0
  )
  md <- dsdpm_matrix_diagnostics(J)

  data.frame(
    gmm_n_moments = nrow(J),
    gmm_n_parameters = ncol(J),
    gmm_jacobian_rank = md$rank,
    gmm_jacobian_condition_number = md$condition_number,
    gmm_jacobian_min_singular_value = md$min_singular_value
  )
}

dsdpm_identification_precheck <- function(sim,
                                           stability_tol = 0.999) {
  out1 <- dsdpm_outcome_identification(sim)
  out2 <- dsdpm_qmle_local_identification(
    sim, sim$true, stability_tol
  )
  out3 <- dsdpm_gmm_local_identification(
    sim, sim$true, stability_tol
  )

  out4 <- dsdpm_distribution_local_identification(sim)
  cbind(out1, out2, out3, out4)
}

# A full-rank derivative of a finite collection of Gaussian mean/covariance
# entries is a sufficient local identification condition. Use the first three
# observations: identification of this subvector implies local identification
# for the larger panel. This is population-distribution sensitivity, distinct
# from a random observed Hessian or instrument count.
dsdpm_gaussian_observation_moments <- function(sim, z14, bounds, periods=3L) {
 p<-dsdpm_gmm_decode14(z14,bounds)
 N<-sim$N;Tt<-min(sim$Tt,as.integer(periods));n<-N*Tt
 W<-as.matrix(sim$W);I<-diag(N)
 S<-I-p$delta*W;L<-I-p$lambda*W;F<-p$tau*I+p$eta*W
 B<-matrix(0,n,n);h<-numeric(n);D<-dsdpm_design_beta_list(sim)
 beta<-unlist(p[dsdpm_beta_names()])
 for(t in seq_len(Tt)) {
  ix<-(t-1L)*N+seq_len(N)
  B[ix,ix]<-L%*%S
  h[ix]<-L%*%D[[t]]%*%beta
  if(t>=2L) {
   B[ix,ix-N]<- -(L%*%F+p$rho*S)
   h[ix]<-h[ix]-p$rho*D[[t-1L]]%*%beta
  }
  if(t>=3L) B[ix,ix-2L*N]<-p$rho*F
 }
 cv<-dsdpm_covariance_components(W,Tt,p)
 V<-kronecker(diag(Tt),cv$Omega)+kronecker(matrix(1,Tt,Tt),cv$Sigma_mu)
 Bi<-solve(B)
 list(mean=as.numeric(Bi%*%h),covariance=Bi%*%V%*%t(Bi))
}

dsdpm_distribution_local_identification <- function(sim, par=sim$true,periods=3L) {
 bounds<-dsdpm_default_bounds(sim$W)
 z<-dsdpm_gmm_encode14(dsdpm_truth_vector(par),dsdpm_truth_vector(par),bounds)
 ref<-dsdpm_gaussian_observation_moments(sim,z,bounds,periods)
 sd0<-sqrt(diag(ref$covariance));ix<-lower.tri(ref$covariance,diag=TRUE)
 cvscale<-outer(sd0,sd0)
 fn<-function(zz) {
  a<-dsdpm_gaussian_observation_moments(sim,zz,bounds,periods)
  c(a$mean/sd0,(a$covariance/cvscale)[ix])
 }
 J<-dsdpm_numeric_jacobian(fn,z,rel_step=1e-5)
 dg<-dsdpm_matrix_diagnostics(J,tol=1e-7)
 data.frame(distribution_periods=min(periods,sim$Tt),distribution_jacobian_rank=dg$rank,
  distribution_jacobian_min_singular_value=dg$min_singular_value,
  distribution_jacobian_condition=dg$condition_number,
  local_identification_sufficient=dg$rank==14L)
}

# <<< END R/08_identification.R <<<

# >>> BEGIN R/05_full_gmm.R >>>
# ======================================================================
# 05_full_gmm.R
#
# Structurally matched FULL two-step nonlinear GMM for the exact DSDPM.
#
# The moment conditions are derived from:
#   r_t = (I-delta W)Y_t
#         - alpha 1 - tau Y_{t-1} - eta WY_{t-1}
#         - X_t beta1 - WX_t beta2 - X_{t-1} beta3 - WX_{t-1} beta4
#
#   e_t = (I-lambda W) r_t - rho r_{t-1}
#       = mu + xi_t 1 + epsilon_t.
#
# First-differencing e_t removes time-invariant mu. Lagged outcomes dated
# t-2 and strictly exogenous X terms provide moment instruments.
# Additional second-moment blocks identify the innovation variances and
# the persistent SAR effect (kappa, sigma_zeta^2).
#
# All 14 reported parameters are jointly estimated; the nonlinear block
# is transformed and rejected outside the joint stable region.
# ======================================================================

dsdpm_gmm_decode14 <- function(z, bounds) {
  if (length(z) != 14L) stop("FULL_GMM internal parameter length must be 14.")
  b <- z[1:5]
  names(b) <- dsdpm_beta_names()
  p <- dsdpm_decode_theta9(z[6:14], bounds)
  c(as.list(b), p)
}

dsdpm_gmm_encode14 <- function(main, variance, bounds) {
  b <- c(
    alpha = main["alpha"],
    beta1 = main["beta1"],
    beta2 = main["beta2"],
    beta3 = main["beta3"],
    beta4 = main["beta4"]
  )

  p <- list(
    delta = unname(main["delta"]),
    tau = unname(main["tau"]),
    eta = unname(main["eta"]),
    lambda = unname(main["lambda"]),
    rho = unname(main["rho"]),
    kappa = unname(main["kappa"]),
    sigma_epsilon = sqrt(unname(variance["sigma2_epsilon"])),
    sigma_xi = sqrt(unname(variance["sigma2_xi"])),
    sigma_zeta = sqrt(unname(variance["sigma2_zeta"]))
  )

  c(unname(b), dsdpm_encode_theta9(p, bounds))
}

dsdpm_demean <- function(x) x - mean(x)

# Fixed data-side instruments are cached once per fit. Spatial trace
# coefficients below evaluate the same SAR covariance moments by eigen algebra.
dsdpm_prepare_gmm <- function(sim,spatial_cache=NULL) {
  if(sim$Tt<3L) stop("GMM requires at least three periods")
  N<-sim$N;tt<-3:sim$Tt;W<-as.matrix(sim$W)
  H<-list(sim$X[,tt,drop=FALSE],sim$WX[,tt,drop=FALSE],
    sim$X[,tt-1L,drop=FALSE],sim$WX[,tt-1L,drop=FALSE],
    sim$X[,tt-2L,drop=FALSE],sim$WX[,tt-2L,drop=FALSE],
    sim$Y[,tt-2L,drop=FALSE],sim$WY[,tt-2L,drop=FALSE],W%*%sim$WY[,tt-2L,drop=FALSE])
  H<-lapply(H,function(x) {
    x<-sweep(x,2,colMeans(x),"-")
    ss<-sqrt(colSums(x*x)/(N-1));ss[ss<1e-8|!is.finite(ss)]<-1
    sweep(x,2,ss,"/")
  })
  if(is.null(spatial_cache)) {
    P<-diag(N)-matrix(1/N,N,N);Ws<-(W+t(W))/2
    eg<-eigen(W);if(max(abs(Im(eg$values)))>1e-8) stop("Real spectrum required")
    V<-Re(eg$vectors);Vi<-solve(V);K<-tcrossprod(Vi)
    mats<-list(P,P%*%Ws%*%P,P%*%(Ws%*%Ws)%*%P)
    coeff<-lapply(mats,function(A) (t(V)%*%A%*%V)*K/N)
    spatial_cache<-list(eigenvalues=Re(eg$values),coeff=coeff,Ws=Ws)
  }
  sim$gmm_cache<-list(H=H,spatial=spatial_cache)
  sim
}

dsdpm_gmm_structural_residual <- function(sim,pars) {
  lagmat<-function(x)cbind(0,x[,-ncol(x),drop=FALSE])
  R<-sim$Y-pars$delta*sim$WY-pars$alpha-
    pars$tau*lagmat(sim$Y)-pars$eta*lagmat(sim$WY)-
    pars$beta1*sim$X-pars$beta2*sim$WX-
    pars$beta3*sim$Xlag-pars$beta4*sim$WXlag
  E<-R-pars$lambda*as.matrix(sim$W)%*%R-pars$rho*lagmat(R)
  list(R=R,E=E)
}

dsdpm_gmm_moment_contributions <- function(z,sim,bounds,stability_tol=.999) {
  pars<-dsdpm_gmm_decode14(z,bounds)
  st<-try(dsdpm_check_structure(sim$W,pars,tol=stability_tol),silent=TRUE)
  if(inherits(st,"try-error")||!isTRUE(st$admissible[1])) return(NULL)
  if(is.null(sim$gmm_cache)) sim<-dsdpm_prepare_gmm(sim)
  N<-sim$N;tt<-3:sim$Tt;cache<-sim$gmm_cache
  E<-dsdpm_gmm_structural_residual(sim,pars)$E
  de<-E[,tt,drop=FALSE]-E[,tt-1L,drop=FALSE]
  q<-sweep(de,2,colMeans(de),"-")
  first<-matrix(vapply(cache$H,function(H) colMeans(H*q),numeric(length(tt))),nrow=length(tt),ncol=9L)
  pE<-sweep(E,2,colMeans(E),"-")
  a<-pE[,tt,drop=FALSE];b<-pE[,tt-1L,drop=FALSE]
  Ws<-cache$spatial$Ws
  obs<-cbind(colMeans(a*b),colMeans(a*(Ws%*%b)),colMeans(a*(Ws%*%(Ws%*%b))))
  fv<-1/(1-pars$kappa*cache$spatial$eigenvalues)
  theo<-vapply(cache$spatial$coeff,function(C) pars$sigma_zeta^2*sum(C*outer(fv,fv)),numeric(1))
  out<-cbind(first,level_mean=colMeans(E[,tt,drop=FALSE]),
    sigma_eps=colMeans(q*q)-2*pars$sigma_epsilon^2*(N-1)/N,
    sigma_xi=colMeans(de)^2-2*(pars$sigma_xi^2+pars$sigma_epsilon^2/N),
    sweep(obs,2,theo,"-"))
  colnames(out)<-c("x_t","wx_t","x_t1","wx_t1","x_t2","wx_t2","y_t2","wy_t2","w2y_t2",
    "level_mean","sigma_eps","sigma_xi","mu_I","mu_W","mu_W2")
  out
}

dsdpm_gmm_moments <- function(z, sim, bounds, stability_tol = 0.999) {
  C <- dsdpm_gmm_moment_contributions(
    z, sim, bounds, stability_tol = stability_tol
  )
  if (is.null(C) || any(!is.finite(C))) return(rep(1e6, 15))
  colMeans(C)
}

# Estimate covariance of complete-panel moments, not covariance over the
# T-2 overlapping time contributions. Parametric panels preserve all spatial,
# temporal and common-effect dependence under the first-step fitted model.
dsdpm_gmm_panel_weight <- function(z, sim, bounds, B = 128L,
                                    shrinkage = 0.05, seed = 54101L,
                                    stability_tol = 0.999) {
  if (B < 32L || shrinkage <= 0 || shrinkage >= 1) stop("Use B>=32 and 0<shrinkage<1")
  if(is.null(sim$gmm_cache)) sim<-dsdpm_prepare_gmm(sim)
  pars <- dsdpm_gmm_decode14(z, bounds)
  par <- pars
  par$sigma2_epsilon <- pars$sigma_epsilon^2
  par$sigma2_xi <- pars$sigma_xi^2
  par$sigma2_zeta <- pars$sigma_zeta^2
  draws <- dsdpm_with_seed(seed, {
    out <- matrix(NA_real_, B, 15L)
    for (b in seq_len(B)) {
      sb <- dsdpm_simulate(sim$N, sim$Tt, sim$W, par, keep_innovations=FALSE)
      sb<-dsdpm_prepare_gmm(sb,sim$gmm_cache$spatial)
      out[b,] <- dsdpm_gmm_moments(z, sb, bounds, stability_tol)
    }
    out
  })
  S <- stats::cov(draws)
  if (any(!is.finite(S))) stop("Non-finite panel moment covariance")
  scales <- sqrt(pmax(diag(S), 1e-12))
  C <- S / outer(scales, scales)
  Creg <- (1-shrinkage)*C + shrinkage*diag(15L)
  Sreg <- Creg * outer(scales, scales)
  list(weight=chol2inv(chol(Sreg)), covariance=S, regularized=Sreg,
       rank=qr(S)$rank, B=B, shrinkage=shrinkage, seed=seed)
}

# Bounded internal search avoids floating-point saturation of tanh/exp.
# These are explicit numerical search bounds, not altered DGP parameters.
dsdpm_gmm_search_bounds <- function() {
  limits<-c(rep(Inf,5),12,8,8,12,8,12,10,10,10)
  list(lower=-limits,upper=limits)
}

dsdpm_gmm_optimize <- function(z, objective, maxit, trace=FALSE,
    lower=rep(-Inf,length(z)),upper=rep(Inf,length(z))) {
  z<-pmin(pmax(z,lower),upper)
  first<-try(stats::optim(z,objective,method="L-BFGS-B",lower=lower,upper=upper,
    control=list(maxit=maxit,factr=1e7,pgtol=1e-7,trace=as.integer(trace))),silent=TRUE)
  valid<-function(x) !inherits(x,"try-error") && is.finite(x$value) && x$value<1e90
  attempts<-list(LBFGSB=first)
  if(valid(first) && identical(first$convergence,0L)) {
    first$attempts<-list(LBFGSB=list(value=first$value,convergence=first$convergence,
      message=first$message %||% ""))
    return(first)
  }
  znext<-if(valid(first)) first$par else z
  second<-try(stats::nlminb(znext,objective,lower=lower,upper=upper,
    control=list(iter.max=maxit,eval.max=10L*maxit,rel.tol=1e-9,
                 x.tol=1e-7,trace=as.integer(trace))),silent=TRUE)
  if(!inherits(second,"try-error")) second$value<-second$objective
  attempts$nlminb<-second
  good<-vapply(attempts,valid,logical(1))
  if(!any(good)) stop("GMM optimization produced no finite objective")
  ok<-good & vapply(attempts,function(x) !inherits(x,"try-error") &&
                     isTRUE(x$convergence==0L),logical(1))
  choices<-which(if(any(ok)) ok else good)
  best<-attempts[[choices[which.min(vapply(attempts[choices],function(x)x$value,numeric(1)))]]]
  best$attempts<-lapply(attempts,function(x) if(inherits(x,"try-error"))
    list(error=as.character(x)) else list(value=x$value,convergence=x$convergence,
      message=x$message %||% ""))
  if(!isTRUE(best$convergence==0L)) best$message<-paste("Optimizer did not converge:",best$message %||% "iteration limit")
  best
}

fit_dsdpm_gmm_full <- function(sim,
                               start = NULL,
                               maxit = 800,
                               weight_replications = 128L,
                               weight_shrinkage = 0.05,
                               weight_seed = 54101L,
                               stability_tol = 0.999,
                               trace = FALSE) {
  dsdpm_require()
  sim <- dsdpm_prepare_gmm(sim)
  bounds <- dsdpm_default_bounds(sim$W, stability_tol=stability_tol)

  if (is.null(start)) {
    qml <- try(
      fit_dsdpm_qmle_full(
        sim,
        maxit = 250,
        n_starts = 2,
        stability_tol = stability_tol,
        trace = FALSE
      ),
      silent = TRUE
    )

    if (!inherits(qml, "try-error")) {
      z0 <- dsdpm_gmm_encode14(qml$main, qml$variance, bounds)
    } else {
      main0 <- c(
        alpha = 0,
        beta1 = 0,
        beta2 = 0,
        beta3 = 0,
        beta4 = 0,
        tau = 0.1,
        delta = 0.1,
        eta = 0.05,
        lambda = 0.2,
        rho = 0.3,
        kappa = 0.4
      )
      var0 <- c(
        sigma2_epsilon = 1,
        sigma2_xi = 0.5,
        sigma2_zeta = 0.5
      )
      z0 <- dsdpm_gmm_encode14(main0, var0, bounds)
    }
  } else {
    z0 <- dsdpm_gmm_encode14(start$main, start$variance, bounds)
  }

  obj1 <- function(z) {
    m <- try(
      dsdpm_gmm_moments(z, sim, bounds, stability_tol),
      silent = TRUE
    )
    if (inherits(m, "try-error") || any(!is.finite(m))) return(1e100)
    val <- sum(m^2)
    if (!is.finite(val)) 1e100 else val
  }

  sb<-dsdpm_gmm_search_bounds()
  fit1 <- dsdpm_gmm_optimize(z0,obj1,maxit,trace,sb$lower,sb$upper)

  C1 <- dsdpm_gmm_moment_contributions(
    fit1$par, sim, bounds, stability_tol
  )
  if (is.null(C1) || any(!is.finite(C1))) {
    stop("Could not construct second-step GMM moments.")
  }

  wg <- dsdpm_gmm_panel_weight(fit1$par, sim, bounds,
    B=weight_replications, shrinkage=weight_shrinkage,
    seed=weight_seed, stability_tol=stability_tol)
  Sg <- wg$covariance
  Wg <- wg$weight

  obj2 <- function(z) {
    m <- try(
      dsdpm_gmm_moments(z, sim, bounds, stability_tol),
      silent = TRUE
    )
    if (inherits(m, "try-error") || any(!is.finite(m))) return(1e100)
    val <- as.numeric(crossprod(m, Wg %*% m))
    if (!is.finite(val)) 1e100 else val
  }

  fit2 <- dsdpm_gmm_optimize(fit1$par,obj2,maxit,trace,sb$lower,sb$upper)

  pars <- dsdpm_gmm_decode14(fit2$par, bounds)

  main <- c(
    alpha = pars$alpha,
    beta1 = pars$beta1,
    beta2 = pars$beta2,
    beta3 = pars$beta3,
    beta4 = pars$beta4,
    tau = pars$tau,
    delta = pars$delta,
    eta = pars$eta,
    lambda = pars$lambda,
    rho = pars$rho,
    kappa = pars$kappa
  )

  variance <- c(
    sigma2_epsilon = pars$sigma_epsilon^2,
    sigma2_xi = pars$sigma_xi^2,
    sigma2_zeta = pars$sigma_zeta^2
  )

  st <- dsdpm_check_structure(sim$W, pars, tol = stability_tol)
  moments <- dsdpm_gmm_moments(fit2$par, sim, bounds, stability_tol)

  # Local Jacobian diagnostic at the fitted value.
  jac <- dsdpm_numeric_jacobian(
    function(zz) dsdpm_gmm_moments(
      zz, sim, bounds, stability_tol
    ),
    fit2$par
  )

  list(
    method = "FULL_GMM",
    numerical_search_bounds = sb,
    numerical_boundary_step1 = any(pmin(fit1$par-sb$lower,sb$upper-fit1$par)<1e-4),
    numerical_boundary_step2 = any(pmin(fit2$par-sb$lower,sb$upper-fit2$par)<1e-4),
    main = main[dsdpm_main_parameter_names()],
    variance = variance[dsdpm_variance_parameter_names()],
    objective_step1 = fit1$value,
    objective_step2 = fit2$value,
    convergence_step1 = fit1$convergence,
    convergence_step2 = fit2$convergence,
    convergence = if (identical(fit1$convergence, 0L) && identical(fit2$convergence, 0L)) 0L else 1L,
    message = paste(
      c(fit1$message %||% "", fit2$message %||% "")[nzchar(c(fit1$message %||% "", fit2$message %||% ""))],
      collapse = " | "
    ),
    moments = moments,
    weight_matrix = Wg,
    weight_matrix_condition_number = tryCatch(kappa(Wg), error = function(e) Inf),
    moment_covariance_condition_number = tryCatch(kappa(wg$regularized), error = function(e) Inf),
    weight_covariance_rank = wg$rank,
    weight_replications = wg$B,
    weight_shrinkage = wg$shrinkage,
    weight_seed = wg$seed,
    weighting = "regularized parametric complete-panel covariance",
    efficient_gmm_claim = FALSE,
    moment_jacobian = jac,
    moment_jacobian_diagnostics = dsdpm_matrix_diagnostics(jac),
    structure = st,
    optimizer_step1 = fit1,
    optimizer_step2 = fit2
  )
}

# <<< END R/05_full_gmm.R <<<

# >>> BEGIN R/06_full_inla_rgeneric.R >>>
# ======================================================================
# 06_full_inla_rgeneric.R
# FULL INLA for the exact manuscript DSDPM using the collapsed innovation
# representation described in manuscript Sections 2.2--2.2.2.
#
# This is NOT profile-INLA:
# - delta is integrated jointly with the other hyperparameters;
# - lambda, rho and kappa are not recovered from residuals;
# - no BYM2 or generic AR(1) surrogate is substituted for the manuscript
#   SAR/state equations;
# - observed Y is NOT imposed through extraconstr.
#
# Conditional on the nine nonlinear/hyperparameters, the exact reduced
# Gaussian field is x=(b,mu), b=(alpha,beta1,...,beta4).  The time-varying
# v_t trajectory is analytically marginalised by the innovation likelihood,
# exactly as in the manuscript.  The rgeneric model supplies the normalized
# conditional Gaussian p(x|Y,theta); its theta log-density is augmented by
# the exact marginal likelihood p(Y|theta).  Thus INLA performs the nested
# deterministic integration over the full nine-dimensional theta vector.
# ======================================================================

dsdpm_inla_default_start <- function() {
  list(
    delta = 0.10,
    tau = 0.10,
    eta = 0.05,
    lambda = 0.20,
    rho = 0.30,
    kappa = 0.40,
    sigma_epsilon = 1.0,
    sigma_xi = sqrt(0.5),
    sigma_zeta = sqrt(0.5)
  )
}

# Serialize the complete transitive closure of project helpers. Functions
# with .GlobalEnv environments are otherwise unavailable in INLA's libR.
dsdpm_function_bundle <- function(seeds, source_env = environment(dsdpm_function_bundle)) {
  api <- new.env(parent = baseenv())
  pending <- seeds
  while (length(pending)) {
    nm <- pending[1L]; pending <- pending[-1L]
    if (exists(nm, api, inherits = FALSE)) next
    fn <- get(nm, envir = source_env, inherits = TRUE)
    deps <- codetools::findGlobals(fn, merge = FALSE)$functions
    project <- deps[grepl("^dsdpm_", deps) | deps == "%||%"]
    for (dep in setdiff(deps, project)) {
      if (!exists(dep, api, inherits = TRUE) && exists(dep, source_env, inherits = TRUE)) {
        assign(dep, get(dep, source_env, inherits = TRUE), api)
      }
    }
    environment(fn) <- api
    assign(nm, fn, api)
    pending <- unique(c(pending, project))
  }
  api
}

# rgeneric latent model.  Q and mu define the exact normalized
# p(x|Y,theta), while log.prior() returns log p(theta)+log p(Y|theta).
# This is an exact re-expression of p(x,theta|Y), not an approximation of
# the DSDPM structure.  INLA is then used only for its deterministic nested
# integration over theta and Gaussian mixture marginals for x.
dsdpm_rgeneric_collapsed_model <- function(
    cmd = c("graph", "Q", "mu", "initial", "log.norm.const", "log.prior", "quit"),
    theta = NULL
) {
  envir <- parent.env(environment())
  get_env <- function(name) get(name, envir = envir, inherits = FALSE)

  api <- get_env("api")
  initial <- function() get_env("init_z")
  theta_use <- function() {
    th <- theta
    if (is.null(th) || !length(th)) th <- initial()
    th
  }

  kernel <- function(th) {
    sim <- get_env("sim")
    bounds <- get_env("bounds")
    prior <- get_env("prior")
    cached <- get_env("cache")
    if (identical(cached$theta, th)) return(cached$value)
    p <- api$dsdpm_decode_theta9(th, bounds)
    value <- api$dsdpm_beta_mu_posterior_kernel(
      sim = sim,
      theta_nat = p,
      prior = prior,
      stability_tol = prior$stability_tol
    )
    cached$theta <- th; cached$value <- value
    value
  }

  safe_kernel <- function(th) {
    # INLA/GMRFLib can request Q/mu before evaluating log.prior.  For a
    # theta point outside the joint stability support, return the proper
    # Gaussian kernel at the initial admissible point; log.prior() then
    # returns -Inf for the requested theta.  This prevents numerical/API
    # crashes without giving inadmissible points posterior mass.
    out <- try(kernel(th), silent = TRUE)
    if (!inherits(out, "try-error")) return(out)
    kernel(initial())
  }

  graph <- function() {
    # N<=100 gives latent dimension <=105.  A full graph is intentionally
    # used as a safe union graph because the beta-mu posterior cross-block
    # can change numerically with theta.  Q() may return exact zeros.
    n <- get_env("n_latent")
    Matrix::Matrix(matrix(1, n, n), sparse = TRUE)
  }

  Q <- function() {
    safe_kernel(theta_use())$Q
  }

  mu <- function() {
    safe_kernel(theta_use())$mean
  }

  log.norm.const <- function() {
    # Empty => GMRFLib computes the normalizing constant from Q(theta).
    numeric(0)
  }

  log.prior <- function() {
    th <- theta_use()
    sim <- get_env("sim")
    bounds <- get_env("bounds")
    prior <- get_env("prior")

    lp <- api$dsdpm_logprior_theta9_internal(
      th, W = sim$W, bounds = bounds, prior = prior
    )
    if (!is.finite(lp)) return(-Inf)

    kk <- try(kernel(th), silent = TRUE)
    if (inherits(kk, "try-error") || !is.finite(kk$log_marginal)) return(-Inf)

    as.numeric(lp + kk$log_marginal)
  }

  quit <- function() invisible()
  do.call(match.arg(cmd), args = list())
}

dsdpm_inla_precheck <- function(sim,
                                 start = NULL,
                                 prior = dsdpm_default_prior_spec()) {
  dsdpm_require(include_inla = TRUE)
  bounds <- dsdpm_default_bounds(sim$W, stability_tol=prior$stability_tol)
  start <- start %||% dsdpm_inla_default_start()
  st <- dsdpm_check_structure(sim$W, start, tol = prior$stability_tol)
  if (!isTRUE(st$admissible[1])) stop("INLA initial point is not admissible.")

  k <- dsdpm_beta_mu_posterior_kernel(sim, start, prior)
  chol_ok <- !inherits(try(chol(as.matrix(k$Q)), silent = TRUE), "try-error")
  id <- dsdpm_validate_collapsed_kernel_identity(sim, start, prior)

  data.frame(
    n_latent = nrow(k$Q),
    n_hyperparameters = 9L,
    q_nonzero = length(k$Q@x),
    cholesky_ok = chol_ok,
    collapsed_identity_difference = id$absolute_difference,
    collapsed_identity_passed = id$passed,
    outcome_spectral_radius = st$outcome_spectral_radius,
    disturbance_spectral_radius = st$disturbance_spectral_radius,
    admissible = st$admissible
  )
}

fit_dsdpm_inla_full <- function(
    sim,
    start = NULL,
    prior = dsdpm_default_prior_spec(),
    int_strategy = "ccd",
    strategy = "gaussian",
    verbose = FALSE,
    debug_rgeneric = FALSE,
    improve_hyperpar = FALSE,
    num_threads = "1:1",
    transform_points = 8192L
) {
  dsdpm_require(include_inla = TRUE)
  if (int_strategy == "eb") stop("Empirical Bayes is not full posterior integration")

  bounds <- dsdpm_default_bounds(sim$W, stability_tol=prior$stability_tol)
  start <- start %||% dsdpm_inla_default_start()
  st0 <- dsdpm_check_structure(sim$W, start, tol = prior$stability_tol)
  if (!isTRUE(st0$admissible[1])) stop("FULL_INLA start is outside the joint admissible region.")

  init_z <- dsdpm_encode_theta9(start, bounds)
  k0 <- dsdpm_beta_mu_posterior_kernel(sim, start, prior)
  n_latent <- nrow(k0$Q)

  rmodel <- INLA::inla.rgeneric.define(
    model = dsdpm_rgeneric_collapsed_model,
    n = n_latent,
    debug = debug_rgeneric,
    sim = sim,
    bounds = bounds,
    prior = prior,
    init_z = init_z,
    n_latent = n_latent,
    api = dsdpm_function_bundle(c("dsdpm_decode_theta9",
      "dsdpm_beta_mu_posterior_kernel", "dsdpm_logprior_theta9_internal")),
    cache = new.env(parent = emptyenv())
  )

  # Instantiate every latent component through a zero-weight random-effect
  # covariate.  The Gaussian dummy likelihood is therefore N(0,1) with
  # predictor identically zero: it contributes only a theta-independent
  # constant and cannot double-count the DSDPM data likelihood.  This is
  # more robust than relying on an all-NA response while preserving the
  # exact target encoded in Q/mu/log.prior.
  dat <- data.frame(
    y_inla = rep(0, n_latent),
    state_idx = seq_len(n_latent),
    latent_weight = rep(0, n_latent)
  )
  f <- INLA::f
  formula <- y_inla ~ -1 +
    f(state_idx, latent_weight, model = rmodel, constr = FALSE)

  tm0 <- proc.time()[3]
  fit <- INLA::inla(
    formula = formula,
    family = "gaussian",
    data = dat,
    verbose = verbose,
    num.threads = num_threads,
    control.family = list(
      hyper = list(prec = list(initial = 0, fixed = TRUE))
    ),
    control.predictor = list(compute = TRUE),
    control.compute = list(config = TRUE, mlik = TRUE, return.marginals = TRUE),
    control.inla = list(strategy = strategy, int.strategy = int_strategy)
  )
  if (isTRUE(improve_hyperpar)) {
    fit2 <- try(INLA::inla.hyperpar(fit), silent = TRUE)
    if (!inherits(fit2, "try-error")) fit <- fit2
  }
  elapsed <- proc.time()[3] - tm0

  sr_list <- fit$summary.random
  if (is.null(sr_list) || !length(sr_list)) {
    stop("FULL_INLA did not return the custom latent-field summary.")
  }
  sr <- sr_list[[1L]]
  if (nrow(sr) < 5L) stop("FULL_INLA latent summary is unexpectedly short.")

  bmean <- sr$mean[1:5]
  names(bmean) <- dsdpm_beta_names()
  bint <- cbind(
    q025 = sr[["0.025quant"]][1:5],
    q50 = sr[["0.5quant"]][1:5],
    q975 = sr[["0.975quant"]][1:5]
  )
  rownames(bint) <- dsdpm_beta_names()

  mh <- fit$internal.marginals.hyperpar
  if(length(mh)!=9L) stop("Expected nine internal hyperparameter marginals")
  # Natural tau, eta and rho depend jointly on multiple internal coordinates.
  # Use the continuous INLA joint approximation, not discrete CCD node
  # quantiles. The construction matches inla.hyperpar.sample(improve.marginals=TRUE)
  # with deterministic low-discrepancy uniforms for reproducibility.
  zmat <- dsdpm_inla_joint_quadrature(fit,transform_points)
  nat_names <- c("delta","tau","eta","lambda","rho","kappa",
                 "sigma2_epsilon","sigma2_xi","sigma2_zeta")
  nat <- t(apply(zmat,1,function(z) {
    pp<-dsdpm_decode_theta9(z,bounds)
    c(pp$delta,pp$tau,pp$eta,pp$lambda,pp$rho,pp$kappa,
      pp$sigma_epsilon^2,pp$sigma_xi^2,pp$sigma_zeta^2)
  }))
  colnames(nat)<-nat_names
  nat_sum<-t(apply(nat,2,function(x) c(mean=mean(x),sd=stats::sd(x),
    q025=unname(stats::quantile(x,.025)),q50=stats::median(x),q975=unname(stats::quantile(x,.975)))))
  nat_marg<-list()
  for(j in c(1L,4L,6L,7L,8L,9L)) {
    transform<-if(j<=6L) local({iv<-bounds[[c("delta","lambda","kappa")[match(j,c(1,4,6))]]];function(z) dsdpm_interval_from_z(z,iv)}) else function(z) exp(2*z)
    mm<-INLA::inla.tmarginal(transform,mh[[j]])
    mn<-INLA::inla.emarginal(function(x)x,mm)
    nat_sum[j,]<-c(mn,sqrt(max(0,INLA::inla.emarginal(function(x)x*x,mm)-mn^2)),
      INLA::inla.qmarginal(.025,mm),INLA::inla.qmarginal(.5,mm),INLA::inla.qmarginal(.975,mm))
    nat_marg[[nat_names[j]]]<-mm
  }
  bsd <- sr$sd[1:5]
  transform_error <- vapply(c(2L,3L,5L),function(j)
    abs(mean(nat[seq_len(nrow(nat)%/%2),j])-mean(nat[,j]))/max(stats::sd(nat[,j]),1e-12),numeric(1))

  main <- c(
    alpha = unname(bmean["alpha"]),
    beta1 = unname(bmean["beta1"]),
    beta2 = unname(bmean["beta2"]),
    beta3 = unname(bmean["beta3"]),
    beta4 = unname(bmean["beta4"]),
    tau = nat_sum["tau", "mean"],
    delta = nat_sum["delta", "mean"],
    eta = nat_sum["eta", "mean"],
    lambda = nat_sum["lambda", "mean"],
    rho = nat_sum["rho", "mean"],
    kappa = nat_sum["kappa", "mean"]
  )
  variance <- c(
    sigma2_epsilon = nat_sum["sigma2_epsilon", "mean"],
    sigma2_xi = nat_sum["sigma2_xi", "mean"],
    sigma2_zeta = nat_sum["sigma2_zeta", "mean"]
  )

  intervals <- matrix(
    NA_real_, 14L, 3L,
    dimnames = list(dsdpm_all_parameter_names(), c("q025", "q50", "q975"))
  )
  intervals[dsdpm_beta_names(), ] <- bint
  for (nm in nat_names) intervals[nm, ] <- nat_sum[nm, c("q025", "q50", "q975")]

  st_mean <- dsdpm_check_structure(
    sim$W,
    list(
      delta = main["delta"], tau = main["tau"], eta = main["eta"],
      lambda = main["lambda"], rho = main["rho"], kappa = main["kappa"]
    ),
    tol = prior$stability_tol
  )

  list(
    method = "FULL_INLA",
    main = main[dsdpm_main_parameter_names()],
    variance = variance[dsdpm_variance_parameter_names()],
    intervals = intervals,
    hyper_summary_natural = nat_sum,
    posterior_sd = c(setNames(bsd,dsdpm_beta_names()),
      setNames(nat_sum[,"sd"],nat_names))[dsdpm_all_parameter_names()],
    integration_nodes_internal = zmat,
    integration_nodes_natural = nat,
    hyper_marginals_natural = nat_marg,
    transformation_points = transform_points,
    transformation_half_sample_mean_error_sd = transform_error,
    interval_method = "INLA marginals; continuous marginal-corrected joint quadrature for tau eta rho",
    integration_strategy = int_strategy,
    latent_summary = sr,
    structure_at_posterior_mean = st_mean,
    elapsed_seconds = proc.time()[3]-tm0,
    n_hyperparameters = 9L,
    external_profile_used = FALSE,
    residual_recovery_used = FALSE,
    bym2_used = FALSE,
    generic_ar1_substitute_used = FALSE,
    exact_y_constraints = FALSE,
    collapsed_innovation_representation = TRUE,
    posterior_support_restricted = TRUE,
    convergence = as.integer(fit$mode$mode.status %||% NA_integer_),
    message = if (identical(as.integer(fit$mode$mode.status %||% NA_integer_), 0L)) "" else "INLA mode status missing or nonzero",
    inla_fit = fit,
    rmodel = rmodel,
    bounds = bounds
  )
}

# Deterministic analogue of INLA::inla.hyperpar.sample with marginal
# correction; preserves the fitted joint dependence for coupled transforms.
dsdpm_halton <- function(n, bases=c(2,3,5,7,11,13,17,19,23)) {
  vapply(bases,function(base) {
    k<-seq_len(n)+32L; out<-numeric(n); factor<-1/base
    while(any(k>0)) { out<-out+factor*(k%%base); k<-k%/%base;factor<-factor/base }
    out
  },numeric(n))
}

dsdpm_inla_joint_quadrature <- function(fit,n=8192L) {
  if(n<1024L) stop("Use at least 1024 joint transformation points")
  u<-dsdpm_halton(n)
  sp<-fit$misc$stdev.corr.positive %||% rep(1,9)
  sn<-fit$misc$stdev.corr.negative %||% rep(1,9)
  z<-u
  for(j in 1:9) {
    pn<-sn[j]/(sn[j]+sp[j]);left<-u[,j]<pn
    z[left,j]<-sn[j]*stats::qnorm(u[left,j]/(2*pn))
    z[!left,j]<-sp[j]*stats::qnorm(.5+(u[!left,j]-pn)/(2*(1-pn)))
  }
  A<-fit$misc$cov.intern.eigenvectors%*%diag(sqrt(fit$misc$cov.intern.eigenvalues),9)
  th<-sweep(z%*%t(A),2,fit$misc$theta.mode,"+")
  for(j in 1:9) th[,j]<-INLA::inla.qmarginal(rank(th[,j],ties.method="first")/(n+1),
                                          fit$internal.marginals.hyperpar[[j]])
  th
}

# <<< END R/06_full_inla_rgeneric.R <<<

# >>> BEGIN R/07_full_mcmc.R >>>
# ======================================================================
# 07_full_mcmc.R
#
# FULL MCMC Bayesian reference for the same DSDPM and the same priors as
# FULL_INLA.
#
# This is a collapsed / Rao-Blackwellized MCMC:
# - MCMC samples all nine constrained nonlinear/hyper parameters jointly;
# - b = (alpha,beta1,...,beta4) and mu are analytically integrated in the
#   acceptance probability;
# - b is drawn from its exact conditional Gaussian posterior for retained
#   hyperparameter draws;
# - mu can optionally be drawn from its exact conditional posterior.
#
# Collapsing does NOT change the target posterior. It only improves MCMC
# efficiency and is a standard exact marginalization strategy.
# ======================================================================

dsdpm_logpost_theta9_internal <- function(
    z,
    sim,
    bounds,
    prior = dsdpm_default_prior_spec()
) {
  lp <- dsdpm_logprior_theta9_internal(
    z,
    W = sim$W,
    bounds = bounds,
    prior = prior
  )
  if (!is.finite(lp)) return(-Inf)

  p <- dsdpm_decode_theta9(z, bounds)

  cc <- try(
    dsdpm_likelihood_components(
      sim = sim,
      theta_nat = p,
      beta_prior = dsdpm_default_beta_prior(prior),
      stability_tol = prior$stability_tol
    ),
    silent = TRUE
  )

  if (inherits(cc, "try-error") || !is.finite(cc$log_marginal)) {
    return(-Inf)
  }

  as.numeric(cc$log_marginal + lp)
}

dsdpm_conditional_beta_draw <- function(sim, theta_nat,
                                        prior = dsdpm_default_prior_spec()) {
  cc <- dsdpm_likelihood_components(
    sim = sim,
    theta_nat = theta_nat,
    beta_prior = dsdpm_default_beta_prior(prior),
    stability_tol = prior$stability_tol
  )

  draw <- as.numeric(
    MASS::mvrnorm(
      n = 1L,
      mu = cc$beta_mean,
      Sigma = cc$beta_cov
    )
  )
  names(draw) <- dsdpm_beta_names()

  list(
    draw = draw,
    mean = cc$beta_mean,
    cov = cc$beta_cov
  )
}

dsdpm_conditional_mu_draw <- function(sim, theta_nat, beta,
                                      seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  ag <- dsdpm_build_aG5(sim, theta_nat)

  n <- sim$N
  Tt <- sim$Tt
  ehat <- matrix(0, n, Tt)

  for (tt in seq_len(Tt)) {
    ehat[, tt] <- (
      ag$a[, tt] -
      as.numeric(ag$G[, , tt] %*% beta)
    )
  }

  I_N <- diag(n)
  Omega <- (
    theta_nat$sigma_epsilon^2 * I_N +
    theta_nat$sigma_xi^2 * matrix(1, n, n)
  )
  Omega_inv <- solve(Omega)

  M <- I_N - theta_nat$kappa * as.matrix(sim$W)
  Q_mu_prior <- (
    (1 / theta_nat$sigma_zeta^2) *
    crossprod(M)
  )

  Q_post <- Q_mu_prior + Tt * Omega_inv
  V_post <- solve(Q_post)
  h <- Omega_inv %*% rowSums(ehat)
  m_post <- as.numeric(V_post %*% h)

  draw <- as.numeric(
    MASS::mvrnorm(1L, mu = m_post, Sigma = V_post)
  )

  list(draw = draw, mean = m_post, cov = V_post)
}

dsdpm_default_mcmc_start <- function() {
  dsdpm_inla_default_start()
}

dsdpm_posterior_mode <- function(sim, prior=dsdpm_default_prior_spec(), maxit=1000L) {
  bounds<-dsdpm_default_bounds(sim$W,stability_tol=prior$stability_tol)
  z0<-dsdpm_encode_theta9(dsdpm_default_mcmc_start(),bounds)
  objective<-function(z) {
    lp<-dsdpm_logpost_theta9_internal(z,sim,bounds,prior)
    if (is.finite(lp)) -lp else 1e100
  }
  fit<-stats::optim(z0,objective,method="BFGS",control=list(maxit=maxit,reltol=1e-10))
  H<-try(stats::optimHess(fit$par,objective),silent=TRUE)
  good<-!inherits(H,"try-error")&&all(is.finite(H))
  V<-diag(.1^2,9)
  if(good) {
    ev<-eigen((H+t(H))/2,symmetric=TRUE)
    good<-min(ev$values)>1e-6
    if(good) V<-ev$vectors%*%diag(1/ev$values)%*%t(ev$vectors)
  }
  list(z=fit$par,covariance=V,hessian_positive=good,convergence=fit$convergence,
       bounds=bounds,logpost=-fit$value)
}

fit_dsdpm_mcmc_full <- function(
    sim,
    start = NULL,
    prior = dsdpm_default_prior_spec(),
    n_iter = 12000,
    burn = 4000,
    thin = 4,
    adapt_start = 500,
    adapt_every = 100,
    proposal_scale = NULL,
    initial_covariance = NULL,
    initial_internal = NULL,
    seed = 7001,
    return_mu_draws = FALSE,
    progress = FALSE
) {
  dsdpm_require()

  if (burn >= n_iter) stop("burn must be less than n_iter.")
  if (thin < 1L) stop("thin must be >= 1.")
  if (adapt_start >= burn) {
    warning("adapt_start >= burn: adaptation may be too short.")
  }

  set.seed(seed)

  bounds <- dsdpm_default_bounds(sim$W, stability_tol=prior$stability_tol)
  start <- start %||% dsdpm_default_mcmc_start()

  st0 <- dsdpm_check_structure(sim$W, start, tol = prior$stability_tol)
  if (!isTRUE(st0$admissible[1])) {
    stop("FULL_MCMC starting point is not admissible.")
  }

  z_curr <- initial_internal %||% dsdpm_encode_theta9(start, bounds)
  d <- length(z_curr)
  lp_curr <- dsdpm_logpost_theta9_internal(
    z_curr, sim, bounds, prior
  )
  if (!is.finite(lp_curr)) stop("Initial MCMC log posterior is not finite.")

  if (is.null(proposal_scale)) {
    proposal_scale <- 2.38 / sqrt(d)
  }

  prop_cov <- initial_covariance %||% diag(0.08^2, d)
  base_cov <- prop_cov
  chain <- matrix(NA_real_, nrow = n_iter, ncol = d)
  colnames(chain) <- paste0("z", seq_len(d))
  lp_chain <- rep(NA_real_, n_iter)
  accepted <- logical(n_iter)

  running <- matrix(NA_real_, nrow = 0L, ncol = d)

  for (it in seq_len(n_iter)) {
    step <- as.numeric(
      MASS::mvrnorm(
        1L,
        mu = rep(0, d),
        Sigma = prop_cov
      )
    )
    z_prop <- z_curr + proposal_scale * step

    lp_prop <- dsdpm_logpost_theta9_internal(
      z_prop, sim, bounds, prior
    )

    loga <- lp_prop - lp_curr
    if (is.finite(loga) && log(stats::runif(1)) < min(0, loga)) {
      z_curr <- z_prop
      lp_curr <- lp_prop
      accepted[it] <- TRUE
    }

    chain[it, ] <- z_curr
    lp_chain[it] <- lp_curr

    if (it <= burn && it >= adapt_start) {
      running <- rbind(running, z_curr)

      if (it %% adapt_every == 0L && nrow(running) > d + 5L) {
        emp <- stats::cov(running)
        if (all(is.finite(emp))) {
          prop_cov <- 0.95*emp + 0.05*base_cov + diag(1e-8, d)
        }

        # Simple Robbins-Monro scale adaptation toward ~0.234.
        recent <- accepted[max(1L, it - adapt_every + 1L):it]
        ar <- mean(recent)
        proposal_scale <- proposal_scale * exp(0.5 * (ar - 0.234))
        proposal_scale <- min(max(proposal_scale, 0.05), 5)
      }
    }

    if (progress && (it %% max(1L, floor(n_iter / 20L)) == 0L)) {
      cat(
        sprintf(
          "MCMC %d/%d | acceptance=%.3f | scale=%.3f\n",
          it, n_iter, mean(accepted[seq_len(it)]), proposal_scale
        )
      )
    }
  }

  keep <- seq.int(burn + 1L, n_iter, by = thin)
  z_keep <- chain[keep, , drop = FALSE]
  n_keep <- nrow(z_keep)

  theta_nat <- matrix(
    NA_real_,
    nrow = n_keep,
    ncol = 9L,
    dimnames = list(
      NULL,
      c(
        "delta", "tau", "eta", "lambda", "rho", "kappa",
        "sigma_epsilon", "sigma_xi", "sigma_zeta"
      )
    )
  )

  beta_draws <- matrix(
    NA_real_,
    nrow = n_keep,
    ncol = 5L,
    dimnames = list(NULL, dsdpm_beta_names())
  )

  mu_draws <- if (return_mu_draws) {
    matrix(NA_real_, nrow = n_keep, ncol = sim$N)
  } else {
    NULL
  }

  for (ii in seq_len(n_keep)) {
    p <- dsdpm_decode_theta9(z_keep[ii, ], bounds)
    theta_nat[ii, ] <- c(
      p$delta, p$tau, p$eta, p$lambda, p$rho, p$kappa,
      p$sigma_epsilon, p$sigma_xi, p$sigma_zeta
    )

    bd <- dsdpm_conditional_beta_draw(sim, p, prior)
    beta_draws[ii, ] <- bd$draw

    if (return_mu_draws) {
      mu_draws[ii, ] <- dsdpm_conditional_mu_draw(
        sim, p, bd$draw
      )$draw
    }
  }

  all_draws <- cbind(
    beta_draws,
    tau = theta_nat[, "tau"],
    delta = theta_nat[, "delta"],
    eta = theta_nat[, "eta"],
    lambda = theta_nat[, "lambda"],
    rho = theta_nat[, "rho"],
    kappa = theta_nat[, "kappa"],
    sigma2_epsilon = theta_nat[, "sigma_epsilon"]^2,
    sigma2_xi = theta_nat[, "sigma_xi"]^2,
    sigma2_zeta = theta_nat[, "sigma_zeta"]^2
  )
  all_draws <- all_draws[, dsdpm_all_parameter_names(), drop = FALSE]

  posterior_mean <- colMeans(all_draws)
  q <- t(apply(
    all_draws,
    2,
    stats::quantile,
    probs = c(0.025, 0.5, 0.975),
    names = FALSE
  ))
  colnames(q) <- c("q025", "q50", "q975")

  main <- posterior_mean[dsdpm_main_parameter_names()]
  variance <- posterior_mean[dsdpm_variance_parameter_names()]

  pmean <- list(
    delta = main["delta"],
    tau = main["tau"],
    eta = main["eta"],
    lambda = main["lambda"],
    rho = main["rho"],
    kappa = main["kappa"]
  )
  st_mean <- dsdpm_check_structure(
    sim$W, pmean, tol = prior$stability_tol
  )

  list(
    method = "FULL_MCMC",
    main = main,
    variance = variance,
    intervals = q,
    draws = all_draws,
    draws_internal_theta = z_keep,
    draws_theta_natural = theta_nat,
    draws_mu = mu_draws,
    logpost = lp_chain[keep],
    acceptance_rate = mean(accepted),
    acceptance_postburn = mean(accepted[(burn + 1L):n_iter]),
    proposal_scale_final = proposal_scale,
    structure_at_posterior_mean = st_mean,
    posterior_support_restricted = TRUE,
    posterior_sd = apply(all_draws,2,stats::sd),
    convergence = NA_integer_,
    diagnostics_validated = FALSE,
    message = "Single chain: convergence not certified; use multichain wrapper"
  )
}

# Rank-normalized split R-hat, bulk/tail ESS and MCSE from posterior.
# A completed chain is not automatically a converged estimator.
fit_dsdpm_mcmc_multichain_full <- function(sim, n_chains=4L,
    seeds=9101L+seq_len(n_chains), prior=dsdpm_default_prior_spec(),
    rhat_limit=1.01, ess_min=400, ...) {
  dsdpm_require(include_mcmc_diag=TRUE)
  if(n_chains<2L || length(seeds)!=n_chains || anyDuplicated(seeds))
    stop("At least two independently seeded chains required")
  tm<-proc.time()[3]
  mode<-dsdpm_posterior_mode(sim,prior)
  starts<-lapply(seq_len(n_chains),function(j) dsdpm_with_seed(seeds[j],
    mode$z+as.numeric(MASS::mvrnorm(1,rep(0,9),mode$covariance))))
  fits<-lapply(seq_len(n_chains),function(j) fit_dsdpm_mcmc_full(sim,
    prior=prior,seed=seeds[j],initial_internal=starts[[j]],
    initial_covariance=mode$covariance,...))
  n<-nrow(fits[[1]]$draws);pn<-dsdpm_all_parameter_names()
  ar<-array(NA_real_,c(n,n_chains,length(pn)),dimnames=list(NULL,NULL,pn))
  for(j in seq_len(n_chains)) ar[,j,]<-fits[[j]]$draws
  dr<-posterior::as_draws_array(ar)
  dg<-as.data.frame(posterior::summarise_draws(dr,"mean","sd","rhat","ess_bulk","ess_tail","mcse_mean"))
  names(dg)[names(dg)=="variable"]<-"parameter"
  dg$passed<-is.finite(dg$rhat)&dg$rhat<=rhat_limit&
    is.finite(dg$ess_bulk)&dg$ess_bulk>=ess_min&is.finite(dg$ess_tail)&dg$ess_tail>=ess_min
  pooled<-do.call(rbind,lapply(fits,function(x)x$draws))
  mn<-colMeans(pooled);qq<-t(apply(pooled,2,stats::quantile,c(.025,.5,.975),names=FALSE))
  colnames(qq)<-c("q025","q50","q975")
  out<-fits[[1]]
  out$main<-mn[dsdpm_main_parameter_names()]
  out$variance<-mn[dsdpm_variance_parameter_names()]
  out$intervals<-qq;out$draws<-pooled;out$posterior_sd<-apply(pooled,2,stats::sd)
  out$draws_internal_theta<-do.call(rbind,lapply(fits,function(x)x$draws_internal_theta))
  out$draws_theta_natural<-do.call(rbind,lapply(fits,function(x)x$draws_theta_natural))
  out$draws_mu<-do.call(rbind,lapply(fits,function(x)x$draws_mu))
  out$logpost<-unlist(lapply(fits,function(x)x$logpost),use.names=FALSE)
  out$draws_array<-ar
  out$acceptance_rate<-mean(vapply(fits,function(x)x$acceptance_rate,numeric(1)))
  out$proposal_scale_final<-vapply(fits,function(x)x$proposal_scale_final,numeric(1))
  out$diagnostics<-dg;out$diagnostics_validated<-all(dg$passed)
  out$convergence<-if(all(dg$passed)) 0L else 1L
  out$message<-if(all(dg$passed)) "" else "Multichain R-hat/ESS criteria not met"
  out$acceptance_postburn<-mean(vapply(fits,function(x)x$acceptance_postburn,numeric(1)))
  out$elapsed_seconds<-proc.time()[3]-tm
  out$mode_initialization<-mode;out$n_chains<-n_chains;out$seeds<-seeds
  out$diagnostic_thresholds<-list(rhat_limit=rhat_limit,ess_min=ess_min)
  out$structure_at_posterior_mean<-dsdpm_check_structure(sim$W,as.list(mn),prior$stability_tol)
  out
}

dsdpm_mcmc_multichain <- function(sim, ...) {
  fit_dsdpm_mcmc_multichain_full(sim, ...)$diagnostics
}

# <<< END R/07_full_mcmc.R <<<

# >>> BEGIN R/09_simulation_design.R >>>
# ======================================================================
# 09_simulation_design.R
#
# Main manuscript-aligned design:
#   N x T in {49,100} x {10,20}
#   delta,tau,eta in {0,0.2,0.4}
#   lambda=0.3, rho=0.5, kappa=0.6
#   sigma_xi^2=0.5, sigma_epsilon^2=1, sigma_zeta^2=0.5
#
# IMPORTANT reviewer correction:
# The Cartesian grid is filtered by the JOINT spectral-radius conditions.
# A value <=0.4 is not automatically admissible. The main design does not
# include 0.6. Optional near-boundary stable scenarios are generated
# separately as a stress-test supplement.
# ======================================================================

dsdpm_make_paper_factorial_design <- function(
    N_values = c(49, 100),
    T_values = c(10, 20),
    dependence_levels = c(0, 0.2, 0.4),
    lambda = 0.3,
    rho = 0.5,
    kappa = 0.6,
    stability_tol = 0.999,
    filter_admissible = TRUE
) {
  rows <- list()
  ii <- 1L

  for (N in N_values) {
    Wobj <- dsdpm_make_queen_W(N)
    W <- Wobj$W

    for (Tt in T_values) {
      grid <- expand.grid(
        delta = dependence_levels,
        tau = dependence_levels,
        eta = dependence_levels,
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      )

      for (gg in seq_len(nrow(grid))) {
        par <- dsdpm_true_parameters(
          delta = grid$delta[gg],
          tau = grid$tau[gg],
          eta = grid$eta[gg],
          lambda = lambda,
          rho = rho,
          kappa = kappa
        )

        st <- dsdpm_check_structure(W, par, tol = stability_tol)

        rows[[ii]] <- data.frame(
          design_family = "paper_factorial",
          N = N,
          Tt = Tt,
          delta = par$delta,
          tau = par$tau,
          eta = par$eta,
          lambda = par$lambda,
          rho = par$rho,
          kappa = par$kappa,
          sigma2_epsilon = par$sigma2_epsilon,
          sigma2_xi = par$sigma2_xi,
          sigma2_zeta = par$sigma2_zeta,
          outcome_spectral_radius = st$outcome_spectral_radius,
          disturbance_spectral_radius = st$disturbance_spectral_radius,
          nonsingular_delta = st$nonsingular_delta,
          nonsingular_lambda = st$nonsingular_lambda,
          nonsingular_kappa = st$nonsingular_kappa,
          admissible = st$admissible,
          stringsAsFactors = FALSE
        )
        ii <- ii + 1L
      }
    }
  }

  ans <- do.call(rbind, rows)
  ans$scenario <- seq_len(nrow(ans))
  ans$design <- sprintf(
    "d%.1f_t%.1f_e%.1f",
    ans$delta, ans$tau, ans$eta
  )

  ans <- ans[, c("scenario", "design", setdiff(names(ans), c("scenario", "design")))]

  if (filter_admissible) {
    ans <- ans[ans$admissible, , drop = FALSE]
    rownames(ans) <- NULL
    ans$scenario <- seq_len(nrow(ans))
  }

  ans
}

dsdpm_find_near_outcome_boundary <- function(
    W,
    target = 0.95,
    lambda = 0.3,
    rho = 0.5,
    kappa = 0.6,
    stability_tol = 0.999
) {
  # Equal-strength d=tau=eta path gives an interpretable one-dimensional
  # stress sequence and avoids an unnecessary 3-D brute-force search.
  vals <- seq(0.01, 0.60, by = 0.005)
  candidates <- lapply(vals, function(v) {
    p <- dsdpm_true_parameters(
      delta = v, tau = v, eta = v,
      lambda = lambda, rho = rho, kappa = kappa
    )
    st <- dsdpm_check_structure_eigen(W, p, tol = stability_tol)
    if (!isTRUE(st$admissible[1])) return(NULL)
    c(delta = v, tau = v, eta = v, spr = st$outcome_spectral_radius)
  })
  candidates <- Filter(Negate(is.null), candidates)
  if (!length(candidates)) stop("No stable outcome-boundary candidate found.")
  C <- do.call(rbind, candidates)
  C[which.min(abs(C[, "spr"] - target)), ]
}

dsdpm_find_near_disturbance_boundary <- function(
    W,
    target = 0.92,
    lambda_grid = seq(0.05, 0.75, by = 0.01),
    rho_grid = seq(0.05, 0.90, by = 0.01),
    delta = 0.2,
    tau = 0.2,
    eta = 0.2,
    kappa = 0.6,
    stability_tol = 0.999
) {
  candidates <- list(); ii <- 1L
  ev <- Re(eigen(as.matrix(W),only.values=TRUE)$values)
  for (lam in lambda_grid) {
    for (rr in rho_grid) {
      p <- dsdpm_true_parameters(
        delta = delta, tau = tau, eta = eta,
        lambda = lam, rho = rr, kappa = kappa
      )
      spr <- max(abs(rr/(1-lam*ev)))
      st <- list(admissible=is.finite(spr) && spr < stability_tol, disturbance_spectral_radius=spr)
      if (isTRUE(st$admissible[1])) {
        candidates[[ii]] <- c(lambda = lam, rho = rr, spr = st$disturbance_spectral_radius)
        ii <- ii + 1L
      }
    }
  }
  if (!length(candidates)) stop("No stable disturbance-boundary candidate found.")
  C <- do.call(rbind, candidates)
  C[which.min(abs(C[, "spr"] - target)), ]
}

dsdpm_make_targeted_pilot_design <- function(
    N = 49,
    Tt = 10,
    stability_tol = 0.999
) {
  W <- dsdpm_make_queen_W(N)$W

  s6 <- dsdpm_find_near_outcome_boundary(
    W,
    target = 0.94,
    stability_tol = stability_tol
  )

  s7 <- dsdpm_find_near_disturbance_boundary(
    W,
    target = 0.91,
    stability_tol = stability_tol
  )

  raw <- data.frame(
    design = c(
      "S0_baseline",
      "S1_spatial",
      "S2_temporal",
      "S3_spatiotemporal",
      "S4_weak_combined",
      "S5_moderate_combined",
      "S6_near_outcome_boundary",
      "S7_near_disturbance_boundary"
    ),
    delta = c(0, 0.4, 0, 0, 0.2, 0.4, s6["delta"], 0.2),
    tau = c(0, 0, 0.4, 0, 0.2, 0.2, s6["tau"], 0.2),
    eta = c(0, 0, 0, 0.4, 0.2, 0.2, s6["eta"], 0.2),
    lambda = c(rep(0.3, 7), s7["lambda"]),
    rho = c(rep(0.5, 7), s7["rho"]),
    kappa = rep(0.6, 8),
    stringsAsFactors = FALSE
  )

  rows <- lapply(seq_len(nrow(raw)), function(i) {
    p <- dsdpm_true_parameters(
      delta = raw$delta[i],
      tau = raw$tau[i],
      eta = raw$eta[i],
      lambda = raw$lambda[i],
      rho = raw$rho[i],
      kappa = raw$kappa[i]
    )
    st <- dsdpm_check_structure(W, p, tol = stability_tol)

    cbind(
      data.frame(
        scenario = i,
        design = raw$design[i],
        N = N,
        Tt = Tt,
        delta = p$delta,
        tau = p$tau,
        eta = p$eta,
        lambda = p$lambda,
        rho = p$rho,
        kappa = p$kappa,
        sigma2_epsilon = p$sigma2_epsilon,
        sigma2_xi = p$sigma2_xi,
        sigma2_zeta = p$sigma2_zeta,
        stringsAsFactors = FALSE
      ),
      st
    )
  })

  do.call(rbind, rows)
}

dsdpm_scenario_to_par <- function(row) {
  dsdpm_true_parameters(
    delta = as.numeric(row$delta),
    tau = as.numeric(row$tau),
    eta = as.numeric(row$eta),
    lambda = as.numeric(row$lambda),
    rho = as.numeric(row$rho),
    kappa = as.numeric(row$kappa),
    sigma2_epsilon = as.numeric(row$sigma2_epsilon),
    sigma2_xi = as.numeric(row$sigma2_xi),
    sigma2_zeta = as.numeric(row$sigma2_zeta)
  )
}

# <<< END R/09_simulation_design.R <<<

# >>> BEGIN R/10_monte_carlo.R >>>
# ======================================================================
# 10_monte_carlo.R
# Unified fitting, strict status, checkpointed Monte Carlo.
# ======================================================================

dsdpm_extract_point_estimates <- function(fit) {
  if (is.null(fit$main) || is.null(fit$variance)) {
    stop("Estimator did not return main and variance components.")
  }
  c(
    fit$main[dsdpm_main_parameter_names()],
    fit$variance[dsdpm_variance_parameter_names()]
  )[dsdpm_all_parameter_names()]
}

dsdpm_extract_intervals <- function(fit) {
  if (is.null(fit$intervals)) {
    return(NULL)
  }
  M <- as.matrix(fit$intervals)
  if (!all(dsdpm_all_parameter_names() %in% rownames(M))) {
    return(NULL)
  }
  M[dsdpm_all_parameter_names(), , drop = FALSE]
}

dsdpm_strict_fit_status <- function(fit, sim,
                                    stability_tol = 0.999) {
  if (inherits(fit, "try-error") || is.null(fit)) {
    return(list(
      finite_ok = FALSE,
      convergence_ok = FALSE,
      structure_ok = FALSE,
      ok_strict = FALSE,
      message = as.character(fit)
    ))
  }

  est <- try(dsdpm_extract_point_estimates(fit), silent = TRUE)
  finite_ok <- !inherits(est, "try-error") && all(is.finite(est))

  conv <- fit$convergence %||% NA_integer_
  convergence_ok <- isTRUE(conv == 0L)

  structure_point_ok <- FALSE
  st <- NULL

  if (finite_ok) {
    st <- try(
      dsdpm_check_structure(
        sim$W,
        list(
          delta = est["delta"],
          tau = est["tau"],
          eta = est["eta"],
          lambda = est["lambda"],
          rho = est["rho"],
          kappa = est["kappa"]
        ),
        tol = stability_tol
      ),
      silent = TRUE
    )
    structure_point_ok <- (
      !inherits(st, "try-error") &&
      isTRUE(st$admissible[1])
    )
  }

  support_restricted <- isTRUE(fit$posterior_support_restricted)
  structure_ok <- structure_point_ok || support_restricted
  ok <- finite_ok && convergence_ok && structure_ok

  list(
    finite_ok = finite_ok,
    convergence_ok = convergence_ok,
    structure_point_ok = structure_point_ok,
    posterior_support_restricted = support_restricted,
    structure_ok = structure_ok,
    ok_strict = ok,
    structure = st,
    message = fit$message %||% ""
  )
}

dsdpm_fit_one_method <- function(method, sim,
                                 qmle_args = list(),
                                 gmm_args = list(),
                                 inla_args = list(),
                                 mcmc_args = list()) {
  method <- match.arg(
    method,
    c("QMLE", "GMM", "INLA", "MCMC")
  )

  tm0 <- proc.time()[3]

  fit <- try(
    switch(
      method,
      QMLE = do.call(
        fit_dsdpm_qmle_full,
        c(list(sim = sim), qmle_args)
      ),
      GMM = do.call(
        fit_dsdpm_gmm_full,
        c(list(sim = sim), gmm_args)
      ),
      INLA = do.call(
        fit_dsdpm_inla_full,
        c(list(sim = sim), inla_args)
      ),
      MCMC = do.call(
        fit_dsdpm_mcmc_multichain_full,
        c(list(sim = sim), mcmc_args)
      )
    ),
    silent = TRUE
  )

  elapsed <- proc.time()[3] - tm0

  list(
    method = method,
    fit = fit,
    elapsed_seconds = elapsed
  )
}

dsdpm_long_result <- function(fit_wrapper, sim, scenario_row,
                              replication,
                              stability_tol = 0.999) {
  method <- fit_wrapper$method
  fit <- fit_wrapper$fit
  status <- dsdpm_strict_fit_status(
    fit, sim, stability_tol = stability_tol
  )

  truth <- dsdpm_truth_vector(sim$true)

  if (inherits(fit, "try-error") || is.null(fit)) {
    est <- rep(NA_real_, length(truth))
    names(est) <- names(truth)
    intervals <- NULL
  } else {
    est <- try(dsdpm_extract_point_estimates(fit), silent = TRUE)
    if (inherits(est, "try-error")) {
      est <- rep(NA_real_, length(truth))
      names(est) <- names(truth)
    }
    intervals <- dsdpm_extract_intervals(fit)
  }

  q025 <- q975 <- rep(NA_real_, length(truth))
  names(q025) <- names(q975) <- names(truth)

  if (!is.null(intervals)) {
    q025[rownames(intervals)] <- intervals[, "q025"]
    q975[rownames(intervals)] <- intervals[, "q975"]
  }

  data.frame(
    scenario = scenario_row$scenario,
    design = scenario_row$design,
    N = scenario_row$N,
    Tt = scenario_row$Tt,
    replication = replication,
    method = method,
    parameter = names(truth),
    truth = as.numeric(truth),
    estimate = as.numeric(est[names(truth)]),
    error = as.numeric(est[names(truth)] - truth),
    q025 = as.numeric(q025[names(truth)]),
    q975 = as.numeric(q975[names(truth)]),
    interval_available = is.finite(q025[names(truth)]) &
      is.finite(q975[names(truth)]),
    finite_ok = status$finite_ok,
    convergence_ok = status$convergence_ok,
    structure_ok = status$structure_ok,
    ok_strict = status$ok_strict,
    elapsed_seconds = fit_wrapper$elapsed_seconds,
    gmm_numerical_boundary_step1 = if(!inherits(fit,"try-error")) fit$numerical_boundary_step1 %||% NA else NA,
    gmm_numerical_boundary_step2 = if(!inherits(fit,"try-error")) fit$numerical_boundary_step2 %||% NA else NA,
    message = status$message,
    stringsAsFactors = FALSE
  )
}

dsdpm_status_result <- function(fit_wrapper, sim, scenario_row,
                                replication,
                                stability_tol = 0.999) {
  fit <- fit_wrapper$fit
  method <- fit_wrapper$method
  status <- dsdpm_strict_fit_status(
    fit, sim, stability_tol = stability_tol
  )

  spr_y <- spr_v <- NA_real_
  if (!is.null(status$structure) &&
      !inherits(status$structure, "try-error")) {
    spr_y <- status$structure$outcome_spectral_radius
    spr_v <- status$structure$disturbance_spectral_radius
  }

  acceptance <- min_ess <- NA_real_
  if (!inherits(fit, "try-error") && !is.null(fit)) {
    acceptance <- fit$acceptance_postburn %||% fit$acceptance_rate %||% NA_real_

    if(!is.null(fit$diagnostics)) {
      min_ess<-min(fit$diagnostics$ess_bulk,na.rm=TRUE)
    } else if (!is.null(fit$draws) && requireNamespace("coda",quietly=TRUE)) {
      ee<-try(coda::effectiveSize(coda::mcmc(fit$draws)),silent=TRUE)
      if(!inherits(ee,"try-error")) min_ess<-min(ee,na.rm=TRUE)
    }
  }

  data.frame(
    scenario = scenario_row$scenario,
    design = scenario_row$design,
    N = scenario_row$N,
    Tt = scenario_row$Tt,
    replication = replication,
    method = method,
    elapsed_seconds = fit_wrapper$elapsed_seconds,
    finite_ok = status$finite_ok,
    convergence_ok = status$convergence_ok,
    structure_point_ok = status$structure_point_ok %||% FALSE,
    posterior_support_restricted = status$posterior_support_restricted %||% FALSE,
    structure_ok = status$structure_ok,
    outcome_spectral_radius_est = spr_y,
    disturbance_spectral_radius_est = spr_v,
    acceptance_rate = acceptance,
    min_ESS = min_ess,
    ok_strict = status$ok_strict,
    diagnostic_max_Rhat = if(!inherits(fit,"try-error") && !is.null(fit$diagnostics)) max(fit$diagnostics$rhat) else NA_real_,
    diagnostic_min_bulk_ESS = if(!inherits(fit,"try-error") && !is.null(fit$diagnostics)) min(fit$diagnostics$ess_bulk) else NA_real_,
    gmm_weight_covariance_rank = if(!inherits(fit,"try-error")) fit$weight_covariance_rank %||% NA_real_ else NA_real_,
    gmm_weight_condition = if(!inherits(fit,"try-error")) fit$weight_matrix_condition_number %||% NA_real_ else NA_real_,
    gmm_moment_jacobian_rank = if(!inherits(fit,"try-error") && !is.null(fit$moment_jacobian_diagnostics)) fit$moment_jacobian_diagnostics$rank else NA_real_,
    gmm_numerical_boundary_step1 = if(!inherits(fit,"try-error")) fit$numerical_boundary_step1 %||% NA else NA,
    gmm_numerical_boundary_step2 = if(!inherits(fit,"try-error")) fit$numerical_boundary_step2 %||% NA else NA,
    message = status$message,
    stringsAsFactors = FALSE
  )
}

dsdpm_config_hash <- function(x) {
  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf), add = TRUE)
  saveRDS(x, tf, version = 3)
  unname(tools::md5sum(tf))
}

dsdpm_run_one_job <- function(
    scenario_row,
    replication,
    methods = c("QMLE", "GMM", "INLA"),
    base_seed = 12345,
    qmle_args = list(),
    gmm_args = list(),
    inla_args = list(),
    mcmc_args = list(),
    validate_dgp = TRUE,
    stability_tol = 0.999
) {
  N <- as.integer(scenario_row$N)
  Tt <- as.integer(scenario_row$Tt)
  W <- dsdpm_make_queen_W(N)$W
  par <- dsdpm_scenario_to_par(scenario_row)

  st <- dsdpm_check_structure(W, par, tol = stability_tol)
  if (!isTRUE(st$admissible[1])) {
    stop("Monte Carlo job requested an inadmissible true scenario.")
  }

  seed <- as.integer(
    base_seed +
      100000L * as.integer(scenario_row$scenario) +
      replication
  )

  sim <- dsdpm_simulate(
    N = N,
    Tt = Tt,
    W = W,
    par = par,
    seed = seed,
    keep_innovations = validate_dgp
  )

  dgp_check <- if (validate_dgp) dsdpm_validate_dgp(sim) else NULL

  fit_wrappers <- lapply(methods, function(meth) {
    mcmc_local <- mcmc_args
    if (meth == "MCMC" && is.null(mcmc_local$seeds)) {
      mcmc_local$seeds <- seed + 700000L + seq_len(mcmc_local$n_chains %||% 4L)
    }

    dsdpm_fit_one_method(
      method = meth,
      sim = sim,
      qmle_args = qmle_args,
      gmm_args = gmm_args,
      inla_args = inla_args,
      mcmc_args = mcmc_local
    )
  })

  estimates <- do.call(
    rbind,
    lapply(fit_wrappers, function(fw) {
      dsdpm_long_result(
        fw, sim, scenario_row, replication, stability_tol
      )
    })
  )

  status <- do.call(
    rbind,
    lapply(fit_wrappers, function(fw) {
      dsdpm_status_result(
        fw, sim, scenario_row, replication, stability_tol
      )
    })
  )

  list(
    estimates = estimates,
    status = status,
    dgp_check = dgp_check,
    scenario = scenario_row,
    replication = replication
  )
}

dsdpm_run_monte_carlo <- function(
    scenarios,
    R = 500,
    methods = c("QMLE", "GMM", "INLA"),
    output_dir = "outputs/monte_carlo",
    resume = TRUE,
    base_seed = 12345,
    qmle_args = list(),
    gmm_args = list(),
    inla_args = list(),
    mcmc_args = list(),
    validate_dgp_first_rep_only = TRUE,
    stability_tol = 0.999,
    verbose = TRUE
) {
  dsdpm_safe_dir(output_dir)
  jobs_dir <- dsdpm_safe_dir(file.path(output_dir, "jobs"))

  scenarios <- scenarios[scenarios$admissible, , drop = FALSE]
  if (!nrow(scenarios)) stop("No admissible scenarios to run.")

  run_config <- list(
    version = "v3.4",
    fingerprint = dsdpm_validation_signature(),
    effective_prior = dsdpm_default_prior_spec(),
    scenarios = scenarios,
    R = R,
    methods = methods,
    base_seed = base_seed,
    qmle_args = qmle_args,
    gmm_args = gmm_args,
    inla_args = inla_args,
    mcmc_args = mcmc_args,
    stability_tol = stability_tol
  )
  config_hash <- dsdpm_config_hash(run_config)
  hash_file <- file.path(output_dir, "CONFIG_HASH.txt")
  if(resume && file.exists(hash_file) && !identical(readLines(hash_file),config_hash))
    stop("Output directory belongs to a different configuration/code/environment")
  writeLines(config_hash, hash_file)

  all_est <- list()
  all_status <- list()
  all_dgp <- list()
  kk <- 1L

  total <- nrow(scenarios) * R
  counter <- 0L

  for (i in seq_len(nrow(scenarios))) {
    sc <- scenarios[i, , drop = FALSE]

    for (r in seq_len(R)) {
      counter <- counter + 1L
      fjob <- file.path(
        jobs_dir,
        sprintf("scenario_%04d_rep_%04d.rds", sc$scenario, r)
      )

      if (resume && file.exists(fjob)) {
        ans <- readRDS(fjob)
        if (is.null(ans$config_hash) || !identical(ans$config_hash, config_hash)) {
          stop(
            "Checkpoint configuration mismatch for ", fjob,
            ". Remove the old jobs directory or run with a new output_dir."
          )
        }
      } else {
        if (verbose) {
          cat(sprintf(
            "Job %d/%d | %s | N=%d T=%d | rep=%d\n",
            counter, total, sc$design, sc$N, sc$Tt, r
          ))
        }

        ans <- dsdpm_run_one_job(
          scenario_row = sc,
          replication = r,
          methods = methods,
          base_seed = base_seed,
          qmle_args = qmle_args,
          gmm_args = gmm_args,
          inla_args = inla_args,
          mcmc_args = mcmc_args,
          validate_dgp = (
            !validate_dgp_first_rep_only || r == 1L
          ),
          stability_tol = stability_tol
        )
        ans$config_hash <- config_hash

        saveRDS(ans, fjob)
      }

      all_est[[kk]] <- ans$estimates
      all_status[[kk]] <- ans$status

      if (!is.null(ans$dgp_check)) {
        dg <- cbind(
          sc[, c("scenario", "design", "N", "Tt"), drop = FALSE],
          replication = r,
          ans$dgp_check
        )
        all_dgp[[length(all_dgp) + 1L]] <- dg
      }

      kk <- kk + 1L
    }
  }

  estimates <- do.call(rbind, all_est)
  status <- do.call(rbind, all_status)
  dgp <- if (length(all_dgp)) do.call(rbind, all_dgp) else NULL

  utils::write.csv(
    estimates,
    file.path(output_dir, "estimates_long.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    status,
    file.path(output_dir, "fit_status.csv"),
    row.names = FALSE
  )
  if (!is.null(dgp)) {
    utils::write.csv(
      dgp,
      file.path(output_dir, "dgp_validation.csv"),
      row.names = FALSE
    )
  }

  list(
    estimates = estimates,
    status = status,
    dgp_validation = dgp,
    config_hash = config_hash
  )
}

# Hash actual loaded function bodies/defaults, not an editable version label.
dsdpm_validation_signature <- function() {
  env<-environment(dsdpm_validation_signature)
  nms<-sort(ls(env,all.names=TRUE))
  nms<-nms[grepl("^(dsdpm_|fit_dsdpm_)",nms)|nms=="%||%"]
  nms<-nms[vapply(nms,function(nm)is.function(get(nm,env)),logical(1))]
  code<-setNames(lapply(nms,function(nm) {
    fn<-get(nm,env);list(formals=deparse(formals(fn)),body=deparse(body(fn)))
  }),nms)
  pkgs<-dsdpm_required_packages()
  versions<-setNames(vapply(pkgs,function(nm) {
    if(requireNamespace(nm,quietly=TRUE)) as.character(utils::packageVersion(nm)) else "unavailable"
  },character(1)),pkgs)
  list(code_hash=dsdpm_config_hash(code),prior=dsdpm_default_prior_spec(),
       R=R.version.string,packages=versions)
}

dsdpm_write_certificate <- function(stage,details,file) {
  if(!isTRUE(details$passed)) stop("Cannot certify a failed validation")
  saveRDS(list(stage=stage,signature=dsdpm_validation_signature(),
    timestamp=dsdpm_timestamp(),details=details),file)
}

dsdpm_require_certificate <- function(file,stage) {
  if(!file.exists(file)) stop("Required validation missing: ",stage,". Run the documented validation first.")
  x<-readRDS(file)
  if(!identical(x$stage,stage)||!isTRUE(x$details$passed)||
     !identical(x$signature,dsdpm_validation_signature()))
    stop("Validation certificate is failed/stale: ",stage)
  invisible(x)
}

# <<< END R/10_monte_carlo.R <<<

# >>> BEGIN R/11_metrics_plots.R >>>
# ======================================================================
# 11_metrics_plots.R
# Reviewer 3 requested metrics and signed estimation-error plots.
# ======================================================================

dsdpm_summarize_metrics <- function(estimates, status = NULL, subset = c("all_finite","strict_success")) {
  subset <- match.arg(subset)
  if (!nrow(estimates)) return(data.frame())

  key <- interaction(
    estimates$scenario,
    estimates$design,
    estimates$N,
    estimates$Tt,
    estimates$method,
    estimates$parameter,
    drop = TRUE
  )

  split_est <- split(estimates, key)

  rows <- lapply(split_est, function(d) {
    ok <- is.finite(d$estimate)
    if(subset=="strict_success") ok <- ok & !is.na(d$ok_strict) & d$ok_strict
    dd <- d[ok, , drop = FALSE]

    if (!nrow(dd)) {
      return(data.frame(
        scenario = d$scenario[1],
        design = d$design[1],
        N = d$N[1],
        Tt = d$Tt[1],
        method = d$method[1],
        parameter = d$parameter[1],
        Truth = d$truth[1],
        Mean = NA_real_,
        Median = NA_real_,
        Bias = NA_real_,
        SD = NA_real_,
        RMSE = NA_real_,
        MAE = NA_real_,
        MedianError = NA_real_,
        MAD = NA_real_,
        Coverage95 = NA_real_,
        MeanIntervalLength = NA_real_,
        MCSE_Bias = NA_real_,
        n_estimates = 0L,
        n_attempted = nrow(d),
        n_intervals = 0L,
        SummaryPopulation = subset,
        stringsAsFactors = FALSE
      ))
    }

    err <- dd$estimate - dd$truth
    mederr <- stats::median(err)

    interval_ok <- dd$interval_available &
      is.finite(dd$q025) &
      is.finite(dd$q975) & dd$q025 <= dd$q975

    coverage <- if (any(interval_ok)) {
      mean(
        dd$truth[interval_ok] >= dd$q025[interval_ok] &
        dd$truth[interval_ok] <= dd$q975[interval_ok]
      )
    } else {
      NA_real_
    }

    ilen <- if (any(interval_ok)) {
      mean(dd$q975[interval_ok] - dd$q025[interval_ok])
    } else {
      NA_real_
    }

    data.frame(
      scenario = dd$scenario[1],
      design = dd$design[1],
      N = dd$N[1],
      Tt = dd$Tt[1],
      method = dd$method[1],
      parameter = dd$parameter[1],
      Truth = dd$truth[1],
      Mean = mean(dd$estimate),
      Median = stats::median(dd$estimate),
      Bias = mean(err),
      SD = if (length(err) > 1L) stats::sd(dd$estimate) else NA_real_,
      RMSE = sqrt(mean(err^2)),
      MAE = mean(abs(err)),
      MedianError = mederr,
      MAD = stats::median(abs(err - mederr)),
      Coverage95 = coverage,
      MeanIntervalLength = ilen,
      MCSE_Bias = if (length(err) > 1L) stats::sd(err) / sqrt(length(err)) else NA_real_,
      n_estimates = length(err),
      n_attempted = nrow(d),
      n_intervals = sum(interval_ok),
      SummaryPopulation = subset,
      stringsAsFactors = FALSE
    )
  })

  met <- do.call(rbind, rows)
  rownames(met) <- NULL

  if (!is.null(status) && nrow(status)) {
    skey <- interaction(
      status$scenario,
      status$design,
      status$N,
      status$Tt,
      status$method,
      drop = TRUE
    )

    ss <- split(status, skey)

    srows <- lapply(ss, function(d) {
      data.frame(
        scenario = d$scenario[1],
        design = d$design[1],
        N = d$N[1],
        Tt = d$Tt[1],
        method = d$method[1],
        ConvergenceRate = mean(d$ok_strict, na.rm = TRUE),
        MeanRuntimeSeconds = mean(d$elapsed_seconds, na.rm = TRUE),
        MedianRuntimeSeconds = stats::median(d$elapsed_seconds, na.rm = TRUE),
        MeanAcceptance = if (all(is.na(d$acceptance_rate))) NA_real_ else mean(d$acceptance_rate, na.rm = TRUE),
        MedianMinESS = if (all(is.na(d$min_ESS))) NA_real_ else stats::median(d$min_ESS, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    })

    sm <- do.call(rbind, srows)

    met <- merge(
      met,
      sm,
      by = c("scenario", "design", "N", "Tt", "method"),
      all.x = TRUE,
      sort = FALSE
    )
  }

  met
}

dsdpm_inla_mcmc_agreement <- function(metrics) {
  if (!all(c("INLA", "MCMC") %in% unique(metrics$method))) return(data.frame())
  ii <- metrics[metrics$method == "INLA", , drop = FALSE]
  mm <- metrics[metrics$method == "MCMC", , drop = FALSE]
  keys <- c("scenario", "design", "N", "Tt", "parameter", "SummaryPopulation")
  out <- merge(ii, mm, by = keys, suffixes = c("_INLA", "_MCMC"), all = FALSE)
  if (!nrow(out)) return(out)
  out$AbsoluteMeanDifference <- abs(out$Mean_INLA - out$Mean_MCMC)
  out$AbsoluteBiasDifference <- abs(out$Bias_INLA - out$Bias_MCMC)
  out$SDRatio_INLA_to_MCMC <- out$SD_INLA / out$SD_MCMC
  out$RMSEDifference_INLA_minus_MCMC <- out$RMSE_INLA - out$RMSE_MCMC
  out$RuntimeRatio_INLA_to_MCMC <- out$MeanRuntimeSeconds_INLA / out$MeanRuntimeSeconds_MCMC
  out
}

dsdpm_plot_signed_errors <- function(estimates,
                                     output_dir,
                                     strict_only = FALSE) {
  dsdpm_require(include_plot = TRUE)
  dsdpm_safe_dir(output_dir)

  dat <- estimates[is.finite(estimates$error), , drop = FALSE]
  if (strict_only) dat <- dat[dat$ok_strict, , drop = FALSE]

  files <- character(0)

  design_pages <- split(unique(dat$design), ceiling(seq_along(unique(dat$design))/4))
  for (par in unique(dat$parameter)) for(page in seq_along(design_pages)) {
    d <- dat[dat$parameter == par & dat$design %in% design_pages[[page]], , drop = FALSE]
    d$NT <- paste0("N=", d$N, ", T=", d$Tt)

    p <- ggplot2::ggplot(
      d,
      ggplot2::aes(x = method, y = error)
    ) +
      ggplot2::geom_hline(
        yintercept = 0,
        linetype = 2,
        linewidth = 0.4
      ) +
      ggplot2::geom_boxplot(
        outlier.alpha = 0.35,
        width = 0.72
      ) +
      ggplot2::facet_grid(NT ~ design, scales = "free_y") +
      ggplot2::labs(
        title = paste0(
          "Signed estimation error for ", par
        ),
        subtitle = "Error = estimate - true value; horizontal line = zero error",
        x = "Estimator",
        y = paste0("Estimation error for ", par)
      ) +
      ggplot2::theme_bw(base_size = 9) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
        strip.text.x = ggplot2::element_text(size = 7),
        strip.text.y = ggplot2::element_text(size = 8)
      )

    fn <- file.path(
      output_dir,
      paste0("signed_error_", gsub("[^A-Za-z0-9_]+", "_", par), "_page_",page,".png")
    )

    ggplot2::ggsave(
      fn,
      p,
      width = 16,
      height = 8.5,
      dpi = 180
    )
    files <- c(files, fn)
  }

  invisible(files)
}

dsdpm_write_metric_outputs <- function(mc, output_dir) {
  dsdpm_safe_dir(output_dir)

  metrics <- rbind(dsdpm_summarize_metrics(mc$estimates,mc$status,"all_finite"),
                   dsdpm_summarize_metrics(mc$estimates,mc$status,"strict_success"))

  agree <- dsdpm_inla_mcmc_agreement(metrics)

  utils::write.csv(
    metrics,
    file.path(output_dir, "monte_carlo_metrics_reviewer.csv"),
    row.names = FALSE
  )

  if (nrow(agree)) {
    utils::write.csv(
      agree,
      file.path(output_dir, "INLA_vs_MCMC_agreement.csv"),
      row.names = FALSE
    )
  }

  dsdpm_plot_signed_errors(
    mc$estimates,
    file.path(output_dir, "signed_error_figures"),
    strict_only = FALSE
  )

  list(metrics = metrics, agreement = agree)
}

# <<< END R/11_metrics_plots.R <<<

# >>> BEGIN R/12_reviewer_outputs.R >>>
# ======================================================================
# 12_reviewer_outputs.R
# Reviewer-facing diagnostics and output tables.
# ======================================================================

dsdpm_run_identification_by_scenario <- function(
    scenarios,
    seed = 8888,
    stability_tol = 0.999,
    verbose = TRUE
) {
  rows <- list()

  for (i in seq_len(nrow(scenarios))) {
    sc <- scenarios[i, , drop = FALSE]

    if (verbose) {
      cat(sprintf(
        "Identification precheck %d/%d: %s, N=%d, T=%d\n",
        i, nrow(scenarios), sc$design, sc$N, sc$Tt
      ))
    }

    W <- dsdpm_make_queen_W(sc$N)$W
    par <- dsdpm_scenario_to_par(sc)

    sim <- dsdpm_simulate(
      N = sc$N,
      Tt = sc$Tt,
      W = W,
      par = par,
      seed = seed + i,
      keep_innovations = FALSE
    )

    diag <- dsdpm_identification_precheck(
      sim,
      stability_tol = stability_tol
    )

    rows[[i]] <- cbind(
      sc[, c(
        "scenario", "design", "N", "Tt",
        "delta", "tau", "eta", "lambda", "rho", "kappa",
        "outcome_spectral_radius",
        "disturbance_spectral_radius",
        "admissible"
      ), drop = FALSE],
      diag
    )
  }

  do.call(rbind, rows)
}

dsdpm_reviewer_crosswalk_table <- function() {
  data.frame(
    reviewer = c(
      "R1", "R1", "R1", "R1",
      "R2", "R3", "R3", "R3", "R3", "R3", "R3"
    ),
    issue = c(
      "Model motivation / comparison to existing models",
      "Joint stability of outcome and disturbance",
      "INLA must be compared with full MCMC",
      "QML and GMM formulations must be explicit",
      "Latent Gaussian / state-space identity is a strength",
      "Common W may create identification problems",
      "M >= 500",
      "MedAE and RMedSE redundant; add mean/SD/RMSE/MAD",
      "Use signed estimation-error boxplots",
      "Drop inadmissible 0.6 and clarify parameter design",
      "Discuss all N x T dimensions"
    ),
    code_evidence = c(
      "Documentation / manuscript edits (not a coding issue)",
      "dsdpm_check_structure(); admissible scenario filtering; strict estimation",
      "FULL_INLA and FULL_MCMC on identical datasets and priors",
      "04_full_qmle.R and 05_full_gmm.R; identification diagnostics",
      "06_full_inla_rgeneric.R uses the exact collapsed x=(b,mu) rgeneric representation",
      "08_identification.R: design rank, QMLE Hessian/information, GMM Jacobian",
      "RUN_FINAL_R500.R sets R=500",
      "11_metrics_plots.R",
      "dsdpm_plot_signed_errors()",
      "09_simulation_design.R filters joint stable {0,.2,.4}; no 0.6 main case",
      "Main scenarios include N x T = {49,100} x {10,20}"
    ),
    stringsAsFactors = FALSE
  )
}

dsdpm_finalize_reviewer_outputs <- function(
    scenarios,
    mc,
    output_dir = "outputs/final",
    identification = NULL
) {
  dsdpm_safe_dir(output_dir)

  utils::write.csv(
    scenarios,
    file.path(output_dir, "scenario_stability_table.csv"),
    row.names = FALSE
  )

  if (!is.null(identification)) {
    utils::write.csv(
      identification,
      file.path(output_dir, "identification_diagnostics.csv"),
      row.names = FALSE
    )
  }

  # Reviewer-facing copies of the replication-level audit tables. The
  # Monte Carlo engine also writes estimates_long.csv/fit_status.csv inside
  # its own mc/ directory; these aliases make the finalized folder complete.
  utils::write.csv(
    mc$estimates,
    file.path(output_dir, "estimates_reviewer_long.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    mc$status,
    file.path(output_dir, "fit_status_reviewer.csv"),
    row.names = FALSE
  )

  mo <- dsdpm_write_metric_outputs(mc, output_dir)

  crosswalk <- dsdpm_reviewer_crosswalk_table()
  utils::write.csv(
    crosswalk,
    file.path(output_dir, "reviewer_crosswalk.csv"),
    row.names = FALSE
  )

  # Convergence/runtime table independent of parameter.
  s <- mc$status
  key <- interaction(
    s$scenario, s$design, s$N, s$Tt, s$method,
    drop = TRUE
  )
  ss <- split(s, key)
  rt <- do.call(rbind, lapply(ss, function(d) {
    data.frame(
      scenario = d$scenario[1],
      design = d$design[1],
      N = d$N[1],
      Tt = d$Tt[1],
      method = d$method[1],
      convergence_rate = mean(d$ok_strict, na.rm = TRUE),
      mean_runtime_seconds = mean(d$elapsed_seconds, na.rm = TRUE),
      median_runtime_seconds = stats::median(d$elapsed_seconds, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))

  utils::write.csv(
    rt,
    file.path(output_dir, "convergence_and_runtime_summary.csv"),
    row.names = FALSE
  )

  list(
    metrics = mo$metrics,
    agreement = mo$agreement,
    convergence_runtime = rt,
    crosswalk = crosswalk
  )
}

# <<< END R/12_reviewer_outputs.R <<<

# >>> BEGIN R/13_bayesian_validation.R >>>
# Matched posterior diagnostics; no substitution of runtime success for mixing.
dsdpm_bayesian_comparison <- function(sim, inla_fit, mcmc_fit, inla_seconds, mcmc_seconds) {
 pn<-dsdpm_all_parameter_names();truth<-dsdpm_truth_vector(sim$true)
 extract<-function(f) if(inherits(f,'try-error')) rep(NA_real_,14) else dsdpm_extract_point_estimates(f)
 sdval<-function(f) if(inherits(f,'try-error')) rep(NA_real_,14) else f$posterior_sd[pn]
 intval<-function(f) if(inherits(f,'try-error')) matrix(NA_real_,14,3) else f$intervals[pn,]
 a<-extract(inla_fit);b<-extract(mcmc_fit);sa<-sdval(inla_fit);sb<-sdval(mcmc_fit)
 ia<-intval(inla_fit);ib<-intval(mcmc_fit)
 si<-dsdpm_strict_fit_status(inla_fit,sim);sm<-dsdpm_strict_fit_status(mcmc_fit,sim)
 dg<-if(!inherits(mcmc_fit,'try-error')) mcmc_fit$diagnostics else NULL
 refdiag<-function(nm) if(is.null(dg)) rep(NA_real_,14) else dg[[nm]][match(pn,dg$parameter)]
 out<-data.frame(N=sim$N,Tt=sim$Tt,parameter=pn,truth=as.numeric(truth),
  INLA_mean=as.numeric(a),MCMC_mean=as.numeric(b),INLA_sd=as.numeric(sa),MCMC_sd=as.numeric(sb),
  INLA_q025=ia[,1],INLA_q975=ia[,3],MCMC_q025=ib[,1],MCMC_q975=ib[,3],
  INLA_coverage=truth>=ia[,1]&truth<=ia[,3],MCMC_coverage=truth>=ib[,1]&truth<=ib[,3],
  INLA_seconds=inla_seconds,MCMC_seconds=mcmc_seconds,
  INLA_strict=si$ok_strict,MCMC_strict=sm$ok_strict,
  Rhat=refdiag('rhat'),ESS_bulk=refdiag('ess_bulk'),ESS_tail=refdiag('ess_tail'),
  MCMC_mcse_mean=refdiag('mcse_mean'),
  INLA_message=paste(si$message,collapse=' '),MCMC_message=paste(sm$message,collapse=' '))
 out$absolute_mean_difference<-abs(out$INLA_mean-out$MCMC_mean)
 out$mean_difference_in_posterior_sd<-out$absolute_mean_difference/out$MCMC_sd
 out$posterior_sd_ratio<-out$INLA_sd/out$MCMC_sd
 out$interval_width_ratio<-(out$INLA_q975-out$INLA_q025)/(out$MCMC_q975-out$MCMC_q025)
 # Explicit numerical screen, not a theorem that approximation errors vanish.
 out$reference_valid<-out$MCMC_strict & is.finite(out$MCMC_mcse_mean)
 out$agreement_screen<-out$reference_valid & out$INLA_strict &
   out$mean_difference_in_posterior_sd<=.25 & out$posterior_sd_ratio>=.75 & out$posterior_sd_ratio<=1.33
 out
}

dsdpm_validation_scenarios <- function() {
 rows<-list();k<-1L
 for(nt in list(c(49,10),c(100,20))) {
  sc<-dsdpm_make_targeted_pilot_design(nt[1],nt[2])
  rows[[k]]<-sc[sc$design %in% c('S0_baseline','S5_moderate_combined','S6_near_outcome_boundary','S7_near_disturbance_boundary'),]
  k<-k+1L
 }
 out<-do.call(rbind,rows);out$scenario<-seq_len(nrow(out));rownames(out)<-NULL;out
}

# <<< END R/13_bayesian_validation.R <<<
