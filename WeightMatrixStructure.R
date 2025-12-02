# ==========================================================
# DSPM – QMLE, GMM, dan INLA (BYM2 + AR1) + post-hoc SAR-κ
# Versi Stabil: δ-bounds spektral, centering, R replikasi,
# keluaran lengkap (beta, tau, eta, rho, lambda, kappa, hypers)
# ==========================================================

suppressPackageStartupMessages({
  library(Matrix)
  library(MASS)
  library(spdep)
  library(dplyr)
  library(tidyr)
  library(INLA)
})

set.seed(123)

# ------------------------------------------------------------
# 1) Struktur parameter
# ------------------------------------------------------------
.make_true_par <- function(delta, tau, eta,
                           lambda = 0.3, rho = 0.5, kappa = 0.6,
                           beta = c(0.5, 0.5, 0.5, 0.5, 0.5),
                           sig2_eps = 1.0, sig2_zeta = 0.5) {
  list(
    delta = delta, tau = tau, eta = eta,
    lambda = lambda, rho = rho, kappa = kappa,
    beta = beta, sig2_eps = sig2_eps, sig2_zeta = sig2_zeta
  )
}

# ------------------------------------------------------------
# 2) Bound δ dari spektral radius W
# ------------------------------------------------------------
delta_bounds_from_W <- function(W_mat, margin = 0.05) {
  ev   <- eigen(as.matrix(W_mat), only.values = TRUE)$values
  rhoW <- max(Mod(ev))
  bound <- 0.9 / rhoW
  c(-bound + margin, bound - margin)
}

# ------------------------------------------------------------
# 3) Init spatial structure (Queen row-stand.) + subset nb + graph BYM2
# ------------------------------------------------------------
init_problem <- function(N_new, Tt_new){
  # grid mendekati persegi lalu subset 1..N_new
  nrow_grid <- floor(sqrt(N_new))
  ncol_grid <- ceiling(N_new / nrow_grid)
  nb_full <- spdep::cell2nb(nrow_grid, ncol_grid, type = "queen")
  keep <- seq_len(N_new)
  nb <- lapply(nb_full[keep], function(nei) intersect(nei, keep))
  class(nb) <- "nb"
  attr(nb, "region.id") <- as.character(keep)
  attr(nb, "type") <- "queen"
  attr(nb, "sym") <- TRUE
  
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  W  <- spdep::listw2mat(lw)
  W_mat <- Matrix::Matrix(W, sparse = TRUE)
  
  graph_file <- "spatial_graph.adj"
  spdep::nb2INLA(graph_file, nb)
  
  eigW <- eigen(as.matrix(W_mat), only.values = TRUE)$values
  
  assign("N", N_new, envir = .GlobalEnv)
  assign("Tt", Tt_new, envir = .GlobalEnv)
  assign("W_mat", W_mat, envir = .GlobalEnv)
  assign("W", W, envir = .GlobalEnv)
  assign("lw", lw, envir = .GlobalEnv)
  assign("spatial_graph_file", graph_file, envir = .GlobalEnv)
  assign("eigW", eigW, envir = .GlobalEnv)
}

# ------------------------------------------------------------
# 4) DGP: simulate_dspm2
# ------------------------------------------------------------
simulate_dspm2 <- function(N, Tt, W, par){
  I_N <- Diagonal(N)
  delta  <- par$delta; tau <- par$tau; eta <- par$eta
  lambda <- par$lambda; rho <- par$rho; kappa <- par$kappa
  beta   <- par$beta
  sig_eps  <- sqrt(par$sig2_eps)
  sig_zeta <- sqrt(par$sig2_zeta)
  
  Q_mu <- I_N - kappa * W_mat
  zeta <- as.numeric(MASS::mvrnorm(1, mu = rep(0,N), Sigma = (sig_zeta^2) * diag(N)))
  mu   <- as.numeric(solve(Q_mu, zeta))
  
  Y <- matrix(0, N, Tt)
  v <- matrix(0, N, Tt)
  Y_prev <- rep(0, N)
  v_prev <- rep(0, N)
  X_prev <- rep(0, N)
  
  X_store     <- matrix(0, N, Tt)
  WX_store    <- matrix(0, N, Tt)
  Xlag_store  <- matrix(0, N, Tt)
  WXlag_store <- matrix(0, N, Tt)
  
  for (t in 1:Tt) {
    X_t   <- as.numeric(rnorm(N))
    WX_t  <- as.numeric(W_mat %*% X_t)
    X_lag <- if (t>1) as.numeric(X_prev) else rep(0,N)
    WX_lag<- as.numeric(W_mat %*% X_lag)
    
    Z_t <- cbind(1, X_t, WX_t, X_lag, WX_lag)
    
    eps_t <- rnorm(N, 0, sig_eps)
    v_t   <- as.numeric(lambda * (W_mat %*% v_prev) + rho * v_prev + mu + eps_t)
    
    WY_t1 <- as.numeric(W_mat %*% Y_prev)
    R_t <- as.numeric(tau * Y_prev + eta * WY_t1 + (Z_t %*% beta) + v_t)
    Y_t <- as.numeric(solve(I_N - delta * W_mat, R_t))
    
    Y[,t] <- Y_t; v[,t] <- v_t
    X_store[,t]     <- X_t
    WX_store[,t]    <- WX_t
    Xlag_store[,t]  <- X_lag
    WXlag_store[,t] <- WX_lag
    
    Y_prev <- Y_t; v_prev <- v_t; X_prev <- X_t
  }
  
  list(Y=Y, v=v, W=W, par=par,
       X=X_store, WX=WX_store, Xlag=Xlag_store, WXlag=WXlag_store)
}

# ------------------------------------------------------------
# 5) Stack & Metrics helpers
# ------------------------------------------------------------
build_stack <- function(sim){
  Y <- sim$Y; N <- nrow(Y); Tt <- ncol(Y)
  Wy <- apply(Y, 2, function(col) as.numeric(W_mat %*% col))
  Y_lag  <- cbind(rep(0,N), Y[,1:(Tt-1), drop=FALSE])
  Wy_lag <- cbind(rep(0,N), Wy[,1:(Tt-1), drop=FALSE])
  
  Z_t <- array(0, dim=c(N,5,Tt))
  for(t in 1:Tt){
    Z_t[,,t] <- cbind(1, sim$X[,t], sim$WX[,t], sim$Xlag[,t], sim$WXlag[,t])
  }
  
  y_stack    <- as.numeric(Y)
  ylag_stack <- as.numeric(Y_lag)
  wyl_stack  <- as.numeric(Wy_lag)
  Z_stack    <- do.call(rbind, lapply(1:Tt, function(t) Z_t[,,t]))
  
  list(y=y_stack, ylag=ylag_stack, wyl=wyl_stack, Z=Z_stack,
       W=sim$W, N=N, Tt=Tt, sim=sim)
}

bias_vec <- function(x, true) { x <- as.numeric(x); mean(x - true, na.rm = TRUE) }
rmse_vec <- function(x, true) { x <- as.numeric(x); sqrt(mean((x - true)^2, na.rm = TRUE)) }
.clip01 <- function(x) pmin(1, pmax(0, x))

# ------------------------------------------------------------
# 6) Estimator κ SAR post-hoc dari residu panel (profile log-lik)
# ------------------------------------------------------------
.kappa_interval_from_W <- function(W_mat, margin = 1e-3) {
  ev <- eigen(as.matrix(W_mat), only.values = TRUE)$values
  list(lo = -1 + margin, hi = 1 - margin, ev = ev)
}

estimate_kappa_sar_panel <- function(W_mat, resid_mat, margin = 1e-3) {
  stopifnot(nrow(resid_mat) == nrow(W_mat))
  N <- nrow(W_mat); T_res <- ncol(resid_mat)
  v <- as.numeric(resid_mat)
  iv <- .kappa_interval_from_W(W_mat, margin)
  ev <- iv$ev
  logdet_Iminus_kW <- function(k) sum(log(abs(1 - k * ev)))
  obj <- function(k) {
    A  <- Diagonal(N) - k * W_mat
    Av <- as.numeric(kronecker(Diagonal(T_res), A) %*% v)
    ll <- T_res * logdet_Iminus_kW(k) - (length(v)/2)*log(mean(Av^2) + 1e-12)
    -ll
  }
  opt <- optimize(obj, interval = c(iv$lo, iv$hi))
  list(kappa = opt$minimum, obj = opt$objective, interval = c(iv$lo, iv$hi))
}

# ------------------------------------------------------------
# 7) Estimator lambda & rho(error) dari residu panel (OLS-2 reg)
# ------------------------------------------------------------
estimate_lambda_rho_from_resid <- function(W_mat, resid_mat) {
  N <- nrow(resid_mat); Tt <- ncol(resid_mat)
  v_t   <- as.numeric(resid_mat)
  vlag  <- cbind(rep(0, N), resid_mat[, 1:(Tt-1), drop = FALSE])
  v_1   <- as.numeric(vlag)
  Wv    <- apply(resid_mat, 2, function(col) as.numeric(W_mat %*% col))
  Wv_t  <- as.numeric(Wv)
  fit   <- lm(v_t ~ Wv_t + v_1 - 1)
  cf    <- coef(fit)
  lambda_hat <- unname(ifelse("Wv_t" %in% names(cf), cf["Wv_t"], NA_real_))
  rho_hat    <- unname(ifelse("v_1"  %in% names(cf), cf["v_1"],  NA_real_))
  list(lambda = lambda_hat, rho = rho_hat)
}

# ------------------------------------------------------------
# 8) QMLE (profil δ) — lengkap
# ------------------------------------------------------------
logdet_Iminus_aW <- function(a, W){
  ev <- eigen(W, only.values = TRUE)$values
  sum(log(abs(1 - a*ev)))
}

estimate_qmle <- function(stack, max_iter=6){
  y   <- stack$y; y1 <- stack$ylag; wy1 <- stack$wyl; Z <- stack$Z
  N   <- stack$N; Tt <- stack$Tt
  colnames(Z) <- c("const","x","wx","x_lag","wx_lag")
  
  delta_bounds <- delta_bounds_from_W(W_mat, margin = 0.05)
  delta <- 0.1; tau <- 0.1; eta <- 0.1
  beta  <- rep(0, ncol(Z)); names(beta) <- colnames(Z)
  mat_from_stack <- function(vec) matrix(vec, N, Tt)
  
  obj_delta <- function(d, RHS_vec){
    A   <- (Diagonal(N) - d * W_mat)
    Ay  <- as.numeric(A %*% mat_from_stack(y))
    e   <- Ay - RHS_vec
    RSS <- sum(e^2)
    val <- logdet_Iminus_aW(d, as.matrix(W_mat)) - 0.5*length(y)*log(RSS + 1e-12)
    -val
  }
  
  for (it in 1:max_iter){
    RHS_vec <- tau * y1 + eta * wy1 + as.numeric(Z %*% beta)
    delta <- optimize(function(d) obj_delta(d, RHS_vec), interval = delta_bounds)$minimum
    A    <- (Diagonal(N) - delta * W_mat)
    Ay   <- as.numeric(A %*% mat_from_stack(y))
    X_t  <- cbind(y1, wy1, Z)
    fit  <- lm(Ay ~ X_t - 1)
    co   <- coef(fit); tau <- unname(co[1]); eta <- unname(co[2]); beta <- unname(co[-c(1,2)])
    names(beta) <- colnames(Z)
  }
  
  resid <- Ay - as.numeric(X_t %*% c(tau, eta, beta))
  resid_mat <- matrix(resid, N, Tt)
  kappa <- estimate_kappa_sar_panel(W_mat, resid_mat)$kappa
  lr    <- estimate_lambda_rho_from_resid(W_mat, resid_mat)
  sig2_eps <- stats::var(resid, na.rm = TRUE)
  
  list(
    delta=delta, tau=tau, eta=eta,
    beta=c(beta_x=beta["x"], beta_wx=beta["wx"],
           beta_x_lag=beta["x_lag"], beta_wx_lag=beta["wx_lag"]),
    lambda = lr$lambda, rho = lr$rho,
    kappa=kappa, sig2_eps=sig2_eps,
    hypers = list() # placeholder agar flat_row seragam
  )
}

# ------------------------------------------------------------
# 9) GMM (two-step)
# ------------------------------------------------------------
build_regressors <- function(stack){
  N <- stack$N; Tt <- stack$Tt
  Ymat <- matrix(stack$y, N, Tt)
  Wy_mat <- apply(Ymat, 2, function(col) as.numeric(W_mat %*% col))
  Wy_stack <- as.numeric(Wy_mat)
  Xreg <- cbind(Wy_stack, stack$ylag, stack$wyl, stack$Z)
  colnames(Xreg) <- c("Wy","ylag","Wylag","const","x","wx","x_lag","wx_lag")
  Xreg
}

build_instruments <- function(stack){
  N <- stack$N; Tt <- stack$Tt
  ylag  <- stack$ylag
  Wylag <- stack$wyl
  
  ylag_mat <- matrix(ylag, N, Tt)
  Wyl_mat  <- apply(ylag_mat, 2, function(col) as.numeric(W_mat %*% col))
  W2yl_mat <- apply(ylag_mat, 2, function(col) as.numeric(W_mat %*% (W_mat %*% col)))
  
  Wyl   <- as.numeric(Wyl_mat)
  W2yl  <- as.numeric(W2yl_mat)
  
  Z   <- stack$Z
  Zmat_list <- lapply(seq_len(ncol(Z)), function(j) matrix(Z[,j], N, Tt))
  WZ  <- do.call(cbind, lapply(Zmat_list, function(M) as.numeric(W_mat %*% M)))
  W2Z <- do.call(cbind, lapply(Zmat_list, function(M) as.numeric(W_mat %*% (W_mat %*% M))))
  colnames(WZ)  <- paste0("WZ",  seq_len(ncol(Z)))
  colnames(W2Z) <- paste0("W2Z", seq_len(ncol(Z)))
  
  cbind(ylag, Wylag, Wyl = Wyl, W2yl = W2yl, Z, WZ, W2Z)
}

estimate_gmm <- function(stack){
  y <- stack$y
  X <- build_regressors(stack)
  Z <- build_instruments(stack)
  
  ZtZ <- crossprod(Z)
  W1  <- try(solve(ZtZ), silent=TRUE)
  if (inherits(W1, "try-error")) W1 <- MASS::ginv(ZtZ)
  
  XtZ <- crossprod(X, Z)
  Zty <- crossprod(Z, y)
  A1  <- XtZ %*% W1 %*% t(XtZ)
  B1  <- XtZ %*% W1 %*% Zty
  b1  <- try(solve(A1, B1), silent=TRUE)
  if (inherits(b1, "try-error")) b1 <- MASS::ginv(A1) %*% B1
  u1  <- y - X %*% b1
  
  nT <- length(y)
  S  <- crossprod(Z * as.numeric(u1), Z) / nT
  W2 <- try(solve(S), silent=TRUE)
  if (inherits(W2, "try-error")) W2 <- MASS::ginv(S)
  
  A2 <- XtZ %*% W2 %*% t(XtZ)
  B2 <- XtZ %*% W2 %*% Zty
  b2 <- try(solve(A2, B2), silent=TRUE)
  if (inherits(b2, "try-error")) b2 <- MASS::ginv(A2) %*% B2
  
  u2 <- y - X %*% b2
  coef <- drop(b2); names(coef) <- colnames(X)
  
  N <- stack$N; Tt <- stack$Tt
  Umat <- matrix(u2, N, Tt)
  kappa <- estimate_kappa_sar_panel(W_mat, Umat)$kappa
  lr    <- estimate_lambda_rho_from_resid(W_mat, Umat)
  
  list(
    delta = unname(coef["Wy"]),
    tau   = unname(coef["ylag"]),
    eta   = unname(coef["Wylag"]),
    beta  = c(beta_x = unname(coef["x"]),
              beta_wx = unname(coef["wx"]),
              beta_x_lag = unname(coef["x_lag"]),
              beta_wx_lag = unname(coef["wx_lag"])),
    lambda = lr$lambda, rho = lr$rho,
    kappa = kappa,
    sig2_eps = stats::var(u2, na.rm = TRUE),
    hypers = list()
  )
}

# ------------------------------------------------------------
# 10) INLA (BYM2 + AR1) dengan profiling δ (ekstraksi hyper BYM2 benar)
# ------------------------------------------------------------
fit_inla_profile_delta_eta_BYM2 <- function(sim, include_exog = TRUE){
  Y  <- sim$Y; N <- nrow(Y); Tt <- ncol(Y)
  Wy <- apply(Y, 2, function(col) as.numeric(W_mat %*% col))
  
  blk   <- function(t) ((t-1)*N+1):(t*N)
  idx_t  <- unlist(lapply(2:Tt, blk))
  idx_t1 <- unlist(lapply(1:(Tt-1), blk))
  
  base <- data.frame(
    y      = as.numeric(Y)[idx_t],
    wy_t   = as.numeric(Wy)[idx_t],
    y_lag  = as.numeric(Y)[idx_t1],
    wy_lag = as.numeric(Wy)[idx_t1],
    space    = rep(1:N, times = Tt-1),
    time_idx = rep(1:(Tt-1), each = N)
  )
  
  # centering agar stabil
  base$y_lag  <- base$y_lag  - mean(base$y_lag)
  base$wy_lag <- base$wy_lag - mean(base$wy_lag)
  
  if (include_exog) {
    base$x      <- as.numeric(sim$X)[idx_t]
    base$wx     <- as.numeric(sim$WX)[idx_t]
    base$x_lag  <- as.numeric(sim$Xlag)[idx_t]  - mean(as.numeric(sim$Xlag)[idx_t])
    base$wx_lag <- as.numeric(sim$WXlag)[idx_t] - mean(as.numeric(sim$WXlag)[idx_t])
  }
  
  delta_bounds <- delta_bounds_from_W(W_mat, margin = 0.05)
  
  evW <- eigen(as.matrix(W_mat), only.values = TRUE)$values
  logdet_Iminus_dW <- function(d) sum(log(abs(1 - d * evW)))
  
  fam_ctrl <- list(hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.05))))
  bym2_hyp <- list(
    phi  = list(prior = "logitbeta", params = c(1,1)),
    prec = list(prior = "pc.prec",  param  = c(1, 0.05))
  )
  ar1_hyp  <- list(
    rho  = list(prior = "pc.cor0", param = c(0.5, 0.5)),
    prec = list(prior = "pc.prec", param = c(1, 0.05))
  )
  
  inla_fit_for_delta <- function(d){
    dat <- base
    dat$y_star <- dat$y - d * dat$wy_t
    
    if (include_exog) {
      formula <- y_star ~ y_lag + wy_lag + x + wx + x_lag + wx_lag +
        f(space, model = "bym2", graph = spatial_graph_file, constr = TRUE,
          scale.model = TRUE, hyper = bym2_hyp) +
        f(time_idx, model = "ar1",  constr = TRUE, hyper = ar1_hyp)
    } else {
      formula <- y_star ~ y_lag + wy_lag +
        f(space, model = "bym2", graph = spatial_graph_file, constr = TRUE,
          scale.model = TRUE, hyper = bym2_hyp) +
        f(time_idx, model = "ar1",  constr = TRUE, hyper = ar1_hyp)
    }
    
    fit <- try(INLA::inla(
      formula, data = dat, family = "gaussian",
      control.family    = fam_ctrl,
      control.predictor = list(compute = TRUE),
      control.compute   = list(dic = TRUE, waic = TRUE, cpo = FALSE, config = FALSE),
      control.inla      = list(strategy = "gaussian", int.strategy = "eb",
                               diagonal = 1e-3, tolerance = 1e-6, safe = TRUE)
    ), silent = TRUE)
    
    if (inherits(fit, "try-error") || is.null(fit$mlik)) return(list(ok = FALSE))
    list(ok = TRUE, fit = fit, mlik = fit$mlik[1,1])
  }
  
  obj <- function(d) {
    ff <- inla_fit_for_delta(d)
    if (!ff$ok || is.na(ff$mlik)) return(1e8)
    -(ff$mlik + (Tt-1) * logdet_Iminus_dW(d))
  }
  
  opt <- optimize(obj, interval = delta_bounds)
  delta_hat <- opt$minimum
  
  ff <- inla_fit_for_delta(delta_hat)
  if (!ff$ok) return(list(delta = delta_hat, tau = NA, eta = NA, rho = NA, lambda = NA,
                          kappa = NA, betas = rep(NA_real_, if (include_exog) 4 else 0),
                          hypers = list(), sig2_eps = NA_real_, fit = NULL))
  fit <- ff$fit
  
  fixed <- fit$summary.fixed
  hyper <- fit$summary.hyperpar
  
  tau_hat <- if ("y_lag"  %in% rownames(fixed)) fixed["y_lag","mean"]  else NA_real_
  eta_hat <- if ("wy_lag" %in% rownames(fixed)) fixed["wy_lag","mean"] else NA_real_
  betas_hat <- c()
  if (include_exog) {
    betas_hat <- c(
      beta_x      = if ("x"      %in% rownames(fixed)) fixed["x","mean"]      else NA_real_,
      beta_wx     = if ("wx"     %in% rownames(fixed)) fixed["wx","mean"]     else NA_real_,
      beta_x_lag  = if ("x_lag"  %in% rownames(fixed)) fixed["x_lag","mean"]  else NA_real_,
      beta_wx_lag = if ("wx_lag" %in% rownames(fixed)) fixed["wx_lag","mean"] else NA_real_
    )
  }
  
  # --- HYPER dari INLA ---
  rho_hat <- {
    ix <- grep("^Rho for time_idx", rownames(hyper))
    if (length(ix)) hyper$mean[ix[1]] else NA_real_
  }
  sigma_eps_hat <- {
    ix <- grep("^Precision for the Gaussian observations", rownames(hyper))
    if (length(ix)) sqrt(1 / hyper$mean[ix[1]]) else NA_real_
  }
  tau_space_hat <- {
    ix <- grep("^Precision for space", rownames(hyper))
    if (length(ix)) hyper$mean[ix[1]] else NA_real_
  }
  phi_mix_hat <- {
    ix <- grep("^Phi for space", rownames(hyper))
    if (length(ix)) {
      .clip01(hyper$mean[ix[1]])
    } else {
      NA_real_
    }
  }
  
  sigma_struct_hat <- if (!is.na(tau_space_hat) && !is.na(phi_mix_hat)) {
    (1 / sqrt(tau_space_hat)) * sqrt(phi_mix_hat)
  } else NA_real_
  sigma_unstruct_hat <- if (!is.na(tau_space_hat) && !is.na(phi_mix_hat)) {
    (1 / sqrt(tau_space_hat)) * sqrt(1 - phi_mix_hat)
  } else NA_real_
  
  # residu pada skala y_star (y - delta*Wy)
  fitted_mean <- fit$summary.fitted.values$mean
  resid_vec   <- (base$y - delta_hat * base$wy_t) - fitted_mean
  resid_mat   <- matrix(resid_vec, nrow = N, ncol = (Tt-1), byrow = FALSE)
  
  kappa_hat  <- estimate_kappa_sar_panel(W_mat, resid_mat)$kappa
  lr         <- estimate_lambda_rho_from_resid(W_mat, resid_mat)
  lambda_hat <- lr$lambda
  rho_err    <- lr$rho
  
  list(
    delta  = delta_hat,
    tau    = tau_hat,
    eta    = eta_hat,
    rho    = rho_hat,      # AR1 hyper (proses waktu)
    lambda = lambda_hat,   # dari residu
    kappa  = kappa_hat,    # dari residu
    betas  = betas_hat,
    hypers = list(
      sigma_epsilon   = sigma_eps_hat,
      sigma_struct    = sigma_struct_hat,
      sigma_unstruct  = sigma_unstruct_hat,
      phi_mix         = phi_mix_hat,
      rho_error       = rho_err
    ),
    sig2_eps = NA_real_,
    fit = fit
  )
}

# ------------------------------------------------------------
# 11) Runner satu skenario (R replikasi) + tabel lengkap
#     Back-fill sigma_epsilon <-> sig2_eps
# ------------------------------------------------------------
run_one_scenario <- function(N_s, Tt_s, delta_s, tau_s, eta_s, R = 1, include_exog_in_inla = TRUE){
  init_problem(N_s, Tt_s)
  tp <- .make_true_par(delta_s, tau_s, eta_s)
  assign("true_par", tp, envir = .GlobalEnv)
  
  rec_qmle <- vector("list", R)
  rec_gmm  <- vector("list", R)
  rec_inla <- vector("list", R)
  
  for (r in seq_len(R)){
    sim <- simulate_dspm2(N_s, Tt_s, W_mat, tp)
    stk <- build_stack(sim)
    
    rec_qmle[[r]] <- try(estimate_qmle(stk), silent = TRUE)
    rec_gmm[[r]]  <- try(estimate_gmm(stk),  silent = TRUE)
    rec_inla[[r]] <- try(fit_inla_profile_delta_eta_BYM2(sim, include_exog = include_exog_in_inla), silent = TRUE)
  }
  
  .pick_beta <- function(x){
    if (!is.null(x$beta)) return(x$beta)
    if (!is.null(x$betas)) return(x$betas)
    c(beta_x=NA, beta_wx=NA, beta_x_lag=NA, beta_wx_lag=NA)
  }
  
  flat_row <- function(x, method, rep){
    if (inherits(x, "try-error") || is.null(x)) return(
      data.frame(rep=rep, method=method,
                 delta=NA, tau=NA, eta=NA,
                 rho=NA, lambda=NA, kappa=NA,
                 beta_x=NA, beta_wx=NA, beta_x_lag=NA, beta_wx_lag=NA,
                 sigma_epsilon=NA, sigma_struct=NA, sigma_unstruct=NA, phi_mix=NA,
                 rho_proc=NA,                  # simpan rho hyper (proses waktu INLA) jika ada
                 sigma_zeta=sqrt(tp$sig2_zeta),
                 sig2_eps=NA)
    )
    # ambil beta
    bx <- if (!is.null(x$beta)) x$beta else if (!is.null(x$betas)) x$betas else
      c(beta_x=NA, beta_wx=NA, beta_x_lag=NA, beta_wx_lag=NA)
    if (is.null(names(bx)) || !all(c("beta_x","beta_wx","beta_x_lag","beta_wx_lag") %in% names(bx))) {
      names(bx) <- c("beta_x","beta_wx","beta_x_lag","beta_wx_lag")[seq_along(bx)]
    }
    
    hypers <- if (!is.null(x$hypers)) x$hypers else list()
    
    # rho yang dipakai untuk METRIK = rho_error (AR1 pada error); fallback ke x$rho kalau tidak ada
    rho_err <- if (!is.null(hypers$rho_error)) hypers$rho_error else if (!is.null(x$rho)) x$rho else NA_real_
    rho_proc <- if (!is.null(x$rho)) x$rho else NA_real_  # simpan juga hyper AR1 proses waktu INLA (informasi tambahan)
    
    data.frame(
      rep = rep, method = method,
      delta = x$delta, tau = x$tau, eta = x$eta,
      rho = rho_err,                     # <- ini yang dipakai untuk banding ke truth$rho
      lambda = if (!is.null(x$lambda)) x$lambda else NA,
      kappa  = x$kappa,
      beta_x = unname(bx["beta_x"]),
      beta_wx = unname(bx["beta_wx"]),
      beta_x_lag = unname(bx["beta_x_lag"]),
      beta_wx_lag = unname(bx["beta_wx_lag"]),
      sigma_epsilon = if (!is.null(hypers$sigma_epsilon)) hypers$sigma_epsilon else NA,
      sigma_struct  = if (!is.null(hypers$sigma_struct))  hypers$sigma_struct  else NA,
      sigma_unstruct= if (!is.null(hypers$sigma_unstruct))hypers$sigma_unstruct else NA,
      phi_mix       = if (!is.null(hypers$phi_mix))       hypers$phi_mix       else NA,
      rho_proc      = rho_proc,          # kolom tambahan informatif (tidak dipakai di metrik)
      sigma_zeta    = sqrt(tp$sig2_zeta),
      sig2_eps      = if (!is.null(x$sig2_eps)) x$sig2_eps else NA
    )
  }
  
  est_qmle <- do.call(rbind, lapply(seq_along(rec_qmle), function(i) flat_row(rec_qmle[[i]], "QMLE", i)))
  est_gmm  <- do.call(rbind, lapply(seq_along(rec_gmm),  function(i) flat_row(rec_gmm[[i]],  "GMM",  i)))
  est_inla <- do.call(rbind, lapply(seq_along(rec_inla), function(i) flat_row(rec_inla[[i]], "INLA", i)))
  all_est  <- dplyr::bind_rows(est_qmle, est_gmm, est_inla)
  
  truth <- c(
    delta = tp$delta, tau = tp$tau, eta = tp$eta,
    lambda = tp$lambda, rho = tp$rho, kappa = tp$kappa
  )
  truth_beta <- c(
    beta_x      = tp$beta[2],
    beta_wx     = tp$beta[3],
    beta_x_lag  = tp$beta[4],
    beta_wx_lag = tp$beta[5]
  )
  
  metrics <- all_est %>%
    dplyr::group_by(method) %>%
    dplyr::summarise(
      # inti
      Bias_delta = bias_vec(delta, truth["delta"]),
      RMSE_delta = rmse_vec(delta, truth["delta"]),
      Bias_tau   = bias_vec(tau,   truth["tau"]),
      RMSE_tau   = rmse_vec(tau,   truth["tau"]),
      Bias_eta   = bias_vec(eta,   truth["eta"]),
      RMSE_eta   = rmse_vec(eta,   truth["eta"]),
      Bias_lambda = bias_vec(lambda, truth["lambda"]),
      RMSE_lambda = rmse_vec(lambda, truth["lambda"]),
      Bias_rho    = bias_vec(rho,    truth["rho"]),     # rho = rho_error yang kita set di flat_row
      RMSE_rho    = rmse_vec(rho,    truth["rho"]),
      Bias_kappa  = bias_vec(kappa,  truth["kappa"]),
      RMSE_kappa  = rmse_vec(kappa,  truth["kappa"]),
      
      # beta
      Bias_beta_x      = bias_vec(beta_x,      truth_beta["beta_x"]),
      RMSE_beta_x      = rmse_vec(beta_x,      truth_beta["beta_x"]),
      Bias_beta_wx     = bias_vec(beta_wx,     truth_beta["beta_wx"]),
      RMSE_beta_wx     = rmse_vec(beta_wx,     truth_beta["beta_wx"]),
      Bias_beta_x_lag  = bias_vec(beta_x_lag,  truth_beta["beta_x_lag"]),
      RMSE_beta_x_lag  = rmse_vec(beta_x_lag,  truth_beta["beta_x_lag"]),
      Bias_beta_wx_lag = bias_vec(beta_wx_lag, truth_beta["beta_wx_lag"]),
      RMSE_beta_wx_lag = rmse_vec(beta_wx_lag, truth_beta["beta_wx_lag"]),
      .groups = "drop"
    )
  
  list(estimates = all_est, metrics = metrics, truth = tp)
}


# ------------------------------------------------------------
# 12) Batch 108 skenario (opsional)
# ------------------------------------------------------------
run_108_scenarios <- function(R_per_scenario = 10, include_exog_in_inla = TRUE, out_dir = "mc_108"){
  scenarios <- expand.grid(
    N = c(49, 100),
    Tt = c(10, 20),
    Tau = c(0.2, 0.4, 0.6),
    Delta = c(0.2, 0.4, 0.6),
    Eta = c(0.2, 0.4, 0.6)
  ) %>% tibble::as_tibble() %>% dplyr::mutate(Scenario = row_number(), .before = 1)
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(scenarios, file.path(out_dir, "scenarios_index.csv"), row.names = FALSE)
  
  for (i in seq_len(nrow(scenarios))) {
    sc <- scenarios[i, ]
    cat(sprintf("\n--- Scenario %d/%d: N=%d T=%d Tau=%.1f Delta=%.1f Eta=%.1f ---\n",
                sc$Scenario, nrow(scenarios), sc$N, sc$Tt, sc$Tau, sc$Delta, sc$Eta))
    
    out <- run_one_scenario(sc$N, sc$Tt, sc$Delta, sc$Tau, sc$Eta,
                            R = R_per_scenario, include_exog_in_inla = include_exog_in_inla)
    
    sdir <- file.path(out_dir, sprintf("scenario_N%d_T%d_tau%.1f_delta%.1f_eta%.1f",
                                       sc$N, sc$Tt, sc$Tau, sc$Delta, sc$Eta))
    dir.create(sdir, showWarnings = FALSE, recursive = TRUE)
    
    utils::write.csv(out$estimates, file.path(sdir, "estimates.csv"), row.names = FALSE)
    utils::write.csv(out$metrics,   file.path(sdir, "metrics.csv"),   row.names = FALSE)
  }
  
  invisible(TRUE)
}

# ===== Helper nama folder (pastikan konsisten dengan yang lama) =====
.scenario_dir_name <- function(N, Tt, Tau, Delta, Eta){
  sprintf("scenario_N%d_T%d_tau%.1f_delta%.1f_eta%.1f", N, Tt, Tau, Delta, Eta)
}

.is_scenario_done <- function(sdir){
  file.exists(file.path(sdir, "estimates.csv")) &&
    file.exists(file.path(sdir, "metrics.csv"))
}

# ===== Tambahan: jalankan HANYA skenario dengan 0.3 di Tau/Delta/Eta =====
run_0_3_extension <- function(R_per_scenario = 100,
                              include_exog_in_inla = TRUE,
                              out_dir = "mc_108") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  # grid lengkap baru (dengan 0.3)
  grid_full <- expand.grid(
    N     = c(49, 100),
    Tt    = c(10, 20),
    Tau   = c(0.2, 0.3, 0.4, 0.6),
    Delta = c(0.2, 0.3, 0.4, 0.6),
    Eta   = c(0.2, 0.3, 0.4, 0.6)
  )
  
  # ambil HANYA kombinasi yang mengandung 0.3 di salah satu parameter
  grid_new <- subset(grid_full, Tau == 0.3 | Delta == 0.3 | Eta == 0.3)
  
  # loop skenario baru
  for (i in seq_len(nrow(grid_new))) {
    sc <- grid_new[i, ]
    
    sdir <- file.path(
      out_dir,
      .scenario_dir_name(sc$N, sc$Tt, sc$Tau, sc$Delta, sc$Eta)
    )
    
    # kalau sudah ada estimates & metrics, SKIP (tidak diulang)
    if (.is_scenario_done(sdir)) {
      message(sprintf(
        "[SKIP] N=%d T=%d Tau=%.1f Delta=%.1f Eta=%.1f sudah ada.",
        sc$N, sc$Tt, sc$Tau, sc$Delta, sc$Eta
      ))
      next
    }
    
    message(sprintf(
      "[RUN ] N=%d T=%d Tau=%.1f Delta=%.1f Eta=%.1f",
      sc$N, sc$Tt, sc$Tau, sc$Delta, sc$Eta
    ))
    dir.create(sdir, showWarnings = FALSE, recursive = TRUE)
    
    # jalankan simulasi skenario ini
    res <- try(
      run_one_scenario(
        N_s = sc$N,
        Tt_s = sc$Tt,
        delta_s = sc$Delta,
        tau_s   = sc$Tau,
        eta_s   = sc$Eta,
        R = R_per_scenario,
        include_exog_in_inla = include_exog_in_inla
      ),
      silent = TRUE
    )
    
    if (inherits(res, "try-error")) {
      warning(sprintf(
        "[ERR ] Gagal untuk N=%d T=%d Tau=%.1f Delta=%.1f Eta=%.1f",
        sc$N, sc$Tt, sc$Tau, sc$Delta, sc$Eta
      ))
      next
    }
    
    utils::write.csv(res$estimates,
                     file.path(sdir, "estimates.csv"),
                     row.names = FALSE)
    utils::write.csv(res$metrics,
                     file.path(sdir, "metrics.csv"),
                     row.names = FALSE)
    
    message(sprintf(
      "[DONE] Selesai N=%d T=%d Tau=%.1f Delta=%.1f Eta=%.1f",
      sc$N, sc$Tt, sc$Tau, sc$Delta, sc$Eta
    ))
  }
  
  invisible(TRUE)
}


# ------------------------------------------------------------
# 3) Init spatial structure (Queen row-stand.) + subset nb + graph BYM2
#     + simpan koordinat grid untuk visualisasi
# ------------------------------------------------------------
init_problem <- function(N_new, Tt_new){
  # grid mendekati persegi lalu subset 1..N_new
  nrow_grid <- floor(sqrt(N_new))
  ncol_grid <- ceiling(N_new / nrow_grid)
  
  # neighbours queen untuk full grid
  nb_full <- spdep::cell2nb(nrow_grid, ncol_grid, type = "queen")
  
  # koordinat grid (row-major, konsisten dengan cell2nb)
  # row = baris (vertikal), col = kolom (horizontal)
  coords_full <- expand.grid(
    row = 1:nrow_grid,
    col = 1:ncol_grid
  )
  
  keep <- seq_len(N_new)
  
  nb <- lapply(nb_full[keep], function(nei) intersect(nei, keep))
  class(nb) <- "nb"
  attr(nb, "region.id") <- as.character(keep)
  attr(nb, "type") <- "queen"
  attr(nb, "sym")  <- TRUE
  
  coords <- coords_full[keep, , drop = FALSE]
  
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  W  <- spdep::listw2mat(lw)
  W_mat <- Matrix::Matrix(W, sparse = TRUE)
  
  graph_file <- "spatial_graph.adj"
  spdep::nb2INLA(graph_file, nb)
  
  eigW <- eigen(as.matrix(W_mat), only.values = TRUE)$values
  
  # simpan ke global environment (sesuai style kode asli)
  assign("N", N_new, envir = .GlobalEnv)
  assign("Tt", Tt_new, envir = .GlobalEnv)
  assign("W_mat", W_mat, envir = .GlobalEnv)
  assign("W", W, envir = .GlobalEnv)
  assign("lw", lw, envir = .GlobalEnv)
  assign("spatial_graph_file", graph_file, envir = .GlobalEnv)
  assign("eigW", eigW, envir = .GlobalEnv)
  assign("nb", nb, envir = .GlobalEnv)
  assign("grid_coords", coords, envir = .GlobalEnv)
  assign("nrow_grid", nrow_grid, envir = .GlobalEnv)
  assign("ncol_grid", ncol_grid, envir = .GlobalEnv)
}

# ------------------------------------------------------------
# 3a) Visualisasi grid dan tetangga (queen)
# ------------------------------------------------------------
plot_spatial_grid <- function(point_labels = FALSE){
  if (!exists("grid_coords") || !exists("nb"))
    stop("Jalankan dulu init_problem(N, Tt) sehingga grid_coords dan nb tersedia.")
  
  coords <- grid_coords
  
  # plot titik
  plot(coords$col, coords$row,
       asp = 1,
       xlab = "Column",
       ylab = "Row",
       pch  = 16,
       main = sprintf("Spatial grid (N = %d, %d x %d, queen neighbours)",
                      N, nrow_grid, ncol_grid))
  
  # garis koneksi neighbour
  for (i in seq_along(nb)) {
    xi <- coords$col[i]
    yi <- coords$row[i]
    if (length(nb[[i]]) > 0) {
      for (j in nb[[i]]) {
        xj <- coords$col[j]
        yj <- coords$row[j]
        segments(xi, yi, xj, yj, lwd = 0.7)
      }
    }
  }
  
  # titik di-plot ulang supaya tidak tertutup garis
  points(coords$col, coords$row, pch = 16)
  
  if (point_labels) {
    text(coords$col, coords$row,
         labels = seq_len(nrow(coords)),
         pos = 3, cex = 0.7)
  }
}

# ------------------------------------------------------------
# 3b) Ringkasan spektrum W: eigen maksimum & minimum
# ------------------------------------------------------------
summarise_W_spectrum <- function(){
  if (!exists("eigW"))
    stop("eigW belum ada. Pastikan init_problem() sudah dipanggil.")
  
  ev <- eigW
  
  c(
    max_real = max(Re(ev)),
    min_real = min(Re(ev)),
    max_mod  = max(Mod(ev)),
    min_mod  = min(Mod(ev))
  )
}


# 1. Set ukuran N dan T (misal skenario pertama: N = 49, T = 10)
init_problem(N_new = 49, Tt_new = 10)

# 2. Visualisasi grid dan hubungan tetangga
plot_spatial_grid(point_labels = TRUE)   # TRUE kalau mau ditulis nomor node

# 3. Hitung eigen maksimum & minimum dari W
summarise_W_spectrum()
#    max_real    min_real     max_mod     min_mod 
#    ...         ...          ...         ...

