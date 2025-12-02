## ============================================================
##  DSPM – QMLE, GMM, INLA (BYM2+AR1)  
##  Jaya and Folmer 2025 
## ============================================================

library(dplyr)
library(readxl)
library(rlang)
library(Matrix)
library(MASS)
library(spdep)
library(INLA)

## ------------------------------------------------------------
## 0. Small helper functions
## ------------------------------------------------------------
.clip01 <- function(x) pmin(1, pmax(0, x))

delta_bounds_from_W <- function(W_mat, margin = 0.05) {
  ev   <- eigen(as.matrix(W_mat), only.values = TRUE)$values
  rhoW <- max(Mod(ev))
  bound <- 0.9 / rhoW
  c(-bound + margin, bound - margin)
}

.kappa_interval_from_W <- function(W_mat, margin = 1e-3) {
  ev <- eigen(as.matrix(W_mat), only.values = TRUE)$values
  list(lo = -1 + margin, hi = 1 - margin, ev = ev)
}

estimate_kappa_sar_panel <- function(W_mat, resid_mat, margin = 1e-3) {
  stopifnot(nrow(resid_mat) == nrow(W_mat))
  N <- nrow(W_mat); T_res <- ncol(resid_mat)
  v <- as.numeric(resid_mat)
  
  if (any(!is.finite(v))) {
    return(list(kappa = NA_real_, obj = NA_real_,
                interval = c(-1 + margin, 1 - margin)))
  }
  
  iv <- .kappa_interval_from_W(W_mat, margin)
  ev <- iv$ev
  
  logdet_Iminus_kW <- function(k) {
    val <- sum(log(abs(1 - k * ev)))
    if (!is.finite(val)) val <- 1e12
    val
  }
  
  obj <- function(k) {
    A  <- Diagonal(N) - k * W_mat
    Av <- as.numeric(kronecker(Diagonal(T_res), A) %*% v)
    mAv2 <- mean(Av^2)
    if (!is.finite(mAv2) || mAv2 <= 0) return(1e12)
    ll <- T_res * logdet_Iminus_kW(k) - (length(v)/2)*log(mAv2 + 1e-12)
    out <- -ll
    if (!is.finite(out)) out <- 1e12
    out
  }
  
  opt <- optimize(obj, interval = c(iv$lo, iv$hi))
  list(kappa = opt$minimum, obj = opt$objective, interval = c(iv$lo, iv$hi))
}

estimate_lambda_rho_from_resid <- function(W_mat, resid_mat) {
  N <- nrow(resid_mat); Tt <- ncol(resid_mat)
  v_t   <- as.numeric(resid_mat)
  vlag  <- cbind(rep(0, N), resid_mat[, 1:(Tt-1), drop = FALSE])
  v_1   <- as.numeric(vlag)
  Wv    <- apply(resid_mat, 2, function(col) as.numeric(W_mat %*% col))
  Wv_t  <- as.numeric(Wv)
  
  if (any(!is.finite(c(v_t, v_1, Wv_t)))) {
    return(list(lambda = NA_real_, rho = NA_real_))
  }
  
  fit   <- lm(v_t ~ Wv_t + v_1 - 1)
  cf    <- coef(fit)
  lambda_hat <- unname(ifelse("Wv_t" %in% names(cf), cf["Wv_t"], NA_real_))
  rho_hat    <- unname(ifelse("v_1"  %in% names(cf), cf["v_1"],  NA_real_))
  list(lambda = lambda_hat, rho = rho_hat)
}

logdet_Iminus_aW <- function(a, W) {
  ev <- eigen(W, only.values = TRUE)$values
  val <- sum(log(abs(1 - a * ev)))
  if (!is.finite(val)) val <- 1e12
  val
}

## ------------------------------------------------------------
## 1. Function to read W – EXACTLY as in the working version
## ------------------------------------------------------------
read_W_matrix <- function(W_path, sheet_W = 1) {
  cat("Reading W matrix from:", W_path, "\n")
  W_raw <- readxl::read_excel(W_path, sheet = sheet_W)
  cat("Initial dimension of W_raw:", dim(W_raw)[1], "x", dim(W_raw)[2], "\n")
  
  W_num <- dplyr::select(W_raw, where(is.numeric))
  cat("Dimension after selecting numeric columns:", dim(W_num)[1], "x", dim(W_num)[2], "\n")
  
  W_mat <- as.matrix(W_num)
  storage.mode(W_mat) <- "numeric"
  
  if (nrow(W_mat) == ncol(W_mat) + 1) {
    cat("Detected 1 extra row in W, will drop it (first row).\n")
    W_mat <- W_mat[-1, , drop = FALSE]
  }
  
  if (ncol(W_mat) == nrow(W_mat) + 1) {
    cat("Detected 1 extra column in W, will drop it (first column).\n")
    W_mat <- W_mat[, -1, drop = FALSE]
  }
  
  cat("Final dimension of W_mat:", dim(W_mat)[1], "x", dim(W_mat)[2], "\n")
  
  if (nrow(W_mat) != ncol(W_mat)) {
    stop("After cleaning, W is still not square. Please check W.xlsx.")
  }
  
  if (any(is.na(W_mat))) {
    warning("There are still NA values in W_mat after cleaning.")
  }
  
  return(W_mat)
}

## ------------------------------------------------------------
## 2. Build sim and stack – EXACTLY like your working code
## ------------------------------------------------------------

build_sim_from_panel <- function(df_long, id_var, time_var, y_var, x_var, W_mat) {
  df <- df_long %>%
    dplyr::select(
      id   = {{ id_var }},
      time = {{ time_var }},
      y    = {{ y_var }},
      x    = {{ x_var }}
    )
  
  id_levels   <- sort(unique(df$id))
  time_levels <- sort(unique(df$time))
  N  <- length(id_levels)
  Tt <- length(time_levels)
  
  if (nrow(df) != N * Tt) {
    warning("Panel is not fully balanced.")
  }
  
  df <- df %>%
    mutate(
      id_index   = match(id, id_levels),
      time_index = match(time, time_levels)
    )
  
  Y  <- matrix(NA_real_, nrow = N, ncol = Tt)
  X  <- matrix(NA_real_, nrow = N, ncol = Tt)
  
  for (i in seq_len(nrow(df))) {
    ii <- df$id_index[i]
    tt <- df$time_index[i]
    Y[ii, tt] <- df$y[i]
    X[ii, tt] <- df$x[i]
  }
  
  if (nrow(W_mat) != N || ncol(W_mat) != N) {
    stop(paste("The dimension of W (", nrow(W_mat), "x", ncol(W_mat),
               ") does not match N =", N))
  }
  
  WX     <- matrix(0, N, Tt)
  Xlag   <- matrix(0, N, Tt)
  WXlag  <- matrix(0, N, Tt)
  
  for (t in 1:Tt) {
    X_t <- X[, t]
    if (any(is.na(X_t))) {
      WX[, t] <- NA_real_
    } else {
      WX[, t] <- as.numeric(W_mat %*% X_t)
    }
    
    if (t > 1) {
      Xlag[, t] <- X[, t - 1]
      if (any(is.na(Xlag[, t]))) {
        WXlag[, t] <- NA_real_
      } else {
        WXlag[, t] <- as.numeric(W_mat %*% Xlag[, t])
      }
    }
  }
  
  list(
    Y     = Y,
    X     = X,
    WX    = WX,
    Xlag  = Xlag,
    WXlag = WXlag,
    W     = W_mat,
    N     = N,
    Tt    = Tt,
    id_levels   = id_levels,
    time_levels = time_levels
  )
}

build_stack <- function(sim) {
  Y    <- sim$Y
  N    <- nrow(Y)
  Tt   <- ncol(Y)
  W_mat <- sim$W
  
  Wy <- apply(Y, 2, function(col) {
    if (any(is.na(col))) {
      return(rep(NA_real_, N))
    } else {
      return(as.numeric(W_mat %*% col))
    }
  })
  
  Y_lag  <- cbind(rep(0, N), Y[, 1:(Tt - 1), drop = FALSE])
  Wy_lag <- cbind(rep(0, N), Wy[, 1:(Tt - 1), drop = FALSE])
  
  Z_t <- array(0, dim = c(N, 5, Tt))
  for (t in 1:Tt) {
    Z_t[, , t] <- cbind(1, sim$X[, t], sim$WX[, t], sim$Xlag[, t], sim$WXlag[, t])
  }
  
  list(
    y    = as.numeric(Y),
    ylag = as.numeric(Y_lag),
    wyl  = as.numeric(Wy_lag),
    Z    = do.call(rbind, lapply(1:Tt, function(t) Z_t[, , t])),
    W    = sim$W,
    N    = N,
    Tt   = Tt,
    sim  = sim
  )
}

## ------------------------------------------------------------
## 3. QMLE (δ profile) – stable version + complete beta
## ------------------------------------------------------------

estimate_qmle_full <- function(stk, W_mat, max_iter = 6) {
  
  y   <- stk$y
  y1  <- stk$ylag
  wy1 <- stk$wyl
  Z   <- stk$Z
  
  N   <- stk$N
  Tt  <- stk$Tt
  
  if (ncol(Z) != 5) {
    stop("Z must have 5 columns: const, x, wx, x_lag, wx_lag.")
  }
  colnames(Z) <- c("const","x","wx","x_lag","wx_lag")
  
  mat_from_stack <- function(v) matrix(v, N, Tt)
  delta_bounds <- delta_bounds_from_W(W_mat, margin = 0.05)
  
  delta <- 0.1
  tau   <- 0.1
  eta   <- 0.1
  beta  <- rep(0, ncol(Z))
  names(beta) <- colnames(Z)
  
  obj_delta <- function(d, RHS_vec) {
    A   <- (Diagonal(N) - d * W_mat)
    Ay  <- as.numeric(A %*% mat_from_stack(y))
    e   <- Ay - RHS_vec
    RSS <- sum(e^2)
    if (!is.finite(RSS) || RSS <= 0) return(1e12)
    val <- logdet_Iminus_aW(d, as.matrix(W_mat)) -
      0.5 * length(y) * log(RSS + 1e-12)
    -val
  }
  
  Ay <- NULL
  for (it in 1:max_iter) {
    RHS_vec <- tau*y1 + eta*wy1 + as.numeric(Z %*% beta)
    
    delta <- optimize(function(d) obj_delta(d, RHS_vec),
                      interval = delta_bounds)$minimum
    
    A    <- (Diagonal(N) - delta * W_mat)
    Ay   <- as.numeric(A %*% mat_from_stack(y))
    
    df_lm <- data.frame(
      Ay   = Ay,
      y1   = y1,
      wy1  = wy1,
      const  = Z[, "const"],
      x      = Z[, "x"],
      wx     = Z[, "wx"],
      x_lag  = Z[, "x_lag"],
      wx_lag = Z[, "wx_lag"]
    )
    
    df_lm <- df_lm[complete.cases(df_lm), , drop = FALSE]
    if (nrow(df_lm) == 0) break
    
    fit <- lm(Ay ~ y1 + wy1 + const + x + wx + x_lag + wx_lag - 1,
              data = df_lm)
    
    co <- coef(fit)
    tau  <- unname(co["y1"])
    eta  <- unname(co["wy1"])
    beta["const"]   <- unname(co["const"])
    beta["x"]       <- unname(co["x"])
    beta["wx"]      <- unname(co["wx"])
    beta["x_lag"]   <- unname(co["x_lag"])
    beta["wx_lag"]  <- unname(co["wx_lag"])
  }
  
  if (is.null(Ay)) {
    return(list(
      method = "QMLE",
      delta = NA, tau = NA, eta = NA,
      lambda = NA, rho = NA, kappa = NA,
      sig2_eps = NA,
      beta = c(beta_x = NA, beta_wx = NA,
               beta_x_lag = NA, beta_wx_lag = NA)
    ))
  }
  
  X_full <- cbind(
    y1   = y1,
    wy1  = wy1,
    const  = Z[, "const"],
    x      = Z[, "x"],
    wx     = Z[, "wx"],
    x_lag  = Z[, "x_lag"],
    wx_lag = Z[, "wx_lag"]
  )
  
  resid <- Ay - as.numeric(
    X_full %*% c(tau, eta, beta["const"], beta["x"], beta["wx"],
                 beta["x_lag"], beta["wx_lag"])
  )
  
  resid_mat <- matrix(resid, N, Tt)
  kappa    <- estimate_kappa_sar_panel(W_mat, resid_mat)$kappa
  lr       <- estimate_lambda_rho_from_resid(W_mat, resid_mat)
  sig2_eps <- stats::var(resid, na.rm = TRUE)
  
  beta_out <- c(
    beta_x      = beta["x"],
    beta_wx     = beta["wx"],
    beta_x_lag  = beta["x_lag"],
    beta_wx_lag = beta["wx_lag"]
  )
  
  list(
    method   = "QMLE",
    delta    = delta,
    tau      = tau,
    eta      = eta,
    lambda   = lr$lambda,
    rho      = lr$rho,
    kappa    = kappa,
    sig2_eps = sig2_eps,
    beta     = beta_out
  )
}


## ------------------------------------------------------------
## 4. GMM two-step
## ------------------------------------------------------------

build_regressors <- function(stk, W_mat) {
  N <- stk$N; Tt <- stk$Tt
  Ymat <- matrix(stk$y, N, Tt)
  Wy_mat <- apply(Ymat, 2, function(col) {
    if (any(is.na(col))) rep(NA_real_, N) else as.numeric(W_mat %*% col)
  })
  Wy_stack <- as.numeric(Wy_mat)
  
  Z <- stk$Z
  colnames(Z) <- c("const","x","wx","x_lag","wx_lag")
  
  Xreg <- cbind(Wy_stack, stk$ylag, stk$wyl, Z)
  colnames(Xreg) <- c("Wy","ylag","Wylag","const","x","wx","x_lag","wx_lag")
  Xreg
}

build_instruments <- function(stk, W_mat) {
  N <- stk$N; Tt <- stk$Tt
  ylag  <- stk$ylag
  Wylag <- stk$wyl
  
  ylag_mat <- matrix(ylag, N, Tt)
  Wyl_mat  <- apply(ylag_mat, 2, function(col) {
    if (any(is.na(col))) rep(NA_real_, N) else as.numeric(W_mat %*% col)
  })
  W2yl_mat <- apply(ylag_mat, 2, function(col) {
    if (any(is.na(col))) rep(NA_real_, N) else as.numeric(W_mat %*% (W_mat %*% col))
  })
  
  Wyl   <- as.numeric(Wyl_mat)
  W2yl  <- as.numeric(W2yl_mat)
  
  Z   <- stk$Z
  colnames(Z) <- c("const","x","wx","x_lag","wx_lag")
  
  Zmat_list <- lapply(seq_len(ncol(Z)), function(j) matrix(Z[,j], N, Tt))
  
  make_WZ <- function(M) {
    tmp <- apply(M, 2, function(col) {
      if (any(is.na(col))) rep(NA_real_, N) else as.numeric(W_mat %*% col)
    })
    as.numeric(tmp)
  }
  make_W2Z <- function(M) {
    tmp <- apply(M, 2, function(col) {
      if (any(is.na(col))) rep(NA_real_, N) else as.numeric(W_mat %*% (W_mat %*% col))
    })
    as.numeric(tmp)
  }
  
  WZ_vecs  <- lapply(Zmat_list, make_WZ)
  W2Z_vecs <- lapply(Zmat_list, make_W2Z)
  
  WZ  <- do.call(cbind, WZ_vecs)
  W2Z <- do.call(cbind, W2Z_vecs)
  colnames(WZ)  <- paste0("WZ", 1:ncol(WZ))
  colnames(W2Z) <- paste0("W2Z",1:ncol(W2Z))
  
  cbind(ylag = ylag, Wylag = Wylag,
        Wyl = Wyl, W2yl = W2yl,
        Z, WZ, W2Z)
}

estimate_gmm_full <- function(stk, W_mat) {
  y <- stk$y
  X <- build_regressors(stk, W_mat)
  Z <- build_instruments(stk, W_mat)
  
  keep <- complete.cases(y, X, Z)
  y <- y[keep]
  X <- X[keep, , drop = FALSE]
  Z <- Z[keep, , drop = FALSE]
  
  if (length(y) == 0) {
    return(list(
      method = "GMM",
      delta = NA, tau = NA, eta = NA,
      lambda = NA, rho = NA, kappa = NA,
      sig2_eps = NA,
      beta = c(beta_x = NA, beta_wx = NA,
               beta_x_lag = NA, beta_wx_lag = NA)
    ))
  }
  
  ZtZ <- crossprod(Z)
  W1  <- try(solve(ZtZ), silent = TRUE)
  if (inherits(W1, "try-error")) W1 <- MASS::ginv(ZtZ)
  
  XtZ <- crossprod(X, Z)
  Zty <- crossprod(Z, y)
  A1  <- XtZ %*% W1 %*% t(XtZ)
  B1  <- XtZ %*% W1 %*% Zty
  b1  <- try(solve(A1, B1), silent = TRUE)
  if (inherits(b1, "try-error")) b1 <- MASS::ginv(A1) %*% B1
  u1  <- y - X %*% b1
  
  nT <- length(y)
  S  <- crossprod(Z * as.numeric(u1), Z) / nT
  W2 <- try(solve(S), silent = TRUE)
  if (inherits(W2, "try-error")) W2 <- MASS::ginv(S)
  
  A2 <- XtZ %*% W2 %*% t(XtZ)
  B2 <- XtZ %*% W2 %*% Zty
  b2 <- try(solve(A2, B2), silent = TRUE)
  if (inherits(b2, "try-error")) b2 <- MASS::ginv(A2) %*% B2
  
  u2 <- y - X %*% b2
  coef_vec <- drop(b2)
  
  cn <- colnames(X)
  names(coef_vec) <- cn
  
  beta_out <- c(
    beta_x      = unname(coef_vec["x"]),
    beta_wx     = unname(coef_vec["wx"]),
    beta_x_lag  = unname(coef_vec["x_lag"]),
    beta_wx_lag = unname(coef_vec["wx_lag"])
  )
  
  N <- stk$N; Tt <- stk$Tt
  Umat <- matrix(u2, N, Tt)
  kappa <- estimate_kappa_sar_panel(W_mat, Umat)$kappa
  lr    <- estimate_lambda_rho_from_resid(W_mat, Umat)
  
  list(
    method = "GMM",
    delta  = unname(coef_vec["Wy"]),
    tau    = unname(coef_vec["ylag"]),
    eta    = unname(coef_vec["Wylag"]),
    lambda = lr$lambda,
    rho    = lr$rho,
    kappa  = kappa,
    sig2_eps = stats::var(u2, na.rm = TRUE),
    beta   = beta_out
  )
}

## ------------------------------------------------------------
## 5. INLA (BYM2 + AR1) from panel data
## ------------------------------------------------------------

create_inla_graph_from_W <- function(W_mat) {
  W_dense <- as.matrix(W_mat)
  nb_list <- apply(W_dense, 1, function(row) which(row != 0))
  class(nb_list) <- "nb"
  graph_file <- tempfile(fileext = ".adj")
  spdep::nb2INLA(graph_file, nb_list)
  graph_file
}

fit_inla_panel <- function(sim, W_mat, include_exog = TRUE) {
  Y  <- sim$Y; N <- nrow(Y); Tt <- ncol(Y)
  Wy <- apply(Y, 2, function(col) {
    if (any(is.na(col))) rep(NA_real_, N) else as.numeric(W_mat %*% col)
  })
  
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
  
  base <- base[complete.cases(base), ]
  if (nrow(base) == 0) {
    return(list(
      method = "INLA",
      delta = NA, tau = NA, eta = NA,
      lambda = NA, rho = NA, kappa = NA,
      sig2_eps = NA,
      betas = c(beta_x = NA, beta_wx = NA,
                beta_x_lag = NA, beta_wx_lag = NA)
    ))
  }
  
  base$y_lag  <- base$y_lag  - mean(base$y_lag)
  base$wy_lag <- base$wy_lag - mean(base$wy_lag)
  
  if (include_exog) {
    base$x      <- as.numeric(sim$X)[idx_t]
    base$wx     <- as.numeric(sim$WX)[idx_t]
    base$x_lag  <- as.numeric(sim$Xlag)[idx_t]
    base$wx_lag <- as.numeric(sim$WXlag)[idx_t]
    
    base$x      <- base$x      - mean(base$x, na.rm = TRUE)
    base$wx     <- base$wx     - mean(base$wx, na.rm = TRUE)
    base$x_lag  <- base$x_lag  - mean(base$x_lag, na.rm = TRUE)
    base$wx_lag <- base$wx_lag - mean(base$wx_lag, na.rm = TRUE)
    
    base <- base[complete.cases(base), ]
  }
  
  delta_bounds <- delta_bounds_from_W(W_mat, margin = 0.05)
  graph_file <- create_inla_graph_from_W(W_mat)
  
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
  
  inla_fit_for_delta <- function(d) {
    dat <- base
    dat$y_star <- dat$y - d * dat$wy_t
    
    if (include_exog) {
      formula <- y_star ~ y_lag + wy_lag + x + wx + x_lag + wx_lag +
        f(space, model = "bym2", graph = graph_file, constr = TRUE,
          scale.model = TRUE, hyper = bym2_hyp) +
        f(time_idx, model = "ar1",  constr = TRUE, hyper = ar1_hyp)
    } else {
      formula <- y_star ~ y_lag + wy_lag +
        f(space, model = "bym2", graph = graph_file, constr = TRUE,
          scale.model = TRUE, hyper = bym2_hyp) +
        f(time_idx, model = "ar1",  constr = TRUE, hyper = ar1_hyp)
    }
    
    fit <- try(INLA::inla(
      formula, data = dat, family = "gaussian",
      control.family    = fam_ctrl,
      control.predictor = list(compute = TRUE),
      control.compute   = list(dic = TRUE, waic = TRUE, cpo = FALSE, config = FALSE),
      control.inla      = list(strategy = "gaussian",
                               int.strategy = "eb",
                               diagonal = 1e-3,
                               tolerance = 1e-6)
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
  if (!ff$ok) {
    return(list(
      method = "INLA",
      delta = delta_hat, tau = NA, eta = NA,
      lambda = NA, rho = NA, kappa = NA,
      sig2_eps = NA,
      betas = c(beta_x = NA, beta_wx = NA,
                beta_x_lag = NA, beta_wx_lag = NA)
    ))
  }
  fit <- ff$fit
  fixed <- fit$summary.fixed
  
  tau_hat <- if ("y_lag"  %in% rownames(fixed)) fixed["y_lag","mean"]  else NA_real_
  eta_hat <- if ("wy_lag" %in% rownames(fixed)) fixed["wy_lag","mean"] else NA_real_
  
  betas_hat <- c(
    beta_x      = if ("x"      %in% rownames(fixed)) fixed["x","mean"]      else NA_real_,
    beta_wx     = if ("wx"     %in% rownames(fixed)) fixed["wx","mean"]     else NA_real_,
    beta_x_lag  = if ("x_lag"  %in% rownames(fixed)) fixed["x_lag","mean"]  else NA_real_,
    beta_wx_lag = if ("wx_lag" %in% rownames(fixed)) fixed["wx_lag","mean"] else NA_real_
  )
  
  fitted_mean <- fit$summary.fitted.values$mean
  resid_vec   <- (base$y - delta_hat * base$wy_t) - fitted_mean
  resid_mat   <- matrix(resid_vec, nrow = N, ncol = (Tt-1), byrow = FALSE)
  
  kappa_hat  <- estimate_kappa_sar_panel(W_mat, resid_mat)$kappa
  lr         <- estimate_lambda_rho_from_resid(W_mat, resid_mat)
  
  list(
    method = "INLA",
    delta  = delta_hat,
    tau    = tau_hat,
    eta    = eta_hat,
    lambda = lr$lambda,
    rho    = lr$rho,
    kappa  = kappa_hat,
    sig2_eps = NA_real_,
    betas  = betas_hat
  )
}

## ------------------------------------------------------------
## 6. Main wrapper: from Excel to result table
## ------------------------------------------------------------

estimate_all_from_excel <- function(
    data_path,
    W_path,
    id_col   = "id",
    time_col = "time",
    y_col    = "y",
    x_col    = "x",
    sheet_data = 1,
    sheet_W    = 1,
    inla_exog  = TRUE
) {
  cat("Reading panel data from:", data_path, "\n")
  df_long <- readxl::read_excel(data_path, sheet = sheet_data)
  cat("Panel data dimension:", dim(df_long)[1], "x", dim(df_long)[2], "\n")
  
  W_mat <- read_W_matrix(W_path, sheet_W = sheet_W)
  
  sim <- build_sim_from_panel(
    df_long  = df_long,
    id_var   = !!rlang::sym(id_col),
    time_var = !!rlang::sym(time_col),
    y_var    = !!rlang::sym(y_col),
    x_var    = !!rlang::sym(x_col),
    W_mat    = W_mat
  )
  
  cat("N =", sim$N, ", T =", sim$Tt, "\n")
  
  stk <- build_stack(sim)
  
  res_qmle <- estimate_qmle_full(stk, W_mat)
  res_gmm  <- estimate_gmm_full(stk, W_mat)
  res_inla <- fit_inla_panel(sim, W_mat, include_exog = inla_exog)
  
  flat <- bind_rows(
    tibble(
      method  = res_qmle$method,
      delta   = res_qmle$delta,
      tau     = res_qmle$tau,
      eta     = res_qmle$eta,
      lambda  = res_qmle$lambda,
      rho     = res_qmle$rho,
      kappa   = res_qmle$kappa,
      beta_x      = res_qmle$beta["beta_x.x"],
      beta_wx     = res_qmle$beta["beta_wx.wx"],
      beta_x_lag  = res_qmle$beta["beta_x_lag.x_lag"],
      beta_wx_lag = res_qmle$beta["beta_wx_lag.wx_lag"]
    ),
    tibble(
      method  = res_gmm$method,
      delta   = res_gmm$delta,
      tau     = res_gmm$tau,
      eta     = res_gmm$eta,
      lambda  = res_gmm$lambda,
      rho     = res_gmm$rho,
      kappa   = res_gmm$kappa,
      beta_x      = res_gmm$beta["beta_x"],
      beta_wx     = res_gmm$beta["beta_wx"],
      beta_x_lag  = res_gmm$beta["beta_x_lag"],
      beta_wx_lag = res_gmm$beta["beta_wx_lag"]
    ),
    tibble(
      method  = res_inla$method,
      delta   = res_inla$delta,
      tau     = res_inla$tau,
      eta     = res_inla$eta,
      lambda  = res_inla$lambda,
      rho     = res_inla$rho,
      kappa   = res_inla$kappa,
      beta_x      = res_inla$betas["beta_x"],
      beta_wx     = res_inla$betas["beta_wx"],
      beta_x_lag  = res_inla$betas["beta_x_lag"],
      beta_wx_lag = res_inla$betas["beta_wx_lag"]
    )
  )
  
  num_cols <- sapply(flat, is.numeric)
  flat[num_cols] <- lapply(flat[num_cols], function(z) {
    z[!is.finite(z)] <- NA_real_
    round(z, 4)
  })
  
  flat
}

## ------------------------------------------------------------
## 7. Example call
## ------------------------------------------------------------
## Adjust working directory & column names to your data

# setwd("/Users/mindra/@Paper9")

Result <- estimate_all_from_excel(
  data_path = "DataContoh.xlsx",
  W_path    = "W.xlsx",
  id_col    = "id",
  time_col  = "time",
  y_col     = "y",
  x_col     = "x",
  sheet_data = 1,
  sheet_W    = 1
)

print(Result)
