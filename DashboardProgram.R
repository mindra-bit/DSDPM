# ============================================================
# DSDPM Dashboard
# Developed by Jaya and Folmer - 2025
# ============================================================

setwd("/Users/mindra/@Paper9")

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(readxl)
library(dplyr)
library(rlang)
library(Matrix)
library(MASS)
library(spdep)
library(INLA)
library(shinyjs)
library(shinycssloaders)

# ============================================================
# ANALYSIS FUNCTIONS (same as your original code)
# ============================================================

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
    return(list(
      kappa   = NA_real_,
      obj     = NA_real_,
      interval = c(-1 + margin, 1 - margin)
    ))
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
    ll <- T_res * logdet_Iminus_kW(k) - (length(v) / 2) * log(mAv2 + 1e-12)
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
  vlag  <- cbind(rep(0, N), resid_mat[, 1:(Tt - 1), drop = FALSE])
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

read_W_matrix <- function(W_path, sheet_W = 1) {
  cat("Reading W matrix from:", W_path, "\n")
  W_raw <- readxl::read_excel(W_path, sheet = sheet_W)
  cat("Initial dimension of W_raw:", dim(W_raw)[1], "x", dim(W_raw)[2], "\n")
  
  W_num <- dplyr::select(W_raw, where(is.numeric))
  cat("Dimension after selecting numeric columns:", dim(W_num)[1], "x", dim(W_num)[2], "\n")
  
  W_mat <- as.matrix(W_num)
  storage.mode(W_mat) <- "numeric"
  
  if (nrow(W_mat) == ncol(W_mat) + 1) {
    cat("Detected one extra row in W, dropping the first row.\n")
    W_mat <- W_mat[-1, , drop = FALSE]
  }
  
  if (ncol(W_mat) == nrow(W_mat) + 1) {
    cat("Detected one extra column in W, dropping the first column.\n")
    W_mat <- W_mat[, -1, drop = FALSE]
  }
  
  cat("Final dimension of W_mat:", dim(W_mat)[1], "x", dim(W_mat)[2], "\n")
  
  if (nrow(W_mat) != ncol(W_mat)) {
    stop("After cleaning, W is still not square. Please check W.xlsx.")
  }
  
  if (any(is.na(W_mat))) {
    warning("There are NA values in W_mat after cleaning.")
  }
  
  return(W_mat)
}

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
    warning("The panel is not fully balanced.")
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
    stop(paste(
      "The dimension of W (", nrow(W_mat), "x", ncol(W_mat),
      ") does not match N =", N
    ))
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
  colnames(Z) <- c("const", "x", "wx", "x_lag", "wx_lag")
  
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
    RHS_vec <- tau * y1 + eta * wy1 + as.numeric(Z %*% beta)
    
    delta <- optimize(
      function(d) obj_delta(d, RHS_vec),
      interval = delta_bounds
    )$minimum
    
    A    <- (Diagonal(N) - delta * W_mat)
    Ay   <- as.numeric(A %*% mat_from_stack(y))
    
    df_lm <- data.frame(
      Ay     = Ay,
      y1     = y1,
      wy1    = wy1,
      const  = Z[, "const"],
      x      = Z[, "x"],
      wx     = Z[, "wx"],
      x_lag  = Z[, "x_lag"],
      wx_lag = Z[, "wx_lag"]
    )
    
    df_lm <- df_lm[complete.cases(df_lm), , drop = FALSE]
    if (nrow(df_lm) == 0) break
    
    fit <- lm(
      Ay ~ y1 + wy1 + const + x + wx + x_lag + wx_lag - 1,
      data = df_lm
    )
    
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
      method   = "QMLE",
      delta    = NA,
      tau      = NA,
      eta      = NA,
      lambda   = NA,
      rho      = NA,
      kappa    = NA,
      sig2_eps = NA,
      beta     = c(
        beta_x      = NA,
        beta_wx     = NA,
        beta_x_lag  = NA,
        beta_wx_lag = NA
      )
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
    X_full %*% c(
      tau, eta, beta["const"], beta["x"], beta["wx"],
      beta["x_lag"], beta["wx_lag"]
    )
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

build_regressors <- function(stk, W_mat) {
  N <- stk$N; Tt <- stk$Tt
  Ymat <- matrix(stk$y, N, Tt)
  Wy_mat <- apply(Ymat, 2, function(col) {
    if (any(is.na(col))) rep(NA_real_, N) else as.numeric(W_mat %*% col)
  })
  Wy_stack <- as.numeric(Wy_mat)
  
  Z <- stk$Z
  colnames(Z) <- c("const", "x", "wx", "x_lag", "wx_lag")
  
  Xreg <- cbind(Wy_stack, stk$ylag, stk$wyl, Z)
  colnames(Xreg) <- c("Wy", "ylag", "Wylag", "const", "x", "wx", "x_lag", "wx_lag")
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
  colnames(Z) <- c("const", "x", "wx", "x_lag", "wx_lag")
  
  Zmat_list <- lapply(seq_len(ncol(Z)), function(j) matrix(Z[, j], N, Tt))
  
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
  colnames(WZ)  <- paste0("WZ",  1:ncol(WZ))
  colnames(W2Z) <- paste0("W2Z", 1:ncol(W2Z))
  
  cbind(
    ylag  = ylag,
    Wylag = Wylag,
    Wyl   = Wyl,
    W2yl  = W2yl,
    Z,
    WZ,
    W2Z
  )
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
      method   = "GMM",
      delta    = NA,
      tau      = NA,
      eta      = NA,
      lambda   = NA,
      rho      = NA,
      kappa    = NA,
      sig2_eps = NA,
      beta     = c(
        beta_x      = NA,
        beta_wx     = NA,
        beta_x_lag  = NA,
        beta_wx_lag = NA
      )
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
    method   = "GMM",
    delta    = unname(coef_vec["Wy"]),
    tau      = unname(coef_vec["ylag"]),
    eta      = unname(coef_vec["Wylag"]),
    lambda   = lr$lambda,
    rho      = lr$rho,
    kappa    = kappa,
    sig2_eps = stats::var(u2, na.rm = TRUE),
    beta     = beta_out
  )
}

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
  
  blk   <- function(t) ((t - 1) * N + 1):(t * N)
  idx_t  <- unlist(lapply(2:Tt, blk))
  idx_t1 <- unlist(lapply(1:(Tt - 1), blk))
  
  base <- data.frame(
    y       = as.numeric(Y)[idx_t],
    wy_t    = as.numeric(Wy)[idx_t],
    y_lag   = as.numeric(Y)[idx_t1],
    wy_lag  = as.numeric(Wy)[idx_t1],
    space    = rep(1:N, times = Tt - 1),
    time_idx = rep(1:(Tt - 1), each = N)
  )
  
  base <- base[complete.cases(base), ]
  if (nrow(base) == 0) {
    return(list(
      method   = "INLA",
      delta    = NA,
      tau      = NA,
      eta      = NA,
      lambda   = NA,
      rho      = NA,
      kappa    = NA,
      sig2_eps = NA,
      betas    = c(
        beta_x      = NA,
        beta_wx     = NA,
        beta_x_lag  = NA,
        beta_wx_lag = NA
      )
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
  
  fam_ctrl <- list(
    hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.05)))
  )
  bym2_hyp <- list(
    phi  = list(prior = "logitbeta", params = c(1, 1)),
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
        f(
          space,
          model = "bym2",
          graph = graph_file,
          constr = TRUE,
          scale.model = TRUE,
          hyper = bym2_hyp
        ) +
        f(
          time_idx,
          model = "ar1",
          constr = TRUE,
          hyper = ar1_hyp
        )
    } else {
      formula <- y_star ~ y_lag + wy_lag +
        f(
          space,
          model = "bym2",
          graph = graph_file,
          constr = TRUE,
          scale.model = TRUE,
          hyper = bym2_hyp
        ) +
        f(
          time_idx,
          model = "ar1",
          constr = TRUE,
          hyper = ar1_hyp
        )
    }
    
    fit <- try(
      INLA::inla(
        formula,
        data = dat,
        family = "gaussian",
        control.family    = fam_ctrl,
        control.predictor = list(compute = TRUE),
        control.compute   = list(
          dic  = TRUE,
          waic = TRUE,
          cpo  = FALSE,
          config = FALSE
        ),
        control.inla      = list(
          strategy    = "gaussian",
          int.strategy = "eb",
          diagonal    = 1e-3,
          tolerance   = 1e-6
        )
      ),
      silent = TRUE
    )
    
    if (inherits(fit, "try-error") || is.null(fit$mlik)) return(list(ok = FALSE))
    list(ok = TRUE, fit = fit, mlik = fit$mlik[1, 1])
  }
  
  obj <- function(d) {
    ff <- inla_fit_for_delta(d)
    if (!ff$ok || is.na(ff$mlik)) return(1e8)
    -(ff$mlik + (Tt - 1) * logdet_Iminus_dW(d))
  }
  
  opt <- optimize(obj, interval = delta_bounds)
  delta_hat <- opt$minimum
  
  ff <- inla_fit_for_delta(delta_hat)
  if (!ff$ok) {
    return(list(
      method   = "INLA",
      delta    = delta_hat,
      tau      = NA,
      eta      = NA,
      lambda   = NA,
      rho      = NA,
      kappa    = NA,
      sig2_eps = NA,
      betas    = c(
        beta_x      = NA,
        beta_wx     = NA,
        beta_x_lag  = NA,
        beta_wx_lag = NA
      )
    ))
  }
  fit <- ff$fit
  fixed <- fit$summary.fixed
  
  tau_hat <- if ("y_lag"  %in% rownames(fixed)) fixed["y_lag",  "mean"] else NA_real_
  eta_hat <- if ("wy_lag" %in% rownames(fixed)) fixed["wy_lag", "mean"] else NA_real_
  
  betas_hat <- c(
    beta_x      = if ("x"      %in% rownames(fixed)) fixed["x",      "mean"] else NA_real_,
    beta_wx     = if ("wx"     %in% rownames(fixed)) fixed["wx",     "mean"] else NA_real_,
    beta_x_lag  = if ("x_lag"  %in% rownames(fixed)) fixed["x_lag",  "mean"] else NA_real_,
    beta_wx_lag = if ("wx_lag" %in% rownames(fixed)) fixed["wx_lag", "mean"] else NA_real_
  )
  
  fitted_mean <- fit$summary.fitted.values$mean
  resid_vec   <- (base$y - delta_hat * base$wy_t) - fitted_mean
  resid_mat   <- matrix(resid_vec, nrow = N, ncol = (Tt - 1), byrow = FALSE)
  
  kappa_hat  <- estimate_kappa_sar_panel(W_mat, resid_mat)$kappa
  lr         <- estimate_lambda_rho_from_resid(W_mat, resid_mat)
  
  list(
    method   = "INLA",
    delta    = delta_hat,
    tau      = tau_hat,
    eta      = eta_hat,
    lambda   = lr$lambda,
    rho      = lr$rho,
    kappa    = kappa_hat,
    sig2_eps = NA_real_,
    betas    = betas_hat
  )
}

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

# ============================================================
# SHINY DASHBOARD - PROFESSIONAL VERSION
# ============================================================

ui <- dashboardPage(
  dashboardHeader(
    title = span(
      icon("chart-line"), 
      "DSDPM Analyzer",
      style = "font-weight: bold; font-size: 22px;"
    ),
    titleWidth = 300,
    tags$li(
      class = "dropdown",
      style = "padding: 8px 15px;",
      span("Developed by Jaya and Folmer", style = "color: #ffffff; font-style: italic;")
    )
  ),
  
  dashboardSidebar(
    width = 300,
    tags$div(
      style = "padding: 20px 15px; background-color: #222d32;",
      h4("DSDPM Analyzer", style = "color: #ffffff; margin: 0 0 10px 0;"),
      p("Dynamic Spatial Panel Data Model", style = "color: #b8c7ce; font-size: 12px; margin: 0;"),
      hr(style = "border-color: #4b646f; margin: 15px 0;"),
      tags$p(
        "Version 1.0 | 2025", 
        style = "color: #8aa4af; font-size: 11px; text-align: center; margin-top: 20px;"
      )
    ),
    sidebarMenu(
      id = "sidebar",
      menuItem(
        "📤 Data Upload", 
        tabName = "upload", 
        icon = icon("upload"),
        selected = TRUE
      ),
      menuItem(
        "🚀 Run Analysis", 
        tabName = "run", 
        icon = icon("play-circle")
      ),
      menuItem(
        "📊 Dashboard", 
        tabName = "dashboard", 
        icon = icon("dashboard"),
        badgeLabel = "Home", 
        badgeColor = "green"
      ),
      menuItem(
        "📈 Results", 
        tabName = "results", 
        icon = icon("table")
      ),
      menuItem(
        "📊 Visualizations", 
        tabName = "viz", 
        icon = icon("chart-bar")
      ),
      menuItem(
        "ℹ️ About", 
        tabName = "about", 
        icon = icon("info-circle")
      )
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        /* Main styling */
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
          font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        }
        
        /* Box styling */
        .box {
          border-radius: 10px;
          box-shadow: 0 4px 12px rgba(0,0,0,0.08);
          border-top: 3px solid #3c8dbc;
          margin-bottom: 20px;
        }
        
        .box-header {
          border-radius: 10px 10px 0 0;
          padding: 15px;
          background-color: #ffffff !important;
        }
        
        .box-title {
          font-weight: 600;
          font-size: 16px;
          color: #333;
        }
        
        /* Value boxes */
        .small-box {
          border-radius: 10px;
          box-shadow: 0 3px 8px rgba(0,0,0,0.1);
          border-bottom: 4px solid rgba(0,0,0,0.1);
        }
        
        .small-box .icon {
          font-size: 70px;
          opacity: 0.9;
        }
        
        /* Buttons */
        .btn-lg {
          padding: 12px 30px;
          font-size: 16px;
          font-weight: 600;
          border-radius: 6px;
        }
        
        .btn-success {
          background: linear-gradient(to right, #00b09b, #96c93d);
          border: none;
        }
        
        .btn-primary {
          background: linear-gradient(to right, #2196F3, #21CBF3);
          border: none;
        }
        
        .btn-warning {
          background: linear-gradient(to right, #FF9800, #FFB74D);
          border: none;
        }
        
        /* Tables */
        .dataTables_wrapper {
          border-radius: 8px;
          overflow: hidden;
          box-shadow: 0 2px 5px rgba(0,0,0,0.05);
        }
        
        /* Status indicators */
        .status-indicator {
          display: inline-block;
          width: 12px;
          height: 12px;
          border-radius: 50%;
          margin-right: 8px;
        }
        
        .status-ready {
          background-color: #4CAF50;
        }
        
        .status-warning {
          background-color: #FF9800;
        }
        
        .status-error {
          background-color: #F44336;
        }
        
        /* Custom header */
        .main-header .logo {
          background-color: #222d32;
          font-weight: bold;
        }
        
        .main-header .navbar {
          background-color: #3c8dbc;
        }
        
        /* Progress bar */
        .progress-bar {
          border-radius: 3px;
          background: linear-gradient(to right, #4CAF50, #8BC34A);
        }
        
        /* Footer */
        .footer {
          position: fixed;
          bottom: 0;
          width: 100%;
          background-color: #222d32;
          color: #b8c7ce;
          padding: 10px 20px;
          text-align: center;
          font-size: 12px;
          border-top: 1px solid #4b646f;
          z-index: 1000;
        }
      "))
    ),
    
    tabItems(
      # ---------------------- Data Upload Tab ----------------
      tabItem(
        tabName = "upload",
        
        fluidRow(
          column(
            width = 12,
            div(
              class = "callout callout-info",
              style = "background-color: #e8f4f8; border-left: 5px solid #3c8dbc; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
              h4(icon("info-circle"), " Instructions", style = "margin-top: 0;"),
              p("1. Upload your panel data Excel file (containing id, time, y, x variables)", br(),
                "2. Upload your spatial weights matrix W Excel file", br(),
                "3. Specify the column names for your data", br(),
                "4. Click 'Preview Data' to verify your uploads")
            )
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("database"), "Upload Panel Data"),
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            collapsible = FALSE,
            
            fileInput(
              "data_file",
              "Choose Excel file containing panel data",
              accept = c(".xlsx", ".xls"),
              buttonLabel = "Browse...",
              placeholder = "No file selected"
            ),
            
            fluidRow(
              column(6,
                     numericInput(
                       "data_sheet",
                       "Sheet number",
                       value = 1,
                       min   = 1,
                       width = "100%"
                     )
              ),
              column(6,
                     textInput(
                       "id_col",
                       "ID column name",
                       value = "id",
                       placeholder = "e.g., id, region_id",
                       width = "100%"
                     )
              )
            ),
            
            fluidRow(
              column(6,
                     textInput(
                       "time_col",
                       "Time column name",
                       value = "time",
                       placeholder = "e.g., time, year",
                       width = "100%"
                     )
              ),
              column(6,
                     textInput(
                       "y_col",
                       "Dependent variable (y)",
                       value = "y",
                       placeholder = "e.g., y, gdp",
                       width = "100%"
                     )
              )
            ),
            
            textInput(
              "x_col",
              "Explanatory variable (x)",
              value = "x",
              placeholder = "e.g., x, investment",
              width = "100%"
            )
          ),
          
          box(
            title = span(icon("th"), "Upload Spatial Weights Matrix W"),
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            collapsible = FALSE,
            
            fileInput(
              "w_file",
              "Choose Excel file containing W matrix",
              accept = c(".xlsx", ".xls"),
              buttonLabel = "Browse...",
              placeholder = "No file selected"
            ),
            
            numericInput(
              "w_sheet",
              "Sheet number (W matrix)",
              value = 1,
              min   = 1
            ),
            
            hr(),
            
            div(
              style = "text-align: center;",
              actionButton(
                "preview_data",
                span(icon("eye"), "Preview Data & W Matrix"),
                class = "btn-primary",
                width = "100%",
                style = "margin-bottom: 10px;"
              ),
              
              div(
                id = "upload_status",
                style = "margin-top: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
                uiOutput("status_panel")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("table"), "Panel Data Preview"),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            withSpinner(
              DT::dataTableOutput("data_preview"),
              type = 4,
              color = "#3c8dbc"
            ),
            footer = textOutput("data_info")
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("project-diagram"), "Spatial Weights Matrix W Preview"),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            withSpinner(
              DT::dataTableOutput("w_preview"),
              type = 4,
              color = "#3c8dbc"
            ),
            footer = textOutput("w_matrix_info")
          )
        )
      ),
      
      # ---------------------- Run Analysis Tab ----------------
      tabItem(
        tabName = "run",
        
        fluidRow(
          column(
            width = 12,
            div(
              class = "callout callout-success",
              style = "background-color: #e8f6e8; border-left: 5px solid #4CAF50; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
              h4(icon("play-circle"), " Ready to Analyze", style = "margin-top: 0;"),
              p("Click the button below to run the analysis. This will execute all three estimation methods (QMLE, GMM, INLA) and generate results.")
            )
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("rocket"), "Analysis Control Panel"),
            status = "success",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(
                width = 6,
                div(
                  style = "text-align: center; padding: 30px;",
                  h4("Run Full Analysis", style = "margin-bottom: 20px;"),
                  actionButton(
                    "run_analysis",
                    span(icon("play"), " Start Analysis"),
                    class = "btn-success btn-lg",
                    style = "padding: 15px 40px; font-size: 18px;"
                  ),
                  br(), br(),
                  uiOutput("analysis_progress")
                )
              ),
              column(
                width = 6,
                div(
                  style = "padding: 20px; border-left: 1px solid #eee;",
                  h4(icon("clipboard-check"), "Checklist"),
                  tags$ul(
                    style = "list-style-type: none; padding-left: 0;",
                    tags$li(
                      style = "margin-bottom: 10px;",
                      uiOutput("checklist_panel")
                    ),
                    tags$li(
                      style = "margin-bottom: 10px;",
                      uiOutput("checklist_w")
                    ),
                    tags$li(
                      style = "margin-bottom: 10px;",
                      uiOutput("checklist_analysis")
                    )
                  ),
                  hr(),
                  h5("Methods to be executed:"),
                  tags$span(class = "label label-primary", "QMLE"),
                  tags$span(class = "label label-warning", "GMM"),
                  tags$span(class = "label label-success", "INLA")
                )
              )
            ),
            
            hr(),
            
            uiOutput("analysis_status_box")
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("history"), "Analysis Log"),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            verbatimTextOutput("analysis_log")
          )
        )
      ),
      
      # ---------------------- Dashboard Tab ------------------
      tabItem(
        tabName = "dashboard",
        
        fluidRow(
          valueBoxOutput("n_units",    width = 3),
          valueBoxOutput("n_time",     width = 3),
          valueBoxOutput("total_obs",  width = 3),
          valueBoxOutput("w_dim",      width = 3)
        ),
        
        fluidRow(
          column(
            width = 8,
            box(
              title = span(icon("chart-line"), "Model Comparison"),
              status = "info",
              solidHeader = TRUE,
              width = 12,
              withSpinner(
                plotlyOutput("model_comparison_plot"),
                type = 4,
                color = "#3c8dbc"
              )
            )
          ),
          column(
            width = 4,
            box(
              title = span(icon("tachometer-alt"), "Quick Stats"),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              uiOutput("quick_stats")
            ),
            box(
              title = span(icon("lightbulb"), "Tips"),
              status = "warning",
              solidHeader = TRUE,
              width = 12,
              tags$ul(
                tags$li("Check the Results tab for detailed parameter estimates"),
                tags$li("Use Visualizations tab for interactive plots"),
                tags$li("All results can be downloaded from the Results tab"),
                tags$li("Make sure your W matrix is properly standardized")
              )
            )
          )
        )
      ),
      
      # ---------------------- Results Tab --------------------
      tabItem(
        tabName = "results",
        
        fluidRow(
          box(
            title = span(icon("table"), "Estimation Results"),
            status = "success",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            div(
              style = "margin-bottom: 15px;",
              downloadButton("download_results", "Download Results (CSV)", class = "btn-primary"),
              downloadButton("download_report", "Download Report (HTML)", class = "btn-success", style = "margin-left: 10px;")
            ),
            withSpinner(
              DT::dataTableOutput("results_table"),
              type = 4,
              color = "#3c8dbc"
            )
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("fire"), "Parameter Heatmap"),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            withSpinner(
              plotlyOutput("parameter_heatmap"),
              type = 4,
              color = "#3c8dbc"
            )
          )
        )
      ),
      
      # ---------------------- Visualizations Tab -------------
      tabItem(
        tabName = "viz",
        
        fluidRow(
          box(
            title = span(icon("map-marked-alt"), "Spatial Parameters"),
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            withSpinner(
              plotlyOutput("spatial_plot"),
              type = 4,
              color = "#3c8dbc"
            )
          ),
          box(
            title = span(icon("clock"), "Temporal Parameters"),
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            withSpinner(
              plotlyOutput("temporal_plot"),
              type = 4,
              color = "#3c8dbc"
            )
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("chart-bar"), "Beta Coefficients Comparison"),
            status = "success",
            solidHeader = TRUE,
            width = 12,
            withSpinner(
              plotlyOutput("beta_plot"),
              type = 4,
              color = "#3c8dbc"
            )
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("sliders-h"), "Interactive Controls"),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(4,
                     selectInput(
                       "plot_type",
                       "Select Plot Type:",
                       choices = c("Bar Chart", "Line Chart", "Scatter Plot"),
                       selected = "Bar Chart"
                     )
              ),
              column(4,
                     checkboxGroupInput(
                       "methods_show",
                       "Show Methods:",
                       choices = c("QMLE", "GMM", "INLA"),
                       selected = c("QMLE", "GMM", "INLA")
                     )
              ),
              column(4,
                     sliderInput(
                       "font_size",
                       "Font Size:",
                       min = 10,
                       max = 20,
                       value = 14
                     )
              )
            )
          )
        )
      ),
      
      # ---------------------- About Tab ----------------------
      tabItem(
        tabName = "about",
        
        fluidRow(
          box(
            title = span(icon("info-circle"), "About DSDPM Analyzer"),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            h3("Dynamic Spatial Panel Model Analyzer"),
            p("This dashboard implements three state-of-the-art estimation methods for Dynamic Spatial Panel Models (DSPM)."),
            
            hr(),
            
            h4(icon("cogs"), " Estimation Methods"),
            fluidRow(
              column(4,
                     div(
                       class = "panel panel-primary",
                       style = "border: 1px solid #ddd; border-radius: 5px; padding: 15px; height: 200px;",
                       h5(icon("calculator"), " QMLE", style = "color: #3c8dbc;"),
                       p("Quasi-Maximum Likelihood Estimation provides efficient parameter estimates for spatial panel models."),
                       tags$small("Recommended for: Balanced panels with normal errors")
                     )
              ),
              column(4,
                     div(
                       class = "panel panel-warning",
                       style = "border: 1px solid #ddd; border-radius: 5px; padding: 15px; height: 200px;",
                       h5(icon("balance-scale"), " GMM", style = "color: #f39c12;"),
                       p("Generalized Method of Moments offers robust estimation without distributional assumptions."),
                       tags$small("Recommended for: Large samples, heteroskedastic errors")
                     )
              ),
              column(4,
                     div(
                       class = "panel panel-success",
                       style = "border: 1px solid #ddd; border-radius: 5px; padding: 15px; height: 200px;",
                       h5(icon("project-diagram"), " INLA", style = "color: #00a65a;"),
                       p("Integrated Nested Laplace Approximation with BYM2 spatial and AR1 temporal effects."),
                       tags$small("Recommended for: Bayesian inference, small samples")
                     )
              )
            ),
            
            hr(),
            
            h4(icon("file-excel"), " Data Requirements"),
            tags$ul(
              tags$li("Panel data in Excel format with columns: ID, time, dependent variable (y), explanatory variable (x)"),
              tags$li("Spatial weights matrix W in Excel format (must be square matrix)"),
              tags$li("Both balanced and unbalanced panel data are supported"),
              tags$li("Missing values should be left as empty cells in Excel")
            ),
            
            hr(),
            
            h4(icon("users"), " Development Team"),
            fluidRow(
              column(6,
                     div(
                       style = "background: #f8f9fa; padding: 15px; border-radius: 5px;",
                       h5("Jaya", style = "color: #3c8dbc;"),
                       p("Lead Developer & Spatial Econometrics Specialist"),
                       tags$small("Email: mindra@unpad.ac.id")
                     )
              ),
              column(6,
                     div(
                       style = "background: #f8f9fa; padding: 15px; border-radius: 5px;",
                       h5("Folmer", style = "color: #3c8dbc;"),
                       p("Statistical Methods & Model Validation"),
                       tags$small("Email: -")
                     )
              )
            ),
            
            hr(),
            
            div(
              style = "text-align: center; padding: 20px; background: #222d32; color: white; border-radius: 5px;",
              h4("DSDPM Analyzer v1.0"),
              p("© 2025 Developed by Jaya and Folmer"),
              p("All rights reserved.")
            )
          )
        )
      )
    ),
    
    # Footer
    tags$div(
      class = "footer",
      "DSDPM Analyzer v1.0 | Developed by Jaya and Folmer | © 2025"
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values to store data and results
  values <- reactiveValues(
    panel_data    = NULL,
    w_matrix      = NULL,
    results       = NULL,
    analysis_done = FALSE,
    analysis_log  = ""
  )
  
  # Add to log
  add_to_log <- function(message) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    values$analysis_log <- paste(values$analysis_log, 
                                 paste0("[", timestamp, "] ", message), 
                                 sep = "\n")
  }
  
  # Read panel data from uploaded file
  observeEvent(input$data_file, {
    req(input$data_file)
    add_to_log(paste("Reading panel data from:", input$data_file$name))
    
    tryCatch({
      values$panel_data <- read_excel(
        input$data_file$datapath,
        sheet = input$data_sheet
      )
      add_to_log("✓ Panel data loaded successfully")
      
      # Update column names suggestions
      updateTextInput(session, "id_col", value = names(values$panel_data)[1])
      updateTextInput(session, "time_col", value = names(values$panel_data)[2])
      updateTextInput(session, "y_col", value = names(values$panel_data)[3])
      updateTextInput(session, "x_col", value = names(values$panel_data)[4])
      
    }, error = function(e) {
      add_to_log(paste("✗ Error reading panel data:", e$message))
      showNotification(
        paste("Error reading panel data:", e$message),
        type = "error"
      )
    })
  })
  
  # Read W matrix from uploaded file
  observeEvent(input$w_file, {
    req(input$w_file)
    add_to_log(paste("Reading W matrix from:", input$w_file$name))
    
    tryCatch({
      values$w_matrix <- read_W_matrix(
        input$w_file$datapath,
        sheet_W = input$w_sheet
      )
      add_to_log("✓ W matrix loaded successfully")
      
    }, error = function(e) {
      add_to_log(paste("✗ Error reading W matrix:", e$message))
      showNotification(
        paste("Error reading W matrix:", e$message),
        type = "error"
      )
    })
  })
  
  # Update status panel
  output$status_panel <- renderUI({
    has_data <- !is.null(values$panel_data)
    has_w <- !is.null(values$w_matrix)
    
    if (has_data && has_w) {
      tags$div(
        style = "color: #4CAF50;",
        icon("check-circle"),
        strong(" Ready for analysis"),
        br(),
        tags$small("Both panel data and W matrix are loaded")
      )
    } else if (has_data) {
      tags$div(
        style = "color: #FF9800;",
        icon("exclamation-triangle"),
        strong(" W matrix required"),
        br(),
        tags$small("Panel data loaded, waiting for W matrix")
      )
    } else if (has_w) {
      tags$div(
        style = "color: #FF9800;",
        icon("exclamation-triangle"),
        strong(" Panel data required"),
        br(),
        tags$small("W matrix loaded, waiting for panel data")
      )
    } else {
      tags$div(
        style = "color: #9E9E9E;",
        icon("info-circle"),
        strong(" Upload required"),
        br(),
        tags$small("Please upload both panel data and W matrix")
      )
    }
  })
  
  # Checklist items
  output$checklist_panel <- renderUI({
    has_data <- !is.null(values$panel_data)
    tagList(
      icon("check-circle", class = ifelse(has_data, "text-success", "text-muted")),
      " Panel data uploaded"
    )
  })
  
  output$checklist_w <- renderUI({
    has_w <- !is.null(values$w_matrix)
    tagList(
      icon("check-circle", class = ifelse(has_w, "text-success", "text-muted")),
      " W matrix uploaded"
    )
  })
  
  output$checklist_analysis <- renderUI({
    tagList(
      icon("check-circle", class = ifelse(values$analysis_done, "text-success", "text-muted")),
      " Analysis completed"
    )
  })
  
  # Run full analysis
  observeEvent(input$run_analysis, {
    req(values$panel_data, values$w_matrix)
    
    add_to_log("Starting full analysis...")
    
    # Show progress modal
    showModal(modalDialog(
      title = span(icon("spinner", class = "fa-spin"), " Running Analysis"),
      footer = NULL,
      size = "m",
      tags$div(
        style = "text-align: center;",
        h4("Please wait while the analysis runs..."),
        br(),
        tags$div(
          class = "progress",
          tags$div(
            class = "progress-bar progress-bar-striped active",
            role = "progressbar",
            style = "width: 100%;",
            "Processing"
          )
        ),
        br(),
        p("This may take several minutes depending on data size."),
        p("Do not close this window.")
      )
    ))
    
    tryCatch({
      # Run the analysis
      values$results <- estimate_all_from_excel(
        data_path  = input$data_file$datapath,
        W_path     = input$w_file$datapath,
        id_col     = input$id_col,
        time_col   = input$time_col,
        y_col      = input$y_col,
        x_col      = input$x_col,
        sheet_data = input$data_sheet,
        sheet_W    = input$w_sheet
      )
      
      values$analysis_done <- TRUE
      add_to_log("✓ Analysis completed successfully!")
      add_to_log(paste("Results generated for", nrow(values$results), "methods"))
      
      # Switch to results tab
      updateTabItems(session, "sidebar", "results")
      
      removeModal()
      
      showNotification(
        span(icon("check"), "Analysis completed successfully!"),
        type = "success",
        duration = 5
      )
      
    }, error = function(e) {
      removeModal()
      add_to_log(paste("✗ Analysis failed:", e$message))
      showNotification(
        span(icon("exclamation-triangle"), paste("Analysis failed:", e$message)),
        type = "error",
        duration = 10
      )
    })
  })
  
  # Analysis progress UI
  output$analysis_progress <- renderUI({
    if (values$analysis_done) {
      tags$div(
        style = "color: #4CAF50;",
        icon("check-circle", style = "font-size: 48px;"),
        h4("Analysis Complete!"),
        p("Results are ready in the Results tab.")
      )
    } else {
      tags$div(
        style = "color: #FF9800;",
        icon("clock", style = "font-size: 48px;"),
        h4("Ready to Run"),
        p("Click the button above to start analysis")
      )
    }
  })
  
  # Analysis status box
  output$analysis_status_box <- renderUI({
    if (values$analysis_done) {
      tags$div(
        class = "alert alert-success",
        style = "margin-bottom: 0;",
        icon("check-circle"),
        strong(" Analysis Status: Complete"),
        br(),
        paste("Last run:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        br(),
        paste("Methods executed:", ifelse(!is.null(values$results), paste(values$results$method, collapse = ", "), "None"))
      )
    } else if (!is.null(values$panel_data) && !is.null(values$w_matrix)) {
      tags$div(
        class = "alert alert-info",
        style = "margin-bottom: 0;",
        icon("info-circle"),
        strong(" Analysis Status: Ready"),
        br(),
        "All required data is uploaded. Click 'Start Analysis' to proceed."
      )
    } else {
      tags$div(
        class = "alert alert-warning",
        style = "margin-bottom: 0;",
        icon("exclamation-triangle"),
        strong(" Analysis Status: Not Ready"),
        br(),
        "Please upload both panel data and W matrix first."
      )
    }
  })
  
  # ----------------- Value boxes on Dashboard ----------------
  output$n_units <- renderValueBox({
    n <- if (!is.null(values$panel_data)) {
      length(unique(values$panel_data[[input$id_col]]))
    } else 0
    valueBox(
      n,
      "Spatial Units",
      icon  = icon("map-marker-alt"),
      color = "blue"
    )
  })
  
  output$n_time <- renderValueBox({
    n <- if (!is.null(values$panel_data)) {
      length(unique(values$panel_data[[input$time_col]]))
    } else 0
    valueBox(
      n,
      "Time Periods",
      icon  = icon("calendar-alt"),
      color = "green"
    )
  })
  
  output$total_obs <- renderValueBox({
    n <- if (!is.null(values$panel_data)) {
      nrow(values$panel_data)
    } else 0
    valueBox(
      n,
      "Total Observations",
      icon  = icon("database"),
      color = "purple"
    )
  })
  
  output$w_dim <- renderValueBox({
    n <- if (!is.null(values$w_matrix)) {
      nrow(values$w_matrix)
    } else 0
    valueBox(
      n,
      "W Matrix Dimension",
      icon  = icon("th"),
      color = "orange"
    )
  })
  
  # Quick stats
  output$quick_stats <- renderUI({
    if (values$analysis_done && !is.null(values$results)) {
      tags$div(
        h5("Latest Analysis Summary"),
        tags$table(
          class = "table",
          tags$tr(tags$td("Methods:"), tags$td(paste(values$results$method, collapse = ", "))),
          tags$tr(tags$td("Parameters:"), tags$td(ncol(values$results) - 1)),
          tags$tr(tags$td("Last run:"), tags$td(format(Sys.time(), "%H:%M:%S")))
        )
      )
    } else {
      tags$div(
        h5("No analysis yet"),
        p("Run analysis from the 'Run Analysis' tab")
      )
    }
  })
  
  # ----------------- Data previews ---------------------------
  output$data_preview <- DT::renderDataTable({
    req(values$panel_data)
    DT::datatable(
      head(values$panel_data, 20),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      class = 'display nowrap'
    )
  })
  
  output$data_info <- renderText({
    if (!is.null(values$panel_data)) {
      paste("Showing first 20 rows of", nrow(values$panel_data), 
            "rows and", ncol(values$panel_data), "columns")
    }
  })
  
  output$w_preview <- DT::renderDataTable({
    req(values$w_matrix)
    preview_size <- min(10, nrow(values$w_matrix))
    DT::datatable(
      as.data.frame(
        values$w_matrix[
          1:preview_size,
          1:preview_size
        ]
      ),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      class = 'display nowrap'
    )
  })
  
  output$w_matrix_info <- renderText({
    if (!is.null(values$w_matrix)) {
      paste("W matrix:", nrow(values$w_matrix), "×", ncol(values$w_matrix), 
            "(showing first 10×10)")
    }
  })
  
  # ----------------- Analysis log ----------------------------
  output$analysis_log <- renderText({
    values$analysis_log
  })
  
  # ----------------- Results table ---------------------------
  output$results_table <- DT::renderDataTable({
    req(values$results)
    DT::datatable(
      values$results,
      options  = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      rownames = FALSE
    ) %>%
      DT::formatRound(
        columns = sapply(values$results, is.numeric),
        digits  = 4
      ) %>%
      DT::formatStyle(
        columns = names(values$results),
        fontSize = '14px'
      )
  })
  
  # Download results
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("dsdpm_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      write.csv(values$results, file, row.names = FALSE)
    }
  )
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("dsdpm_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      # Create a simple HTML report
      report <- tags$html(
        tags$head(
          tags$title("DSDPM Analysis Report"),
          tags$style(HTML("
            body { font-family: Arial, sans-serif; margin: 40px; }
            h1 { color: #3c8dbc; }
            h2 { color: #00a65a; }
            table { border-collapse: collapse; width: 100%; }
            th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
            th { background-color: #f2f2f2; }
            .method { font-weight: bold; }
          "))
        ),
        tags$body(
          h1("DSDPM Analysis Report"),
          p("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          h2("Results"),
          renderTable(values$results)
        )
      )
      
      save_html(report, file)
    }
  )
  
  # ----------------- Model comparison plot -------------------
  output$model_comparison_plot <- renderPlotly({
    req(values$results)
    
    df_long <- values$results %>%
      tidyr::pivot_longer(
        cols      = -method,
        names_to  = "parameter",
        values_to = "value"
      ) %>%
      dplyr::filter(parameter %in% c(
        "delta", "tau", "eta", "lambda", "rho", "kappa"
      ))
    
    p <- ggplot(df_long, aes(x = parameter, y = value, fill = method)) +
      geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
      labs(
        title = "Parameter Estimates by Method",
        x     = "Parameter",
        y     = "Estimated Value",
        fill  = "Method"
      ) +
      theme_minimal(base_size = 14) +
      scale_fill_brewer(palette = "Set1") +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
    
    ggplotly(p) %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(size = 12)),
        margin = list(l = 50, r = 50, b = 50, t = 50)
      )
  })
  
  # ----------------- Parameter heatmap -----------------------
  output$parameter_heatmap <- renderPlotly({
    req(values$results)
    
    df_wide <- values$results %>%
      tibble::column_to_rownames("method") %>%
      dplyr::select(
        delta, tau, eta, lambda, rho, kappa,
        beta_x, beta_wx, beta_x_lag, beta_wx_lag
      )
    
    plot_ly(
      x = colnames(df_wide),
      y = rownames(df_wide),
      z = as.matrix(df_wide),
      type  = "heatmap",
      colorscale = "RdYlBu",
      reversescale = TRUE,
      hoverinfo = "x+y+z",
      colorbar = list(title = "Value")
    ) %>%
      layout(
        title = "Parameter Estimates Heatmap",
        xaxis = list(title = "", tickangle = 45),
        yaxis = list(title = ""),
        margin = list(l = 100, r = 50, b = 100, t = 50)
      )
  })
  
  # ----------------- Spatial parameters plot -----------------
  output$spatial_plot <- renderPlotly({
    req(values$results)
    
    # Filter selected methods
    selected_methods <- input$methods_show
    if (is.null(selected_methods)) selected_methods <- unique(values$results$method)
    
    spatial_params <- values$results %>%
      dplyr::filter(method %in% selected_methods) %>%
      dplyr::select(method, delta, lambda, kappa) %>%
      tidyr::pivot_longer(
        cols      = -method,
        names_to  = "parameter",
        values_to = "value"
      )
    
    plot_type <- input$plot_type
    
    if (plot_type == "Bar Chart") {
      p <- ggplot(spatial_params, aes(x = method, y = value, fill = parameter)) +
        geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
        labs(
          title = "Spatial Parameters (δ, λ, κ)",
          x     = "Method",
          y     = "Estimated Value",
          fill  = "Parameter"
        ) +
        theme_minimal(base_size = input$font_size)
    } else if (plot_type == "Line Chart") {
      p <- ggplot(spatial_params, aes(x = parameter, y = value, color = method, group = method)) +
        geom_line(size = 1.5) +
        geom_point(size = 3) +
        labs(
          title = "Spatial Parameters (δ, λ, κ)",
          x     = "Parameter",
          y     = "Estimated Value",
          color = "Method"
        ) +
        theme_minimal(base_size = input$font_size)
    } else {
      p <- ggplot(spatial_params, aes(x = method, y = value, color = parameter, size = value)) +
        geom_point() +
        labs(
          title = "Spatial Parameters (δ, λ, κ)",
          x     = "Method",
          y     = "Estimated Value",
          color = "Parameter",
          size  = "Value"
        ) +
        theme_minimal(base_size = input$font_size)
    }
    
    ggplotly(p) %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(size = 12)),
        margin = list(l = 50, r = 50, b = 50, t = 50)
      )
  })
  
  # ----------------- Temporal parameters plot ----------------
  output$temporal_plot <- renderPlotly({
    req(values$results)
    
    # Filter selected methods
    selected_methods <- input$methods_show
    if (is.null(selected_methods)) selected_methods <- unique(values$results$method)
    
    temporal_params <- values$results %>%
      dplyr::filter(method %in% selected_methods) %>%
      dplyr::select(method, tau, eta, rho) %>%
      tidyr::pivot_longer(
        cols      = -method,
        names_to  = "parameter",
        values_to = "value"
      )
    
    plot_type <- input$plot_type
    
    if (plot_type == "Bar Chart") {
      p <- ggplot(temporal_params, aes(x = method, y = value, fill = parameter)) +
        geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
        labs(
          title = "Temporal Parameters (τ, η, ρ)",
          x     = "Method",
          y     = "Estimated Value",
          fill  = "Parameter"
        ) +
        theme_minimal(base_size = input$font_size) +
        scale_fill_brewer(palette = "Set3")
    } else if (plot_type == "Line Chart") {
      p <- ggplot(temporal_params, aes(x = parameter, y = value, color = method, group = method)) +
        geom_line(size = 1.5) +
        geom_point(size = 3) +
        labs(
          title = "Temporal Parameters (τ, η, ρ)",
          x     = "Parameter",
          y     = "Estimated Value",
          color = "Method"
        ) +
        theme_minimal(base_size = input$font_size)
    } else {
      p <- ggplot(temporal_params, aes(x = method, y = value, color = parameter, size = value)) +
        geom_point() +
        labs(
          title = "Temporal Parameters (τ, η, ρ)",
          x     = "Method",
          y     = "Estimated Value",
          color = "Parameter",
          size  = "Value"
        ) +
        theme_minimal(base_size = input$font_size)
    }
    
    ggplotly(p) %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(size = 12)),
        margin = list(l = 50, r = 50, b = 50, t = 50)
      )
  })
  
  # ----------------- Beta coefficients plot ------------------
  output$beta_plot <- renderPlotly({
    req(values$results)
    
    # Filter selected methods
    selected_methods <- input$methods_show
    if (is.null(selected_methods)) selected_methods <- unique(values$results$method)
    
    beta_params <- values$results %>%
      dplyr::filter(method %in% selected_methods) %>%
      dplyr::select(method, beta_x, beta_wx, beta_x_lag, beta_wx_lag) %>%
      tidyr::pivot_longer(
        cols      = -method,
        names_to  = "parameter",
        values_to = "value"
      )
    
    plot_type <- input$plot_type
    
    if (plot_type == "Bar Chart") {
      p <- ggplot(beta_params, aes(x = parameter, y = value, fill = method)) +
        geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
        labs(
          title = "Beta Coefficients",
          x     = "Coefficient",
          y     = "Estimated Value",
          fill  = "Method"
        ) +
        theme_minimal(base_size = input$font_size) +
        scale_fill_brewer(palette = "Set1") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (plot_type == "Line Chart") {
      p <- ggplot(beta_params, aes(x = method, y = value, color = parameter, group = parameter)) +
        geom_line(size = 1.5) +
        geom_point(size = 3) +
        labs(
          title = "Beta Coefficients",
          x     = "Method",
          y     = "Estimated Value",
          color = "Coefficient"
        ) +
        theme_minimal(base_size = input$font_size)
    } else {
      p <- ggplot(beta_params, aes(x = method, y = value, color = parameter, size = abs(value))) +
        geom_point(alpha = 0.7) +
        labs(
          title = "Beta Coefficients",
          x     = "Method",
          y     = "Estimated Value",
          color = "Coefficient",
          size  = "Absolute Value"
        ) +
        theme_minimal(base_size = input$font_size)
    }
    
    ggplotly(p) %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(size = 12)),
        margin = list(l = 50, r = 50, b = 100, t = 50)
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

