# DSDPM final paper code — v3.4, R = 300

This folder contains the final R implementation aligned with the revised manuscript:

**Bayesian Estimation of the Dynamic Spatial Durbin Panel Model: Theory and Simulation Evidence from a Comparison with Quasi-Maximum Likelihood and the Generalized Method of Moments**

## Final main Monte Carlo design

- Panel dimensions: (N,T) in {(49,10), (49,20), (100,10), (100,20)}
- delta, tau, eta in {0, 0.2, 0.4}
- lambda = 0.3, rho = 0.5, kappa = 0.6
- sigma_epsilon^2 = 1, sigma_xi^2 = 0.5, sigma_zeta^2 = 0.5
- Joint nonsingularity and spectral-radius stability screening is applied before simulation.
- 23 admissible dependence configurations remain from the 27 candidate combinations.
- 23 x 4 = 92 admissible scenarios.
- R = 300 replications per scenario.
- 27,600 simulated datasets and 27,600 estimation runs per estimator.
- Main estimators: BE-INLA, QML/QMLE, and GMM.

## Separate INLA–MCMC validation

The Bayesian validation is intentionally separate from the 92-scenario main experiment.

- 4 prespecified dependence regimes x 2 panel dimensions = 8 validation scenarios.
- R = 100 matched replications per scenario.
- MCMC: 4 chains, 30,000 iterations, burn-in 10,000, thinning 2.
- The same likelihood, prior structure, spatial Jacobians, and admissible support are used for INLA and MCMC.

See `RUN_INLA_MCMC_VALIDATION.R`.

## Files

- `DSDPM_ALL_IN_ONE_v3_4.R` — self-contained model, DGP, QML, GMM, INLA, MCMC, stability, identification, metrics, and validation functions.
- `RUN_MAIN_R300.R` — final 92-scenario x 300-replication Monte Carlo.
- `RUN_INLA_MCMC_VALIDATION.R` — separate 8-scenario x 100-replication matched Bayesian validation.
- `RUN_IDENTIFICATION_ONLY.R` — local-identification diagnostics.
- `INSTALL_PACKAGES.R` — package installation helper.

## Recommended execution order

```r
source("INSTALL_PACKAGES.R")
source("RUN_IDENTIFICATION_ONLY.R")
source("RUN_MAIN_R300.R")
source("RUN_INLA_MCMC_VALIDATION.R")
```

The main Monte Carlo and MCMC validation are separate experiments and should not be pooled.

## Repository note

Files in the repository root predate this final R=300 package and are retained for reproducibility/history. For the revised paper, use the files in this `final_R300/` folder.
