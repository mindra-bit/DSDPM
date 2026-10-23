# Model and simulation design

## DSDPM

The implementation follows the final manuscript formulation with three connected components:

1. outcome equation with contemporaneous spatial dependence, temporal persistence, spatiotemporal feedback, and current/lagged local and spatially lagged covariates;
2. dynamic spatial disturbance equation;
3. time-invariant persistent spatial-effect equation.

The same row-standardized queen-contiguity spatial weights matrix is used in the simulation design.

## Main parameter values

Regression parameters:
- alpha = 0.5
- beta1 = beta2 = beta3 = beta4 = 0.5

Fixed disturbance/persistent-spatial parameters:
- lambda = 0.3
- rho = 0.5
- kappa = 0.6

Innovation variances:
- sigma_epsilon^2 = 1
- sigma_xi^2 = 0.5
- sigma_zeta^2 = 0.5

Varied outcome-dependence parameters:
- delta, tau, eta in {0, 0.2, 0.4}

The 3^3 grid produces 27 candidate dependence configurations. Joint nonsingularity and spectral-radius stability screening excludes four configurations:
- (0.4, 0.4, 0.2)
- (0.4, 0.2, 0.4)
- (0.2, 0.4, 0.4)
- (0.4, 0.4, 0.4)

This leaves 23 admissible dependence configurations. Combined with four panel dimensions, this gives 92 scenarios.

## Panel dimensions

- (N,T) = (49,10)
- (49,20)
- (100,10)
- (100,20)

## Main Monte Carlo

- 300 replications per admissible scenario
- 92 × 300 = 27,600 simulated datasets
- estimators: BE-INLA, QML/QMLE, GMM

## Separate Bayesian validation

The INLA–MCMC comparison is a separate prespecified validation design, not a subset of the 92 main scenarios.

Four regimes:
1. baseline: delta=tau=eta=0, rho=0.50
2. moderate combined: delta=0.40, tau=0.20, eta=0.20, rho=0.50
3. near outcome-stability boundary: delta=tau=eta=0.32, rho=0.50
4. near disturbance-stability boundary: delta=tau=eta=0.20, rho=0.64

Each regime is evaluated at (49,10) and (100,20), yielding 8 scenarios. Each uses 100 matched replications.
