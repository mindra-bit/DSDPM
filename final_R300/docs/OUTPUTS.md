# Outputs

## Main Monte Carlo

`RUN_MAIN_R300.R` writes to `outputs/main_R300/`.

Key outputs include:
- scenario definitions and admissibility checks;
- replication-level estimates;
- estimator fit/convergence status;
- local-identification diagnostics;
- Monte Carlo performance summaries;
- reviewer/paper-support output tables;
- R session information.

The primary paper ranking uses 10 structural parameters across 92 scenarios, giving 920 parameter-scenario comparisons.

## INLA–MCMC validation

`RUN_INLA_MCMC_VALIDATION.R` writes to `outputs/inla_mcmc_validation/`.

It records matched posterior summaries, MCMC reference diagnostics, posterior agreement, coverage, and runtime for the separate 8-scenario validation experiment.
