# RUN_MAIN_R300.R
# Final Monte Carlo design used in the revised DSDPM paper.
# 23 admissible dependence configurations x 4 panel dimensions = 92 scenarios.
# R = 300 independent replications per scenario.

rm(list = ls())
gc()

source("DSDPM_ALL_IN_ONE_v3_4.R")

OUT <- dsdpm_safe_dir("outputs/main_R300")

scenarios <- dsdpm_make_paper_factorial_design(
  N_values = c(49, 100),
  T_values = c(10, 20),
  dependence_levels = c(0, 0.2, 0.4),
  lambda = 0.3,
  rho = 0.5,
  kappa = 0.6,
  stability_tol = 0.999,
  filter_admissible = TRUE
)

if (nrow(scenarios) != 92L) {
  stop("Expected 92 admissible scenarios; found ", nrow(scenarios), ".")
}

write.csv(scenarios, file.path(OUT, "scenarios_R300.csv"), row.names = FALSE)

mc <- dsdpm_run_monte_carlo(
  scenarios = scenarios,
  R = 300L,
  methods = c("QMLE", "GMM", "INLA"),
  output_dir = file.path(OUT, "mc"),
  resume = TRUE,
  base_seed = 12345L,
  stability_tol = 0.999,
  verbose = TRUE
)

identification <- dsdpm_run_identification_by_scenario(
  scenarios = scenarios,
  stability_tol = 0.999,
  verbose = TRUE
)

write.csv(
  identification,
  file.path(OUT, "identification_diagnostics.csv"),
  row.names = FALSE
)

dsdpm_finalize_reviewer_outputs(
  scenarios = scenarios,
  mc = mc,
  output_dir = file.path(OUT, "paper_outputs"),
  identification = identification
)

dsdpm_session_info(file.path(OUT, "sessionInfo.txt"))

cat("\nFINAL R=300 RUN COMPLETE\n")
cat("Scenarios:", nrow(scenarios), "\n")
cat("Replications per scenario: 300\n")
cat("Simulated datasets:", nrow(scenarios) * 300L, "\n")
cat("Estimator runs per method:", nrow(scenarios) * 300L, "\n")
