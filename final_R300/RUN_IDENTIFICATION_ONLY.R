# RUN_IDENTIFICATION_ONLY.R
source("DSDPM_ALL_IN_ONE_v3_4.R")
OUT <- dsdpm_safe_dir("outputs/identification_only")
scenarios <- dsdpm_make_paper_factorial_design(
  N_values = c(49,100),
  T_values = c(10,20),
  dependence_levels = c(0,0.2,0.4),
  filter_admissible = TRUE
)
id <- dsdpm_run_identification_by_scenario(scenarios)
write.csv(id, file.path(OUT, "identification_diagnostics.csv"), row.names = FALSE)
print(id)