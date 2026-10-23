# INSTALL_PACKAGES.R
cran <- c("Matrix", "MASS", "spdep", "ggplot2", "coda", "posterior")
missing <- cran[!vapply(cran, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)

if (!requireNamespace("INLA", quietly = TRUE)) {
  install.packages(
    "INLA",
    repos = c(
      getOption("repos"),
      INLA = "https://inla.r-inla-download.org/R/stable"
    ),
    dep = TRUE
  )
}
