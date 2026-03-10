**Bayesian Estimation of the Dynamic Spatial Durbin Panel Model Theory and Simulation Evidence from a Comparison with Quasi-Maximum Likelihood and the General Method of Moments**


**Overview**

This repository contains complete R code and reproducible workflows used in the manuscript:

“**Bayesian Estimation of the Dynamic Spatial Durbin Panel Model Theory and Simulation Evidence from a Comparison with Quasi-Maximum Likelihood and the General Method of Moments**”

The scripts support full replication of Monte Carlo simulations, model estimation (QMLE, GMM, and Bayesian INLA), and interactive visualization through Shiny dashboards.

Purpose of the Repository

- The goal of this repository is to make the full research workflow transparent and reusable, including:

- Data generation and simulation framework

- Implementation of Dynamic Spatial Durbin Panel Model estimators

- Model evaluation metrics and visualization

- Interactive dashboards for exploring results

All components are modular and can be reused or extended for future methodological or empirical work.

**File Structure and Description**

├── CompleteCode.R          → Full end-to-end analysis script (no dashboard required)

├── DashboardProgram.R      → Shiny dashboard for running DSDPM estimation interactively

├── DashboardResult.R       → Visualization of simulation results (interactive dashboard)

├── SimulationCode.R        → Monte Carlo simulation engine used in the paper

├── Data.xlsx               → Example panel dataset (for replication/demo)

├── W.xlsx                  → Example spatial weight matrix

└── RekapFull.xlsx          → Simulation output summary used for final analysis

**How to Use**

1️⃣ Install Required Packages
install.packages(c("Matrix","MASS","spdep","dplyr","tidyr","ggplot2",
                   "openxlsx","DT","shiny","shinydashboard","plotly"))


Install INLA (required for Bayesian estimation):

install.packages("INLA", repos="https://inla.r-inla-download.org/R/stable")

2️⃣ Run the Full Analysis from Script
source("CompleteCode.R")


This script:

- Loads data and spatial weights

- Estimates QMLE, GMM, and Bayesian INLA models

- Computes evaluation metrics

- Produces tables and figures used in the manuscript

3️⃣ Run the Shiny Dashboard (Estimation Interface)
source("DashboardProgram.R")

This version allows users to run the DSDPM interactively using uploaded data or the included examples.

4️⃣ Run the Simulation Analysis Dashboard
source("DashboardResult.R")

This dashboard visualizes estimator performance (bias, MAE, RMSE, parameter distributions) based on results from the paper or user-generated simulations.

5️⃣ Reproduce Monte Carlo Simulation
source("SimulationCode.R")
run_simulation(reps = 100)

Outputs will be saved in formatted files similar to RekapFull.xlsx.

**Outputs**

The file RekapFull.xlsx is required to visualize the results. It can be downloaded from:
https://docs.google.com/spreadsheets/d/186CpfU4CqJz82_GWYAgkHgapzSSMU__W?rtpof=true&usp=drive_fs

Running the scripts generates:

- Summary statistics: bias, MAE, RMSE, median-based metrics

- Model stability comparisons across spatial and temporal dependencies

- Boxplots, density plots, and panel-grid diagnostics

- Exportable tables for supplementary materials

**Citation**

Please cite the paper if you use this repository:

Jaya, I. G. N. M., & Folmer, H. (2026). Bayesian Estimation of the Dynamic Spatial Durbin Panel Model Theory and Simulation Evidence from a Comparison with Quasi-Maximum Likelihood and the General Method of Moments.

Contact
Author	Affiliation	Email
I Gede Nyoman Mindra Jaya	Universitas Padjadjaran	
mindra@unpad.ac.id

Repository URL:
🔗 https://github.com/mindra-bit/DSDPM
