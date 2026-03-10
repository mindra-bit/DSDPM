# ============================================================
# DSDPM Simulation Dashboard
# Visualization of Dynamic Spatial Durbin Panel Model Simulation Results
# Jaya and Folmer 2026
# ============================================================

library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)
library(DT)
library(shinythemes)
library(shinyWidgets)
library(scales)
library(ggthemes)
library(viridis)
library(shinycssloaders)
library(fresh)
library(ggpubr)

# NEW
library(plotly)
library(htmlwidgets)

# ------------------------------------------------------------
# 0. THEME
# ------------------------------------------------------------
create_theme <- create_theme(
  theme = "flatly",
  bs_vars_global(
    body_bg = "#ffffff",
    text_color = "#2c3e50"
  ),
  bs_vars_font(
    size_base = "14px"
  ),
  bs_vars_wells(
    bg = "#f8f9fa",
    border = "1px solid #e9ecef"
  )
)

# ------------------------------------------------------------
# 1. READ & PREPARE DATA
# ------------------------------------------------------------
df_raw <- read_excel("RekapFull.xlsx")

params <- c(
  "delta", "tau", "eta",
  "rho", "lambda", "kappa",
  "beta_x", "beta_wx", "beta_x_lag", "beta_wx_lag"
)

surface_methods <- c("GMM", "Bayesian", "QML")
surface_var_labels <- c(
  true_delta = "Delta (δ)",
  true_tau   = "Tau (τ)",
  true_eta   = "Eta (η)"
)

available_delta <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
available_tau   <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
available_eta   <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
available_T     <- sort(unique(df_raw$T))

param_label_map <- c(
  delta       = "δ",
  tau         = "τ",
  eta         = "η",
  rho         = "ρ",
  lambda      = "λ",
  kappa       = "κ",
  beta_x      = "β[x]",
  beta_wx     = "β[wx[t]]",
  beta_x_lag  = "β[x[t-1]]",
  beta_wx_lag = "β[wx[t-1]]"
)

metric_label_map <- c(
  Bias = "MedBias",
  RMSE = "RMedSE",
  MAE  = "MedAE"
)

metric_key <- function(x) {
  if (is.null(x)) return(x)
  if (x == "MedBias") return("Bias")
  if (x == "RMedSE") return("RMSE")
  if (x == "MedAE") return("MAE")
  x
}

metric_label <- function(x) {
  if (is.null(x)) return(x)
  out <- metric_label_map[x]
  ifelse(is.na(out), x, out)
}

theta_label_map <- c(
  delta       = "θ[δ]",
  tau         = "θ[τ]",
  eta         = "θ[η]",
  rho         = "θ[ρ]",
  lambda      = "θ[λ]",
  kappa       = "θ[κ]",
  beta_x      = "θ[β[x]]",
  beta_wx     = "θ[β[wx[t]]]",
  beta_x_lag  = "θ[β[x[t-1]]]",
  beta_wx_lag = "θ[β[wx[t-1]]]"
)

if (!("method" %in% names(df_raw))) {
  stop("Column 'method' not found in RekapFullRev.xlsx. Please ensure you have a 'method' column.")
}

df_long <- df_raw %>%
  mutate(
    NT = N * T,
    method = as.character(method),
    
    true_delta       = Delta,
    true_tau         = Tau,
    true_eta         = Eta,
    true_rho         = 0.5,
    true_lambda      = 0.3,
    true_kappa       = 0.6,
    true_beta_x      = 0.5,
    true_beta_wx     = 0.5,
    true_beta_x_lag  = 0.5,
    true_beta_wx_lag = 0.5
  ) %>%
  pivot_longer(
    cols      = all_of(params),
    names_to  = "param",
    values_to = "est"
  ) %>%
  mutate(
    truth = case_when(
      param == "delta"       ~ true_delta,
      param == "tau"         ~ true_tau,
      param == "eta"         ~ true_eta,
      param == "rho"         ~ true_rho,
      param == "lambda"      ~ true_lambda,
      param == "kappa"       ~ true_kappa,
      param == "beta_x"      ~ true_beta_x,
      param == "beta_wx"     ~ true_beta_wx,
      param == "beta_x_lag"  ~ true_beta_x_lag,
      param == "beta_wx_lag" ~ true_beta_wx_lag,
      TRUE ~ NA_real_
    ),
    error     = est - truth,
    abs_error = abs(error),
    
    param_lab = factor(param, levels = params, labels = param_label_map),
    
    # rounding to avoid floating artifacts for filtering/sliders
    true_delta = round(true_delta, 1),
    true_tau   = round(true_tau, 1),
    true_eta   = round(true_eta, 1),
    
    delta_fac = factor(true_delta, levels = available_delta,
                       labels = paste0("δ = ", available_delta)),
    tau_fac   = factor(true_tau,   levels = available_tau,
                       labels = paste0("τ = ", available_tau)),
    eta_fac   = factor(true_eta,   levels = available_eta,
                       labels = paste0("η = ", available_eta)),
    N_fac     = factor(N),
    T_fac     = factor(T, levels = available_T,
                       labels = paste0("T = ", available_T))
  ) %>%
  filter(!is.na(error), !is.na(est), !is.na(truth))

# ------------------------------------------------------------
# 2. UI
# ------------------------------------------------------------
ui <- fluidPage(
  use_theme(create_theme),
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        background-color: #ffffff;
        color: #2c3e50;
        line-height: 1.6;
      }
      .navbar {
        background: #000000 !important;
        border: none;
        box-shadow: 0 2px 8px rgba(0,0,0,0.3);
      }
      .navbar-brand {
        font-weight: 700;
        font-size: 1.4em;
        color: #ffffff !important;
      }
      .nav-tabs > li > a {
        color: #2c3e50;
        font-weight: 600;
        border-radius: 6px 6px 0 0;
      }
      .nav-tabs > li.active > a {
        background: #007acc !important;
        color: #ffffff !important;
        border: none;
        font-weight: 700;
      }
      .main-header {
        background: linear-gradient(135deg, #000000 0%, #007acc 100%);
        color: white;
        padding: 30px 0;
        margin-bottom: 25px;
        border-bottom: none;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
      }
      .content-panel {
        background: white;
        border-radius: 8px;
        box-shadow: 0 2px 12px rgba(0,0,0,0.08);
        margin-bottom: 25px;
        border: 1px solid #e9ecef;
        overflow: hidden;
      }
      .panel-header {
        background: linear-gradient(135deg, #007acc 0%, #005a9e 100%);
        color: white;
        padding: 18px 20px;
        margin: 0;
        font-weight: 700;
        font-size: 1.1em;
        border-bottom: 2px solid #ffa500;
      }
      .panel-body {
        padding: 25px;
        background: white;
      }
      .metric-card {
        background: linear-gradient(135deg, #007acc 0%, #005a9e 100%);
        color: white;
        border-radius: 8px;
        padding: 20px;
        text-align: center;
        box-shadow: 0 4px 8px rgba(0,122,204,0.2);
        transition: all 0.3s ease;
        border: none;
      }
      .metric-card:hover {
        transform: translateY(-3px);
        box-shadow: 0 6px 15px rgba(0,122,204,0.3);
      }
      .metric-value {
        font-size: 2em;
        font-weight: 800;
        color: #ffffff;
        margin: 10px 0;
        text-shadow: 1px 1px 2px rgba(0,0,0,0.2);
      }
      .metric-label {
        font-size: 0.9em;
        color: rgba(255,255,255,0.9);
        text-transform: uppercase;
        letter-spacing: 1px;
        font-weight: 600;
      }
      .section-title {
        color: #000000;
        border-bottom: 3px solid #ffa500;
        padding-bottom: 12px;
        margin: 30px 0 25px 0;
        font-weight: 800;
        font-size: 1.4em;
      }
      .control-label {
        font-weight: 700;
        color: #000000;
        margin-bottom: 10px;
        font-size: 1em;
      }
      .param-control-section {
        background: #f8f9fa;
        border-radius: 8px;
        padding: 18px;
        margin-bottom: 20px;
        border-left: 4px solid #007acc;
        border-right: 1px solid #e9ecef;
        border-top: 1px solid #e9ecef;
        border-bottom: 1px solid #e9ecef;
      }
      .btn-research {
        background: linear-gradient(135deg, #ffa500 0%, #ff8c00 100%);
        color: #000000;
        border: none;
        border-radius: 6px;
        font-weight: 700;
        padding: 12px 24px;
        transition: all 0.3s ease;
        box-shadow: 0 2px 6px rgba(255,165,0,0.3);
      }
      .btn-research:hover {
        background: linear-gradient(135deg, #ff8c00 0%, #ff7b00 100%);
        color: #000000;
        transform: translateY(-2px);
        box-shadow: 0 4px 12px rgba(255,165,0,0.4);
      }
      .well {
        background: white;
        border: 1px solid #e9ecef;
        border-radius: 8px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.05);
      }
      .awesome-checkbox input[type='checkbox']:checked + span {
        background: #007acc;
        border-color: #007acc;
      }
      .awesome-radio input[type='radio']:checked + span {
        background: #007acc;
        border-color: #007acc;
      }
      .bttn-gradient {
        background: linear-gradient(135deg, #007acc 0%, #005a9e 100%) !important;
        color: white !important;
        border: none !important;
      }
    "))
  ),
  
  div(class = "main-header",
      fluidRow(
        column(
          12,
          div(
            style = "padding-left: 25px; padding-right: 20px;",
            h1("DSDPM SIMULATION DASHBOARD",
               style = "margin: 0; color: #ffffff; font-weight: 800; letter-spacing: 1px;"),
            p("Visualization of Dynamic Spatial Durbin Panel Model Simulation Results",
              style = "margin: 10px 0 0 0; color: rgba(255,255,255,0.9); font-size: 1.1em; font-weight: 500;"),
            p("Developed by Jaya and Folmer 2026",
              style = "margin: 5px 0 0 0; color: rgba(255,255,255,0.7); font-size: 0.9em; font-style: italic;")
          )
        )
      )
  ),
  
  fluidRow(
    # Sidebar
    column(3,
           div(class = "content-panel",
               div(class = "panel-header", "ANALYSIS CONTROLS"),
               div(class = "panel-body",
                   
                   h4("Parameter Selection", class = "control-label"),
                   pickerInput(
                     "param",
                     "Select Parameters:",
                     choices  = setNames(params, param_label_map),
                     selected = c("rho", "lambda", "kappa"),
                     multiple = TRUE,
                     options = list(
                       `actions-box`          = TRUE,
                       `selected-text-format` = "count > 2",
                       `count-selected-text`  = "{0} parameters selected",
                       `live-search`          = TRUE
                     )
                   ),
                   
                   hr(style = "border-color: #e9ecef;"),
                   
                   h4("Method Selection", class = "control-label"),
                   awesomeCheckboxGroup(
                     "method",
                     "Estimation Methods:",
                     choices  = sort(unique(df_long$method)),
                     selected = sort(unique(df_long$method))[1:min(2, length(unique(df_long$method)))],
                     status   = "primary"
                   ),
                   
                   hr(style = "border-color: #e9ecef;"),
                   
                   h4("Autocorrelation Parameters", class = "control-label"),
                   
                   div(class = "param-control-section",
                       h5("Delta (δ) Values",
                          style = "color: #000000; font-weight: 700; margin-bottom: 15px;"),
                       awesomeCheckboxGroup(
                         "delta_values",
                         NULL,
                         choices  = available_delta,
                         selected = available_delta,
                         status   = "primary",
                         inline   = TRUE
                       )
                   ),
                   
                   div(class = "param-control-section",
                       h5("Tau (τ) Values",
                          style = "color: #000000; font-weight: 700; margin-bottom: 15px;"),
                       awesomeCheckboxGroup(
                         "tau_values",
                         NULL,
                         choices  = available_tau,
                         selected = available_tau,
                         status   = "primary",
                         inline   = TRUE
                       )
                   ),
                   
                   div(class = "param-control-section",
                       h5("Eta (η) Values",
                          style = "color: #000000; font-weight: 700; margin-bottom: 15px;"),
                       awesomeCheckboxGroup(
                         "eta_values",
                         NULL,
                         choices  = available_eta,
                         selected = available_eta,
                         status   = "primary",
                         inline   = TRUE
                       )
                   )
               )
           ),
           
           div(class = "content-panel",
               div(class = "panel-header", "EXPORT RESULTS"),
               div(class = "panel-body",
                   numericInput("export_width", "Figure Width (cm):",
                                value = 20, min = 10, max = 30),
                   numericInput("export_height", "Figure Height (cm):",
                                value = 15, min = 10, max = 25),
                   downloadButton("download_plot", "Download Figure (PNG / HTML for 3D)",
                                  class = "btn-research btn-block",
                                  style = "margin-bottom: 10px;"),
                   downloadButton("download_data", "Export Dataset (Excel)",
                                  class = "btn-research btn-block")
               )
           )
    ),
    
    # Main area
    column(9,
           fluidRow(
             column(3, div(class = "metric-card",
                           div(class = "metric-value", textOutput("param_count")),
                           div(class = "metric-label", "Parameters"))),
             column(3, div(class = "metric-card",
                           div(class = "metric-value", textOutput("method_count")),
                           div(class = "metric-label", "Methods"))),
             column(3, div(class = "metric-card",
                           div(class = "metric-value", textOutput("scenario_count")),
                           div(class = "metric-label", "Scenarios"))),
             column(3, div(class = "metric-card",
                           div(class = "metric-value", textOutput("obs_count")),
                           div(class = "metric-label", "Observations")))
           ),
           
           br(),
           
           tabsetPanel(
             type = "tabs",
             id   = "main_tabs",
             
             # Distribution Analysis
             tabPanel(
               "Distribution Analysis",
               icon = icon("chart-area"),
               div(class = "content-panel",
                   div(class = "panel-body",
                       fluidRow(
                         column(4,
                                selectInput("dist_type", "Plot Type:",
                                            choices  = c("Box Plot"     = "box",
                                                         "Violin Plot"  = "violin",
                                                         "Density Plot" = "density"),
                                            selected = "box")
                         ),
                         column(4,
                                numericInput("error_cap", "Error Limit:",
                                             value = 5, min = 0, max = 20, step = 0.5)
                         ),
                         column(4,
                                materialSwitch("show_stats", "Show Median Points",
                                               value = TRUE, status = "primary")
                         )
                       ),
                       withSpinner(
                         plotOutput("dist_plot", height = "600px"),
                         type = 4, color = "#007acc"
                       )
                   )
               )
             ),
             
             # Performance Metrics
             tabPanel(
               "Performance Metrics",
               icon = icon("table"),
               div(class = "content-panel",
                   div(class = "panel-body",
                       fluidRow(
                         column(3,
                                selectInput("metric_type", "Metric:",
                                            choices  = c("MedBias" = "Bias",
                                                         "RMedSE" = "RMSE",
                                                         "MedAE"  = "MAE"),
                                            selected = "RMSE")
                         ),
                         column(3,
                                selectInput("viz_type", "View:",
                                            choices  = c("Heatmap"   = "heatmap",
                                                         "Bar Chart" = "bar",
                                                         "Line Chart" = "line"),
                                            selected = "heatmap")
                         ),
                         column(3,
                                selectInput("group_by", "Compare by:",
                                            choices  = c("Method" = "method",
                                                         "Delta"  = "delta_fac",
                                                         "Tau"    = "tau_fac",
                                                         "Eta"    = "eta_fac"),
                                            selected = "method")
                         ),
                         column(3,
                                numericInput("metric_cap", "Maximum Value:",
                                             value = 2, min = 0.1, max = 10, step = 0.1)
                         )
                       ),
                       withSpinner(
                         plotOutput("metric_plot", height = "500px"),
                         type = 4, color = "#007acc"
                       ),
                       br(),
                       DTOutput("metric_table")
                   )
               )
             ),
             
             # Summary Statistics
             tabPanel(
               "Summary Statistics",
               icon = icon("calculator"),
               div(class = "content-panel",
                   div(class = "panel-body",
                       h4("Comprehensive Bias and RMSE Summary", class = "section-title"),
                       p("Average performance metrics across selected scenarios for each method and parameter"),
                       fluidRow(
                         column(6,
                                h5("Bias Summary", style = "color: #007acc; font-weight: 700;"),
                                DTOutput("bias_summary_table")
                         ),
                         column(6,
                                h5("RMSE Summary", style = "color: #ffa500; font-weight: 700;"),
                                DTOutput("rmse_summary_table")
                         )
                       ),
                       br(),
                       h5("Detailed Performance Summary", style = "color: #000000; font-weight: 700;"),
                       DTOutput("detailed_summary_table")
                   )
               )
             ),
             
             # Comparative Analysis
             tabPanel(
               "Comparative Analysis",
               icon = icon("chart-line"),
               div(class = "content-panel",
                   div(class = "panel-body",
                       fluidRow(
                         column(3,
                                selectInput("comp_metric", "Metric:",
                                            choices  = c("MedBias" = "Bias",
                                                         "RMedSE" = "RMSE",
                                                         "MedAE"  = "MAE"),
                                            selected = "RMSE")
                         ),
                         column(3,
                                selectInput("comp_x", "Horizontal axis:",
                                            choices = c("N"     = "N_fac",
                                                        "Delta" = "delta_fac",
                                                        "Tau"   = "tau_fac",
                                                        "Eta"   = "eta_fac"),
                                            selected = "N_fac")
                         ),
                         column(3,
                                selectInput("comp_row", "Facet rows (vertical):",
                                            choices = c("T" = "T_fac",
                                                        "N" = "N_fac"),
                                            selected = "T_fac")
                         ),
                         column(3,
                                numericInput("comp_cap", "Maximum Value:",
                                             value = 2, min = 0.1, max = 10, step = 0.1)
                         )
                       ),
                       helpText("Comparative boxplots of summary metrics by chosen horizontal factor (N / δ / τ / η),",
                                "with facet rows given by N or T, and parameters in columns. Values are capped at Maximum Value."),
                       withSpinner(
                         plotOutput("comparison_plot", height = "600px"),
                         type = 4, color = "#007acc"
                       )
                   )
               )
             ),
             
             # NEW: Metric Change (Sensitivity) with Line / Boxplot / 3D
             tabPanel(
               "Metric Change (Sensitivity)",
               icon = icon("sliders-h"),
               div(class = "content-panel",
                   div(class = "panel-body",
                       fluidRow(
                         column(3,
                                selectInput("sens_metric", "Metric:",
                                            choices = c("MedBias" = "Bias",
                                                        "RMedSE" = "RMSE",
                                                        "MedAE"  = "MAE"),
                                            selected = "RMSE")
                         ),
                         column(3,
                                selectInput("sens_plot_type", "Plot type:",
                                            choices = c("Line plot" = "line",
                                                        "Boxplot"  = "box",
                                                        "3D (δ, τ, η)" = "3d",
                                                        "3D Surface (δ × τ | fixed η)" = "3d_surface_eta",
                                                        "3D Surface (δ × η | fixed τ)" = "3d_surface_tau",
                                                        "3D Surface (τ × η | fixed δ)" = "3d_surface_delta"
                                            ),
                                            selected = "line")
                         ),
                         column(3,
                                selectInput("sens_vary", "Vary (x-axis for Line/Box):",
                                            choices = c("Delta (δ)" = "true_delta",
                                                        "Tau (τ)"   = "true_tau",
                                                        "Eta (η)"   = "true_eta"),
                                            selected = "true_delta")
                         ),
                         column(3,
                                numericInput("sens_cap", "Maximum Value (cap):",
                                             value = 2, min = 0.1, max = 10, step = 0.1)
                         ),
                         column(3,
                                numericInput("sens_cap_min", "Minimum Value (cap):",
                                             value = 0, min = -10, max = 10, step = 0.1)
                         )
                       ),
                       
                       fluidRow(
                         column(4,
                                selectInput("sens_agg", "Aggregation over N & T:",
                                            choices = c("Average over all N,T" = "all",
                                                        "Keep N,T separate"   = "byNT"),
                                            selected = "all")
                         ),
                         column(4,
                                conditionalPanel(
                                  condition = "input.sens_plot_type == 'box'",
                                  materialSwitch("sens_show_points", "Show points (boxplot jitter)",
                                                 value = TRUE, status = "primary")
                                )
                         ),
                         column(4,
                                conditionalPanel(
                                  condition = "input.sens_plot_type == 'box'",
                                  sliderInput("sens_box_width", "Box width:",
                                              min = 0.3, max = 1.0, value = 0.7, step = 0.1)
                                )
                         )
                       ),
                       
                       # Parameter selection for 3D (choose one θ for clarity)
                       conditionalPanel(
                         condition = "input.sens_plot_type == '3d' || input.sens_plot_type.indexOf('3d_surface') === 0",
                         fluidRow(
                           column(6,
                                  div(
                                    class = "param-control-section",
                                    h5("Multiple Parameter Grid",
                                       style = "color: #000000; font-weight: 700; margin-bottom: 12px;"),
                                    awesomeCheckboxGroup(
                                      "sens_param3d",
                                      "Parameter (θ):",
                                      choices = setNames(params, theta_label_map),
                                      selected = params,
                                      status = "primary",
                                      inline = FALSE
                                    ),
                                    div(
                                      style = "margin-top: 8px;",
                                      actionButton("sens_param3d_select_all", "Select All", class = "btn btn-default btn-sm"),
                                      actionButton("sens_param3d_clear_all", "Deselect All", class = "btn btn-default btn-sm")
                                    )
                                  )
                           ),
                           column(6,
                                  selectInput("sens_3d_mode", "3D: Marker mode:",
                                              choices = c("Points" = "markers",
                                                          "Points + Labels (hover)" = "markers"),
                                              selected = "markers")
                           )
                         )
                       ),
                       
                       # >>> ADDED: surface slice + method selection
                       conditionalPanel(
                         condition = "input.sens_plot_type.indexOf('3d_surface') === 0",
                         fluidRow(
                           column(4,
                                  selectInput(
                                    "sens_surface_fixed",
                                    "Fixed parameter:",
                                    choices = c("Eta (η)" = "true_eta",
                                                "Delta (δ)" = "true_delta",
                                                "Tau (τ)"   = "true_tau"),
                                    selected = "true_eta"
                                  )
                           ),
                           column(4,
                                  uiOutput("sens_surface_fixed_value_ui")
                           ),
                           column(4,
                                  pickerInput(
                                    "sens_surface_methods",
                                    "Surface methods:",
                                    choices = surface_methods,
                                    selected = surface_methods,
                                    multiple = TRUE,
                                    options = list(
                                      `actions-box` = TRUE,
                                      `selected-text-format` = "count > 2",
                                      `count-selected-text` = "{0} methods selected"
                                    )
                                  )
                           )
                         ),
                         fluidRow(
                           column(4,
                                  selectInput(
                                    "sens_surface_smooth",
                                    "Smoothing / interpolation:",
                                    choices = c("None" = "none", "LOESS" = "loess"),
                                    selected = "none"
                                  )
                           ),
                           column(4,
                                  conditionalPanel(
                                    condition = "input.sens_surface_smooth == 'loess'",
                                    sliderInput("sens_surface_span", "LOESS span:",
                                                min = 0.2, max = 1.0, value = 0.6, step = 0.05)
                                  )
                           ),
                           column(4,
                                  sliderInput("sens_surface_grid", "Grid size:",
                                              min = 10, max = 60, value = 25, step = 1)
                           )
                         ),
                         fluidRow(
                           column(12,
                                  uiOutput("sens_surface_xy_ui")
                           )
                         )
                       ),
                       # <<< ADDED
                       
                       hr(style = "border-color: #e9ecef;"),
                       
                       fluidRow(
                         column(4,
                                sliderInput("sens_delta_range", "Delta range (δ):",
                                            min = 0, max = 0.6, value = c(0, 0.6), step = 0.1)
                         ),
                         column(4,
                                sliderInput("sens_tau_range", "Tau range (τ):",
                                            min = 0, max = 0.6, value = c(0, 0.6), step = 0.1)
                         ),
                         column(4,
                                sliderInput("sens_eta_range", "Eta range (η):",
                                            min = 0, max = 0.6, value = c(0, 0.6), step = 0.1)
                         )
                       ),
                       
                       helpText(
                         "Line plot: average metric versus δ/τ/η. Boxplot: distribution of the metric for each δ/τ/η value.",
                         "3D: axes X=δ, Y=τ, Z=η; point color represents the metric value.",
                         "3D Surface: fix one parameter (δ/τ/η), then plot the other two on X and Y."
                       ),
                       
                       withSpinner(
                         uiOutput("sens_plot_ui"),
                         type = 4, color = "#007acc"
                       ),
                       br(),
                       DTOutput("sens_table")
                   )
               )
             ),
             tabPanel(
               "Sensitivity Boxplot Grid",
               icon = icon("th"),
               div(class = "content-panel",
                   div(class = "panel-body",
                       fluidRow(
                         column(3,
                                selectInput("sb_metric", "Metric:",
                                            choices = c("MedBias" = "Bias",
                                                        "RMedSE" = "RMSE",
                                                        "MedAE"  = "MAE"),
                                            selected = "RMSE")
                         ),
                         column(3,
                                selectInput("sb_fixed_param", "Fixed parameter:",
                                            choices = c("Delta (δ)" = "delta",
                                                        "Tau (τ)"   = "tau",
                                                        "Eta (η)"   = "eta"),
                                            selected = "tau")
                         ),
                         column(3,
                                uiOutput("sb_fixed_value_ui")
                         ),
                         column(3,
                                uiOutput("sb_vertical_param_ui")
                         ),
                         column(3,
                                numericInput("sb_y_min", "Y min:", value = 0)
                         ),
                         column(3,
                                numericInput("sb_y_max", "Y max:", value = 1)
                         )
                         ,
                         column(3,
                                numericInput("sb_dl_width", "Download width (cm):",
                                             value = 45, min = 10, max = 100)
                         ),
                         column(3,
                                numericInput("sb_dl_height", "Download height (cm):",
                                             value = 25, min = 10, max = 100)
                         )
                       ),
                       downloadButton("sb_download_plot", "Download Plot",
                                      class = "btn-research btn-block"),
                       helpText("Mapping: choose which remaining parameter is vertical (facet rows).",
                                "The other remaining parameter becomes the horizontal axis."),
                       withSpinner(
                         plotOutput("sb_plot", height = "700px"),
                         type = 4, color = "#007acc"
                       )
                   )
               )
             ),
             tabPanel(
               "Parameter Grid (Δ×τ×η)",
               icon = icon("project-diagram"),
               div(class = "content-panel",
                   div(class = "panel-body",
                       fluidRow(
                         column(3,
                                tagList(
                                  pickerInput(
                                    "pf_param",
                                    "Parameter (θ):",
                                    choices = setNames(params, param_label_map),
                                    selected = params,
                                    multiple = TRUE,
                                    options = list(
                                      `actions-box` = TRUE,
                                      `live-search` = TRUE,
                                      `selected-text-format` = "count > 2",
                                      `count-selected-text` = "{0} parameters selected"
                                    )
                                  ),
                                  div(
                                    style = "margin-top: 6px;",
                                    actionButton("pf_param_select_all", "Select All", class = "btn btn-default btn-sm"),
                                    actionButton("pf_param_clear_all", "Deselect All", class = "btn btn-default btn-sm")
                                  )
                                )
                         ),
                         column(3,
                                selectInput("pf_metric", "Metric:",
                                            choices = c("MedBias" = "Bias",
                                                        "RMedSE" = "RMSE",
                                                        "MedAE"  = "MAE"),
                                            selected = "RMSE")
                         ),
                         column(3,
                                selectInput("pf_plot_type", "Plot type:",
                                            choices = c("Boxplot" = "box",
                                                        "Line plot" = "line"),
                                            selected = "box")
                         ),
                         column(3,
                                pickerInput(
                                  "pf_N",
                                  "N (optional):",
                                  choices = sort(unique(df_raw$N)),
                                  selected = NULL,
                                  multiple = TRUE,
                                  options = list(`actions-box` = TRUE)
                                )
                         ),
                         column(3,
                                pickerInput(
                                  "pf_T",
                                  "T (optional):",
                                  choices = sort(unique(df_raw$T)),
                                  selected = NULL,
                                  multiple = TRUE,
                                  options = list(`actions-box` = TRUE)
                                )
                         ),
                         column(3,
                                numericInput("pf_y_min", "Y min:", value = 0)
                         ),
                         column(3,
                                numericInput("pf_y_max", "Y max:", value = 1)
                         )
                         ,
                         column(3,
                                numericInput("pf_fig_width", "Figure Width (cm):",
                                             value = 45, min = 10, max = 100)
                         ),
                         column(3,
                                numericInput("pf_fig_height", "Figure Height (cm):",
                                             value = 25, min = 10, max = 100)
                         )
                       ),
                       helpText("Facets: rows = Delta (δ), columns = Tau (τ), x-axis = Eta (η).",
                                "If N/T not selected, all combinations are used."),
                       withSpinner(
                         plotOutput("pf_plot", height = "700px"),
                         type = 4, color = "#007acc"
                       ),
                       downloadButton("pf_download_plot", "Download Plot",
                                      class = "btn-research btn-block"),
                       downloadButton("pf_download_table", "Download Table (XLSX)",
                                      class = "btn-research btn-block"),
                       br(),
                       DTOutput("pf_table")
                   )
               )
             ),
             tabPanel(
               "Coverage & Interval Length",
               icon = icon("chart-bar"),
               div(class = "content-panel",
                   div(class = "panel-body",
                       fluidRow(
                         column(3,
                                pickerInput(
                                  "cov_params",
                                  "Parameters (θ):",
                                  choices = setNames(params, param_label_map),
                                  selected = c("rho", "lambda", "kappa"),
                                  multiple = TRUE,
                                  options = list(`actions-box` = TRUE)
                                )
                         ),
                         column(3,
                                selectInput(
                                  "cov_level",
                                  "Interval level:",
                                  choices = c("90%" = "0.90", "95%" = "0.95", "99%" = "0.99"),
                                  selected = "0.95"
                                )
                         ),
                         column(3,
                                selectInput(
                                  "cov_interval_method",
                                  "Interval method:",
                                  choices = c("Auto (SE if available)" = "auto",
                                              "Analytic SE (θ̂ ± z·SE)" = "se",
                                              "Empirical percentile" = "empirical"),
                                  selected = "auto"
                                )
                         ),
                         column(3,
                                pickerInput(
                                  "cov_N",
                                  "N (optional):",
                                  choices = sort(unique(df_raw$N)),
                                  selected = NULL,
                                  multiple = TRUE,
                                  options = list(`actions-box` = TRUE)
                                )
                         ),
                         column(3,
                                pickerInput(
                                  "cov_T",
                                  "T (optional):",
                                  choices = sort(unique(df_raw$T)),
                                  selected = NULL,
                                  multiple = TRUE,
                                  options = list(`actions-box` = TRUE)
                                )
                         )
                       ),
                       helpText("Coverage rate: proportion of replications where the interval contains the true value.",
                                "Interval length: average width of the interval. Filters follow current Parameter Selection and δ/τ/η ranges."),
                       downloadButton("cov_download_table", "Download Table (XLSX)",
                                      class = "btn-research btn-block"),
                       br(),
                       withSpinner(
                         DTOutput("cov_table"),
                         type = 4, color = "#007acc"
                       )
                   )
               )
             )
           )
    )
  )
)

# ------------------------------------------------------------
# 3. SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  method_colors <- c(
    "INLA"     = "#0072B2",
    "Bayesian" = "#0072B2",
    "GMM"      = "#F0E442",
    "QML"     = "#D55E00"
  )
  surface_colors <- c(
    "GMM"      = "#F0E442",
    "Bayesian" = "#0072B2",
    "QML"     = "#E69F00"
  )
  
  sb_param_axis_labels <- c(
    delta = "Delta (δ)",
    tau   = "Tau (τ)",
    eta   = "Eta (η)"
  )
  
  sb_summary_stats <- reactive({
    dat <- df_filtered()
    req(nrow(dat) > 0)
    dat %>%
      group_by(N, T, method, param, param_lab,
               true_delta, true_tau, true_eta) %>%
      summarise(
        Bias = mean(error, na.rm = TRUE),
        RMSE = sqrt(mean(error^2, na.rm = TRUE)),
        MAE  = mean(abs_error, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  sb_levels_for <- function(param_name) {
    switch(param_name,
           delta = available_delta,
           tau   = available_tau,
           eta   = available_eta,
           available_delta)
  }
  
  sb_other_params <- reactive({
    fixed <- input$sb_fixed_param
    setdiff(c("delta", "tau", "eta"), fixed)
  })
  
  sb_mapping <- reactive({
    fixed <- input$sb_fixed_param
    vertical <- input$sb_vertical_param
    remaining <- setdiff(c("delta", "tau", "eta"), fixed)
    if (is.null(vertical) || !(vertical %in% remaining)) {
      vertical <- remaining[1]
    }
    horizontal <- setdiff(remaining, vertical)[1]
    list(fixed = fixed, x = horizontal, row = vertical)
  })
  
  pf_rep_stats <- reactive({
    req(input$pf_param)
    validate(need(length(input$pf_param) > 0, "Select at least one parameter (θ)."))
    dat <- df_long %>%
      filter(
        param %in% input$pf_param,
        method %in% input$method,
        true_delta %in% input$delta_values,
        true_tau %in% input$tau_values,
        true_eta %in% input$eta_values
      )
    
    if (!is.null(input$pf_N) && length(input$pf_N) > 0) {
      dat <- dat %>% filter(N %in% input$pf_N)
    }
    if (!is.null(input$pf_T) && length(input$pf_T) > 0) {
      dat <- dat %>% filter(T %in% input$pf_T)
    }
    
    if (nrow(dat) == 0) return(data.frame())
    
    rep_candidates <- c("rep", "Rep", "replication", "Replication", "sim", "Sim", "iteration", "Iteration", "iter", "Iter", "run", "Run")
    rep_col <- rep_candidates[rep_candidates %in% names(dat)][1]
    
    if (!is.na(rep_col) && length(rep_col) > 0) {
      dat <- dat %>% mutate(rep_id = .data[[rep_col]])
    } else {
      dat <- dat %>%
        group_by(N, T, method, true_delta, true_tau, true_eta) %>%
        mutate(rep_id = dplyr::row_number()) %>%
        ungroup()
    }
    
    dat %>%
      group_by(N, T, method, param, param_lab,
               true_delta, true_tau, true_eta, rep_id) %>%
      summarise(
        Bias = mean(error, na.rm = TRUE),
        RMSE = sqrt(mean(error^2, na.rm = TRUE)),
        MAE  = mean(abs_error, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  cov_rep_data <- reactive({
    req(input$cov_params)
    dat <- df_long %>%
      filter(
        param %in% input$cov_params,
        method %in% input$method,
        true_delta %in% input$delta_values,
        true_tau %in% input$tau_values,
        true_eta %in% input$eta_values
      )
    
    if (!is.null(input$cov_N) && length(input$cov_N) > 0) {
      dat <- dat %>% filter(N %in% input$cov_N)
    }
    if (!is.null(input$cov_T) && length(input$cov_T) > 0) {
      dat <- dat %>% filter(T %in% input$cov_T)
    }
    
    if (nrow(dat) == 0) return(data.frame())
    
    rep_candidates <- c("rep", "Rep", "replication", "Replication", "sim", "Sim", "iteration", "Iteration", "iter", "Iter", "run", "Run")
    rep_col <- rep_candidates[rep_candidates %in% names(dat)][1]
    if (!is.na(rep_col) && length(rep_col) > 0) {
      dat <- dat %>%
        mutate(rep_id = paste(N, T, .data[[rep_col]], sep = "|"))
    } else {
      dat <- dat %>%
        group_by(N, T, method, param, true_delta, true_tau, true_eta) %>%
        mutate(rep_id = paste(N, T, dplyr::row_number(), sep = "|")) %>%
        ungroup()
    }
    
    dat
  })
  
  cov_se_column <- function(param_name, data_cols) {
    candidates <- c(
      paste0("se_", param_name),
      paste0("SE_", param_name),
      paste0("se.", param_name),
      paste0("SE.", param_name),
      paste0("sd_", param_name),
      paste0("SD_", param_name)
    )
    candidates[candidates %in% data_cols][1]
  }
  
  cov_summary <- reactive({
    dat <- cov_rep_data()
    req(nrow(dat) > 0)
    
    level <- as.numeric(input$cov_level)
    alpha <- 1 - level
    z_val <- qnorm(1 - alpha / 2)
    
    method_pref <- input$cov_interval_method
    data_cols <- names(dat)
    
    results <- list()
    
    for (p in unique(dat$param)) {
      dsub <- dat %>% filter(param == p)
      se_col <- cov_se_column(p, data_cols)
      
      rep_metrics <- dsub %>%
        group_by(method, param, param_lab, true_delta, true_tau, true_eta, rep_id) %>%
        summarise(
          est = mean(est, na.rm = TRUE),
          truth = unique(truth),
          Bias = mean(error, na.rm = TRUE),
          RMSE = sqrt(mean(error^2, na.rm = TRUE)),
          MAE = mean(abs_error, na.rm = TRUE),
          se_rep = if (!is.na(se_col)) mean(.data[[se_col]], na.rm = TRUE) else NA_real_,
          .groups = "drop"
        )
      
      use_se <- !is.na(se_col) && (method_pref %in% c("auto", "se"))
      if (method_pref == "empirical") use_se <- FALSE
      
      if (use_se) {
        res <- rep_metrics %>%
          mutate(
            L = est - z_val * se_rep,
            U = est + z_val * se_rep,
            Covered = (truth >= L & truth <= U),
            Length = U - L,
            IntervalMethod = "Analytic SE"
          )
      } else {
        sd_est <- rep_metrics %>%
          group_by(method, param, param_lab, true_delta, true_tau, true_eta) %>%
          summarise(SD_est = sd(est, na.rm = TRUE), .groups = "drop")
        res <- rep_metrics %>%
          left_join(sd_est, by = c("method", "param", "param_lab", "true_delta", "true_tau", "true_eta")) %>%
          mutate(
            L = est - z_val * SD_est,
            U = est + z_val * SD_est,
            Covered = (truth >= L & truth <= U),
            Length = U - L,
            IntervalMethod = "Empirical SD"
          )
      }
      
      results[[p]] <- res
    }
    
    bind_rows(results)
  })
  # ---- Reactive data ----
  df_filtered <- reactive({
    req(input$param, input$method,
        input$delta_values, input$tau_values, input$eta_values)
    
    result <- df_long %>%
      filter(
        param      %in% input$param,
        method     %in% input$method,
        true_delta %in% input$delta_values,
        true_tau   %in% input$tau_values,
        true_eta   %in% input$eta_values
      )
    
    if (nrow(result) == 0) return(data.frame())
    result
  })
  
  summary_stats_reactive <- reactive({
    dat <- df_filtered()
    req(nrow(dat) > 0)
    
    dat %>%
      group_by(N, T, NT, method, param, param_lab,
               true_delta, true_tau, true_eta,
               delta_fac, tau_fac, eta_fac, N_fac, T_fac) %>%
      summarise(
        Bias   = mean(error, na.rm = TRUE),
        RMSE   = sqrt(mean(error^2, na.rm = TRUE)),
        MAE    = mean(abs_error, na.rm = TRUE),
        SD     = sd(error, na.rm = TRUE),
        n_obs  = n(),
        .groups = "drop"
      )
  })
  
  # Sensitivity tab should be able to access all theta parameters
  # regardless of the global parameter picker.
  summary_stats_sens_reactive <- reactive({
    req(input$method,
        input$delta_values, input$tau_values, input$eta_values)
    
    dat <- df_long %>%
      filter(
        method     %in% input$method,
        true_delta %in% input$delta_values,
        true_tau   %in% input$tau_values,
        true_eta   %in% input$eta_values
      )
    
    if (nrow(dat) == 0) return(data.frame())
    
    dat %>%
      group_by(N, T, NT, method, param, param_lab,
               true_delta, true_tau, true_eta,
               delta_fac, tau_fac, eta_fac, N_fac, T_fac) %>%
      summarise(
        Bias   = mean(error, na.rm = TRUE),
        RMSE   = sqrt(mean(error^2, na.rm = TRUE)),
        MAE    = mean(abs_error, na.rm = TRUE),
        SD     = sd(error, na.rm = TRUE),
        n_obs  = n(),
        .groups = "drop"
      )
  })
  
  line_data_nt_reactive <- reactive({
    stats <- summary_stats_reactive()
    req(nrow(stats) > 0)
    
    stats %>%
      group_by(method, param_lab, NT) %>%
      summarise(
        Bias = mean(Bias, na.rm = TRUE),
        RMSE = mean(RMSE, na.rm = TRUE),
        MAE  = mean(MAE,  na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  summary_table_reactive <- reactive({
    stats <- summary_stats_reactive()
    req(nrow(stats) > 0)
    
    stats %>%
      group_by(method, param, param_lab) %>%
      summarise(
        Mean_Bias = mean(Bias, na.rm = TRUE),
        SD_Bias   = sd(Bias, na.rm = TRUE),
        Mean_RMSE = mean(RMSE, na.rm = TRUE),
        SD_RMSE   = sd(RMSE, na.rm = TRUE),
        Min_Bias  = min(Bias, na.rm = TRUE),
        Max_Bias  = max(Bias, na.rm = TRUE),
        Min_RMSE  = min(RMSE, na.rm = TRUE),
        Max_RMSE  = max(RMSE, na.rm = TRUE),
        Total_Scenarios = n(),
        .groups = "drop"
      )
  })
  
  # ---- Summary metrics boxes ----
  output$param_count  <- renderText({ if (length(input$param)  == 0) "0" else as.character(length(input$param)) })
  output$method_count <- renderText({ if (length(input$method) == 0) "0" else as.character(length(input$method)) })
  
  output$sb_fixed_value_ui <- renderUI({
    req(input$sb_fixed_param)
    choices <- sb_levels_for(input$sb_fixed_param)
    selectInput("sb_fixed_value", "Fixed value:", choices = choices, selected = choices[1])
  })
  
  output$sb_vertical_param_ui <- renderUI({
    req(input$sb_fixed_param)
    choices <- sb_other_params()
    selectInput(
      "sb_vertical_param",
      "Vertical axis (facet rows):",
      choices = setNames(choices, sb_param_axis_labels[choices]),
      selected = choices[1]
    )
  })
  
  output$sb_plot <- renderPlot({
    sb_build_plot()
  })
  
  sb_build_plot <- reactive({
    req(input$sb_fixed_param, input$sb_fixed_value, input$sb_metric, input$param)
    sb_metric_key <- metric_key(input$sb_metric)
    map <- sb_mapping()
    fixed_col <- paste0("true_", map$fixed)
    x_col <- paste0("true_", map$x)
    row_col <- paste0("true_", map$row)
    
    dat <- sb_summary_stats() %>%
      filter(.data[[fixed_col]] == as.numeric(input$sb_fixed_value))
    if (nrow(dat) == 0) return(NULL)
    
    x_levels <- sb_levels_for(map$x)
    row_levels <- sb_levels_for(map$row)
    row_labels <- paste0(param_label_map[map$row], "=", row_levels)
    param_labels <- unname(param_label_map[input$param])
    if (length(param_labels) == 0) return(NULL)
    
    dat <- dat %>%
      mutate(
        Metric = .data[[sb_metric_key]],
        x_fac = factor(.data[[x_col]], levels = x_levels),
        row_fac = factor(.data[[row_col]], levels = row_levels, labels = row_labels)
      )
    
    y_min <- input$sb_y_min
    y_max <- input$sb_y_max
    y_limits <- NULL
    if (is.finite(y_min) && is.finite(y_max) && y_min < y_max) {
      y_limits <- c(y_min, y_max)
    }
    
    make_panel_plot <- function(param_label, show_row_labels) {
      dat_param <- dat %>%
        filter(param_lab == param_label)
      if (nrow(dat_param) == 0) return(NULL)
      
      p <- ggplot(dat_param,
                  aes(x = x_fac,
                      y = Metric,
                      fill = method)) +
        geom_boxplot(
          width = 0.7,
          position = position_dodge(0.8),
          linewidth = 0.25,
          outlier.size = 0.6
        ) +
        facet_grid(
          rows = vars(row_fac),
          cols = vars(param_lab),
          scales = "free_y",
          labeller = labeller(row = label_value, col = label_value)
        ) +
        scale_fill_manual(values = method_colors) +
        labs(x = NULL, y = NULL) +
        theme_bw(base_size = 14) +
        theme(
          legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          strip.placement = "outside",
          strip.text.y.right = if (show_row_labels) element_text(face = "bold", angle = 90) else element_blank(),
          strip.background = element_rect(fill = "grey90", color = "black")
        )
      
      if (!is.null(y_limits)) {
        p <- p + coord_cartesian(ylim = y_limits)
      }
      
      p
    }
    
    show_row <- seq_along(param_labels) == length(param_labels)
    plots <- mapply(make_panel_plot, param_labels, show_row, SIMPLIFY = FALSE)
    plots <- Filter(Negate(is.null), plots)
    if (length(plots) == 0) return(NULL)
    
    combined <- ggpubr::ggarrange(
      plotlist = plots,
      ncol = length(plots),
      common.legend = TRUE,
      legend = "bottom"
    )
    
    ggpubr::annotate_figure(
      combined,
      left = ggpubr::text_grob(metric_label(input$sb_metric), rot = 90),
      bottom = ggpubr::text_grob(sb_param_axis_labels[[map$x]])
    )
  })
  
  output$sb_download_plot <- downloadHandler(
    filename = function() {
      paste0("Sensitivity_Boxplot_Grid_", input$sb_metric, ".png")
    },
    content = function(file) {
      p <- sb_build_plot()
      req(!is.null(p))
      ggsave(
        filename = file,
        plot = p,
        width = input$sb_dl_width,
        height = input$sb_dl_height,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$pf_plot <- renderPlot({
    dat <- pf_rep_stats()
    req(nrow(dat) > 0)
    pf_param_title <- paste(unname(param_label_map[input$pf_param]), collapse = ", ")
    
    metric <- metric_key(input$pf_metric)
    dat <- dat %>%
      mutate(
        Metric = .data[[metric]],
        delta_fac = factor(true_delta, levels = available_delta,
                           labels = paste0("delta==", available_delta)),
        tau_fac   = factor(true_tau, levels = available_tau,
                           labels = paste0("tau==", available_tau)),
        eta_fac   = factor(true_eta, levels = available_eta)
      )
    
    y_min <- input$pf_y_min
    y_max <- input$pf_y_max
    y_limits <- NULL
    if (is.finite(y_min) && is.finite(y_max) && y_min < y_max) {
      y_limits <- c(y_min, y_max)
    }
    
    metric_disp <- metric_label(metric)
    if (input$pf_plot_type == "line") {
      dat_plot <- dat %>%
        group_by(method, param_lab, delta_fac, tau_fac, eta_fac) %>%
        summarise(Metric = mean(Metric, na.rm = TRUE), .groups = "drop")
      
      p <- ggplot(dat_plot, aes(x = eta_fac, y = Metric, color = method, group = method)) +
        geom_line(linewidth = 0.9) +
        geom_point(size = 1.8) +
        facet_grid(delta_fac ~ tau_fac + param_lab, scales = "free_y", labeller = label_parsed) +
        labs(
          x = "Eta (η)",
          y = metric_disp,
          title = paste("Parameter", pf_param_title, "-", metric_disp)
        ) +
        theme_bw(base_size = 14) +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
        ) +
        scale_color_manual(values = method_colors)
    } else {
      p <- ggplot(dat, aes(x = eta_fac, y = Metric, fill = method)) +
        geom_boxplot(alpha = 0.85, outlier.size = 0.6,
                     color = "#000000", linewidth = 0.25,
                     position = position_dodge(width = 0.8)) +
        facet_grid(delta_fac ~ tau_fac + param_lab, scales = "free_y", labeller = label_parsed) +
        labs(
          x = "Eta (η)",
          y = metric_disp,
          title = paste("Parameter", pf_param_title, "-", metric_disp)
        ) +
        theme_bw(base_size = 14) +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
        ) +
        scale_fill_manual(values = method_colors)
    }
    if (!is.null(y_limits)) {
      p <- p + coord_cartesian(ylim = y_limits)
    }
    
    p
  })
  
  output$pf_table <- renderDT({
    dat <- pf_rep_stats()
    req(nrow(dat) > 0)
    metric <- input$pf_metric
    
    dat_out <- dat %>%
      mutate(Metric = .data[[metric]]) %>%
      group_by(param_lab, method, true_delta, true_tau, true_eta) %>%
      summarise(
        Min = min(Metric, na.rm = TRUE),
        Max = max(Metric, na.rm = TRUE),
        SD = sd(Metric, na.rm = TRUE),
        Median = median(Metric, na.rm = TRUE),
        Mean = mean(Metric, na.rm = TRUE),
        N = n(),
        CI_Low = Mean - 1.96 * (SD / sqrt(N)),
        CI_High = Mean + 1.96 * (SD / sqrt(N)),
        .groups = "drop"
      ) %>%
      arrange(param_lab, method, true_delta, true_tau, true_eta)
    
    datatable(
      dat_out,
      options = list(pageLength = 10, scrollX = TRUE, dom = "t"),
      rownames = FALSE
    ) %>%
      formatRound(columns = c("Min", "Max", "SD", "Median", "Mean", "CI_Low", "CI_High"), digits = 4)
  })
  
  output$pf_download_plot <- downloadHandler(
    filename = function() {
      paste0("Parameter_Grid_", paste(input$pf_param, collapse = "-"), "_", input$pf_metric, ".png")
    },
    content = function(file) {
      dat <- pf_rep_stats()
      req(nrow(dat) > 0)
      pf_param_title <- paste(unname(param_label_map[input$pf_param]), collapse = ", ")
      
      metric <- metric_key(input$pf_metric)
      dat <- dat %>%
        mutate(
          Metric = .data[[metric]],
          delta_fac = factor(true_delta, levels = available_delta,
                             labels = paste0("delta==", available_delta)),
          tau_fac   = factor(true_tau, levels = available_tau,
                             labels = paste0("tau==", available_tau)),
          eta_fac   = factor(true_eta, levels = available_eta)
        )
      
      y_min <- input$pf_y_min
      y_max <- input$pf_y_max
      y_limits <- NULL
      if (is.finite(y_min) && is.finite(y_max) && y_min < y_max) {
        y_limits <- c(y_min, y_max)
      }
      
      metric_disp <- metric_label(metric)
      if (input$pf_plot_type == "line") {
        dat_plot <- dat %>%
          group_by(method, param_lab, delta_fac, tau_fac, eta_fac) %>%
          summarise(Metric = mean(Metric, na.rm = TRUE), .groups = "drop")
        
        p <- ggplot(dat_plot, aes(x = eta_fac, y = Metric, color = method, group = method)) +
          geom_line(linewidth = 0.9) +
          geom_point(size = 1.8) +
          facet_grid(delta_fac ~ tau_fac + param_lab, scales = "free_y", labeller = label_parsed) +
          labs(
            x = "Eta (η)",
            y = metric_disp,
            title = paste("Parameter", pf_param_title, "-", metric_disp)
          ) +
          theme_bw(base_size = 14) +
          theme(
            legend.position = "bottom",
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
          ) +
          scale_color_manual(values = method_colors)
      } else {
        p <- ggplot(dat, aes(x = eta_fac, y = Metric, fill = method)) +
          geom_boxplot(alpha = 0.85, outlier.size = 0.6,
                       color = "#000000", linewidth = 0.25,
                       position = position_dodge(width = 0.8)) +
          facet_grid(delta_fac ~ tau_fac + param_lab, scales = "free_y", labeller = label_parsed) +
          labs(
            x = "Eta (η)",
            y = metric_disp,
            title = paste("Parameter", pf_param_title, "-", metric_disp)
          ) +
          theme_bw(base_size = 14) +
          theme(
            legend.position = "bottom",
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
          ) +
          scale_fill_manual(values = method_colors)
      }
      
      if (!is.null(y_limits)) {
        p <- p + coord_cartesian(ylim = y_limits)
      }
      
      ggsave(
        filename = file,
        plot = p,
        width = input$pf_fig_width,
        height = input$pf_fig_height,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$pf_download_table <- downloadHandler(
    filename = function() {
      paste0("Parameter_Grid_Table_", paste(input$pf_param, collapse = "-"), "_", input$pf_metric, ".xlsx")
    },
    content = function(file) {
      dat <- pf_rep_stats()
      req(nrow(dat) > 0)
      metric <- input$pf_metric
      
      dat_out <- dat %>%
        mutate(Metric = .data[[metric]]) %>%
        group_by(param_lab, method, true_delta, true_tau, true_eta) %>%
        summarise(
          Min = min(Metric, na.rm = TRUE),
          Max = max(Metric, na.rm = TRUE),
          SD = sd(Metric, na.rm = TRUE),
          Median = median(Metric, na.rm = TRUE),
          Mean = mean(Metric, na.rm = TRUE),
          N = n(),
          CI_Low = Mean - 1.96 * (SD / sqrt(N)),
          CI_High = Mean + 1.96 * (SD / sqrt(N)),
          .groups = "drop"
        ) %>%
        arrange(param_lab, method, true_delta, true_tau, true_eta)
      
      openxlsx::write.xlsx(as.data.frame(dat_out), file, overwrite = TRUE)
    }
  )
  
  output$cov_table <- renderDT({
    dat <- cov_summary()
    req(nrow(dat) > 0)
    
    level <- as.numeric(input$cov_level)
    alpha <- 1 - level
    
    dat_out <- dat %>%
      group_by(method, param, param_lab, true_delta, true_tau, true_eta, IntervalMethod) %>%
      summarise(
        Min = min(est, na.rm = TRUE),
        Max = max(est, na.rm = TRUE),
        Mean = mean(est, na.rm = TRUE),
        Median = median(est, na.rm = TRUE),
        SD = sd(est, na.rm = TRUE),
        Mean_Bias = mean(Bias, na.rm = TRUE),
        Median_Bias = median(Bias, na.rm = TRUE),
        Mean_RMSE = mean(RMSE, na.rm = TRUE),
        Median_RMSE = median(RMSE, na.rm = TRUE),
        Mean_MAE = mean(MAE, na.rm = TRUE),
        Median_MAE = median(MAE, na.rm = TRUE),
        N = n(),
        CI_Low = quantile(est, probs = alpha / 2, na.rm = TRUE),
        CI_High = quantile(est, probs = 1 - alpha / 2, na.rm = TRUE),
        CoverageRate = mean(Covered, na.rm = TRUE),
        IntervalLength = mean(Length, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(param, method, true_delta, true_tau, true_eta)
    
    datatable(
      dat_out,
      options = list(pageLength = 10, scrollX = TRUE, dom = "t"),
      rownames = FALSE
    ) %>%
      formatRound(columns = c("Min", "Max", "Mean", "Median", "SD",
                              "Mean_Bias", "Median_Bias", "Mean_RMSE", "Median_RMSE",
                              "Mean_MAE", "Median_MAE",
                              "CI_Low", "CI_High", "CoverageRate", "IntervalLength"),
                  digits = 4)
  })
  
  output$cov_download_table <- downloadHandler(
    filename = function() {
      paste0("Coverage_Interval_Table_", input$cov_level, ".xlsx")
    },
    content = function(file) {
      dat <- cov_summary()
      req(nrow(dat) > 0)
      
      level <- as.numeric(input$cov_level)
      alpha <- 1 - level
      
      dat_out <- dat %>%
        group_by(method, param, param_lab, true_delta, true_tau, true_eta, IntervalMethod) %>%
        summarise(
          Min = min(est, na.rm = TRUE),
          Max = max(est, na.rm = TRUE),
          Mean = mean(est, na.rm = TRUE),
          Median = median(est, na.rm = TRUE),
          SD = sd(est, na.rm = TRUE),
          Mean_Bias = mean(Bias, na.rm = TRUE),
          Median_Bias = median(Bias, na.rm = TRUE),
          Mean_RMSE = mean(RMSE, na.rm = TRUE),
          Median_RMSE = median(RMSE, na.rm = TRUE),
          Mean_MAE = mean(MAE, na.rm = TRUE),
          Median_MAE = median(MAE, na.rm = TRUE),
          N = n(),
          CI_Low = quantile(est, probs = alpha / 2, na.rm = TRUE),
          CI_High = quantile(est, probs = 1 - alpha / 2, na.rm = TRUE),
          CoverageRate = mean(Covered, na.rm = TRUE),
          IntervalLength = mean(Length, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(param, method, true_delta, true_tau, true_eta)
      
      openxlsx::write.xlsx(as.data.frame(dat_out), file, overwrite = TRUE)
    }
  )
  
  output$scenario_count <- renderText({
    dat <- df_filtered()
    if (nrow(dat) == 0) return("0")
    as.character(n_distinct(paste(dat$N, dat$T, dat$true_delta, dat$true_tau, dat$true_eta)))
  })
  
  output$obs_count <- renderText({
    dat <- df_filtered()
    if (nrow(dat) == 0) return("0")
    format(nrow(dat), big.mark = ",")
  })
  
  # --------------------------------------------------------
  # Helpers: plots
  # --------------------------------------------------------
  make_dist_plot <- function(dat, error_cap, dist_type, show_stats, method_colors) {
    if (!is.na(error_cap) && error_cap > 0) {
      cap <- error_cap
      dat <- dat %>%
        mutate(error_plot = pmin(pmax(error, -cap), cap)) %>%
        filter(!is.na(error_plot))
    } else {
      dat <- dat %>%
        mutate(error_plot = error) %>%
        filter(!is.na(error_plot))
    }
    
    if (nrow(dat) == 0) {
      return(ggplot() + labs(title = "No data available for the selected filters") + theme_bw())
    }
    
    if (dist_type == "density") {
      ggplot(dat, aes(x = error_plot, fill = method, color = method)) +
        geom_density(alpha = 0.6, linewidth = 0.4) +
        labs(
          x = "Estimation Error",
          y = "Density",
          title = "Density Distribution of Estimation Errors"
        ) +
        theme_bw(base_size = 14) +
        scale_fill_manual(values = method_colors) +
        scale_color_manual(values = method_colors) +
        facet_grid(T_fac ~ param_lab, scales = "free")
    } else {
      p <- ggplot(dat, aes(x = N_fac, y = error_plot, fill = method)) +
        labs(
          x = "N",
          y = expression(hat(theta) - theta),
          fill = "Method",
          title = "Distribution of Estimation Errors"
        ) +
        theme_bw(base_size = 14) +
        theme(
          legend.position   = "bottom",
          plot.title        = element_text(face = "bold", size = 16, color = "#000000"),
          axis.text.x       = element_text(angle = 45, hjust = 1),
          panel.grid.minor  = element_blank(),
          panel.grid.major  = element_line(color = "#f0f0f0")
        ) +
        scale_fill_manual(values = method_colors) +
        facet_grid(T_fac ~ param_lab, scales = "free_y")
      
      if (dist_type == "box") {
        p <- p + geom_boxplot(alpha = 0.8, outlier.size = 0.5,
                              color = "#000000", linewidth = 0.25)
      } else if (dist_type == "violin") {
        p <- p + geom_violin(alpha = 0.8, trim = FALSE,
                             color = "#000000", linewidth = 0.25)
      }
      
      if (show_stats) {
        p <- p +
          stat_summary(
            fun = median, geom = "point",
            position = position_dodge(width = 0.9),
            size = 2, color = "#ffa500", shape = 18
          )
      }
      p
    }
  }
  
  make_metric_plot <- function(dat, metric, viz_type, metric_cap,
                               group_by, method_colors) {
    metric <- metric_key(metric)
    metric_disp <- metric_label(metric)
    
    dat_capped <- dat %>%
      mutate(!!metric := pmin(.data[[metric]], metric_cap))
    
    if (viz_type == "heatmap") {
      p <- ggplot(dat_capped, aes(x = N_fac, y = T, fill = .data[[metric]])) +
        geom_tile(color = "white", linewidth = 1) +
        scale_fill_gradient2(
          low  = "#007acc", mid = "white", high = "#ffa500",
          midpoint = 0, name = metric_disp,
          limits   = c(0, metric_cap)
        ) +
        labs(x = "N", y = "T",
             title = paste(metric_disp, "Heatmap"),
             subtitle = paste("Max value:", metric_cap)) +
        theme_bw(base_size = 12)
      
      if (group_by == "method") {
        p <- p + facet_grid(method ~ param_lab)
      } else if (group_by == "delta_fac") {
        p <- p + facet_grid(delta_fac ~ param_lab)
      } else if (group_by == "tau_fac") {
        p <- p + facet_grid(tau_fac ~ param_lab)
      } else if (group_by == "eta_fac") {
        p <- p + facet_grid(eta_fac ~ param_lab)
      }
      
    } else if (viz_type == "bar") {
      p <- ggplot(dat_capped, aes(x = N_fac, y = .data[[metric]], fill = method)) +
        geom_col(position = "dodge", alpha = 0.8) +
        labs(x = "Cross-sectional Units (N)", y = metric_disp,
             title = paste(metric_disp, "by Sample Size"),
             subtitle = paste("Max value:", metric_cap)) +
        theme_bw(base_size = 14) +
        scale_fill_manual(values = method_colors) +
        ylim(0, metric_cap)
      
      if (group_by == "method") {
        p <- p + facet_grid(. ~ param_lab)
      } else if (group_by == "delta_fac") {
        p <- p + facet_grid(delta_fac ~ param_lab)
      } else if (group_by == "tau_fac") {
        p <- p + facet_grid(tau_fac ~ param_lab)
      } else if (group_by == "eta_fac") {
        p <- p + facet_grid(eta_fac ~ param_lab)
      }
      
    } else { # line
      line_data <- line_data_nt_reactive() %>%
        mutate(!!metric := pmin(.data[[metric]], metric_cap))
      
      p <- ggplot(line_data, aes(x = NT, y = .data[[metric]],
                                 color = method, group = method)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2) +
        labs(x = "Sample Size (N × T)", y = metric_disp,
             title = paste("Average", metric_disp, "vs Sample Size"),
             subtitle = paste("Max value:", metric_cap),
             color = "Method") +
        theme_bw(base_size = 14) +
        scale_color_manual(values = method_colors) +
        theme(legend.position = "bottom") +
        ylim(0, metric_cap) +
        facet_wrap(~ param_lab, scales = "free_y")
    }
    p
  }
  
  make_comparison_plot <- function(dat, metric, method_colors,
                                   x_var, row_var, metric_cap) {
    metric <- metric_key(metric)
    metric_disp <- metric_label(metric)
    
    dat_capped <- dat %>%
      mutate(!!metric := pmin(.data[[metric]], metric_cap))
    
    x_lab_map <- c(
      N_fac     = "N",
      delta_fac = "Delta (δ)",
      tau_fac   = "Tau (τ)",
      eta_fac   = "Eta (η)"
    )
    x_lab <- x_lab_map[[x_var]]
    
    p <- ggplot(dat_capped, aes(x = .data[[x_var]], y = .data[[metric]], fill = method)) +
      geom_boxplot(alpha = 0.8, outlier.size = 0.5,
                   color = "#000000", linewidth = 0.25) +
      labs(
        x = x_lab,
        y = metric_disp,
        fill = "Method",
        title = paste("Comparative Analysis of", metric_disp),
        subtitle = paste("Horizontal axis:", x_lab,
                         "| Facet rows:", ifelse(row_var == "T_fac", "T", "N"),
                         "| Max value:", metric_cap)
      ) +
      theme_bw(base_size = 14) +
      theme(
        legend.position   = "bottom",
        plot.title        = element_text(face = "bold", size = 16, color = "#000000"),
        axis.text.x       = element_text(angle = 45, hjust = 1),
        panel.grid.minor  = element_blank(),
        panel.grid.major  = element_line(color = "#f0f0f0")
      ) +
      scale_fill_manual(values = method_colors)
    
    if (row_var == "T_fac") {
      p <- p + facet_grid(T_fac ~ param_lab, scales = "free_y")
    } else {
      p <- p + facet_grid(N_fac ~ param_lab, scales = "free_y")
    }
    p
  }
  
  # --------------------------------------------------------
  # Sensitivity datasets
  # --------------------------------------------------------
  sens_stats_filtered <- reactive({
    stats <- summary_stats_sens_reactive()
    req(nrow(stats) > 0)
    
    stats2 <- stats %>%
      dplyr::filter(
        dplyr::between(true_delta, input$sens_delta_range[1], input$sens_delta_range[2]),
        dplyr::between(true_tau,   input$sens_tau_range[1],   input$sens_tau_range[2]),
        dplyr::between(true_eta,   input$sens_eta_range[1],   input$sens_eta_range[2])
      )
    
    if (nrow(stats2) == 0) return(data.frame())
    stats2
  })
  
  sens_line_data <- reactive({
    stats2 <- sens_stats_filtered()
    req(nrow(stats2) > 0)
    
    xvar   <- input$sens_vary
    metric <- metric_key(input$sens_metric)
    
    if (input$sens_agg == "all") {
      out <- stats2 %>%
        group_by(method, param_lab, .data[[xvar]]) %>%
        summarise(
          Metric = mean(.data[[metric]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        rename(x = !!xvar)
    } else {
      out <- stats2 %>%
        group_by(method, param_lab, T_fac, N_fac, .data[[xvar]]) %>%
        summarise(
          Metric = mean(.data[[metric]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        rename(x = !!xvar)
    }
    out
  })
  
  sens_box_data <- reactive({
    stats2 <- sens_stats_filtered()
    req(nrow(stats2) > 0)
    
    xvar   <- input$sens_vary
    metric <- metric_key(input$sens_metric)
    
    stats2 %>%
      transmute(
        method,
        param_lab,
        T_fac,
        N_fac,
        x = .data[[xvar]],
        Metric = .data[[metric]]
      )
  })
  
  # 3D data: X=delta, Y=tau, Z=eta, color=Metric (scatter cloud)
  sens_3d_data <- reactive({
    stats2 <- sens_stats_filtered()
    req(nrow(stats2) > 0)
    
    metric <- metric_key(input$sens_metric)
    cap_max <- input$sens_cap
    cap_min <- input$sens_cap_min
    
    req(input$sens_param3d)
    theta_params <- input$sens_param3d
    validate(need(length(theta_params) > 0, "Select at least one theta parameter in Multiple Parameter Grid."))
    
    stats3 <- stats2 %>%
      filter(param %in% theta_params)
    
    if (nrow(stats3) == 0) return(data.frame())
    
    if (input$sens_agg == "all") {
      out <- stats3 %>%
        group_by(method, param, true_delta, true_tau, true_eta) %>%
        summarise(
          Metric = mean(.data[[metric]], na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      out <- stats3 %>%
        group_by(method, param, T_fac, N_fac, true_delta, true_tau, true_eta) %>%
        summarise(
          Metric = mean(.data[[metric]], na.rm = TRUE),
          .groups = "drop"
        )
    }
    
    out %>%
      mutate(
        Metric = pmin(pmax(Metric, cap_min), cap_max),
        theta  = unname(theta_label_map[param])
      )
  })
  
  # >>> ADDED: surface data (X=delta, Y=tau, Z=Metric) at fixed eta slice
  sens_surface_data <- reactive({
    req(startsWith(input$sens_plot_type, "3d_surface"))
    stats2 <- sens_stats_filtered()
    req(nrow(stats2) > 0)
    
    metric <- metric_key(input$sens_metric)
    cap_max <- input$sens_cap
    cap_min <- input$sens_cap_min
    
    req(input$sens_param3d)
    theta_params <- input$sens_param3d
    validate(need(length(theta_params) > 0, "Select at least one theta parameter in Multiple Parameter Grid."))
    
    req(input$sens_surface_fixed)
    req(input$sens_surface_fixed_values)
    fixed_var <- input$sens_surface_fixed
    fixed_vals <- input$sens_surface_fixed_values
    fixed_val <- round(as.numeric(fixed_vals[1]), 1)
    
    req(input$sens_surface_x, input$sens_surface_y)
    xvar <- input$sens_surface_x
    yvar <- input$sens_surface_y
    xy_choices <- setdiff(names(surface_var_labels), fixed_var)
    if (xvar == yvar || xvar == fixed_var || yvar == fixed_var) {
      xvar <- xy_choices[1]
      yvar <- if (length(xy_choices) > 1) xy_choices[2] else xy_choices[1]
    }
    
    methods_sel <- input$sens_surface_methods
    validate(
      need(!is.null(methods_sel) && length(methods_sel) > 0,
           "Select at least one surface method.")
    )
    
    base_stats <- stats2 %>%
      filter(param %in% theta_params) %>%
      filter(.data[[fixed_var]] == fixed_val)
    
    if (nrow(base_stats) == 0) {
      showNotification(
        "No data available for this filter combination. Check Parameter Selection and the δ/τ/η ranges.",
        type = "warning",
        duration = 4
      )
      return(data.frame())
    }
    
    available_methods <- intersect(methods_sel, unique(base_stats$method))
    if (length(available_methods) == 0) {
      available_methods <- unique(base_stats$method)
    }
    
    stats3 <- base_stats %>%
      filter(method %in% available_methods)
    
    # aggregate over N,T (and optionally keep method separate)
    # For surface, multiple methods -> multiple surfaces (traces)
    if (input$sens_agg == "all") {
      out <- stats3 %>%
        group_by(method, param, x = .data[[xvar]], y = .data[[yvar]]) %>%
        summarise(
          Metric = mean(.data[[metric]], na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      # if keep N,T separate, surface becomes messy; we still average over NT groups to keep a surface
      out <- stats3 %>%
        group_by(method, param, x = .data[[xvar]], y = .data[[yvar]]) %>%
        summarise(
          Metric = mean(.data[[metric]], na.rm = TRUE),
          .groups = "drop"
        )
    }
    
    out %>%
      mutate(
        Metric = pmin(pmax(Metric, cap_min), cap_max),
        theta  = unname(theta_label_map[param]),
        fixed_var = fixed_var,
        fixed_value = fixed_val,
        x_var = xvar,
        y_var = yvar,
        method = factor(method, levels = surface_methods)
      )
  })
  # <<< ADDED
  
  # --------------------------------------------------------
  # Dynamic UI for sensitivity plot output
  # --------------------------------------------------------
  output$sens_plot_ui <- renderUI({
    if (input$sens_plot_type == "3d") {
      plotlyOutput("sens_3d", height = "650px")
    } else if (startsWith(input$sens_plot_type, "3d_surface")) {
      # >>> ADDED
      uiOutput("sens_surface_multi_ui")
      # <<< ADDED
    } else {
      plotOutput("sens_plot", height = "650px")
    }
  })
  
  output$sens_surface_fixed_value_ui <- renderUI({
    req(startsWith(input$sens_plot_type, "3d_surface"))
    req(input$sens_surface_fixed)
    label <- surface_var_labels[[input$sens_surface_fixed]]
    choices <- switch(input$sens_surface_fixed,
                      true_delta = available_delta,
                      true_tau   = available_tau,
                      true_eta   = available_eta,
                      available_delta)
    pickerInput(
      "sens_surface_fixed_values",
      paste0("Fixed values: ", label),
      choices = choices,
      selected = choices[1],
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    )
  })
  
  output$sens_surface_xy_ui <- renderUI({
    req(startsWith(input$sens_plot_type, "3d_surface"))
    req(input$sens_surface_fixed)
    choices <- setdiff(names(surface_var_labels), input$sens_surface_fixed)
    choice_labels <- surface_var_labels[choices]
    x_default <- choices[1]
    y_default <- if (length(choices) > 1) choices[2] else choices[1]
    
    fluidRow(
      column(6,
             selectInput(
               "sens_surface_x",
               "Surface X:",
               choices = setNames(choices, choice_labels),
               selected = x_default
             )
      ),
      column(6,
             selectInput(
               "sens_surface_y",
               "Surface Y:",
               choices = setNames(choices, choice_labels),
               selected = y_default
             )
      )
    )
  })
  
  output$sens_surface_multi_ui <- renderUI({
    req(startsWith(input$sens_plot_type, "3d_surface"))
    vals <- input$sens_surface_fixed_values
    if (is.null(vals) || length(vals) == 0) return(NULL)
    panels <- lapply(seq_along(vals), function(i) {
      plotlyOutput(paste0("sens_3d_surface_", i), height = "650px")
    })
    do.call(tagList, panels)
  })
  
  observeEvent(input$sens_plot_type, {
    if (!startsWith(input$sens_plot_type, "3d_surface")) return()
    
    if (input$sens_plot_type == "3d_surface_eta") {
      updateSelectInput(session, "sens_surface_fixed", selected = "true_eta")
      updateSelectInput(session, "sens_surface_x", selected = "true_delta")
      updateSelectInput(session, "sens_surface_y", selected = "true_tau")
    } else if (input$sens_plot_type == "3d_surface_tau") {
      updateSelectInput(session, "sens_surface_fixed", selected = "true_tau")
      updateSelectInput(session, "sens_surface_x", selected = "true_delta")
      updateSelectInput(session, "sens_surface_y", selected = "true_eta")
    } else if (input$sens_plot_type == "3d_surface_delta") {
      updateSelectInput(session, "sens_surface_fixed", selected = "true_delta")
      updateSelectInput(session, "sens_surface_x", selected = "true_tau")
      updateSelectInput(session, "sens_surface_y", selected = "true_eta")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$sens_param3d_select_all, {
    updateAwesomeCheckboxGroup(
      session,
      "sens_param3d",
      selected = params
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$sens_param3d_clear_all, {
    updateAwesomeCheckboxGroup(
      session,
      "sens_param3d",
      selected = character(0)
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$pf_param_select_all, {
    updatePickerInput(
      session,
      "pf_param",
      selected = params
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$pf_param_clear_all, {
    updatePickerInput(
      session,
      "pf_param",
      selected = character(0)
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$sens_surface_fixed, {
    req(startsWith(input$sens_plot_type, "3d_surface"))
    choices <- switch(input$sens_surface_fixed,
                      true_delta = available_delta,
                      true_tau   = available_tau,
                      true_eta   = available_eta,
                      available_delta)
    updatePickerInput(session, "sens_surface_fixed_values", selected = choices[1])
  }, ignoreInit = TRUE)
  
  # --------------------------------------------------------
  # Outputs: Plots
  # --------------------------------------------------------
  output$dist_plot <- renderPlot({
    dat <- df_filtered()
    req(nrow(dat) > 0)
    make_dist_plot(dat, input$error_cap, input$dist_type, input$show_stats, method_colors)
  })
  
  output$metric_plot <- renderPlot({
    dat <- summary_stats_reactive()
    req(nrow(dat) > 0)
    make_metric_plot(dat, input$metric_type, input$viz_type, input$metric_cap, input$group_by, method_colors)
  })
  
  output$comparison_plot <- renderPlot({
    dat <- summary_stats_reactive()
    req(nrow(dat) > 0)
    make_comparison_plot(dat, input$comp_metric, method_colors, input$comp_x, input$comp_row, input$comp_cap)
  })
  
  # Sensitivity ggplot (line/box)
  output$sens_plot <- renderPlot({
    req(input$sens_plot_type != "3d")
    req(!startsWith(input$sens_plot_type, "3d_surface"))  # >>> ADDED
    
    cap_max <- input$sens_cap
    cap_min <- input$sens_cap_min
    metric_disp <- metric_label(metric_key(input$sens_metric))
    x_lab_map <- c(true_delta = "Delta (δ)", true_tau = "Tau (τ)", true_eta = "Eta (η)")
    x_lab <- x_lab_map[[input$sens_vary]]
    
    if (input$sens_plot_type == "line") {
      dat <- sens_line_data()
      req(nrow(dat) > 0)
      dat <- dat %>% mutate(Metric = pmin(pmax(Metric, cap_min), cap_max))
      
      p <- ggplot(dat, aes(x = x, y = Metric, color = method, group = method)) +
        geom_line(linewidth = 1.1) +
        geom_point(size = 2) +
        labs(
          x = x_lab,
          y = metric_disp,
          title = paste("Metric Change:", metric_disp, "vs", x_lab),
          subtitle = paste("Plot: Line | Cap:", cap_min, "to", cap_max,
                           "| δ:", paste(input$sens_delta_range, collapse = "–"),
                           "| τ:", paste(input$sens_tau_range, collapse = "–"),
                           "| η:", paste(input$sens_eta_range, collapse = "–")),
          color = "Method"
        ) +
        theme_bw(base_size = 14) +
        theme(legend.position = "bottom") +
        scale_color_manual(values = method_colors)
      
      if (input$sens_agg == "all") {
        p <- p + facet_wrap(~ param_lab, scales = "free_y")
      } else {
        p <- p + facet_grid(T_fac ~ param_lab, scales = "free_y")
      }
      p
      
    } else { # boxplot
      dat <- sens_box_data()
      req(nrow(dat) > 0)
      dat <- dat %>% mutate(Metric = pmin(pmax(Metric, cap_min), cap_max))
      dat <- dat %>% mutate(x_fac = factor(x, levels = sort(unique(x))))
      
      p <- ggplot(dat, aes(x = x_fac, y = Metric, fill = method)) +
        geom_boxplot(alpha = 0.85, outlier.size = 0.6,
                     color = "#000000", linewidth = 0.25,
                     width = input$sens_box_width,
                     position = position_dodge(width = 0.8)) +
        labs(
          x = x_lab,
          y = metric_disp,
          title = paste("Metric Distribution:", metric_disp, "across", x_lab),
          subtitle = paste("Plot: Boxplot | Cap:", cap_min, "to", cap_max,
                           "| δ:", paste(input$sens_delta_range, collapse = "–"),
                           "| τ:", paste(input$sens_tau_range, collapse = "–"),
                           "| η:", paste(input$sens_eta_range, collapse = "–")),
          fill = "Method"
        ) +
        theme_bw(base_size = 14) +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        scale_fill_manual(values = method_colors)
      
      if (isTRUE(input$sens_show_points)) {
        p <- p +
          geom_point(
            aes(color = method),
            position = position_jitterdodge(jitter.width = 0.12, dodge.width = 0.8),
            alpha = 0.35,
            size = 1.2
          ) +
          scale_color_manual(values = method_colors, guide = "none")
      }
      
      if (input$sens_agg == "all") {
        p <- p + facet_wrap(~ param_lab, scales = "free_y")
      } else {
        p <- p + facet_grid(T_fac ~ param_lab, scales = "free_y")
      }
      
      p
    }
  })
  
  # Sensitivity 3D scatter plotly
  output$sens_3d <- renderPlotly({
    req(input$sens_plot_type == "3d")
    dat <- sens_3d_data()
    req(nrow(dat) > 0)
    metric_disp <- metric_label(input$sens_metric)
    theta_title <- paste(unique(dat$theta), collapse = ", ")
    
    plt <- plot_ly()
    
    trace_keys <- unique(dat %>% transmute(trace_name = paste(method, theta, sep = " | ")))
    for (k in trace_keys$trace_name) {
      dsub <- dat %>% filter(paste(method, theta, sep = " | ") == k)
      
      hover_txt <- paste0(
        "Method: ", dsub$method,
        "<br>θ: ", dsub$theta,
        "<br>δ: ", dsub$true_delta,
        "<br>τ: ", dsub$true_tau,
        "<br>η: ", dsub$true_eta,
        "<br>", metric_disp, ": ", round(dsub$Metric, 4)
      )
      
      plt <- plt %>%
        add_trace(
          data = dsub,
          x = ~true_delta, y = ~true_tau, z = ~true_eta,
          type = "scatter3d",
          mode = "markers",
          name = k,
          text = hover_txt,
          hoverinfo = "text",
          marker = list(
            size = 4,
            color = ~Metric,
            colorscale = "Viridis",
            showscale = (k == trace_keys$trace_name[1]),
            colorbar = list(title = metric_disp)
          )
        )
    }
    
    plt %>%
      layout(
        title = list(
          text = paste0("3D Sensitivity: X=δ, Y=τ, Z=η (θ = ", theta_title, ")"),
          x = 0.02
        ),
        scene = list(
          xaxis = list(title = "Delta (δ)"),
          yaxis = list(title = "Tau (τ)"),
          zaxis = list(title = "Eta (η)")
        ),
        legend = list(orientation = "h", x = 0, y = -0.1)
      )
  })
  
  # >>> ADDED: Sensitivity 3D surface plotly
  build_surface_plot <- function(fixed_val) {
    stats2 <- sens_stats_filtered()
    req(nrow(stats2) > 0)
    req(input$sens_param3d)
    req(input$sens_surface_fixed)
    req(input$sens_surface_x, input$sens_surface_y)
    req(input$sens_surface_methods)
    
    metric <- metric_key(input$sens_metric)
    metric_disp <- metric_label(metric)
    theta_params <- input$sens_param3d
    validate(need(length(theta_params) > 0, "Select at least one theta parameter in Multiple Parameter Grid."))
    fixed_var <- input$sens_surface_fixed
    xvar <- input$sens_surface_x
    yvar <- input$sens_surface_y
    
    xy_choices <- setdiff(names(surface_var_labels), fixed_var)
    if (xvar == yvar || xvar == fixed_var || yvar == fixed_var) {
      xvar <- xy_choices[1]
      yvar <- if (length(xy_choices) > 1) xy_choices[2] else xy_choices[1]
    }
    
    base_stats <- stats2 %>%
      filter(param %in% theta_params)
    if (nrow(base_stats) == 0) return(NULL)
    
    dat_fixed <- base_stats %>%
      filter(.data[[fixed_var]] == fixed_val,
             method %in% input$sens_surface_methods)
    if (nrow(dat_fixed) == 0) return(NULL)
    
    x_label <- surface_var_labels[[xvar]]
    y_label <- surface_var_labels[[yvar]]
    fixed_label <- surface_var_labels[[fixed_var]]
    theta_title <- paste(unique(unname(theta_label_map[theta_params])), collapse = ", ")
    
    plt <- plot_ly()
    
    for (th in unique(dat_fixed$theta)) {
      for (m in surface_methods) {
        if (!(m %in% unique(as.character(dat_fixed$method)))) next
        if (!(m %in% input$sens_surface_methods)) next
        dm <- dat_fixed %>% filter(method == m, theta == th)
        if (nrow(dm) == 0) next
        
        smooth_mode <- if (is.null(input$sens_surface_smooth)) "none" else input$sens_surface_smooth
        grid_n <- if (is.null(input$sens_surface_grid)) 25 else input$sens_surface_grid
        
        dm2 <- dm %>%
          transmute(x = .data[[xvar]], y = .data[[yvar]], Metric = .data[[metric]])
        
        if (smooth_mode == "loess") {
          xvals <- seq(min(dm2$x), max(dm2$x), length.out = grid_n)
          yvals <- seq(min(dm2$y), max(dm2$y), length.out = grid_n)
          grid <- tidyr::expand_grid(x = xvals, y = yvals)
          span_val <- if (is.null(input$sens_surface_span)) 0.6 else input$sens_surface_span
          fit <- tryCatch(
            stats::loess(Metric ~ x + y, data = dm2, span = span_val),
            error = function(e) NULL
          )
          if (!is.null(fit)) {
            grid$Metric <- predict(fit, newdata = grid)
          } else {
            grid$Metric <- NA_real_
          }
        } else {
          xvals <- sort(unique(dm2$x))
          yvals <- sort(unique(dm2$y))
          grid <- tidyr::expand_grid(x = xvals, y = yvals) %>%
            left_join(dm2, by = c("x", "y")) %>%
            arrange(x, y)
        }
        
        zmat <- matrix(grid$Metric, nrow = length(xvals), ncol = length(yvals), byrow = FALSE)
        method_color <- surface_colors[[m]]
        
        hover_tmpl <- paste0(
          "<b>", m, "</b><br>",
          "θ: ", th, "<br>",
          x_label, ": %{x}<br>",
          y_label, ": %{y}<br>",
          metric_disp, ": %{z}<br>",
          fixed_label, ": ", fixed_val,
          "<extra></extra>"
        )
        
        plt <- plt %>%
          add_surface(
            x = xvals,
            y = yvals,
            z = zmat,
            name = paste(m, th, sep = " | "),
            surfacecolor = matrix(1, nrow = nrow(zmat), ncol = ncol(zmat)),
            colorscale = list(c(0, method_color), c(1, method_color)),
            showscale = FALSE,
            opacity = 0.9,
            showlegend = TRUE,
            hovertemplate = hover_tmpl,
            contours = list(
              z = list(show = TRUE, usecolormap = FALSE, highlight = FALSE,
                       project = list(z = TRUE),
                       color = "rgba(0,0,0,0.25)")
            )
          )
      }
    }
    
    plt %>%
      layout(
        title = list(
          text = paste0(
            "Fixed ", fixed_label, " = ", fixed_val,
            " (θ = ", theta_title, ")"
          ),
          x = 0.02
        ),
        scene = list(
          xaxis = list(title = x_label),
          yaxis = list(title = y_label),
          zaxis = list(title = metric_disp)
        ),
        legend = list(orientation = "h", x = 0, y = -0.1)
      )
  }
  
  observe({
    req(startsWith(input$sens_plot_type, "3d_surface"))
    vals <- input$sens_surface_fixed_values
    if (is.null(vals) || length(vals) == 0) return()
    
    lapply(seq_along(vals), function(i) {
      fixed_val <- round(as.numeric(vals[i]), 1)
      output[[paste0("sens_3d_surface_", i)]] <- renderPlotly({
        build_surface_plot(fixed_val)
      })
    })
  })
  # <<< ADDED
  
  # --------------------------------------------------------
  # Tables
  # --------------------------------------------------------
  output$metric_table <- renderDT({
    dat <- summary_stats_reactive()
    req(nrow(dat) > 0)
    
    dat <- dat %>%
      dplyr::select(
        Parameter = param_lab, Method = method, N, T,
        Delta = true_delta, Tau = true_tau, Eta = true_eta,
        Bias, RMSE, MAE
      ) %>%
      arrange(Parameter, Method, Delta, Tau, Eta, N, T)
    
    datatable(
      dat,
      options  = list(pageLength = 10, scrollX = TRUE, dom = "Blfrtip"),
      rownames = FALSE,
      caption  = "Performance Metrics Summary"
    ) %>%
      formatRound(columns = c("Bias", "RMSE", "MAE"), digits = 4)
  })
  
  output$bias_summary_table <- renderDT({
    dat <- summary_table_reactive()
    req(nrow(dat) > 0)
    
    dat <- dat %>%
      dplyr::select(
        Method = method, Parameter = param_lab,
        `Mean Bias` = Mean_Bias, `Bias SD` = SD_Bias,
        `Min Bias`  = Min_Bias,  `Max Bias` = Max_Bias,
        Scenarios   = Total_Scenarios
      ) %>%
      arrange(Method, Parameter)
    
    datatable(
      dat,
      options  = list(pageLength = 8, scrollX = TRUE, dom = "t"),
      rownames = FALSE
    ) %>%
      formatRound(columns = c("Mean Bias", "Bias SD", "Min Bias", "Max Bias"), digits = 4)
  })
  
  output$rmse_summary_table <- renderDT({
    dat <- summary_table_reactive()
    req(nrow(dat) > 0)
    
    dat <- dat %>%
      dplyr::select(
        Method = method, Parameter = param_lab,
        `Mean RMSE` = Mean_RMSE, `RMSE SD` = SD_RMSE,
        `Min RMSE`  = Min_RMSE,  `Max RMSE` = Max_RMSE,
        Scenarios   = Total_Scenarios
      ) %>%
      arrange(Method, Parameter)
    
    datatable(
      dat,
      options  = list(pageLength = 8, scrollX = TRUE, dom = "t"),
      rownames = FALSE
    ) %>%
      formatRound(columns = c("Mean RMSE", "RMSE SD", "Min RMSE", "Max RMSE"), digits = 4)
  })
  
  output$detailed_summary_table <- renderDT({
    dat <- summary_stats_reactive()
    req(nrow(dat) > 0)
    
    dat <- dat %>%
      group_by(Method = method, Parameter = param_lab,
               Delta = true_delta, Tau = true_tau, Eta = true_eta) %>%
      summarise(
        `Mean Bias` = mean(Bias, na.rm = TRUE),
        `Bias SD`   = sd(Bias,   na.rm = TRUE),
        `Mean RMSE` = mean(RMSE, na.rm = TRUE),
        `RMSE SD`   = sd(RMSE,   na.rm = TRUE),
        `Scenarios` = n(),
        .groups = "drop"
      ) %>%
      arrange(Method, Parameter, Delta, Tau, Eta)
    
    datatable(
      dat,
      options  = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE,
      caption  = "Detailed Performance Summary by Parameter Configuration"
    ) %>%
      formatRound(columns = c("Mean Bias", "Bias SD", "Mean RMSE", "RMSE SD"), digits = 4)
  })
  
  output$sens_table <- renderDT({
    cap_max <- input$sens_cap
    cap_min <- input$sens_cap_min
    
    if (input$sens_plot_type == "line") {
      dat <- sens_line_data()
      req(nrow(dat) > 0)
      dat_out <- dat %>%
        mutate(Metric = pmin(pmax(Metric, cap_min), cap_max),
               MetricName = metric_label(metric_key(input$sens_metric)),
               PlotType = "Line",
               Vary = input$sens_vary) %>%
        arrange(param_lab, method, x)
      
    } else if (input$sens_plot_type == "box") {
      dat <- sens_box_data()
      req(nrow(dat) > 0)
      dat_out <- dat %>%
        mutate(Metric = pmin(pmax(Metric, cap_min), cap_max),
               MetricName = metric_label(metric_key(input$sens_metric)),
               PlotType = "Boxplot",
               Vary = input$sens_vary) %>%
        arrange(param_lab, method, x, T_fac, N_fac)
      
    } else if (startsWith(input$sens_plot_type, "3d_surface")) {
      # >>> ADDED: table for surface
      dat <- sens_surface_data()
      req(nrow(dat) > 0)
      dat_out <- dat %>%
        mutate(MetricName = metric_label(metric_key(input$sens_metric)),
               PlotType = "3D Surface",
               Param3D = paste(unique(theta), collapse = ", "),
               FixedParam = unique(fixed_var),
               FixedValue = unique(fixed_value),
               SurfaceX = unique(x_var),
               SurfaceY = unique(y_var)) %>%
        arrange(method, x, y)
      # <<< ADDED
      
    } else { # 3d
      dat <- sens_3d_data()
      req(nrow(dat) > 0)
      dat_out <- dat %>%
        mutate(MetricName = metric_label(metric_key(input$sens_metric)),
               PlotType = "3D",
               Param3D = paste(unique(theta), collapse = ", ")) %>%
        arrange(method, true_delta, true_tau, true_eta)
    }
    
    datatable(
      dat_out,
      options = list(pageLength = 10, scrollX = TRUE, dom = "t"),
      rownames = FALSE
    ) %>%
      formatRound(columns = c("Metric"), digits = 4)
  })
  
  # --------------------------------------------------------
  # Download handlers
  # - PNG for ggplots
  # - HTML for 3D plotly (scatter + surface)
  # --------------------------------------------------------
  dist_plot_reactive <- reactive({
    dat <- df_filtered()
    req(nrow(dat) > 0)
    make_dist_plot(dat, input$error_cap, input$dist_type, input$show_stats, method_colors)
  })
  
  metric_plot_reactive <- reactive({
    dat <- summary_stats_reactive()
    req(nrow(dat) > 0)
    make_metric_plot(dat, input$metric_type, input$viz_type, input$metric_cap, input$group_by, method_colors)
  })
  
  comparison_plot_reactive <- reactive({
    dat <- summary_stats_reactive()
    req(nrow(dat) > 0)
    make_comparison_plot(dat, input$comp_metric, method_colors, input$comp_x, input$comp_row, input$comp_cap)
  })
  
  # Rebuild sensitivity ggplot for download (line/box only)
  sens_ggplot_reactive <- reactive({
    req(input$sens_plot_type != "3d")
    req(!startsWith(input$sens_plot_type, "3d_surface"))  # >>> ADDED
    
    cap_max <- input$sens_cap
    cap_min <- input$sens_cap_min
    x_lab_map <- c(true_delta = "Delta (δ)", true_tau = "Tau (τ)", true_eta = "Eta (η)")
    x_lab <- x_lab_map[[input$sens_vary]]
    metric_disp <- metric_label(input$sens_metric)
    
    if (input$sens_plot_type == "line") {
      dat <- sens_line_data()
      req(nrow(dat) > 0)
      dat <- dat %>% mutate(Metric = pmin(pmax(Metric, cap_min), cap_max))
      
      p <- ggplot(dat, aes(x = x, y = Metric, color = method, group = method)) +
        geom_line(linewidth = 1.1) +
        geom_point(size = 2) +
        labs(
          x = x_lab, y = metric_disp,
          title = paste("Metric Change:", metric_disp, "vs", x_lab),
          subtitle = paste("Cap:", cap_min, "to", cap_max),
          color = "Method"
        ) +
        theme_bw(base_size = 14) +
        theme(legend.position = "bottom") +
        scale_color_manual(values = method_colors)
      
      if (input$sens_agg == "all") {
        p <- p + facet_wrap(~ param_lab, scales = "free_y")
      } else {
        p <- p + facet_grid(T_fac ~ param_lab, scales = "free_y")
      }
      p
      
    } else {
      dat <- sens_box_data()
      req(nrow(dat) > 0)
      dat <- dat %>% mutate(Metric = pmin(pmax(Metric, cap_min), cap_max))
      dat <- dat %>% mutate(x_fac = factor(x, levels = sort(unique(x))))
      
      p <- ggplot(dat, aes(x = x_fac, y = Metric, fill = method)) +
        geom_boxplot(alpha = 0.85, outlier.size = 0.6,
                     color = "#000000", linewidth = 0.25,
                     width = input$sens_box_width,
                     position = position_dodge(width = 0.8)) +
        labs(
          x = x_lab, y = metric_disp,
          title = paste("Metric Distribution:", metric_disp, "across", x_lab),
          subtitle = paste("Cap:", cap_min, "to", cap_max),
          fill = "Method"
        ) +
        theme_bw(base_size = 14) +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        scale_fill_manual(values = method_colors)
      
      if (isTRUE(input$sens_show_points)) {
        p <- p +
          geom_point(
            aes(color = method),
            position = position_jitterdodge(jitter.width = 0.12, dodge.width = 0.8),
            alpha = 0.35, size = 1.2
          ) +
          scale_color_manual(values = method_colors, guide = "none")
      }
      
      if (input$sens_agg == "all") {
        p <- p + facet_wrap(~ param_lab, scales = "free_y")
      } else {
        p <- p + facet_grid(T_fac ~ param_lab, scales = "free_y")
      }
      p
    }
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      current_tab <- input$main_tabs
      if (current_tab == "Metric Change (Sensitivity)" &&
          (input$sens_plot_type == "3d" || startsWith(input$sens_plot_type, "3d_surface"))) {
        paste0("DSDPM_Sensitivity3D_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
      } else {
        paste0("DSDPM_Analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
      }
    },
    content = function(file) {
      current_tab <- input$main_tabs
      
      # 3D download -> HTML widget (scatter or surface)
      if (current_tab == "Metric Change (Sensitivity)" &&
          (input$sens_plot_type == "3d" || startsWith(input$sens_plot_type, "3d_surface"))) {
        
        metric_disp <- metric_label(input$sens_metric)
        if (input$sens_plot_type == "3d") {
          dat <- sens_3d_data()
          req(nrow(dat) > 0)
          theta_title <- paste(unique(dat$theta), collapse = ", ")
          
          plt <- plot_ly()
          trace_keys <- unique(dat %>% transmute(trace_name = paste(method, theta, sep = " | ")))
          for (k in trace_keys$trace_name) {
            dsub <- dat %>% filter(paste(method, theta, sep = " | ") == k)
            hover_txt <- paste0(
              "Method: ", dsub$method,
              "<br>θ: ", dsub$theta,
              "<br>δ: ", dsub$true_delta,
              "<br>τ: ", dsub$true_tau,
              "<br>η: ", dsub$true_eta,
              "<br>", metric_disp, ": ", round(dsub$Metric, 4)
            )
            plt <- plt %>%
              add_trace(
                data = dsub,
                x = ~true_delta, y = ~true_tau, z = ~true_eta,
                type = "scatter3d",
                mode = "markers",
                name = k,
                text = hover_txt,
                hoverinfo = "text",
                marker = list(
                  size = 4,
                  color = ~Metric,
                  colorscale = "Viridis",
                  showscale = (k == trace_keys$trace_name[1]),
                  colorbar = list(title = metric_disp)
                )
              )
          }
          
          plt <- plt %>%
            layout(
              title = list(
                text = paste0("3D Sensitivity: X=δ, Y=τ, Z=η (θ = ", theta_title, ")"),
                x = 0.02
              ),
              scene = list(
                xaxis = list(title = "Delta (δ)"),
                yaxis = list(title = "Tau (τ)"),
                zaxis = list(title = "Eta (η)")
              ),
              legend = list(orientation = "h", x = 0, y = -0.1)
            )
          
          saveWidget(plt, file = file, selfcontained = TRUE)
          return(invisible(NULL))
        }
        
        # >>> ADDED: surface download
        dats <- sens_surface_data()
        req(nrow(dats) > 0)
        req(input$sens_surface_methods)
        
        missing_methods <- setdiff(input$sens_surface_methods, unique(as.character(dats$method)))
        if (length(missing_methods) > 0) {
          showNotification(
            paste0("Surface methods not found in data: ", paste(missing_methods, collapse = ", ")),
            type = "warning",
            duration = 5
          )
        }
        
        plt <- plot_ly()
        selected_order <- surface_methods[surface_methods %in% input$sens_surface_methods]
        first_method <- selected_order[1]
        x_label <- surface_var_labels[[unique(dats$x_var)]]
        y_label <- surface_var_labels[[unique(dats$y_var)]]
        fixed_label <- surface_var_labels[[unique(dats$fixed_var)]]
        fixed_val <- unique(dats$fixed_value)
        theta_title <- paste(unique(dats$theta), collapse = ", ")
        for (th in unique(dats$theta)) {
          for (m in surface_methods) {
            if (!(m %in% unique(as.character(dats$method)))) next
            if (!(m %in% input$sens_surface_methods)) next
            dm <- dats %>% filter(method == m, theta == th)
            if (nrow(dm) == 0) next
            xvals <- sort(unique(dm$x))
            yvals <- sort(unique(dm$y))
            grid <- tidyr::expand_grid(x = xvals, y = yvals) %>%
              left_join(dm, by = c("x", "y")) %>%
              arrange(x, y)
            zmat <- matrix(grid$Metric, nrow = length(xvals), ncol = length(yvals), byrow = FALSE)
            method_color <- surface_colors[[m]]
            
            hover_tmpl <- paste0(
              "<b>", m, "</b><br>",
              "θ: ", th, "<br>",
              x_label, ": %{x}<br>",
              y_label, ": %{y}<br>",
              metric_disp, ": %{z}<br>",
              fixed_label, ": ", fixed_val,
              "<extra></extra>"
            )
            
            plt <- plt %>% add_surface(x = xvals, y = yvals, z = zmat, name = paste(m, th, sep = " | "), opacity = 0.9,
                                       surfacecolor = matrix(1, nrow = nrow(zmat), ncol = ncol(zmat)),
                                       colorscale = list(c(0, method_color), c(1, method_color)),
                                       showscale = FALSE,
                                       showlegend = TRUE,
                                       hovertemplate = hover_tmpl,
                                       contours = list(
                                         z = list(show = TRUE, usecolormap = FALSE, highlight = FALSE,
                                                  project = list(z = TRUE),
                                                  color = "rgba(0,0,0,0.25)")
                                       ))
          }
        }
        
        plt <- plt %>% layout(
          title = list(
            text = paste0(
              "3D Surface: Z=", metric_disp,
              " over (", x_label, ", ", y_label, ") | fixed ",
              fixed_label, "=", fixed_val,
              " (θ = ", theta_title, ")"
            ),
            x = 0.02
          ),
          scene = list(
            xaxis = list(title = x_label),
            yaxis = list(title = y_label),
            zaxis = list(title = metric_disp)
          ),
          legend = list(orientation = "h", x = 0, y = -0.1)
        )
        
        saveWidget(plt, file = file, selfcontained = TRUE)
        return(invisible(NULL))
        # <<< ADDED
      }
      
      # Otherwise -> PNG ggplot
      p <- if (current_tab == "Distribution Analysis") {
        dist_plot_reactive()
      } else if (current_tab == "Performance Metrics") {
        metric_plot_reactive()
      } else if (current_tab == "Comparative Analysis") {
        comparison_plot_reactive()
      } else if (current_tab == "Metric Change (Sensitivity)") {
        sens_ggplot_reactive()
      } else {
        dist_plot_reactive()
      }
      
      ggsave(
        file,
        plot   = p,
        width  = input$export_width / 2.54,
        height = input$export_height / 2.54,
        units  = "in",
        dpi    = 300,
        bg     = "white"
      )
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("DSDPM_Data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      wb <- createWorkbook()
      
      addWorksheet(wb, "Performance_Summary")
      writeData(wb, "Performance_Summary", summary_stats_reactive())
      
      addWorksheet(wb, "Bias_RMSE_Summary")
      writeData(wb, "Bias_RMSE_Summary", summary_table_reactive())
      
      addWorksheet(wb, "Line_Chart_Data")
      writeData(wb, "Line_Chart_Data", line_data_nt_reactive())
      
      addWorksheet(wb, "Filtered_Raw_Data")
      writeData(wb, "Filtered_Raw_Data", df_filtered())
      
      addWorksheet(wb, "Sensitivity_Line_Data")
      suppressWarnings(writeData(wb, "Sensitivity_Line_Data", sens_line_data()))
      
      addWorksheet(wb, "Sensitivity_Box_Data")
      suppressWarnings(writeData(wb, "Sensitivity_Box_Data", sens_box_data()))
      
      addWorksheet(wb, "Sensitivity_3D_Data")
      suppressWarnings(writeData(wb, "Sensitivity_3D_Data", sens_3d_data()))
      
      # >>> ADDED: surface data export
      addWorksheet(wb, "Sensitivity_Surface_Data")
      suppressWarnings(writeData(wb, "Sensitivity_Surface_Data",
                                 tryCatch(sens_surface_data(), error = function(e) data.frame())))
      # <<< ADDED
      
      addWorksheet(wb, "Configuration_Summary")
      config_summary <- data.frame(
        Parameter = c(
          "Selected Parameters", "Selected Methods",
          "Delta Values", "Tau Values", "Eta Values",
          "Sensitivity Plot Type", "Sensitivity Metric",
          "Sensitivity Vary (Line/Box)", "Sensitivity Param3D",
          "Sensitivity Min Cap", "Sensitivity Max Cap",
          "Sensitivity Fixed Param (Surface)",
          "Sensitivity Fixed Value (Surface)",
          "Sensitivity Surface X",
          "Sensitivity Surface Y",
          "Sensitivity Surface Methods",
          "Sensitivity Delta Range", "Sensitivity Tau Range", "Sensitivity Eta Range",
          "Export Time"
        ),
        Value = c(
          paste(input$param,        collapse = ", "),
          paste(input$method,       collapse = ", "),
          paste(input$delta_values, collapse = ", "),
          paste(input$tau_values,   collapse = ", "),
          paste(input$eta_values,   collapse = ", "),
          input$sens_plot_type,
          input$sens_metric,
          input$sens_vary,
          ifelse(is.null(input$sens_param3d), "", paste(input$sens_param3d, collapse = ", ")),
          ifelse(is.null(input$sens_cap_min), "", input$sens_cap_min),
          ifelse(is.null(input$sens_cap), "", input$sens_cap),
          ifelse(is.null(input$sens_surface_fixed), "", input$sens_surface_fixed),
          ifelse(is.null(input$sens_surface_fixed_value), "", input$sens_surface_fixed_value),
          ifelse(is.null(input$sens_surface_x), "", input$sens_surface_x),
          ifelse(is.null(input$sens_surface_y), "", input$sens_surface_y),
          ifelse(is.null(input$sens_surface_methods), "", paste(input$sens_surface_methods, collapse = ", ")),
          paste(input$sens_delta_range, collapse = "–"),
          paste(input$sens_tau_range,   collapse = "–"),
          paste(input$sens_eta_range,   collapse = "–"),
          format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
      )
      writeData(wb, "Configuration_Summary", config_summary)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)
