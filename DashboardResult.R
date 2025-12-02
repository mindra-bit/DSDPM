# ============================================================
# DSDPM Simulation Dashboard
# Jaya and Folmer 2025
# ============================================================

setwd("/Users/mindra/@Paper9")

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

# Create custom black & blue theme
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

params <- c("delta", "tau", "eta",
            "rho", "lambda", "kappa",
            "beta_x", "beta_wx", "beta_x_lag", "beta_wx_lag")

available_delta <- c(0.2, 0.3, 0.4, 0.6)
available_tau   <- c(0.2, 0.3, 0.4, 0.6)
available_eta   <- c(0.2, 0.3, 0.4, 0.6)
available_T     <- sort(unique(df_raw$T))

# Mathematical notation for parameters
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

df_long <- df_raw %>%
  mutate(
    NT = N * T,
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

available_N <- sort(unique(df_long$N))
available_T2 <- sort(unique(df_long$T))

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
        padding: 8px 16px;
        transition: all 0.3s ease;
        box-shadow: 0 2px 6px rgba(255,165,0,0.3);
        margin-top: 5px;
        margin-right: 5px;
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
        column(12,
               h1("DSDPM SIMULATION DASHBOARD",
                  style = "margin: 0; color: #ffffff; font-weight: 800; letter-spacing: 1px;"),
               p("Dynamic Spatial Panel Model Analysis",
                 style = "margin: 10px 0 0 0; color: rgba(255,255,255,0.9); font-size: 1.1em; font-weight: 500;"),
               p("Developed by Jaya and Folmer 2025",
                 style = "margin: 5px 0 0 0; color: rgba(255,255,255,0.7); font-size: 0.9em; font-style: italic;")
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
                     selected = sort(unique(df_long$method))[1:2],
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
                   ),
                   
                   hr(style = "border-color: #e9ecef;"),
                   
                   h4("Sample Size Filters", class = "control-label"),
                   div(class = "param-control-section",
                       h5("Cross-sectional Units (N)",
                          style = "color: #000000; font-weight: 700; margin-bottom: 15px;"),
                       awesomeCheckboxGroup(
                         "N_filter",
                         NULL,
                         choices  = available_N,
                         selected = available_N,
                         status   = "primary",
                         inline   = TRUE
                       )
                   ),
                   div(class = "param-control-section",
                       h5("Time Periods (T)",
                          style = "color: #000000; font-weight: 700; margin-bottom: 15px;"),
                       awesomeCheckboxGroup(
                         "T_filter",
                         NULL,
                         choices  = available_T2,
                         selected = available_T2,
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
                   downloadButton("download_plot", "Download Figure (PNG)",
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
                                            choices  = c("Bias" = "Bias",
                                                         "RMSE" = "RMSE",
                                                         "MAE"  = "MAE"),
                                            selected = "RMSE")
                         ),
                         column(3,
                                selectInput("viz_type", "View:",
                                            choices  = c("Heatmap"    = "heatmap",
                                                         "Bar Chart"  = "bar",
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
                                numericInput("metric_min", "Min Bias (Bias only):",
                                             value = -2, min = -10, max = 0, step = 0.1),
                                numericInput("metric_cap", "Maximum Value:",
                                             value = 2, min = 0.1, max = 10, step = 0.1)
                         )
                       ),
                       withSpinner(
                         plotOutput("metric_plot", height = "500px"),
                         type = 4, color = "#007acc"
                       ),
                       br(),
                       DTOutput("metric_table"),
                       downloadButton("download_metric_table",
                                      "Download Performance Metrics (Excel)",
                                      class = "btn-research")
                   )
               )
             ),
             
             # Summary Statistics
             tabPanel(
               "Summary Statistics",
               icon = icon("calculator"),
               div(class = "content-panel",
                   div(class = "panel-body",
                       h4("Comprehensive Bias, MAE, and RMSE Summary", class = "section-title"),
                       p("Average performance metrics across selected scenarios for each method and parameter."),
                       
                       h5("Bias Summary", style = "color: #007acc; font-weight: 700;"),
                       DTOutput("bias_summary_table"),
                       downloadButton("download_bias_summary", "Download Bias Summary (Excel)",
                                      class = "btn-research"),
                       br(), br(),
                       
                       h5("MAE Summary", style = "color: #2c3e50; font-weight: 700;"),
                       DTOutput("mae_summary_table"),
                       downloadButton("download_mae_summary", "Download MAE Summary (Excel)",
                                      class = "btn-research"),
                       br(), br(),
                       
                       h5("RMSE Summary", style = "color: #ffa500; font-weight: 700;"),
                       DTOutput("rmse_summary_table"),
                       downloadButton("download_rmse_summary", "Download RMSE Summary (Excel)",
                                      class = "btn-research"),
                       br(), br(),
                       
                       h4("Detailed Performance Summary", class = "section-title"),
                       p("Mean, median, and variability of Bias, MAE, and RMSE for each parameter configuration and method."),
                       DTOutput("detailed_summary_table"),
                       downloadButton("download_detailed_summary", "Download Detailed Summary (Excel)",
                                      class = "btn-research")
                   )
               )
             ),
             
             # Method Comparison (Lollipop Plot)
             tabPanel(
               "Method Comparison",
               icon = icon("sliders-h"),
               div(class = "content-panel",
                   div(class = "panel-body",
                       h4("Method Comparison by Parameter", class = "section-title"),
                       p("Lollipop plot comparing Bayesian, GMM, and QML across selected parameters and metrics."),
                       fluidRow(
                         column(
                           6,
                           checkboxGroupInput(
                             "mc_metrics", "Metrics to display:",
                             choices  = c("Bias", "MAE", "RMSE"),
                             selected = c("Bias", "MAE", "RMSE"),
                             inline   = TRUE
                           )
                         ),
                         column(
                           6,
                           radioButtons(
                             "mc_stat", "Summary statistic:",
                             choices  = c("Median", "Mean"),
                             selected = "Median",
                             inline   = TRUE
                           )
                         )
                       ),
                       withSpinner(
                         plotOutput("method_comp_plot", height = "500px"),
                         type = 4, color = "#007acc"
                       )
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
                                            choices  = c("Bias", "RMSE", "MAE"),
                                            selected = "RMSE")
                         ),
                         column(3,
                                selectInput("comp_plot_type", "Plot type:",
                                            choices = c("Boxplot" = "box",
                                                        "Bar plot" = "bar",
                                                        "Line plot" = "line"),
                                            selected = "box")
                         ),
                         column(3,
                                selectInput("comp_x", "Horizontal axis:",
                                            choices = c("N"      = "N_fac",
                                                        "T"      = "T_fac",
                                                        "N × T"  = "NT_fac",
                                                        "Delta (δ)" = "delta_fac",
                                                        "Tau (τ)"   = "tau_fac",
                                                        "Eta (η)"   = "eta_fac"),
                                            selected = "N_fac")
                         ),
                         column(3,
                                selectInput("comp_row", "Facet rows:",
                                            choices = c("None"      = "none",
                                                        "T"         = "T_fac",
                                                        "N"         = "N_fac",
                                                        "Delta (δ)" = "delta_fac",
                                                        "Tau (τ)"   = "tau_fac",
                                                        "Eta (η)"   = "eta_fac"),
                                            selected = "T_fac")
                         )
                       ),
                       br(),
                       fluidRow(
                         column(4,
                                selectInput("comp_auto", "Autocorrelation (line plot):",
                                            choices = c("None"      = "none",
                                                        "Delta (δ)" = "delta_fac",
                                                        "Tau (τ)"   = "tau_fac",
                                                        "Eta (η)"   = "eta_fac"),
                                            selected = "delta_fac")
                         ),
                         column(4,
                                numericInput("comp_min", "Min Bias (Bias only):",
                                             value = -2, min = -10, max = 0, step = 0.1)
                         ),
                         column(4,
                                numericInput("comp_cap", "Maximum Value:",
                                             value = 2, min = 0.1, max = 10, step = 0.1)
                         )
                       ),
                       helpText("Boxplot mode: comparative boxplots by horizontal factor (N, T, N×T, δ, τ, η) and chosen facet rows.",
                                "Bar plot mode: median metric by horizontal factor, with labels on top of bars.",
                                "Line plot mode: mean metric vs chosen horizontal factor, lines can differ by autocorrelation level or method; facet rows also active."),
                       withSpinner(
                         plotOutput("comparison_plot", height = "600px"),
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
  
  # Colour map: INLA/Bayesian = blue, GMM = yellow, QMLE/QML = red
  method_colors <- c(
    "INLA"     = "#0072B2",
    "Bayesian" = "#0072B2",
    "GMM"      = "#F0E442",
    "QMLE"     = "#D55E00",
    "QML"      = "#D55E00"
  )
  
  # ---- Reactive data ----
  df_filtered <- reactive({
    req(input$param, input$method,
        input$delta_values, input$tau_values, input$eta_values,
        input$N_filter, input$T_filter)
    
    result <- df_long %>%
      filter(
        param      %in% input$param,
        method     %in% input$method,
        true_delta %in% input$delta_values,
        true_tau   %in% input$tau_values,
        true_eta   %in% input$eta_values,
        N          %in% input$N_filter,
        T          %in% input$T_filter
      )
    
    if (nrow(result) == 0) return(data.frame())
    
    sel <- input$param
    result %>%
      mutate(
        param     = factor(param, levels = sel),
        param_lab = factor(param_lab, levels = param_label_map[sel])
      )
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
      ) %>%
      mutate(NT_fac = factor(NT))
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
  
  # ====== Summary table with medians ======
  summary_table_reactive <- reactive({
    stats <- summary_stats_reactive()
    req(nrow(stats) > 0)
    
    stats %>%
      group_by(method, param, param_lab) %>%
      summarise(
        Mean_Bias   = mean(Bias,   na.rm = TRUE),
        Median_Bias = median(Bias, na.rm = TRUE),
        SD_Bias     = sd(Bias,     na.rm = TRUE),
        Min_Bias    = min(Bias,    na.rm = TRUE),
        Max_Bias    = max(Bias,    na.rm = TRUE),
        
        Mean_MAE    = mean(MAE,    na.rm = TRUE),
        Median_MAE  = median(MAE,  na.rm = TRUE),
        SD_MAE      = sd(MAE,      na.rm = TRUE),
        Min_MAE     = min(MAE,     na.rm = TRUE),
        Max_MAE     = max(MAE,     na.rm = TRUE),
        
        Mean_RMSE   = mean(RMSE,   na.rm = TRUE),
        Median_RMSE = median(RMSE, na.rm = TRUE),
        SD_RMSE     = sd(RMSE,     na.rm = TRUE),
        Min_RMSE    = min(RMSE,    na.rm = TRUE),
        Max_RMSE    = max(RMSE,    na.rm = TRUE),
        
        Total_Scenarios = n(),
        .groups = "drop"
      )
  })
  
  # ====== Detailed summary with medians ======
  detailed_summary_reactive <- reactive({
    dat <- summary_stats_reactive()
    req(nrow(dat) > 0)
    
    dat %>%
      group_by(Method = method, Parameter = param_lab,
               Delta = true_delta, Tau = true_tau, Eta = true_eta) %>%
      summarise(
        `Mean Bias`   = mean(Bias,   na.rm = TRUE),
        `Median Bias` = median(Bias, na.rm = TRUE),
        `Bias SD`     = sd(Bias,     na.rm = TRUE),
        
        `Mean MAE`    = mean(MAE,    na.rm = TRUE),
        `Median MAE`  = median(MAE,  na.rm = TRUE),
        `MAE SD`      = sd(MAE,      na.rm = TRUE),
        
        `Mean RMSE`   = mean(RMSE,   na.rm = TRUE),
        `Median RMSE` = median(RMSE, na.rm = TRUE),
        `RMSE SD`     = sd(RMSE,     na.rm = TRUE),
        
        `NT Configurations` = n_distinct(paste(N, T)),
        .groups = "drop"
      ) %>%
      arrange(Method, Parameter, Delta, Tau, Eta)
  })
  
  # ---- Summary metrics boxes ----
  output$param_count <- renderText({
    if (length(input$param)  == 0) "0" else as.character(length(input$param))
  })
  output$method_count <- renderText({
    if (length(input$method) == 0) "0" else as.character(length(input$method))
  })
  
  output$scenario_count <- renderText({
    dat <- df_filtered()
    if (nrow(dat) == 0) return("0")
    as.character(n_distinct(paste(dat$N, dat$T,
                                  dat$true_delta, dat$true_tau, dat$true_eta)))
  })
  
  output$obs_count <- renderText({
    dat <- df_filtered()
    if (nrow(dat) == 0) return("0")
    format(nrow(dat), big.mark = ",")
  })
  
  # --------------------------------------------------------
  # Helper: Distribution plot
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
      return(ggplot() + labs(title = "No data available for the selected filters") +
               theme_bw())
    }
    
    if (dist_type == "density") {
      ggplot(dat, aes(x = error_plot, fill = method, color = method)) +
        geom_density(alpha = 0.6, size = 0.4) +
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
          axis.text.x       = element_text(angle = 0, hjust = 0.5),
          panel.grid.minor  = element_blank(),
          panel.grid.major  = element_line(color = "#f0f0f0")
        ) +
        scale_fill_manual(values = method_colors) +
        facet_grid(T_fac ~ param_lab, scales = "free_y")
      
      if (dist_type == "box") {
        p <- p +
          geom_boxplot(alpha = 0.8, outlier.size = 0.05,
                       color = "#000000", size = 0.05)
      } else if (dist_type == "violin") {
        p <- p +
          geom_violin(alpha = 0.8, trim = FALSE,
                      color = "#000000", size = 0.05)
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
  
  # Helper: metric plot (Performance Metrics tab)
  make_metric_plot <- function(dat, metric, viz_type, metric_min, metric_cap,
                               group_by, method_colors) {
    
    dat_clean <- dat %>%
      filter(!is.na(.data[[metric]]))
    
    if (metric == "Bias") {
      lower <- metric_min
      upper <- metric_cap
      dat_capped <- dat_clean %>%
        filter(.data[[metric]] >= lower, .data[[metric]] <= upper) %>%
        mutate(PlotValue = .data[[metric]])
      y_limits  <- c(lower, upper)
      fill_limits <- c(lower, upper)
    } else {
      lower <- 0
      upper <- metric_cap
      dat_capped <- dat_clean %>%
        filter(.data[[metric]] >= lower, .data[[metric]] <= upper) %>%
        mutate(PlotValue = .data[[metric]])
      y_limits  <- c(lower, upper)
      fill_limits <- c(lower, upper)
    }
    
    if (nrow(dat_capped) == 0) {
      return(ggplot() + labs(title = "No data within selected limits") + theme_bw())
    }
    
    if (viz_type == "heatmap") {
      p <- ggplot(dat_capped, aes(x = N_fac, y = T, fill = PlotValue)) +
        geom_tile(color = "white", size = 1) +
        scale_fill_gradient2(
          low  = "#007acc", mid = "white", high = "#ffa500",
          midpoint = 0, name = metric,
          limits   = fill_limits
        ) +
        labs(x = "N", y = "T",
             title = paste(metric, "Heatmap"),
             subtitle = paste("Limits:",
                              if (metric == "Bias")
                                paste0("[", metric_min, ", ", metric_cap, "]")
                              else
                                paste0("[0, ", metric_cap, "]"))
        ) +
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
      p <- ggplot(dat_capped, aes(x = N_fac, y = PlotValue, fill = method)) +
        geom_col(position = "dodge", alpha = 0.8) +
        labs(x = "Cross-sectional Units (N)", y = metric,
             title = paste(metric, "by Sample Size"),
             subtitle = paste("Limits:",
                              if (metric == "Bias")
                                paste0("[", metric_min, ", ", metric_cap, "]")
                              else
                                paste0("[0, ", metric_cap, "]"))
        ) +
        theme_bw(base_size = 14) +
        scale_fill_manual(values = method_colors) +
        ylim(y_limits)
      
      if (group_by == "method") {
        p <- p + facet_grid(. ~ param_lab)
      } else if (group_by == "delta_fac") {
        p <- p + facet_grid(delta_fac ~ param_lab)
      } else if (group_by == "tau_fac") {
        p <- p + facet_grid(tau_fac ~ param_lab)
      } else if (group_by == "eta_fac") {
        p <- p + facet_grid(eta_fac ~ param_lab)
      }
      
    } else { # line (Performance Metrics tab)
      line_data <- line_data_nt_reactive() %>%
        filter(!is.na(.data[[metric]]))
      
      if (metric == "Bias") {
        line_data <- line_data %>%
          filter(.data[[metric]] >= metric_min,
                 .data[[metric]] <= metric_cap)
      } else {
        line_data <- line_data %>%
          filter(.data[[metric]] >= 0,
                 .data[[metric]] <= metric_cap)
      }
      
      if (nrow(line_data) == 0) {
        return(ggplot() + labs(title = "No data within selected limits") + theme_bw())
      }
      
      p <- ggplot(line_data, aes(x = NT, y = .data[[metric]],
                                 color = method, group = method)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(x = "Sample Size (N × T)", y = metric,
             title = paste("Average", metric, "vs Sample Size"),
             subtitle = paste("Limits:",
                              if (metric == "Bias")
                                paste0("[", metric_min, ", ", metric_cap, "]")
                              else
                                paste0("[0, ", metric_cap, "]")),
             color = "Method") +
        theme_bw(base_size = 14) +
        scale_color_manual(values = method_colors) +
        theme(legend.position = "bottom") +
        ylim(y_limits) +
        facet_wrap(~ param_lab, scales = "free_y")
    }
    p
  }
  
  # Helper: comparison boxplot (Comparative Analysis tab)
  make_comparison_box_plot <- function(dat, metric, method_colors,
                                       x_var, row_var, cap_min, cap_max) {
    
    dat_capped <- dat %>%
      filter(!is.na(.data[[metric]]))
    
    if (metric == "Bias" && !is.na(cap_min)) {
      dat_capped <- dat_capped %>%
        filter(.data[[metric]] >= cap_min,
               .data[[metric]] <= cap_max)
    } else {
      dat_capped <- dat_capped %>%
        filter(.data[[metric]] <= cap_max)
    }
    
    if (nrow(dat_capped) == 0) {
      return(
        ggplot() +
          labs(title = "No data within selected min/max limits") +
          theme_bw()
      )
    }
    
    x_lab_map <- c(
      N_fac     = "N",
      T_fac     = "T",
      NT_fac    = "N × T",
      delta_fac = "Delta (δ)",
      tau_fac   = "Tau (τ)",
      eta_fac   = "Eta (η)"
    )
    x_lab <- x_lab_map[[x_var]]
    
    p <- ggplot(dat_capped, aes(x = .data[[x_var]],
                                y = .data[[metric]],
                                fill = method)) +
      geom_boxplot(alpha = 0.8, outlier.size = 0.2,
                   color = "#000000", size = 0.25) +
      labs(
        x = x_lab,
        y = metric,
        fill = "Method",
        title = paste("Comparative Analysis of", metric, "(Boxplot)"),
        subtitle = paste(
          "Horizontal axis:", x_lab,
          "| Facet rows:", row_var,
          "| Limits:",
          if (metric == "Bias") paste0("[", cap_min, ", ", cap_max, "]")
          else paste0("[0, ", cap_max, "]")
        )
      ) +
      theme_bw(base_size = 14) +
      theme(
        legend.position   = "bottom",
        plot.title        = element_text(face = "bold", size = 16, color = "#000000"),
        axis.text.x       = element_text(angle = 0, hjust = 1),
        panel.grid.minor  = element_blank(),
        panel.grid.major  = element_line(color = "#f0f0f0")
      ) +
      scale_fill_manual(values = method_colors)
    
    if (row_var == "none") {
      p <- p + facet_wrap(~ param_lab, scales = "free_y")
    } else {
      p <- p + facet_grid(as.formula(paste(row_var, "~ param_lab")),
                          scales = "free_y")
    }
    
    p
  }
  
  # Helper: comparison bar plot (median + labels)
  make_comparison_bar_plot <- function(dat, metric, method_colors,
                                       x_var, row_var, cap_min, cap_max) {
    
    dat_clean <- dat %>%
      filter(!is.na(.data[[metric]]))
    
    if (metric == "Bias") {
      dat_clean <- dat_clean %>%
        filter(.data[[metric]] >= cap_min,
               .data[[metric]] <= cap_max)
    } else {
      dat_clean <- dat_clean %>%
        filter(.data[[metric]] >= 0,
               .data[[metric]] <= cap_max)
    }
    
    if (nrow(dat_clean) == 0) {
      return(
        ggplot() +
          labs(title = "No data within selected min/max limits") +
          theme_bw()
      )
    }
    
    x_lab_map <- c(
      N_fac     = "N",
      T_fac     = "T",
      NT_fac    = "N × T",
      delta_fac = "Delta (δ)",
      tau_fac   = "Tau (τ)",
      eta_fac   = "Eta (η)"
    )
    x_lab <- x_lab_map[[x_var]]
    
    group_vars <- c("method", "param_lab", x_var)
    if (row_var != "none") group_vars <- c(group_vars, row_var)
    
    bar_dat <- dat_clean %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        Median = median(.data[[metric]], na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- ggplot(
      bar_dat,
      aes(x = .data[[x_var]], y = Median, fill = method)
    ) +
      geom_col(position = position_dodge(width = 0.8), alpha = 0.85) +
      geom_text(
        aes(label = round(Median, 3)),
        position = position_dodge(width = 0.8),
        angle = 90,
        hjust = -0.3,
        nudge_y = 0.03 * max(bar_dat$Median),
        size = 3,
        fontface = "bold"
      ) +
      labs(
        x = x_lab,
        y = paste("Median", metric),
        fill = "Method",
        title = paste("Comparative Analysis of", metric, "(Bar plot, Median)"),
        subtitle = paste(
          "Horizontal axis:", x_lab,
          "| Facet rows:", row_var,
          "| Bars show median", metric
        )
      ) +
      theme_bw(base_size = 14) +
      theme(
        legend.position   = "bottom",
        plot.title        = element_text(face = "bold", size = 16, color = "#000000"),
        axis.text.x       = element_text(angle = 0, hjust = 1),
        panel.grid.minor  = element_blank(),
        panel.grid.major  = element_line(color = "#f0f0f0"),
        plot.margin       = margin(15, 10, 10, 10)
      ) +
      scale_fill_manual(values = method_colors) +
      coord_cartesian(clip = "off")
    
    if (row_var == "none") {
      p <- p + facet_wrap(~ param_lab, scales = "free_y")
    } else {
      p <- p + facet_grid(as.formula(paste(row_var, "~ param_lab")),
                          scales = "free_y")
    }
    
    if (metric == "Bias") {
      p <- p + ylim(cap_min, cap_max)
    } else {
      p <- p + ylim(0, cap_max)
    }
    
    p
  }
  
  # Helper: comparison line plot (Comparative Analysis tab)
  make_comparison_line_plot <- function(dat, metric, method_colors,
                                        x_var, row_var, auto_var,
                                        cap_min, cap_max) {
    
    dat_clean <- dat %>%
      filter(!is.na(.data[[metric]]))
    
    if (metric == "Bias") {
      dat_clean <- dat_clean %>%
        filter(.data[[metric]] >= cap_min,
               .data[[metric]] <= cap_max)
    } else {
      dat_clean <- dat_clean %>%
        filter(.data[[metric]] >= 0,
               .data[[metric]] <= cap_max)
    }
    
    if (nrow(dat_clean) == 0) {
      return(
        ggplot() +
          labs(title = "No data within selected min/max limits") +
          theme_bw()
      )
    }
    
    x_lab_map <- c(
      N_fac     = "N",
      T_fac     = "T",
      NT_fac    = "N × T",
      delta_fac = "Delta (δ)",
      tau_fac   = "Tau (τ)",
      eta_fac   = "Eta (η)"
    )
    x_lab <- x_lab_map[[x_var]]
    
    auto_label_map <- c(
      delta_fac = "Delta (δ)",
      tau_fac   = "Tau (τ)",
      eta_fac   = "Eta (η)"
    )
    auto_lab  <- if (auto_var == "none") "Method" else auto_label_map[[auto_var]]
    
    group_vars <- c("method", "param_lab", x_var)
    if (auto_var != "none") group_vars <- c(group_vars, auto_var)
    if (row_var  != "none") group_vars <- c(group_vars, row_var)
    
    line_dat <- dat_clean %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        Value = mean(.data[[metric]], na.rm = TRUE),
        .groups = "drop"
      )
    
    aes_mapping <- if (auto_var == "none") {
      aes(x = .data[[x_var]],
          y = Value,
          color = method,
          group = method)
    } else {
      aes(x = .data[[x_var]],
          y = Value,
          color   = .data[[auto_var]],
          linetype = method,
          group   = interaction(.data[[auto_var]], method))
    }
    
    p <- ggplot(line_dat, aes_mapping) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        x = x_lab,
        y = metric,
        color   = auto_lab,
        linetype = if (auto_var == "none") NULL else "Method",
        title = paste("Line Plot of", metric, "vs", x_lab),
        subtitle = "Lines are averaged across selected scenarios"
      ) +
      theme_bw(base_size = 14) +
      theme(
        legend.position   = "bottom",
        plot.title        = element_text(face = "bold", size = 16),
        plot.subtitle     = element_text(size = 12, color = "gray40"),
        axis.text.x       = element_text(angle = 0, hjust = 1),
        panel.grid.minor  = element_blank(),
        panel.grid.major  = element_line(color = "#f0f0f0")
      )
    
    if (row_var == "none") {
      p <- p + facet_wrap(~ param_lab, scales = "free_y")
    } else {
      p <- p + facet_grid(as.formula(paste(row_var, "~ param_lab")),
                          scales = "free_y")
    }
    
    if (metric == "Bias") {
      p <- p + ylim(cap_min, cap_max)
    } else {
      p <- p + ylim(0, cap_max)
    }
    
    p
  }
  
  # ---- Plots on screen ----
  output$dist_plot <- renderPlot({
    dat <- df_filtered()
    req(nrow(dat) > 0)
    make_dist_plot(dat, input$error_cap, input$dist_type,
                   input$show_stats, method_colors)
  })
  
  output$metric_plot <- renderPlot({
    dat <- summary_stats_reactive()
    req(nrow(dat) > 0)
    make_metric_plot(dat, input$metric_type, input$viz_type,
                     input$metric_min, input$metric_cap,
                     input$group_by, method_colors)
  })
  
  output$comparison_plot <- renderPlot({
    dat <- summary_stats_reactive()
    req(nrow(dat) > 0)
    
    if (input$comp_plot_type == "box") {
      make_comparison_box_plot(dat, input$comp_metric, method_colors,
                               input$comp_x, input$comp_row,
                               input$comp_min, input$comp_cap)
    } else if (input$comp_plot_type == "bar") {
      make_comparison_bar_plot(dat, input$comp_metric, method_colors,
                               input$comp_x, input$comp_row,
                               input$comp_min, input$comp_cap)
    } else {
      make_comparison_line_plot(dat, input$comp_metric, method_colors,
                                input$comp_x, input$comp_row,
                                input$comp_auto,
                                input$comp_min, input$comp_cap)
    }
  })
  
  # ---- Tables ----
  output$metric_table <- renderDT({
    dat <- summary_stats_reactive()
    req(nrow(dat) > 0)
    
    dat <- dat %>%
      dplyr::select(
        Parameter = param_lab, Method = method, N, T,
        Delta = true_delta, Tau = true_tau, Eta = true_eta,
        Bias, RMSE, MAE
      ) %>%
      arrange(Method, Parameter, Delta, Tau, Eta, N, T)
    
    datatable(
      dat,
      options  = list(pageLength = 10, scrollX = TRUE, dom = "Blfrtip"),
      rownames = FALSE,
      caption  = "Performance Metrics Summary"
    ) %>%
      formatRound(columns = c("Bias", "RMSE", "MAE"), digits = 4)
  })
  
  # ====== Bias summary with median ======
  output$bias_summary_table <- renderDT({
    dat <- summary_table_reactive()
    req(nrow(dat) > 0)
    
    dat <- dat %>%
      dplyr::select(
        Method = method, Parameter = param_lab,
        `Mean Bias`   = Mean_Bias,
        `Median Bias` = Median_Bias,
        `Bias SD`     = SD_Bias,
        `Min Bias`    = Min_Bias,
        `Max Bias`    = Max_Bias,
        `Number Scenarios` = Total_Scenarios
      ) %>%
      arrange(Method, Parameter)
    
    datatable(
      dat,
      options  = list(pageLength = 8, scrollX = TRUE, dom = "t"),
      rownames = FALSE
    ) %>%
      formatRound(
        columns = c("Mean Bias", "Median Bias", "Bias SD",
                    "Min Bias", "Max Bias"),
        digits = 4
      )
  })
  
  # ====== MAE summary with median ======
  output$mae_summary_table <- renderDT({
    dat <- summary_table_reactive()
    req(nrow(dat) > 0)
    
    dat <- dat %>%
      dplyr::select(
        Method = method, Parameter = param_lab,
        `Mean MAE`   = Mean_MAE,
        `Median MAE` = Median_MAE,
        `MAE SD`     = SD_MAE,
        `Min MAE`    = Min_MAE,
        `Max MAE`    = Max_MAE,
        `Number Scenarios` = Total_Scenarios
      ) %>%
      arrange(Method, Parameter)
    
    datatable(
      dat,
      options  = list(pageLength = 8, scrollX = TRUE, dom = "t"),
      rownames = FALSE
    ) %>%
      formatRound(
        columns = c("Mean MAE", "Median MAE", "MAE SD",
                    "Min MAE", "Max MAE"),
        digits = 4
      )
  })
  
  # ====== RMSE summary with median ======
  output$rmse_summary_table <- renderDT({
    dat <- summary_table_reactive()
    req(nrow(dat) > 0)
    
    dat <- dat %>%
      dplyr::select(
        Method = method, Parameter = param_lab,
        `Mean RMSE`   = Mean_RMSE,
        `Median RMSE` = Median_RMSE,
        `RMSE SD`     = SD_RMSE,
        `Min RMSE`    = Min_RMSE,
        `Max RMSE`    = Max_RMSE,
        `Number Scenarios` = Total_Scenarios
      ) %>%
      arrange(Method, Parameter)
    
    datatable(
      dat,
      options  = list(pageLength = 8, scrollX = TRUE, dom = "t"),
      rownames = FALSE
    ) %>%
      formatRound(
        columns = c("Mean RMSE", "Median RMSE", "RMSE SD",
                    "Min RMSE", "Max RMSE"),
        digits = 4
      )
  })
  
  # ====== Detailed summary table formatting with medians ======
  output$detailed_summary_table <- renderDT({
    dat <- detailed_summary_reactive()
    req(nrow(dat) > 0)
    
    datatable(
      dat,
      options  = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE,
      caption  = "Detailed Performance Summary by Parameter Configuration"
    ) %>%
      formatRound(
        columns = c("Mean Bias", "Median Bias", "Bias SD",
                    "Mean MAE", "Median MAE", "MAE SD",
                    "Mean RMSE", "Median RMSE", "RMSE SD"),
        digits = 4
      )
  })
  
  # ====== Method comparison lollipop data ======
  method_comp_data <- reactive({
    stats <- summary_table_reactive()
    req(nrow(stats) > 0)
    req(input$mc_metrics)
    req(input$mc_stat)
    
    stats_sel <- stats %>%
      filter(param %in% input$param)
    
    stat_choice <- input$mc_stat
    
    stats_sel <- stats_sel %>%
      mutate(
        Bias_val = if (stat_choice == "Mean") Mean_Bias else Median_Bias,
        MAE_val  = if (stat_choice == "Mean") Mean_MAE  else Median_MAE,
        RMSE_val = if (stat_choice == "Mean") Mean_RMSE else Median_RMSE
      )
    
    dat <- stats_sel %>%
      dplyr::select(
        method, param_lab,
        Bias = Bias_val,
        MAE  = MAE_val,
        RMSE = RMSE_val
      ) %>%
      pivot_longer(
        cols      = c(Bias, MAE, RMSE),
        names_to  = "Metric",
        values_to = "Value"
      ) %>%
      filter(Metric %in% input$mc_metrics)
    
    dat$Metric <- factor(dat$Metric, levels = c("Bias", "MAE", "RMSE"))
    dat
  })
  
  # ====== Method comparison lollipop plot (reactive) ======
  method_comp_plot_reactive <- reactive({
    dat <- method_comp_data()
    req(nrow(dat) > 0)
    
    ggplot(dat, aes(x = Value, y = param_lab, color = method)) +
      geom_segment(aes(x = 0, xend = Value, yend = param_lab), size = 0.7) +
      geom_point(size = 3) +
      facet_wrap(~ Metric, scales = "free_x") +
      scale_color_manual(values = method_colors) +
      theme_bw(base_size = 14) +
      labs(
        title = "Comparison of Bayesian, GMM, and QML",
        subtitle = paste(input$mc_stat, "metrics across selected scenarios and parameters"),
        x = "Error Value",
        y = "Parameter",
        color = "Method"
      ) +
      theme(
        legend.position = "bottom",
        plot.title      = element_text(face = "bold", size = 16),
        plot.subtitle   = element_text(size = 12, color = "gray40"),
        strip.text      = element_text(face = "bold")
      )
  })
  
  output$method_comp_plot <- renderPlot({
    method_comp_plot_reactive()
  })
  
  # ---- Reactive plots for download ----
  dist_plot_reactive <- reactive({
    dat <- df_filtered()
    req(nrow(dat) > 0)
    make_dist_plot(dat, input$error_cap, input$dist_type,
                   input$show_stats, method_colors)
  })
  
  metric_plot_reactive <- reactive({
    dat <- summary_stats_reactive()
    req(nrow(dat) > 0)
    make_metric_plot(dat, input$metric_type, input$viz_type,
                     input$metric_min, input$metric_cap,
                     input$group_by, method_colors)
  })
  
  comparison_plot_reactive <- reactive({
    dat <- summary_stats_reactive()
    req(nrow(dat) > 0)
    if (input$comp_plot_type == "box") {
      make_comparison_box_plot(dat, input$comp_metric, method_colors,
                               input$comp_x, input$comp_row,
                               input$comp_min, input$comp_cap)
    } else if (input$comp_plot_type == "bar") {
      make_comparison_bar_plot(dat, input$comp_metric, method_colors,
                               input$comp_x, input$comp_row,
                               input$comp_min, input$comp_cap)
    } else {
      make_comparison_line_plot(dat, input$comp_metric, method_colors,
                                input$comp_x, input$comp_row,
                                input$comp_auto,
                                input$comp_min, input$comp_cap)
    }
  })
  
  # ---- Download handlers: FIGURE & GLOBAL DATA ----
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("DSDPM_Analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      current_tab <- input$main_tabs
      
      p <- if (current_tab == "Distribution Analysis") {
        dist_plot_reactive()
      } else if (current_tab == "Performance Metrics") {
        metric_plot_reactive()
      } else if (current_tab == "Comparative Analysis") {
        comparison_plot_reactive()
      } else if (current_tab == "Method Comparison") {
        method_comp_plot_reactive()
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
      
      addWorksheet(wb, "Bias_MAE_RMSE_Summary")
      writeData(wb, "Bias_MAE_RMSE_Summary", summary_table_reactive())
      
      addWorksheet(wb, "Line_Chart_Data")
      writeData(wb, "Line_Chart_Data", line_data_nt_reactive())
      
      addWorksheet(wb, "Filtered_Raw_Data")
      writeData(wb, "Filtered_Raw_Data", df_filtered())
      
      addWorksheet(wb, "Detailed_Summary")
      writeData(wb, "Detailed_Summary", detailed_summary_reactive())
      
      addWorksheet(wb, "Configuration_Summary")
      config_summary <- data.frame(
        Parameter = c("Selected Parameters", "Selected Methods",
                      "Delta Values", "Tau Values", "Eta Values",
                      "N Filter", "T Filter",
                      "Export Time"),
        Value = c(
          paste(input$param,        collapse = ", "),
          paste(input$method,       collapse = ", "),
          paste(input$delta_values, collapse = ", "),
          paste(input$tau_values,   collapse = ", "),
          paste(input$eta_values,   collapse = ", "),
          paste(input$N_filter,     collapse = ", "),
          paste(input$T_filter,     collapse = ", "),
          format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
      )
      writeData(wb, "Configuration_Summary", config_summary)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # ---- Download handlers: TABLE-SPECIFIC ----
  output$download_metric_table <- downloadHandler(
    filename = function() {
      paste0("Performance_Metrics_Summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      dat <- summary_stats_reactive() %>%
        dplyr::select(
          Parameter = param_lab, Method = method, N, T,
          Delta = true_delta, Tau = true_tau, Eta = true_eta,
          Bias, RMSE, MAE
        ) %>%
        arrange(Method, Parameter, Delta, Tau, Eta, N, T)
      
      wb <- createWorkbook()
      addWorksheet(wb, "Performance Metrics")
      writeData(wb, "Performance Metrics", dat)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$download_bias_summary <- downloadHandler(
    filename = function() {
      paste0("Bias_Summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      dat <- summary_table_reactive() %>%
        dplyr::select(
          Method = method, Parameter = param_lab,
          `Mean Bias`   = Mean_Bias,
          `Median Bias` = Median_Bias,
          `Bias SD`     = SD_Bias,
          `Min Bias`    = Min_Bias,
          `Max Bias`    = Max_Bias,
          `Number Scenarios` = Total_Scenarios
        ) %>%
        arrange(Method, Parameter)
      
      wb <- createWorkbook()
      addWorksheet(wb, "Bias Summary")
      writeData(wb, "Bias Summary", dat)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$download_mae_summary <- downloadHandler(
    filename = function() {
      paste0("MAE_Summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      dat <- summary_table_reactive() %>%
        dplyr::select(
          Method = method, Parameter = param_lab,
          `Mean MAE`   = Mean_MAE,
          `Median MAE` = Median_MAE,
          `MAE SD`     = SD_MAE,
          `Min MAE`    = Min_MAE,
          `Max MAE`    = Max_MAE,
          `Number Scenarios` = Total_Scenarios
        ) %>%
        arrange(Method, Parameter)
      
      wb <- createWorkbook()
      addWorksheet(wb, "MAE Summary")
      writeData(wb, "MAE Summary", dat)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$download_rmse_summary <- downloadHandler(
    filename = function() {
      paste0("RMSE_Summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      dat <- summary_table_reactive() %>%
        dplyr::select(
          Method = method, Parameter = param_lab,
          `Mean RMSE`   = Mean_RMSE,
          `Median RMSE` = Median_RMSE,
          `RMSE SD`     = SD_RMSE,
          `Min RMSE`    = Min_RMSE,
          `Max RMSE`    = Max_RMSE,
          `Number Scenarios` = Total_Scenarios
        ) %>%
        arrange(Method, Parameter)
      
      wb <- createWorkbook()
      addWorksheet(wb, "RMSE Summary")
      writeData(wb, "RMSE Summary", dat)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$download_detailed_summary <- downloadHandler(
    filename = function() {
      paste0("Detailed_Summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      dat <- detailed_summary_reactive()
      wb <- createWorkbook()
      addWorksheet(wb, "Detailed Summary")
      writeData(wb, "Detailed Summary", dat)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)



