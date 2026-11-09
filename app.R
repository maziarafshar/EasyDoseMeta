library(shiny)
library(shinydashboard)
library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(dosresmeta)
library(rms)
library(splines)
library(colourpicker)
library(DT)
library(rmarkdown)
library(pandoc)
library(knitr)
library(shinybusy)

# Helper function to parse themes dynamically
get_ggplot_theme <- function(theme_name, base_family, base_size) {
  if (theme_name == "theme_classic")   return(theme_classic(base_family = base_family, base_size = base_size))
  if (theme_name == "theme_bw")        return(theme_bw(base_family = base_family, base_size = base_size))
  if (theme_name == "theme_light")     return(theme_light(base_family = base_family, base_size = base_size))
  if (theme_name == "theme_dark")      return(theme_dark(base_family = base_family, base_size = base_size))
  if (theme_name == "theme_linedraw")  return(theme_linedraw(base_family = base_family, base_size = base_size))
  if (theme_name == "theme_grey")      return(theme_grey(base_family = base_family, base_size = base_size))
  if (theme_name == "theme_void")      return(theme_void(base_family = base_family, base_size = base_size))
  return(theme_minimal(base_family = base_family, base_size = base_size))
}

# Helper function to add custom lines to plot
add_custom_lines_to_plot <- function(p, lines_list, log_y = FALSE) {
  if (length(lines_list) == 0) return(p)
  for (l in lines_list) {
    if (l$type == "vline") {
      p <- p + geom_vline(xintercept = l$intercept, color = l$color, linetype = l$style, linewidth = 0.8)
      if (l$label != "") {
        p <- p + annotate("text", x = l$intercept, y = Inf, label = l$label, vjust = 1.5, hjust = -0.1, color = l$color)
      }
    } else {
      if (isTRUE(log_y) && (is.na(l$intercept) || l$intercept <= 0)) next
      p <- p + geom_hline(yintercept = l$intercept, color = l$color, linetype = l$style, linewidth = 0.8)
      if (l$label != "") {
        p <- p + annotate("text", x = Inf, y = l$intercept, label = l$label, hjust = 1.1, vjust = -0.5, color = l$color)
      }
    }
  }
  return(p)
}

# ==========================================
# UI
# ==========================================
ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = tags$span(icon("chart-line"), " EasyDoseMeta ", tags$small("v3.7", style = "font-size: 12px; opacity: 0.8; font-weight: normal;")),
    titleWidth = 320,
    tags$li(
      class = "dropdown",
      style = "padding-top: 15px; padding-right: 20px; display: flex; align-items: center; gap: 15px;",
      HTML('
        <style>
         .theme-circle {
            width: 18px; height: 18px; border-radius: 50%; cursor: pointer;
            box-shadow: 0 0 4px rgba(0,0,0,0.6); transition: transform 0.2s;
            border: 2px solid transparent;
          }
         .theme-circle:hover { transform: scale(1.3); }
         .theme-circle.active { border: 2px solid #ffffff; }
         .dark-mode-btn {
            cursor: pointer; font-size: 20px; transition: transform 0.2s; user-select: none;
            line-height: 1; padding-bottom: 2px;
          }
         .dark-mode-btn:hover { transform: scale(1.15); }
        </style>

        <div style="display: flex; gap: 10px; align-items: center; border-right: 1px solid #aaa; padding-right: 15px;">
          <div class="theme-circle" style="background-color: #1E8449;" onclick="setTheme(\'theme1\', this)" title="Dark Green & Teal"></div>
          <div class="theme-circle" style="background-color: #000080;" onclick="setTheme(\'theme2\', this)" title="Navy & Sand"></div>
          <div class="theme-circle" style="background-color: #9E003A;" onclick="setTheme(\'theme3\', this)" title="Cranberry & Black"></div>
          <div class="theme-circle active" style="background-color: #222222;" onclick="setTheme(\'theme4\', this)" title="Black & Navy"></div>
        </div>

        <div class="dark-mode-btn" id="darkModeToggle" onclick="toggleDarkMode()" title="Toggle Dark/Light Mode">🌙</div>

        <script>
          function setTheme(theme, el) {
            Shiny.setInputValue("theme_select", theme);
            document.querySelectorAll(".theme-circle").forEach(c => c.classList.remove("active"));
            el.classList.add("active");
          }

          let isDark = true;
          function toggleDarkMode() {
            isDark = !isDark;
            document.getElementById("darkModeToggle").innerText = isDark ? "🌙" : "☀️";
            Shiny.setInputValue("dark_mode", isDark);
          }

          $(document).on("shiny:connected", function() {
            Shiny.setInputValue("theme_select", "theme4");
            Shiny.setInputValue("dark_mode", isDark);
          });

          $(document).on("click", ".box-header", function(e) {
            if (!$(e.target).closest(".btn-box-tool").length) {
              var btn = $(this).find("[data-widget=\'collapse\']");
              if (btn.length) btn.click();
            }
          });
       </script>
      ')
    )
  ),
  
  dashboardSidebar(
    width = 320,
    tags$style(HTML("
      .sidebar-menu { padding: 6px 10px; }
      .sidebar .form-group { margin-bottom: 8px; }
      .sidebar .radio { margin-top: 2px; margin-bottom: 4px; }
      .sidebar .btn { margin-left: 0 !important; margin-right: 0 !important; }
      .main-sidebar .sidebar {
        position: sticky;
        top: 0;
        max-height: 100vh;
        overflow-y: auto;
        padding-bottom: 15px;
      }
      .sidebar-compact-row .col-sm-6 { padding-left: 6px; padding-right: 6px; }
      .sidebar-compact-row { margin-left: -6px; margin-right: -6px; }
    ")),
    sidebarMenu(
      id = "tabs",
      menuItem("Home & Instructions", tabName = "home", icon = icon("home")),
      menuItem("Data Upload & Options", tabName = "upload", icon = icon("upload")),
      menuItem("Analysis & Results", tabName = "analysis_results", icon = icon("chart-line")),
      menuItem("Model Comparison", tabName = "comparison", icon = icon("layer-group")),
      menuItem("Multi-Model Report", tabName = "multi_report", icon = icon("file-invoice"))
    ),
    conditionalPanel(
      condition = "input.tabs == 'upload'",
      hr(style = "border-color: #555; margin: 8px 15px;"),
      div(style = "padding: 6px 15px; color: #b8c7ce; font-weight: bold; text-transform: uppercase;", "1. Upload Data"),
      div(
        style = "padding: 6px 15px; box-sizing: border-box; width: 100%;",
        fileInput("file", "Data File (.xlsx, .xls, .csv, .tsv)", accept = c(".xlsx", ".xls", ".csv", ".tsv", ".txt")),
        uiOutput("sheet_ui"),
        actionButton("load_data", "Load Data", style = "width: 100%; margin: 0 auto; display: block;", class = "btn-primary", icon = icon("play")),
        actionButton("load_example_data", "Load Example Dataset", style = "width: 100%; margin: 8px auto 0 auto; display: block;", class = "btn-info btn-sm", icon = icon("database"))
      )
    ),
    conditionalPanel(
      condition = "input.tabs == 'analysis_results'",
      hr(style = "border-color: #555; margin: 6px 15px;"),
      div(
        style = "padding: 4px 15px 8px 15px; box-sizing: border-box; width: 100%;",
        actionButton(
          "run_analysis", "Run Analysis",
          class = "btn-success",
          style = "width: 100%; margin: 0 auto !important; display: block; font-weight: bold; font-size: 15px; padding: 8px 12px;",
          icon = icon("rocket"),
          onclick = "window.scrollTo({top: 0, behavior: 'smooth'});"
        )
      ),
      tags$div(
        style = "background: rgba(255,255,255,0.06); padding: 8px 10px; border-radius: 5px; margin: 0 15px 8px 15px;",
        tags$h5(class = "sidebar-section-title", style = "margin-top: 0; margin-bottom: 6px; color: #63c5da !important; font-size: 13px; font-weight: bold; text-transform: uppercase;", "Model Configuration"),
        selectInput("model_type", "Model Type:", choices = c("Linear", "Quadratic", "Spline")),
        conditionalPanel(
          condition = "input.model_type == 'Spline'",
          tags$div(
            title = "rcs: Restricted Cubic Spline (smooth, constrained tails). bs: B-Spline (flexible). ns: Natural Spline (linear tails).",
            selectInput("spline_type", "Spline Type:", choices = c("Restricted Cubic (rcs)" = "rcs", "B-Spline (bs)" = "bs", "Natural Spline (ns)" = "ns"))
          ),
          selectInput("knot_method", "Knot Placement:", choices = c("Auto (Number of Knots)" = "auto", "Custom Percentiles" = "percentiles", "Custom Doses" = "doses")),
          conditionalPanel(
            condition = "input.knot_method == 'auto'",
            numericInput("knots", "Number of Knots:", value = 3, min = 3, max = 7)
          ),
          conditionalPanel(
            condition = "input.knot_method != 'auto'",
            textInput("custom_knots", "Values (comma-sep.):", placeholder = "e.g., 10, 50, 90")
          )
        )
      ),
      tags$div(
        style = "background: rgba(255,255,255,0.06); padding: 8px 10px; border-radius: 5px; margin: 0 15px 8px 15px;",
        tags$h5(class = "sidebar-section-title", style = "margin-top: 0; margin-bottom: 6px; color: #63c5da !important; font-size: 13px; font-weight: bold; text-transform: uppercase;", "Reference & Estimation"),
        div(
          class = "row sidebar-compact-row",
          column(6, selectInput("proc", "Procedure:", choices = c("1-stage" = "1stage", "2-stage" = "2stage"), selected = "2stage")),
          column(
            6,
            tags$div(
              title = "Use ML (Maximum Likelihood) when comparing AIC/BIC across models with different spline or polynomial terms.",
              selectInput("est_method", "Estimator:",
                          choices = c("REML" = "reml", "ML" = "ml",
                                      "Fixed" = "fixed", "MM" = "mm"),
                          selected = "reml")
            )
          )
        ),
        numericInput("ref_dose", "Reference Dose (blank = min dose):", value = NA)
      ),
      tags$details(
        style = "background: rgba(255,255,255,0.06); padding: 8px 10px; border-radius: 5px; margin: 0 15px 10px 15px;",
        tags$summary(
          class = "sidebar-section-title",
          style = "cursor: pointer; color: #63c5da !important; font-size: 13px; font-weight: bold; text-transform: uppercase;",
          icon("sliders-h"), " Prediction Range"
        ),
        div(style = "padding-top: 6px;", uiOutput("pred_range_ui"))
      )
    )
  ),
  
  dashboardBody(
    add_busy_spinner(spin = "double-bounce", color = "#3c8dbc", position = "top-right", margins = c(15, 15)),
    uiOutput("theme_css"),
    tabItems(
      tabItem(
        tabName = "home",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "Welcome to EasyDoseMeta: Dose-Response Meta-Analysis Suite",
            h3("About This Application"),
            p(
              "EasyDoseMeta is a comprehensive interactive environment for visual and statistical synthesis of linear and non-linear dose-response relationships across epidemiological studies, powered by the ",
              tags$code("dosresmeta"), ", ", tags$code("rms"), ", and ", tags$code("splines"), " R packages."
            ),
            tags$details(
              open = "open",
              tags$summary(style = "cursor:pointer;font-weight:bold;color:#3c8dbc;", icon("file-excel"), " Sample Data & Required Formatting"),
              br(),
              p("Download the sample Excel dataset below or click ", strong("Load Example Dataset"), " in the ", strong("Data Upload & Options"), " sidebar to test all features immediately."),
              downloadButton("download_sample", "Download Sample Excel Data (.xlsx)", class = "btn-info"),
              br(), br(),
              tags$ul(
                tags$li(strong("Study ID:"), " Unique identifier for each study (each study requires at least 1 referent dose level; 2-stage models require \u22652 dose levels per study)."),
                tags$li(strong("Dose:"), " Quantitative exposure level (numeric). Automatic power-of-10 scaling is applied when max dose > 10 for numerical stability."),
                tags$li(strong("Effect Size:"), " Relative Risk (RR), Odds Ratio (OR), or Hazard Ratio (HR) on either the natural scale (> 0, referent = 1.0) or log scale (referent = 0.0). Strings such as ", tags$code("1 (Ref)"), " or ", tags$code("Ref"), " are automatically parsed."),
                tags$li(strong("Variability:"), " Standard Error (SE) or 95% Confidence Interval bounds (Lower & Upper CI). Referent rows with missing SE/CI are automatically assigned SE = 0."),
                tags$li(strong("Cases & Participants (N):"), " Required for Greenland & Longnecker (", tags$code("gl"), ") and Hamling (", tags$code("h"), ") within-study covariance reconstruction."),
                tags$li(strong("Study Type:"), " Epidemiological design code per row or globally: ", tags$code("ir"), " (incidence-rate / person-time), ", tags$code("ci"), " (cumulative incidence / cohort), or ", tags$code("cc"), " (case-control)."),
                tags$li(strong("Subgroup (Optional):"), " Categorical column (e.g., Region, Sex, Study Quality) for one-click stratified subgroup modeling.")
              )
            ),
            br(),
            tags$details(
              tags$summary(style = "cursor:pointer;font-weight:bold;color:#3c8dbc;", icon("list-ol"), " Step‑by‑Step Workflow & Key Features"),
              br(),
              tags$ol(
                tags$li(strong("Upload or Load Example Data:"), " Upload ", tags$code(".xlsx"), ", ", tags$code(".xls"), ", ", tags$code(".csv"), ", or ", tags$code(".tsv"), " files—or load the built-in 6-study dataset. Columns are automatically detected via smart regex matching."),
                tags$li(strong("Configure Model & Estimation:"), " Choose between ", strong("Linear"), ", ", strong("Quadratic"), ", or ", strong("Spline"), " models (Restricted Cubic Splines ", tags$code("rcs"), ", B-Splines ", tags$code("bs"), ", or Natural Splines ", tags$code("ns"), ") with automatic or custom percentile/dose knots. Select ", strong("1-stage"), " or ", strong("2-stage"), " synthesis and ", strong("REML"), ", ", strong("ML"), " (recommended when comparing AIC/BIC across different fixed-effect structures), ", strong("Fixed-Effects"), ", or ", strong("Method of Moments"), "."),
                tags$li(strong("Inspect Diagnostics & Sensitivity:"), " Review exact unscaled knot locations, interior Nadir / Peak risk estimates with 95% CIs, Wald non-linearity tests, Cochran's Q and I\u00b2 heterogeneity, and run ", strong("Leave-One-Out (LOO) Sensitivity Analysis"), " with spaghetti-curve overlays."),
                tags$li(strong("Store, Stratify & Compare Models:"), " Store multiple models or click ", strong("Fit & Store by Subgroup"), " to automatically fit and overlay subgroup-specific curves in the ", strong("Model Comparison"), " tab. Save and reload stored model workspaces via ", tags$code(".rds"), " files."),
                tags$li(strong("Export Publication Outputs:"), " Export high-resolution plots (", tags$code(".png"), ", ", tags$code(".pdf"), ", ", tags$code(".svg"), ", ", tags$code(".tiff"), "), 200-point curve CSVs, standalone ", strong("Reproducible R Scripts ("), tags$code(".R"), strong(")"), ", and comprehensive ", strong("HTML / PDF / Word Reports"), ".")
              )
            ),
            br(),
            tags$details(
              tags$summary(style = "cursor:pointer;font-weight:bold;color:#3c8dbc;", icon("book"), " Methodological References"),
              br(),
              tags$ul(
                tags$li("Crippa, A., & Orsini, N. (2016). ", tags$i("Multivariate Dose‑Response Meta‑Analysis: The dosresmeta R Package."), " Journal of Statistical Software, 72(1), 1–15."),
                tags$li("Crippa, A., Discacciati, A., Bottai, M., Spiegelman, D., & Orsini, N. (2019). ", tags$i("One-stage dose-response meta-analysis for aggregated data."), " Statistical Methods in Medical Research, 28(5), 1579–1596."),
                tags$li("Orsini, N., Li, R., Wolk, A., Khudyakov, P., & Spiegelman, D. (2012). ", tags$i("Meta-analysis for linear and nonlinear dose-response relations: examples, an evaluation of approximations, and software."), " American Journal of Epidemiology, 175(1), 66–73."),
                tags$li("Greenland, S., & Longnecker, M. P. (1992). ", tags$i("Methods for trend estimation from summarized dose-response data, with applications to meta-analysis."), " American Journal of Epidemiology, 135(11), 1301–1309."),
                tags$li("Hamling, J., Lee, P., Weitkunat, R., & Ambühl, M. (2008). ", tags$i("Facilitating meta-analyses by deriving relative effect and precision estimates for alternative comparisons from a set of estimates presented by exposure level or disease category."), " Statistics in Medicine, 27(7), 954–970.")
              )
            )
          )
        )
      ),
      
      tabItem(
        tabName = "upload",
        div(
          class = "row equal-height-row",
          box(
            title = "2. Data Configurations", width = 6, status = "primary", solidHeader = TRUE,
            fluidRow(
              column(
                6,
                radioButtons("es_scale", "Effect Size Scale in Data:", choices = c("Natural scale (RR, OR, HR)" = "natural", "Log scale" = "log")),
                radioButtons("se_config", "Standard Error Configuration:", choices = c("I have an SE column" = "se_col", "Calculate from CI" = "from_ci")),
                conditionalPanel(
                  condition = "input.se_config == 'from_ci'",
                  radioButtons("ci_scale", "Confidence Interval Scale in Data:", choices = c("Natural scale" = "natural", "Log scale" = "log"))
                )
              ),
              column(
                6,
                radioButtons("study_type_config", "Study Type (cc, ir, ci):", choices = c("Column in data" = "col", "Manual input for all" = "manual")),
                conditionalPanel(
                  condition = "input.study_type_config == 'manual'",
                  selectInput("manual_study_type", "Select Study Type:", choices = c("cc", "ci", "ir"))
                ),
                selectInput("cov_method", "Covariance Method:", choices = c("Greenland & Longnecker" = "gl", "Hamling" = "h", "Independent" = "indep"))
              )
            )
          ),
          box(
            title = "3. Column Mapping", width = 6, status = "primary", solidHeader = TRUE,
            uiOutput("mapping_ui")
          )
        ),
        fluidRow(
          box(
            title = "Data Preview", width = 12, status = "info",
            uiOutput("data_summary_banner"),
            DTOutput("data_preview")
          )
        )
      ),
      
      tabItem(
        tabName = "analysis_results",
        fluidRow(
          column(
            8,
            box(
              title = "Dose-Response Plot", width = 12, status = "primary",
              plotOutput("dr_plot", height = "460px")
            ),
            div(
              class = "row equal-height-row",
              box(
                title = "Store Model", width = 4, status = "primary", solidHeader = TRUE,
                textInput("model_name", "Model Name:", placeholder = "e.g., Spline 3 Knots"),
                actionButton("store_model", "Store Model", class = "btn-info btn-block", icon = icon("save")),
                uiOutput("stored_models_badge_ui"),
                hr(style = "margin: 10px 0;"),
                uiOutput("subgroup_store_ui")
              ),
              box(
                title = "Reference Lines", width = 8, status = "primary", solidHeader = TRUE,
                fluidRow(
                  column(5, selectInput("line_type", "Type", choices = c("Vertical (X)" = "vline", "Horizontal (Y)" = "hline"))),
                  column(3, numericInput("line_intercept", "Value", value = 1)),
                  column(4, textInput("line_label", "Label", value = "", placeholder = "Optional"))
                ),
                fluidRow(
                  column(4, colourInput("line_color", "Color", value = "#555555")),
                  column(4, selectInput("line_style", "Style", choices = c("dashed", "solid", "dotted"))),
                  column(4, actionButton("add_line", "Add Line", class = "btn-success btn-block", style = "margin-top: 25px;", icon = icon("plus")))
                ),
                fluidRow(
                  column(6, uiOutput("remove_line_ui")),
                  column(3, actionButton("remove_line_btn", "Remove", class = "btn-primary btn-block", style = "margin-top: 25px;", icon = icon("minus-circle"))),
                  column(3, actionButton("clear_lines", "Clear All", class = "btn-info btn-block", style = "margin-top: 25px;", icon = icon("trash")))
                )
              )
            ),
            fluidRow(
              box(
                title = tagList(icon("stethoscope"), " Model Diagnostics & Sensitivity Output"),
                width = 12, status = "info", collapsible = TRUE, collapsed = FALSE,
                tabsetPanel(
                  id = "diag_tabs",
                  type = "tabs",
                  tabPanel(
                    title = tagList(icon("filter"), " Leave-One-Out Table"),
                    value = "loo_tab",
                    br(),
                    uiOutput("loo_table_container_ui")
                  ),
                  tabPanel(
                    title = tagList(icon("terminal"), " Model Terminal Summary"),
                    value = "term_tab",
                    br(),
                    verbatimTextOutput("model_summary")
                  )
                )
              )
            )
          ),
          column(
            4,
            box(
              title = "Plot, Sensitivity & Export", width = 12, status = "primary",
              tabsetPanel(
                id = "right_control_tabs",
                type = "tabs",
                tabPanel(
                  "Plot",
                  br(),
                  fluidRow(
                    column(8, textInput("plot_title", "Plot Title", value = "")),
                    column(4, selectInput("plot_title_loc", "Title Loc.", choices = list("Left" = 0, "Center" = 0.5, "Right" = 1), selected = 0))
                  ),
                  fluidRow(
                    column(6, textInput("plot_xlab", "X-Axis Label", value = "Dose")),
                    column(6, textInput("plot_ylab", "Y-Axis Label", value = "Effect Size"))
                  ),
                  fluidRow(
                    column(4, colourInput("plot_color", "Color", value = "#0073C2")),
                    column(4, numericInput("plot_lwd", "Width", value = 1)),
                    column(4, selectInput("plot_linetype", "Line Type",
                                          choices = c("Solid" = "solid", "Dashed" = "dashed", "Dotted" = "dotted",
                                                      "Dot-Dash" = "dotdash", "Long Dash" = "longdash"),
                                          selected = "solid"))
                  ),
                  sliderInput("plot_alpha", "CI Transparency", min = 0, max = 1, value = 0.2),
                  
                  # Reference Line toggle (RR = 1)
                  checkboxInput("show_ref_one", "Show Reference Line at RR = 1", value = TRUE),
                  
                  # Rug Plot Options
                  hr(),
                  h5(tags$b("Rug Plot Options")),
                  checkboxInput("show_rug", "Add Rug Plot (Dose Distribution)", value = FALSE),
                  conditionalPanel(
                    condition = "input.show_rug == true",
                    fluidRow(
                      column(6, numericInput("rug_width", "Width / Thickness", value = 0.5, min = 0.1, max = 3, step = 0.1)),
                      column(6, sliderInput("rug_alpha", "Transparency", min = 0.05, max = 1, value = 0.5, step = 0.05))
                    ),
                    sliderInput("rug_length", "Height / Length (% of plot)", min = 0.01, max = 0.15, value = 0.03, step = 0.005),
                    checkboxInput("rug_auto_color", "Match Main Plot Color", value = TRUE),
                    conditionalPanel(
                      condition = "input.rug_auto_color == false",
                      colourInput("rug_color", "Rug Color", value = "#333333")
                    )
                  )
                ),
                tabPanel(
                  "Axes",
                  br(),
                  checkboxInput("log_y_scale", "Log Scale (Y-Axis)", value = FALSE),
                  textInput("y_axis_ticks", "Custom Y Ticks (comma-sep.)", placeholder = "e.g., 0.5, 1, 1.5"),
                  textInput("x_axis_ticks", "Custom X Ticks (comma-sep.)", placeholder = "e.g., 0, 10, 20"),
                  h6("Axis View Limits (coord_cartesian)"),
                  fluidRow(
                    column(6, textInput("plot_xlim", "X (min, max)", placeholder = "e.g., 0, 50")),
                    column(6, textInput("plot_ylim", "Y (min, max)", placeholder = "e.g., 0.5, 2"))
                  )
                ),
                tabPanel(
                  "Sensitivity",
                  br(),
                  p(
                    style = "font-size: 13px; opacity: 0.85;",
                    icon("filter"), " Leave-One-Out (LOO) sensitivity analysis re-fits the active model excluding one study at a time to evaluate whether any single study drives the dose-response curve."
                  ),
                  actionButton("run_loo", "Run Leave-One-Out Analysis", class = "btn-warning btn-block", style = "font-weight: bold;", icon = icon("play")),
                  div(
                    style = "margin-top: 10px;",
                    checkboxInput("show_loo_curves", "Overlay LOO Sensitivity Curves on Main Plot", value = TRUE)
                  ),
                  uiOutput("loo_status_badge_ui"),
                  uiOutput("download_loo_ui")
                ),
                tabPanel(
                  "Export",
                  br(),
                  selectInput("plot_theme", "Plot Theme", choices = c(
                    "Minimal" = "theme_minimal", "Classic" = "theme_classic",
                    "Black & White" = "theme_bw", "Line Draw" = "theme_linedraw",
                    "Light" = "theme_light", "Dark" = "theme_dark",
                    "Grey (Default)" = "theme_grey", "Void" = "theme_void"
                  )),
                  fluidRow(
                    column(6, selectInput("plot_font", "Font", choices = c("sans", "serif", "mono"))),
                    column(6, numericInput("plot_fontsize", "Size", value = 12))
                  ),
                  fluidRow(
                    column(4, numericInput("plot_w", "W (in)", value = 8)),
                    column(4, numericInput("plot_h", "H (in)", value = 6)),
                    column(4, numericInput("plot_dpi", "DPI", value = 300))
                  ),
                  selectInput("plot_fmt", "Format", choices = c("png", "pdf", "svg", "tiff")),
                  downloadButton("download_plot", "Download Plot", class = "btn-success btn-block", icon = icon("image")),
                  downloadButton("download_full_curve", "Download Full Curve Data (.csv)", class = "btn-primary btn-block", icon = icon("table"), style = "margin-top: 6px;"),
                  hr(),
                  selectInput("report_format", "Report Format", choices = c("html", "pdf", "word")),
                  downloadButton("download_report", "Download Report", class = "btn-info btn-block", icon = icon("file-alt")),
                  downloadButton("download_r_code", "Download R Script (.R)", class = "btn-warning btn-block", icon = icon("code"), style = "margin-top: 6px;")
                )
              )
            ),
            box(
              title = "Model Statistics", width = 12, status = "info",
              uiOutput("scaling_info"),
              uiOutput("nadir_val"),
              uiOutput("nonlin_p_val"),
              uiOutput("het_val"),
              uiOutput("linear_effect_ui_container"),
              tags$details(
                style = "margin-top: 8px;",
                tags$summary(
                  style = "cursor:pointer; font-weight:bold; color:#605ca8; padding: 6px 0; font-size: 14px;",
                  icon("calculator"), " Custom Dose Predictions"
                ),
                br(),
                uiOutput("custom_preds_ui")
              )
            )
          )
        )
      ),
      
      tabItem(
        tabName = "comparison",
        fluidRow(
          box(
            title = "Stored Models (with Fit Statistics)", width = 12, status = "primary", solidHeader = TRUE,
            uiOutput("stored_models_container_ui"),
            hr(),
            div(
              class = "row stored-models-actions",
              column(3, uiOutput("remove_model_ui")),
              column(2, actionButton("remove_model_btn", "Remove Selected", class = "btn-primary btn-block", style = "margin-top:25px;", icon = icon("minus-circle"))),
              column(2, actionButton("clear_models", "Clear All", class = "btn-info btn-block", style = "margin-top:25px;", icon = icon("trash"))),
              column(2, downloadButton("download_models_rds", "Save (.rds)", class = "btn-success btn-block", style = "margin-top:25px;", icon = icon("download"))),
              column(3, fileInput("upload_models_rds", "Load Saved (.rds):", accept = c(".rds", ".RDS"), width = "100%"))
            ),
            uiOutput("edit_model_ui")
          )
        ),
        fluidRow(
          column(
            8,
            box(
              title = "Model Comparison Plot", width = 12, status = "primary",
              plotOutput("comp_plot", height = "460px")
            )
          ),
          column(
            4,
            box(
              title = "Comparison Plot & Export", width = 12, status = "primary",
              tabsetPanel(
                type = "tabs",
                tabPanel(
                  "Plot",
                  br(),
                  fluidRow(
                    column(8, textInput("comp_plot_title", "Plot Title", value = "Model Comparison")),
                    column(4, selectInput("comp_plot_title_loc", "Title Loc.", choices = list("Left" = 0, "Center" = 0.5, "Right" = 1), selected = 0))
                  ),
                  fluidRow(
                    column(6, textInput("comp_plot_xlab", "X-Axis Label", value = "Dose")),
                    column(6, textInput("comp_plot_ylab", "Y-Axis Label", value = "Effect Size"))
                  ),
                  selectInput("comp_legend_loc", "Legend Position", choices = c("Bottom" = "bottom", "Right" = "right", "Top" = "top", "Left" = "left", "None" = "none"), selected = "bottom"),
                  checkboxInput("comp_show_ci", "Show 95% CI Ribbons", value = TRUE),
                  checkboxInput("comp_show_ref_one", "Show Reference Line at RR = 1", value = TRUE),
                  hr(),
                  h5(tags$b("Rug Plot Options (Comparison)")),
                  checkboxInput("comp_show_rug", "Add Rugs to Comparison Plot", value = FALSE),
                  conditionalPanel(
                    condition = "input.comp_show_rug == true",
                    fluidRow(
                      column(6, numericInput("comp_rug_width", "Width / Thickness", value = 0.5, min = 0.1, max = 3, step = 0.1)),
                      column(6, sliderInput("comp_rug_alpha", "Transparency", min = 0.05, max = 1, value = 0.5, step = 0.05))
                    ),
                    sliderInput("comp_rug_length", "Height / Length (% of plot)", min = 0.01, max = 0.15, value = 0.03, step = 0.005),
                    checkboxInput("comp_rug_auto_color", "Match Model Colors (Separated per Model)", value = TRUE),
                    conditionalPanel(
                      condition = "input.comp_rug_auto_color == false",
                      colourInput("comp_rug_color", "Unified Rug Color", value = "#333333")
                    )
                  )
                ),
                tabPanel(
                  "Axes",
                  br(),
                  checkboxInput("comp_log_y_scale", "Use Log Scale for Y-Axis", value = FALSE),
                  textInput("comp_y_axis_ticks", "Custom Y Ticks (comma-separated)", placeholder = "e.g., 0.5, 1, 1.5"),
                  textInput("comp_x_axis_ticks", "Custom X Ticks (comma-separated)", placeholder = "e.g., 0, 10, 20"),
                  h6("Axis View Limits (coord_cartesian)"),
                  fluidRow(
                    column(6, textInput("comp_xlim", "X Limits (min, max)", placeholder = "e.g., 0, 50")),
                    column(6, textInput("comp_ylim", "Y Limits (min, max)", placeholder = "e.g., 0.5, 2"))
                  )
                ),
                tabPanel(
                  "Export",
                  br(),
                  selectInput("comp_plot_theme", "Plot Theme", choices = c(
                    "Minimal" = "theme_minimal", "Classic" = "theme_classic",
                    "Black & White" = "theme_bw", "Line Draw" = "theme_linedraw",
                    "Light" = "theme_light", "Dark" = "theme_dark",
                    "Grey (Default)" = "theme_grey", "Void" = "theme_void"
                  )),
                  fluidRow(
                    column(6, selectInput("comp_plot_font", "Font", choices = c("sans", "serif", "mono"))),
                    column(6, numericInput("comp_plot_fontsize", "Font Size", value = 14))
                  ),
                  fluidRow(
                    column(4, numericInput("comp_plot_w", "Width (in)", value = 8)),
                    column(4, numericInput("comp_plot_h", "Height (in)", value = 6)),
                    column(4, numericInput("comp_plot_dpi", "DPI", value = 300))
                  ),
                  selectInput("comp_plot_fmt", "Format", choices = c("png", "pdf", "svg", "tiff")),
                  downloadButton("download_comp_plot", "Download Comparison Plot", class = "btn-success btn-block", icon = icon("download"))
                )
              )
            )
          )
        )
      ),
      
      tabItem(
        tabName = "multi_report",
        fluidRow(
          box(
            title = "Comprehensive Multi-Model Report", width = 12, status = "primary", solidHeader = TRUE,
            p("Generate a single document containing the configurations and significance metrics for all stored models."),
            h4("Models Included in Report:"),
            uiOutput("multi_report_preview_ui"),
            hr(),
            fluidRow(
              column(
                4,
                selectInput("multi_report_format", "Report Format", choices = c("html", "pdf", "word"))
              ),
              column(
                5,
                uiOutput("multi_report_btn_ui")
              )
            )
          )
        )
      )
    )
  )
)

# ==========================================
# Server
# ==========================================
server <- function(input, output, session) {
  make_example_dataset <- function() {
    data.frame(
      Study_ID    = c("Smith 2018", "Smith 2018", "Smith 2018",
                      "Chen 2019",  "Chen 2019",  "Chen 2019",
                      "Müller 2020","Müller 2020","Müller 2020",
                      "Patel 2021", "Patel 2021", "Patel 2021",
                      "Silva 2022", "Silva 2022", "Silva 2022",
                      "Tanaka 2023","Tanaka 2023","Tanaka 2023"),
      Dose        = c(0, 10, 25,
                      0, 15, 35,
                      0, 12, 30,
                      0, 8,  20,
                      0, 18, 40,
                      0, 14, 32),
      Effect_Size = c(1.00, 0.88, 1.12,
                      1.00, 0.84, 1.25,
                      1.00, 0.86, 1.18,
                      1.00, 0.92, 1.08,
                      1.00, 0.89, 1.38,
                      1.00, 0.85, 1.22),
      CI_Lower    = c(1.00, 0.76, 0.94,
                      1.00, 0.71, 1.03,
                      1.00, 0.74, 0.98,
                      1.00, 0.80, 0.91,
                      1.00, 0.75, 1.12,
                      1.00, 0.73, 1.01),
      CI_Upper    = c(1.00, 1.02, 1.33,
                      1.00, 0.99, 1.52,
                      1.00, 1.00, 1.42,
                      1.00, 1.06, 1.28,
                      1.00, 1.06, 1.70,
                      1.00, 0.99, 1.47),
      Cases       = c(60,  52,  70,
                      48,  41,  65,
                      55,  47,  68,
                      75,  69,  82,
                      42,  38,  62,
                      50,  43,  64),
      N           = c(1200, 1200, 1200,
                      950,  950,  950,
                      1100, 1100, 1100,
                      800,  800,  800,
                      700,  700,  700,
                      900,  900,  900),
      Study_Type  = c("ir", "ir", "ir",
                      "ir", "ir", "ir",
                      "ir", "ir", "ir",
                      "cc", "cc", "cc",
                      "cc", "cc", "cc",
                      "cc", "cc", "cc"),
      Subgroup    = c("North America & Europe", "North America & Europe", "North America & Europe",
                      "Asia-Pacific",           "Asia-Pacific",           "Asia-Pacific",
                      "North America & Europe", "North America & Europe", "North America & Europe",
                      "North America & Europe", "North America & Europe", "North America & Europe",
                      "Asia-Pacific",           "Asia-Pacific",           "Asia-Pacific",
                      "Asia-Pacific",           "Asia-Pacific",           "Asia-Pacific"),
      stringsAsFactors = FALSE
    )
  }

  output$download_sample <- downloadHandler(
    filename = function() {
      "sample_doseresponse_data.xlsx"
    },
    content = function(file) {
      writexl::write_xlsx(make_example_dataset(), file)
    }
  )
  
  output$theme_css <- renderUI({
    is_dark <- if (is.null(input$dark_mode)) TRUE else as.logical(input$dark_mode)
    theme <- input$theme_select
    if (is.null(theme)) theme <- "theme4"
    
    if (theme == "theme1") {
      primary <- if (is_dark) "#0F4224" else "#1E8449"
      accent <- if (is_dark) "#0A473B" else "#148F77"
    } else if (theme == "theme2") {
      primary <- if (is_dark) "#000040" else "#000080"
      accent <- if (is_dark) "#968A60" else "#C2B280"
    } else if (theme == "theme3") {
      primary <- if (is_dark) "#4F001D" else "#9E003A"
      accent <- if (is_dark) "#111111" else "#333333"
    } else if (theme == "theme4") {
      primary <- if (is_dark) "#000000" else "#222222"
      accent <- if (is_dark) "#000040" else "#000080"
    }
    
    bg_color <- if (is_dark) "#121212" else "#ecf0f5"
    box_bg <- if (is_dark) "#1E1E1E" else "#ffffff"
    txt_color <- if (is_dark) "#E0E0E0" else "#333333"
    dt_border <- if (is_dark) "#444444" else "#dddddd"
    
    custom_css <- paste0("
      <style>
       .content-wrapper, .right-side, .wrapper { background-color: ", bg_color, " !important; }
        body { color: ", txt_color, " !important; }
        .skin-black .main-header .navbar { background-color: ", primary, " !important; }
        .skin-black .main-header .logo { background-color: ", primary, " !important; filter: brightness(0.9); color: #ffffff !important; }
        .skin-black .main-header .logo:hover { background-color: ", primary, " !important; }
        .skin-black .main-header .navbar .sidebar-toggle,
        .skin-black .main-header .navbar .sidebar-toggle:hover { color: #ffffff !important; }
        .skin-black .main-header .navbar .nav > li > a { color: #ffffff !important; }
        .skin-black .left-side, .skin-black .main-sidebar { background-color: #222d32 !important; }
        .skin-black .sidebar a,
        .skin-black .sidebar span,
        .skin-black .sidebar label,
        .skin-black .sidebar .control-label,
        .skin-black .sidebar h4,
        .skin-black .sidebar .help-block { color: #d8e2e7 !important; }
        .skin-black .sidebar .sidebar-section-title,
        .skin-black .sidebar h5,
        .skin-black .sidebar details > summary { color: #63c5da !important; }
        .skin-black .sidebar-menu>li.active>a,
        .skin-black .sidebar-menu>li:hover>a {
          color: #ffffff !important;
          border-left-color: ", accent, " !important;
        }
        .skin-black .sidebar .form-control, .skin-black .sidebar .selectize-input {
          background-color: #374850 !important; color: #ffffff !important; border-color: #374850 !important;
        }
        .skin-black .sidebar .selectize-dropdown { background-color: #374850 !important; color: #ffffff !important; }
        .skin-black .sidebar .selectize-dropdown-content .active { background-color: #1a2226 !important; color: #ffffff !important; }
        .box { background-color: ", box_bg, " !important; border-top-color: ", primary, " !important; color: ", txt_color, " !important;}
        .box-header { color: ", txt_color, " !important; }
        .box.box-primary { border-top-color: ", primary, " !important; }
        .box.box-info { border-top-color: ", accent, " !important; }
        .box.box-warning { border-top-color: #f39c12 !important; }
        .box.box-primary>.box-header { background-color: ", primary, " !important; color: #ffffff !important; }
        .box.box-info>.box-header { background-color: ", accent, " !important; color: #ffffff !important; }
        .box.box-warning>.box-header { background-color: #f39c12 !important; color: #ffffff !important; }
        .box.box-primary>.box-header .box-title,
        .box.box-info>.box-header .box-title,
        .box.box-warning>.box-header .box-title { color: #ffffff !important; }
        .box-header .btn-box-tool { color: #ffffff !important; opacity: 0.85; }
        .box-header:has([data-widget='collapse']) { cursor: pointer; }
        .content-wrapper label { color: ", txt_color, " !important; }
        .content-wrapper .form-control, .content-wrapper .selectize-input {
          background-color: ", if (is_dark) "#2D2D2D" else "#fff", " !important;
          color: ", txt_color, " !important;
          border-color: ", dt_border, " !important;
        }
        .content-wrapper .selectize-dropdown { background-color: ", if (is_dark) "#2D2D2D" else "#fff", " !important; color: ", txt_color, " !important; }
        .content-wrapper .selectize-dropdown-content .active { background-color: ", if (is_dark) "#444" else "#f5f5f5", " !important; }
        .content-wrapper h1, .content-wrapper h2, .content-wrapper h3, .content-wrapper h4, .content-wrapper h6 { color: ", txt_color, " !important; }
        .content-wrapper h5 { color: ", txt_color, "; }
        .table { background-color: ", box_bg, " !important; color: ", txt_color, " !important; }
        .table>tbody>tr>td, .table>thead>tr>th { border-top: 1px solid ", dt_border, " !important; }
        table.dataTable tbody tr { background-color: ", box_bg, " !important; }
        .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter,
        .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,
        .dataTables_wrapper .dataTables_paginate { color: ", txt_color, " !important; }
        table.dataTable, table.dataTable th, table.dataTable td { color: ", txt_color, " !important; }
        table.dataTable thead th, table.dataTable tfoot th { color: ", txt_color, " !important; border-color: ", dt_border, " !important; }
        table.dataTable.no-footer { border-bottom-color: ", dt_border, " !important; }
        .shiny-plot-output { background-color: transparent !important; border-radius: 6px; padding: 0 !important; border: none !important; overflow: hidden; }
        .shiny-plot-output img { border-radius: 6px; display: block; width: 100%; }
        .shiny-output-error-validation { color: ", txt_color, " !important; opacity: 0.7; font-size: 15px; padding: 60px 20px; text-align: center; }
        .equal-height-row { display: flex; flex-wrap: wrap; }
        .equal-height-row > [class*='col-'] { display: flex; flex-direction: column; }
        .equal-height-row > [class*='col-'] > .box { flex: 1; display: flex; flex-direction: column; }
        .equal-height-row > [class*='col-'] > .box > .box-body { flex: 1; display: flex; flex-direction: column; justify-content: space-between; }
        .nav-tabs { border-bottom: 1px solid ", dt_border, " !important; }
        .nav-tabs > li > a {
          color: ", txt_color, " !important;
          background-color: transparent !important;
          border: 1px solid transparent !important;
          border-radius: 4px 4px 0 0 !important;
          opacity: 0.75;
        }
        .nav-tabs > li > a:hover {
          background-color: ", if (is_dark) "#2A2A2A" else "#f0f0f0", " !important;
          border-color: ", dt_border, " ", dt_border, " transparent !important;
          opacity: 1;
        }
        .nav-tabs > li.active > a,
        .nav-tabs > li.active > a:focus,
        .nav-tabs > li.active > a:hover {
          color: ", if (is_dark) "#ffffff" else primary, " !important;
          background-color: ", if (is_dark) "#2D2D2D" else "#ffffff", " !important;
          border: 1px solid ", dt_border, " !important;
          border-bottom-color: transparent !important;
          border-top: 3px solid ", accent, " !important;
          font-weight: bold;
          opacity: 1;
        }
        .stored-models-actions .form-group { margin-bottom: 0 !important; }
        .stored-models-actions .shiny-file-input-progress { display: none !important; }
        .btn-file {
          background-color: ", accent, " !important;
          border-color: ", accent, " !important;
          color: #ffffff !important;
        }
        .btn-file:hover { filter: brightness(0.85); color: #ffffff !important; }
        details > summary { transition: opacity 0.15s ease; }
        details > summary:hover { opacity: 0.85; }
        pre { background-color: ", if (is_dark) "#111111" else "#f5f5f5", " !important;
              color: ", if (is_dark) "#4AF626" else "#333333", " !important;
              border: 1px solid ", if (is_dark) "#333" else "#ccc", " !important; }
        .btn-primary { background-color: ", primary, " !important; border-color: ", primary, " !important; color: #ffffff !important;}
        .btn-primary:hover { filter: brightness(0.85); color: #ffffff !important;}
        .btn-info { background-color: ", accent, " !important; border-color: ", accent, " !important; color: #ffffff !important;}
        .btn-info:hover { filter: brightness(0.85); color: #ffffff !important;}
        .btn-success { background-color: ", accent, " !important; border-color: ", accent, " !important; color: #ffffff !important;}
        .btn-warning { background-color: #f39c12 !important; border-color: #e67e22 !important;}
        .btn-danger { background-color: #c0392b !important; border-color: #c0392b !important;}
      </style>
    ")
    HTML(custom_css)
  })
  
  rv <- reactiveValues(
    data = NULL, columns = NULL, model = NULL, predictions = NULL,
    stored_models = list(), model_counter = 0, sheets = NULL, actual_ref = NULL, scale_factor = 1,
    custom_lines = list(), dose_range_raw = NULL, dose_col_name = NULL,
    rug_data = NULL, fitted_config = NULL, loo_results = NULL
  )
  
  save_plot_file <- function(file, plot_obj, fmt, width, height, dpi) {
    if (identical(fmt, "svg") && !requireNamespace("svglite", quietly = TRUE)) {
      ggsave(file, plot = plot_obj, device = grDevices::svg, width = width, height = height, dpi = dpi)
    } else {
      ggsave(file, plot = plot_obj, width = width, height = height, dpi = dpi)
    }
  }
  
  guess_col <- function(cols, patterns) {
    for (pat in patterns) {
      m <- grep(pat, cols, ignore.case = TRUE, value = TRUE)
      if (length(m) > 0) return(m[1])
    }
    ""
  }
  
  extract_heterogeneity <- function(model, proc = NULL) {
    tryCatch({
      qt <- tryCatch(dosresmeta::qtest(model), error = function(e) NULL)
      if (is.null(qt)) {
        sm <- tryCatch(summary(model), error = function(e) NULL)
        if (!is.null(sm) && !is.null(sm$qstat)) qt <- sm$qstat
      }
      if (!is.null(qt) && !is.null(qt$Q) && !is.null(qt$df) && !is.null(qt$pvalue)) {
        Q_val  <- as.numeric(qt$Q[1])
        df_val <- as.numeric(qt$df[1])
        p_val  <- as.numeric(qt$pvalue[1])
        if (!is.na(Q_val) && !is.na(df_val) && df_val > 0) {
          I2_val <- max(0, 100 * (Q_val - df_val) / Q_val)
          i2_str <- paste0(round(I2_val, 1), "%")
          p_str  <- if (p_val < 0.001) "< 0.001" else as.character(signif(p_val, 3))
          het_level <- if (I2_val < 25) "Low heterogeneity" else if (I2_val < 50) "Moderate heterogeneity" else if (I2_val < 75) "Substantial heterogeneity" else "Considerable heterogeneity"
          return(list(
            html = paste0(
              "<b>I\u00b2 = ", i2_str, "</b> &nbsp;|&nbsp; Q = ", round(Q_val, 2),
              " <span style='color:#888; font-size:13px;'>(df = ", df_val, ", p = ", p_str, ")</span><br>",
              "<span style='font-size:13px; color:#666;'>", het_level, "</span>"
            ),
            md = paste0("I\u00b2 = **", i2_str, "** (Cochran's Q = ", round(Q_val, 2), ", df = ", df_val, ", p = ", p_str, " \u2014 ", het_level, ")"),
            i2 = i2_str,
            p_val = p_str
          ))
        }
      }
      if (!is.null(proc) && proc == "1stage") {
        return(list(
          html = "Cochran's Q & I\u00b2 require <b>2-stage</b> estimation <span style='color:#888; font-size:13px;'>(1-stage models estimate variance components directly; see Model Summary)</span>",
          md   = "Cochran's Q & I\u00b2 not applicable for 1-stage estimation (see variance components in Model Summary)",
          i2   = "N/A (1-stage)",
          p_val = "N/A"
        ))
      }
      list(
        html = "Not available for this model specification",
        md   = "Not available for this model specification",
        i2   = "N/A",
        p_val = "N/A"
      )
    }, error = function(e) {
      list(
        html = "Could not compute heterogeneity statistics",
        md   = "Could not compute heterogeneity statistics",
        i2   = "N/A",
        p_val = "N/A"
      )
    })
  }
  
  compute_nadir_summary <- function(preds, model_type = NULL) {
    n <- nrow(preds)
    idx_min  <- which.min(preds$pred)
    eff_min  <- round(preds$pred[idx_min], 3)
    lb_min   <- round(preds$ci.lb[idx_min], 3)
    ub_min   <- round(preds$ci.ub[idx_min], 3)
    dose_min <- round(preds$dose[idx_min], 2)
    ci_min_str <- paste0("(95% CI: ", lb_min, " to ", ub_min, ")")
    
    idx_max  <- which.max(preds$pred)
    eff_max  <- round(preds$pred[idx_max], 3)
    lb_max   <- round(preds$ci.lb[idx_max], 3)
    ub_max   <- round(preds$ci.ub[idx_max], 3)
    dose_max <- round(preds$dose[idx_max], 2)
    ci_max_str <- paste0("(95% CI: ", lb_max, " to ", ub_max, ")")
    
    is_nonlin  <- is.null(model_type) || model_type != "Linear"
    is_int_min <- (idx_min > 1L && idx_min < n) && is_nonlin
    is_int_max <- (idx_max > 1L && idx_max < n) && is_nonlin
    
    if (is_int_min) {
      html_out <- paste0(
        "Effect = <b>", eff_min, "</b> ", ci_min_str,
        " at dose = <b>", dose_min, "</b> <span style='color:#888; font-size:13px;'>(Interior Nadir)</span>"
      )
      md_out <- paste0(
        "RR = **", eff_min, "** ", ci_min_str,
        " at dose = **", dose_min, "** (Interior Nadir)"
      )
      if (is_int_max) {
        html_out <- paste0(
          html_out, "<br>Peak Effect = <b>", eff_max, "</b> ", ci_max_str,
          " at dose = <b>", dose_max, "</b> <span style='color:#888; font-size:13px;'>(Interior Peak)</span>"
        )
        md_out <- paste0(
          md_out, "; Peak RR = **", eff_max, "** ", ci_max_str,
          " at dose = **", dose_max, "** (Interior Peak)"
        )
      }
      list(html = html_out, md = md_out)
    } else if (is_int_max) {
      list(
        html = paste0(
          "No interior nadir; <b>Interior Peak</b>: Effect = <b>", eff_max, "</b> ", ci_max_str,
          " at dose = <b>", dose_max, "</b><br><span style='color:#888; font-size:13px;'>(Min Effect = <b>",
          eff_min, "</b> ", ci_min_str, " at boundary dose = <b>", dose_min, "</b>)</span>"
        ),
        md = paste0(
          "No interior nadir; Interior Peak RR = **", eff_max, "** ", ci_max_str,
          " at dose = **", dose_max, "** (min RR = **", eff_min, "** ", ci_min_str,
          " at boundary dose = **", dose_min, "**)"
        )
      )
    } else {
      list(
        html = paste0(
          "No interior nadir <span style='color:#888; font-size:13px;'>(monotonic curve; min Effect = <b>",
          eff_min, "</b> ", ci_min_str, " at boundary dose = <b>", dose_min, "</b>)</span>"
        ),
        md = paste0(
          "No interior nadir (monotonic curve; min RR = **",
          eff_min, "** ", ci_min_str, " at boundary dose = **", dose_min, "**)"
        )
      )
    }
  }
  
  robust_predict <- function(model, newdata, dose_col, xref) {
    cf <- coef(model)
    if (length(cf) == 1) {
      X_diff <- matrix(newdata[[dose_col]] - xref, ncol = 1)
      pred_val <- as.vector(X_diff %*% cf)
      V <- matrix(vcov(model), nrow = 1, ncol = 1)
      se_val <- sqrt(pmax(0, as.vector((X_diff %*% V) * X_diff)))
      return(data.frame(
        pred = exp(pred_val), ci.lb = exp(pred_val - 1.96 * se_val), ci.ub = exp(pred_val + 1.96 * se_val)
      ))
    }
    trms <- if (!is.null(model$formula_fixed)) {
      delete.response(terms(model$formula_fixed))
    } else {
      delete.response(terms(model))
    }
    X_pred <- model.matrix(trms, data = newdata)
    ref_df <- data.frame(dose = xref)
    names(ref_df)[1] <- dose_col
    X_ref <- model.matrix(trms, data = ref_df)
    if ("(Intercept)" %in% colnames(X_pred) && !"(Intercept)" %in% names(cf)) {
      X_pred <- X_pred[, -1, drop = FALSE]
      X_ref <- X_ref[, -1, drop = FALSE]
    }
    common_cols <- intersect(colnames(X_pred), names(cf))
    X_pred <- X_pred[, common_cols, drop = FALSE]
    X_ref <- X_ref[, common_cols, drop = FALSE]
    cf <- cf[common_cols]
    X_diff <- sweep(X_pred, 2, X_ref[1, ], "-")
    pred_val <- as.vector(X_diff %*% cf)
    V <- vcov(model)
    V <- V[common_cols, common_cols, drop = FALSE]
    se_val <- sqrt(pmax(0, rowSums((X_diff %*% V) * X_diff)))
    data.frame(
      pred = exp(pred_val), ci.lb = exp(pred_val - 1.96 * se_val), ci.ub = exp(pred_val + 1.96 * se_val)
    )
  }
  
  output$sheet_ui <- renderUI({
    req(input$file)
    ext <- tolower(tools::file_ext(input$file$name))
    if (ext %in% c("csv", "tsv", "txt")) {
      rv$sheets <- basename(input$file$name)
      selectInput("sheet", "Data Source:", choices = rv$sheets)
    } else {
      rv$sheets <- excel_sheets(input$file$datapath)
      selectInput("sheet", "Select Sheet:", choices = rv$sheets)
    }
  })
  
  observeEvent(input$load_data, {
    req(input$file)
    tryCatch(
      {
        ext <- tolower(tools::file_ext(input$file$name))
        if (ext %in% c("csv", "tsv", "txt")) {
          sep_char <- if (ext == "tsv") "\t" else ","
          df <- utils::read.csv(input$file$datapath, sep = sep_char, stringsAsFactors = FALSE, check.names = FALSE)
          if (ncol(df) == 1 && ext == "csv") {
            df_semi <- tryCatch(utils::read.csv(input$file$datapath, sep = ";", stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
            if (!is.null(df_semi) && ncol(df_semi) > 1) df <- df_semi
          }
        } else {
          req(input$sheet)
          df <- read_excel(input$file$datapath, sheet = input$sheet)
        }
        rv$data <- df
        rv$columns <- names(df)
        rv$model <- NULL
        rv$predictions <- NULL
        rv$dose_range_raw <- NULL
        rv$rug_data <- NULL
        rv$fitted_config <- NULL
        rv$loo_results <- NULL
        
        se_match <- guess_col(rv$columns, c("^se$", "^stderr$", "^std[_\\.]?err", "^se[_\\.]"))
        lb_match <- guess_col(rv$columns, c("^ci[_\\.]?l", "^lower", "^lb$", "^lci$", "^ll$"))
        ub_match <- guess_col(rv$columns, c("^ci[_\\.]?u", "^upper", "^ub$", "^uci$", "^ul$"))
        if (se_match == "" && lb_match != "" && ub_match != "") {
          updateRadioButtons(session, "se_config", selected = "from_ci")
        } else if (se_match != "") {
          updateRadioButtons(session, "se_config", selected = "se_col")
        }
        type_match <- guess_col(rv$columns, c("^type$", "study[_\\.]?type", "^design$"))
        if (type_match != "") {
          updateRadioButtons(session, "study_type_config", selected = "col")
        }
        
        showNotification("Data loaded successfully!", type = "message")
      },
      error = function(e) {
        showNotification(paste("Error loading data:", e$message), type = "error")
      }
    )
  })
  
  observeEvent(input$load_example_data, {
    df <- make_example_dataset()
    rv$data <- df
    rv$columns <- names(df)
    rv$model <- NULL
    rv$predictions <- NULL
    rv$dose_range_raw <- NULL
    rv$rug_data <- NULL
    rv$fitted_config <- NULL
    rv$loo_results <- NULL
    
    updateRadioButtons(session, "es_scale", selected = "natural")
    updateRadioButtons(session, "se_config", selected = "from_ci")
    updateRadioButtons(session, "ci_scale", selected = "natural")
    updateRadioButtons(session, "study_type_config", selected = "col")
    showNotification("Built-in example dataset loaded (6 studies, 18 dose levels, J-shaped curve with subgroups)!", type = "message", duration = 6)
  })
  
  output$mapping_ui <- renderUI({
    if (is.null(rv$columns)) {
      return(
        div(
          style = "text-align: center; padding: 35px 20px; opacity: 0.75;",
          icon("columns", style = "font-size: 32px; margin-bottom: 10px; color: #3c8dbc;"),
          p(style = "font-size: 14px; margin: 0;", "Upload a dataset or click ", tags$b("Load Example Dataset"), " in the left sidebar to map your columns.")
        )
      )
    }
    cols <- c("Select Column" = "", rv$columns)
    fluidRow(
      column(
        6,
        selectInput("col_id", "Study ID:", choices = cols,
                    selected = guess_col(rv$columns, c("^id$", "study[_\\.]?id", "^author", "^study$"))),
        selectInput("col_dose", "Dose:", choices = cols,
                    selected = guess_col(rv$columns, c("^dose$", "^exposure", "^conc", "dose"))),
        selectInput("col_es", "Effect Size:", choices = cols,
                    selected = guess_col(rv$columns, c("^rr$", "^or$", "^hr$", "^es$", "effect", "^log[_\\.]?rr", "^yi$"))),
        conditionalPanel(
          condition = "input.study_type_config == 'col'",
          selectInput("col_type", "Study Type:", choices = cols,
                      selected = guess_col(rv$columns, c("^type$", "study[_\\.]?type", "^design$")))
        )
      ),
      column(
        6,
        conditionalPanel(
          condition = "input.se_config == 'se_col'",
          selectInput("col_se", "Standard Error:", choices = cols,
                      selected = guess_col(rv$columns, c("^se$", "^stderr$", "^std[_\\.]?err", "^se[_\\.]")))
        ),
        conditionalPanel(
          condition = "input.se_config == 'from_ci'",
          selectInput("col_ci_lb", "CI Lower Bound:", choices = cols,
                      selected = guess_col(rv$columns, c("^ci[_\\.]?l", "^lower", "^lb$", "^lci$", "^ll$"))),
          selectInput("col_ci_ub", "CI Upper Bound:", choices = cols,
                      selected = guess_col(rv$columns, c("^ci[_\\.]?u", "^upper", "^ub$", "^uci$", "^ul$")))
        ),
        conditionalPanel(
          condition = "input.cov_method != 'indep'",
          selectInput("col_n", "Participants (N):", choices = cols,
                      selected = guess_col(rv$columns, c("^n$", "^total$", "person[_\\.]?years", "^py$", "^sample[_\\.]?size", "^participants"))),
          selectInput("col_cases", "Events/Cases:", choices = cols,
                      selected = guess_col(rv$columns, c("^cases$", "^events$", "^case$", "^event$", "^d$")))
        )
      )
    )
  })
  
  output$data_summary_banner <- renderUI({
    if (is.null(rv$data)) {
      return(
        div(
          style = "padding: 25px 20px; text-align: center; opacity: 0.75; border: 1px dashed #666; border-radius: 6px; margin-bottom: 10px;",
          icon("table", style = "font-size: 28px; color: #00c0ef; margin-bottom: 8px;"),
          p(style = "margin: 0; font-size: 14px;", "No dataset loaded yet. Upload a file or click ", tags$b("Load Example Dataset"), " in the left sidebar to preview your data.")
        )
      )
    }
    n_rows <- nrow(rv$data)
    n_cols <- ncol(rv$data)
    study_txt <- ""
    if (!is.null(input$col_id) && input$col_id != "" && input$col_id %in% names(rv$data)) {
      n_studies <- length(unique(stats::na.omit(rv$data[[input$col_id]])))
      study_txt <- paste0(" &nbsp;|&nbsp; <i class='fa fa-layer-group'></i> <b>Unique Studies:</b> ", n_studies)
    }
    dose_txt <- ""
    if (!is.null(input$col_dose) && input$col_dose != "" && input$col_dose %in% names(rv$data)) {
      d_vals <- suppressWarnings(as.numeric(rv$data[[input$col_dose]]))
      d_vals <- d_vals[!is.na(d_vals)]
      if (length(d_vals) > 0) {
        dose_txt <- paste0(" &nbsp;|&nbsp; <i class='fa fa-sliders-h'></i> <b>Dose Range:</b> ", round(min(d_vals), 3), " to ", round(max(d_vals), 3))
      }
    }
    div(
      style = "padding: 10px 14px; margin-bottom: 12px; border-left: 4px solid #00c0ef; background-color: rgba(0, 192, 239, 0.1); border-radius: 4px; font-size: 14px; display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px;",
      HTML(paste0(
        "<div><i class='fa fa-table'></i> <b>Total Rows:</b> ", n_rows,
        " &nbsp;|&nbsp; <b>Columns:</b> ", n_cols,
        study_txt, dose_txt, "</div>"
      )),
      actionButton("go_to_analysis", "Proceed to Analysis & Results \u2192", class = "btn-success btn-sm", icon = icon("chart-line"), style = "font-weight: bold;")
    )
  })
  
  observeEvent(input$go_to_analysis, {
    updateTabItems(session, "tabs", "analysis_results")
  })
  
  output$data_preview <- renderDT({
    req(rv$data)
    datatable(rv$data, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  observeEvent(input$run_analysis, {
    if (is.null(rv$data)) {
      showNotification("Please upload data first.", type = "error")
      return()
    }
    if (is.null(input$col_id) || input$col_id == "") {
      showNotification("Please select a Study ID column.", type = "error")
      return()
    }
    if (is.null(input$col_dose) || input$col_dose == "") {
      showNotification("Please select a Dose column.", type = "error")
      return()
    }
    if (is.null(input$col_es) || input$col_es == "") {
      showNotification("Please select an Effect Size column.", type = "error")
      return()
    }
    if (input$se_config == "from_ci") {
      if (is.null(input$col_ci_lb) || input$col_ci_lb == "") {
        showNotification("Please select the 'CI Lower Bound' column.", type = "error")
        return()
      }
      if (is.null(input$col_ci_ub) || input$col_ci_ub == "") {
        showNotification("Please select the 'CI Upper Bound' column.", type = "error")
        return()
      }
    } else {
      if (is.null(input$col_se) || input$col_se == "") {
        showNotification("Please select the 'Standard Error' column.", type = "error")
        return()
      }
    }
    if (input$study_type_config == "col") {
      if (is.null(input$col_type) || input$col_type == "") {
        showNotification("Please select the 'Study Type' column.", type = "error")
        return()
      }
    }
    if (input$cov_method != "indep") {
      if (is.null(input$col_n) || input$col_n == "") {
        showNotification("Please select the 'Participants (N)' column.", type = "error")
        return()
      }
      if (is.null(input$col_cases) || input$col_cases == "") {
        showNotification("Please select the 'Events/Cases' column.", type = "error")
        return()
      }
    }
    
    data <- rv$data
    tryCatch(
      {
        es_chr <- trimws(as.character(data[[input$col_es]]))
        ref_pat <- "(^ref$|\\bref\\b|reference|referent)"
        has_ref_token <- !is.na(es_chr) & grepl(ref_pat, es_chr, ignore.case = TRUE)
        if (input$se_config == "se_col") {
          se_chr <- trimws(as.character(data[[input$col_se]]))
          has_ref_token <- has_ref_token | (!is.na(se_chr) & grepl(ref_pat, se_chr, ignore.case = TRUE))
        } else {
          lb_chr <- trimws(as.character(data[[input$col_ci_lb]]))
          ub_chr <- trimws(as.character(data[[input$col_ci_ub]]))
          has_ref_token <- has_ref_token |
            (!is.na(lb_chr) & grepl(ref_pat, lb_chr, ignore.case = TRUE)) |
            (!is.na(ub_chr) & grepl(ref_pat, ub_chr, ignore.case = TRUE))
        }
        es_clean <- gsub("\\s*\\(.*\\)$", "", es_chr)
        data[[input$col_es]] <- suppressWarnings(as.numeric(es_clean))
        if (any(has_ref_token)) {
          data[[input$col_es]][has_ref_token & is.na(data[[input$col_es]])] <- if (input$es_scale == "natural") 1 else 0
        }
        data[[input$col_dose]] <- suppressWarnings(as.numeric(data[[input$col_dose]]))
        if (input$es_scale == "natural") {
          non_na_es <- data[[input$col_es]][!is.na(data[[input$col_es]])]
          if (length(non_na_es) > 0 && any(non_na_es <= 0)) {
            stop("Natural-scale Effect Sizes (RR, OR, HR) must be strictly positive (> 0). Check your data or switch Effect Size Scale to 'Log scale'.")
          }
          data$es_log <- log(data[[input$col_es]])
        } else {
          data$es_log <- data[[input$col_es]]
        }
        
        if (input$se_config == "se_col") {
          se_clean <- gsub("\\s*\\(.*\\)$", "", trimws(as.character(data[[input$col_se]])))
          data$se_calc <- suppressWarnings(as.numeric(se_clean))
          if (any(!is.na(data$se_calc) & data$se_calc < 0)) {
            stop("Standard Error values cannot be negative.")
          }
        } else {
          lb <- suppressWarnings(as.numeric(gsub("\\s*\\(.*\\)$", "", trimws(as.character(data[[input$col_ci_lb]])))))
          ub <- suppressWarnings(as.numeric(gsub("\\s*\\(.*\\)$", "", trimws(as.character(data[[input$col_ci_ub]])))))
          if (any(!is.na(lb) & !is.na(ub) & ub < lb)) {
            stop("CI Upper Bound cannot be smaller than CI Lower Bound. Please check your column mapping.")
          }
          if (input$ci_scale == "natural") {
            if (any((!is.na(lb) & lb <= 0) | (!is.na(ub) & ub <= 0))) {
              stop("Natural-scale Confidence Interval bounds must be strictly positive (> 0). Check your data or switch CI Scale to 'Log scale'.")
            }
            data$se_calc <- (log(ub) - log(lb)) / (2 * 1.96)
          } else {
            data$se_calc <- (ub - lb) / (2 * 1.96)
          }
        }
        
        if (input$study_type_config == "col") {
          data$type_calc <- tolower(trimws(as.character(data[[input$col_type]])))
        } else {
          data$type_calc <- tolower(trimws(as.character(input$manual_study_type)))
        }
        
        # Recover referent rows where Effect Size is null (RR=1 / logRR=0) and SE/CI is missing (NA) or labeled as 'Ref'
        is_ref_row <- has_ref_token | (!is.na(data$es_log) & abs(data$es_log) < 1e-8 & is.na(data$se_calc))
        data$se_calc[is_ref_row] <- 0
        
        essential_cols <- c("es_log", "se_calc", "type_calc", input$col_id, input$col_dose)
        
        if (input$cov_method != "indep") {
          data[[input$col_n]] <- as.numeric(data[[input$col_n]])
          data[[input$col_cases]] <- as.numeric(data[[input$col_cases]])
          essential_cols <- c(essential_cols, input$col_cases, input$col_n)
        }
        
        data <- data[complete.cases(data[, essential_cols]), ]
        if (nrow(data) == 0) stop("No complete cases remaining after cleaning data.")
        
        invalid_types <- setdiff(unique(data$type_calc), c("cc", "ir", "ci"))
        if (length(invalid_types) > 0) {
          stop(paste0("Invalid Study Type value(s): '", paste(invalid_types, collapse = "', '"), "'. Allowed values are 'cc', 'ir', or 'ci'."))
        }
        
        if (input$cov_method != "indep") {
          if (any(data[[input$col_n]] <= 0) || any(data[[input$col_cases]] < 0)) {
            stop("Participants (N) must be > 0 and Events/Cases must be >= 0.")
          }
          if (any(data[[input$col_cases]] > data[[input$col_n]])) {
            stop("Events/Cases cannot exceed Participants (N).")
          }
        }
        
        # Order each study so the referent row (se_calc == 0) comes first, then by ascending dose
        study_idx <- match(data[[input$col_id]], unique(data[[input$col_id]]))
        data <- data[order(study_idx, data$se_calc != 0, data[[input$col_dose]]), ]
        
        # Validate per-study referent row and minimum dose levels
        for (sid in unique(data[[input$col_id]])) {
          s_mask <- data[[input$col_id]] == sid
          s_se   <- data$se_calc[s_mask]
          if (length(s_se) < 2) {
            stop(paste0("Study '", sid, "' has fewer than 2 dose rows after cleaning."))
          }
          n_zero_se <- sum(s_se == 0)
          if (input$cov_method != "indep" && n_zero_se == 0) {
            stop(paste0("Study '", sid, "' has no reference row (SE = 0 or CI Lower = CI Upper = 1)."))
          }
          if (n_zero_se > 1) {
            stop(paste0("Study '", sid, "' has multiple rows with SE = 0 (or identical CI bounds). Only the reference dose row should have SE = 0."))
          }
        }
        
        dose_col <- input$col_dose
        dose_col_bt <- paste0("`", dose_col, "`")
        
        # Validate custom knots upfront and determine exact minimum dose levels per study for 2-stage estimation
        raw_vals <- NULL
        if (input$model_type == "Spline" && input$knot_method != "auto") {
          raw_vals <- suppressWarnings(as.numeric(trimws(unlist(strsplit(input$custom_knots, ",")))))
          raw_vals <- raw_vals[!is.na(raw_vals)]
          if (length(raw_vals) == 0) stop("Please provide valid comma-separated numbers for custom knots.")
          if (input$knot_method == "percentiles" && any(raw_vals < 0 | raw_vals > 100)) {
            stop("Percentiles must be between 0 and 100.")
          }
          if (input$spline_type == "rcs" && length(unique(raw_vals)) < 3L) {
            stop("Restricted cubic splines (rcs) require at least 3 distinct knot values.")
          }
        }
        
        if (input$proc == "2stage") {
          min_rows <- if (input$model_type == "Linear") {
            2L
          } else if (input$model_type == "Quadratic") {
            3L
          } else if (input$knot_method == "auto") {
            nk_est <- max(3L, as.integer(input$knots))
            if (input$spline_type == "rcs") nk_est else nk_est + 1L
          } else {
            k_len <- length(unique(raw_vals))
            n_params_est <- if (input$spline_type == "rcs") k_len - 1L else if (input$spline_type == "bs") k_len + 3L else k_len + 1L
            max(2L, n_params_est + 1L)
          }
          counts <- table(data[[input$col_id]])
          valid <- names(counts)[counts >= min_rows]
          n_dropped <- length(counts) - length(valid)
          data <- data[data[[input$col_id]] %in% valid, ]
          if (nrow(data) == 0) {
            stop(paste0("2-stage model requires studies with at least ", min_rows, " dose levels."))
          }
          if (n_dropped > 0) {
            showNotification(
              paste0(n_dropped, " study/studies with < ", min_rows, " dose levels excluded for 2-stage estimation."),
              type = "warning", duration = 8
            )
          }
        }
        
        max_dose_raw <- max(data[[dose_col]], na.rm = TRUE)
        rv$scale_factor <- if (max_dose_raw > 10) 10^(floor(log10(max_dose_raw))) else 1
        data[[dose_col]] <- data[[dose_col]] / rv$scale_factor
        min_dose_scaled <- min(data[[dose_col]], na.rm = TRUE)
        max_dose_scaled <- max(data[[dose_col]], na.rm = TRUE)
        
        if (is.na(input$ref_dose)) {
          rv$actual_ref <- min_dose_scaled
        } else {
          rv$actual_ref <- input$ref_dose / rv$scale_factor
        }
        
        knot_locations_str <- NULL
        if (input$model_type == "Linear") {
          fmla <- as.formula(paste("es_log ~", dose_col_bt))
        } else if (input$model_type == "Quadratic") {
          fmla <- as.formula(paste("es_log ~", dose_col_bt, "+ I(", dose_col_bt, "^2)"))
        } else if (input$model_type == "Spline") {
          k_method <- input$knot_method
          x_vec <- data[[dose_col]]
          k_str <- ""
          
          if (k_method == "auto") {
            nk <- max(3L, as.integer(input$knots))
            if (input$spline_type == "rcs") {
              calc_knots <- tryCatch(
                as.numeric(rms::rcspline.eval(x_vec, nk = nk, knots.only = TRUE)),
                error = function(e) NULL
              )
              if (is.null(calc_knots) || length(unique(calc_knots)) < 3) {
                probs <- switch(as.character(nk),
                  "3" = c(0.10, 0.50, 0.90),
                  "4" = c(0.05, 0.35, 0.65, 0.95),
                  "5" = c(0.05, 0.275, 0.50, 0.725, 0.95),
                  seq(0.05, 0.95, length.out = nk)
                )
                calc_knots <- sort(unique(unname(quantile(x_vec, probs = probs, na.rm = TRUE))))
                if (length(calc_knots) < 3) {
                  calc_knots <- sort(unique(unname(quantile(unique(x_vec), probs = probs, na.rm = TRUE))))
                }
              }
              if (length(calc_knots) < 3) {
                stop("Could not determine at least 3 distinct knot locations from the dose distribution.")
              }
              k_str <- paste0("c(", paste(signif(calc_knots, 10), collapse = ", "), ")")
              knot_locations_str <- paste(round(calc_knots * rv$scale_factor, 4), collapse = ", ")
            } else {
              tmp_basis <- if (input$spline_type == "bs") {
                splines::bs(x_vec, df = nk)
              } else {
                splines::ns(x_vec, df = nk)
              }
              int_knots <- as.numeric(attr(tmp_basis, "knots"))
              b_knots   <- as.numeric(attr(tmp_basis, "Boundary.knots"))
              b_str <- paste0("Boundary.knots = c(", paste(signif(b_knots, 10), collapse = ", "), ")")
              if (length(int_knots) > 0) {
                k_str <- paste0("knots = c(", paste(signif(int_knots, 10), collapse = ", "), "), ", b_str)
              } else {
                k_str <- b_str
              }
              knot_locations_str <- paste0(
                "Interior: ",
                if (length(int_knots) > 0) paste(round(int_knots * rv$scale_factor, 4), collapse = ", ") else "None",
                " | Boundary: ",
                paste(round(b_knots * rv$scale_factor, 4), collapse = ", ")
              )
            }
          } else {
            if (k_method == "percentiles") {
              calc_knots <- sort(unique(unname(quantile(x_vec, probs = raw_vals / 100, na.rm = TRUE))))
              if (input$spline_type == "rcs" && length(calc_knots) < 3) {
                calc_knots <- sort(unique(unname(quantile(unique(x_vec), probs = raw_vals / 100, na.rm = TRUE))))
              }
            } else {
              calc_knots <- sort(unique(raw_vals / rv$scale_factor))
            }
            
            if (input$spline_type == "rcs" && length(calc_knots) < 3) {
              stop("Restricted cubic splines (rcs) require at least 3 distinct knot values.")
            }
            
            knot_vec_str <- paste0("c(", paste(signif(calc_knots, 10), collapse = ", "), ")")
            
            if (input$spline_type == "rcs") {
              k_str <- knot_vec_str
              knot_locations_str <- paste(round(calc_knots * rv$scale_factor, 4), collapse = ", ")
            } else {
              b_str <- paste0("Boundary.knots = c(", paste(signif(c(min_dose_scaled, max_dose_scaled), 10), collapse = ", "), ")")
              k_str <- paste0("knots = ", knot_vec_str, ", ", b_str)
              knot_locations_str <- paste0(
                "Interior: ", paste(round(calc_knots * rv$scale_factor, 4), collapse = ", "),
                " | Boundary: ", paste(round(c(min_dose_scaled, max_dose_scaled) * rv$scale_factor, 4), collapse = ", ")
              )
            }
          }
          
          if (input$spline_type == "rcs") {
            fmla <- as.formula(paste("es_log ~ rcs(", dose_col_bt, ", ", k_str, ")"))
          } else if (input$spline_type == "bs") {
            fmla <- as.formula(paste("es_log ~ bs(", dose_col_bt, ", ", k_str, ")"))
          } else {
            fmla <- as.formula(paste("es_log ~ ns(", dose_col_bt, ", ", k_str, ")"))
          }
        }
        
        # Save empirical doses of included studies for rugs (on raw unscaled dose scale)
        rv$rug_data <- data.frame(dose = data[[dose_col]] * rv$scale_factor)
        
        if (input$cov_method == "gl") {
          studies <- unique(data[[input$col_id]])
          Slist <- lapply(studies, function(sid) {
            study_data <- data[data[[input$col_id]] == sid, ]
            covar.logrr(
              y = study_data$es_log, v = study_data$se_calc^2, type = study_data$type_calc,
              cases = study_data[[input$col_cases]], n = study_data[[input$col_n]]
            )
          })
          names(Slist) <- as.character(studies)
        } else {
          # Let dosresmeta handle 'h' (Hamling) and 'indep' (Independent) natively
          Slist <- NULL
        }
        
        est_method <- if (!is.null(input$est_method)) input$est_method else "reml"
        model <- dosresmeta(
          formula = fmla, id = data[[input$col_id]], type = data$type_calc, v = data$se_calc^2,
          cases = if (input$cov_method != "indep") data[[input$col_cases]] else NULL,
          n = if (input$cov_method != "indep") data[[input$col_n]] else NULL,
          data = data, proc = input$proc, method = est_method, covariance = input$cov_method, Slist = Slist
        )
        model$formula_fixed <- fmla
        
        m_type_label <- input$model_type
        if (m_type_label == "Spline") {
          knot_info <- if (input$knot_method == "auto") paste0(input$knots, " k") else paste0(input$knot_method, ": ", input$custom_knots)
          m_type_label <- paste0("Spline (", input$spline_type, ", ", knot_info, ")")
        }
        rv$fitted_config <- list(
          model_type     = input$model_type,
          model_label    = m_type_label,
          proc           = input$proc,
          est_method     = est_method,
          cov_method     = input$cov_method,
          col_id         = input$col_id,
          col_dose       = dose_col,
          col_cases      = if (input$cov_method != "indep") input$col_cases else NULL,
          col_n          = if (input$cov_method != "indep") input$col_n else NULL,
          fmla           = fmla,
          knot_locations = knot_locations_str,
          spline_type    = if (input$model_type == "Spline") input$spline_type else NULL,
          knot_method    = if (input$model_type == "Spline") input$knot_method else NULL,
          knots          = if (input$model_type == "Spline") input$knots else NULL,
          custom_knots   = if (input$model_type == "Spline") input$custom_knots else NULL,
          clean_data     = data,
          Slist          = Slist,
          sheet          = if (!is.null(input$sheet)) input$sheet else "Unknown Sheet"
        )
        rv$loo_results <- NULL
        
        rv$model <- model
        rv$dose_range_raw <- c(min_dose_scaled * rv$scale_factor, max_dose_scaled * rv$scale_factor)
        rv$dose_col_name  <- dose_col
        d_seq_scaled <- seq(min_dose_scaled, max_dose_scaled, length.out = 100)
        pred_data <- data.frame(dose = d_seq_scaled)
        names(pred_data)[1] <- dose_col
        p_df <- robust_predict(model, newdata = pred_data, dose_col = dose_col, xref = rv$actual_ref)
        
        preds <- data.frame(
          dose = d_seq_scaled * rv$scale_factor, pred = p_df$pred, ci.lb = p_df$ci.lb, ci.ub = p_df$ci.ub
        )
        rv$predictions <- preds
        showNotification("Analysis complete!", type = "message")
      },
      error = function(e) {
        msg <- e$message
        if (grepl("leading minor", msg, ignore.case = TRUE) || grepl("positive", msg, ignore.case = TRUE)) {
          msg <- "1-stage model failed to converge (singular covariance). Try switching to a '2-stage' procedure or a 'Linear' model."
        }
        showNotification(paste("Model Error:", msg), type = "error", duration = 15)
      }
    )
  })
  
  output$model_summary <- renderPrint({
    if (is.null(rv$model)) {
      cat("No model fitted yet. Click 'Run Analysis' in the left sidebar to view the dosresmeta terminal summary.")
      return(invisible(NULL))
    }
    summary(rv$model)
  })
  
  output$scaling_info <- renderUI({
    if (is.null(rv$model) || is.null(rv$scale_factor)) {
      return(
        div(
          style = "padding: 25px 15px; text-align: center; opacity: 0.75; border: 1px dashed #666; border-radius: 6px;",
          icon("calculator", style = "font-size: 26px; color: #3c8dbc; margin-bottom: 8px;"),
          p(style = "margin: 0; font-size: 14px;", "Click ", tags$b("Run Analysis"), " in the left sidebar to compute scaling, Nadir/Peak risk, Wald non-linearity, and heterogeneity statistics.")
        )
      )
    }
    if (rv$scale_factor == 1) {
      msg <- "Dose data was <b>not scaled</b>."
    } else {
      msg <- paste0("Dose data was <b>scaled</b> by a factor of <b>", rv$scale_factor, "</b> (values divided by ", rv$scale_factor, ") for modeling stability.")
    }
    if (!is.null(rv$fitted_config) && !is.null(rv$fitted_config$knot_locations)) {
      msg <- paste0(msg, "<br><b>Spline Knot Doses (Unscaled):</b> ", rv$fitted_config$knot_locations)
    }
    HTML(paste0(
      "<div style='padding: 12px; margin-bottom: 12px; border-left: 5px solid #999; background-color: rgba(153, 153, 153, 0.1); border-radius: 4px;'>",
      "<h5 style='margin-top: 0; margin-bottom: 5px; color: #999; font-weight: bold;'><i class='fa fa-info-circle'></i> Scaling &amp; Spline Knots</h5>",
      "<span style='font-size: 15px;'>", msg, "</span>",
      "</div>"
    ))
  })
  
  output$pred_range_ui <- renderUI({
    req(!is.null(rv$dose_range_raw))
    tagList(
      helpText(
        paste0("Data range: ", round(rv$dose_range_raw[1], 3), " to ", round(rv$dose_range_raw[2], 3)),
        style = "color: #63c5da !important; font-weight: bold; margin-bottom: 6px;"
      ),
      fluidRow(
        column(6, numericInput("pred_min", "From:", value = rv$dose_range_raw[1], step = 0.1)),
        column(6, numericInput("pred_max", "To:",   value = rv$dose_range_raw[2], step = 0.1))
      ),
      actionButton("reset_pred_range", "Reset to Data Range",
                   class = "btn-info btn-block btn-sm", icon = icon("undo"),
                   style = "margin-top: 4px;")
    )
  })
  
  observeEvent(input$reset_pred_range, {
    req(!is.null(rv$dose_range_raw))
    updateNumericInput(session, "pred_min", value = rv$dose_range_raw[1])
    updateNumericInput(session, "pred_max", value = rv$dose_range_raw[2])
  })
  
  output$nadir_val <- renderUI({
    preds <- plot_predictions()
    req(preds, rv$fitted_config)
    nadir_info <- compute_nadir_summary(preds, rv$fitted_config$model_type)
    HTML(paste0(
      "<div style='padding: 12px; margin-bottom: 12px; border-left: 5px solid #00a65a; background-color: rgba(0, 166, 90, 0.1); border-radius: 4px;'>",
      "<h5 style='margin-top: 0; margin-bottom: 5px; color: #00a65a; font-weight: bold;'><i class='fa fa-arrow-down'></i> NADIR (Minimum Effect)</h5>",
      "<span style='font-size: 15px;'>", nadir_info$html, "</span>",
      "</div>"
    ))
  })
  
  output$nonlin_p_val <- renderUI({
    req(rv$model, rv$fitted_config)
    cf <- coef(rv$model)
    V <- vcov(rv$model)
    n_coef <- length(cf)
    if (rv$fitted_config$model_type == "Linear" || n_coef < 2) {
      val <- "Not Applicable (Linear Model)"
    } else {
      idx <- 2:n_coef
      cf_nonlin <- cf[idx]
      V_nonlin <- V[idx, idx, drop = FALSE]
      val <- tryCatch(
        {
          W <- as.numeric(t(cf_nonlin) %*% solve(V_nonlin) %*% cf_nonlin)
          df <- length(cf_nonlin)
          p_val <- 1 - pchisq(W, df = df)
          if (p_val < 0.001) {
            paste0("p < 0.001 <span style='color:#888; font-size:13px;'>(df = ", df, ")</span><br><strong style='color:#e74c3c;'>Statistically significant non-linearity</strong>")
          } else {
            paste0("p = ", signif(p_val, 4), " <span style='color:#888; font-size:13px;'>(df = ", df, ")</span><br>", if (p_val < 0.05) "<strong style='color:#e74c3c;'>Statistically significant non-linearity</strong>" else "No significant non-linearity")
          }
        },
        error = function(e) "Could not compute Wald test (Covariance matrix issue)"
      )
    }
    HTML(paste0(
      "<div style='padding: 12px; margin-bottom: 12px; border-left: 5px solid #00c0ef; background-color: rgba(0, 192, 239, 0.1); border-radius: 4px;'>",
      "<h5 style='margin-top: 0; margin-bottom: 5px; color: #00c0ef; font-weight: bold;'><i class='fa fa-chart-area'></i> Test for Non-Linearity (Wald Test)</h5>",
      "<span style='font-size: 15px;'>", val, "</span>",
      "</div>"
    ))
  })
  
  output$het_val <- renderUI({
    req(rv$model, rv$fitted_config)
    het <- extract_heterogeneity(rv$model, rv$fitted_config$proc)
    HTML(paste0(
      "<div style='padding: 12px; margin-bottom: 12px; border-left: 5px solid #f39c12; background-color: rgba(243, 156, 18, 0.1); border-radius: 4px;'>",
      "<h5 style='margin-top: 0; margin-bottom: 5px; color: #f39c12; font-weight: bold;'><i class='fa fa-random'></i> Between-Study Heterogeneity</h5>",
      "<span style='font-size: 15px;'>", het$html, "</span>",
      "</div>"
    ))
  })
  
  output$linear_effect_ui_container <- renderUI({
    req(rv$model, rv$fitted_config, rv$fitted_config$model_type == "Linear")
    div(
      style = "padding: 12px; margin-bottom: 12px; border-left: 5px solid #d81b60; background-color: rgba(216, 27, 96, 0.1); border-radius: 4px;",
      h5(style = "margin-top: 0; color: #d81b60; font-weight: bold;", icon("chart-line"), " Linear Effect Estimate"),
      numericInput("linear_delta", "Enter Dose Increment (\u0394):", value = 1),
      uiOutput("linear_delta_val")
    )
  })
  
  linear_delta_data <- reactive({
    req(rv$model, rv$fitted_config, rv$fitted_config$model_type == "Linear", input$linear_delta)
    delta_raw <- input$linear_delta
    delta_scaled <- delta_raw / rv$scale_factor
    
    cf <- coef(rv$model)
    V <- vcov(rv$model)
    
    col_name <- rv$fitted_config$col_dose
    if (!(col_name %in% names(cf))) {
      col_name <- names(cf)[1]
    }
    
    beta <- cf[col_name]
    se <- sqrt(V[col_name, col_name])
    
    log_rr <- beta * delta_scaled
    log_se <- se * delta_scaled
    
    rr <- exp(log_rr)
    lb <- exp(log_rr - 1.96 * log_se)
    ub <- exp(log_rr + 1.96 * log_se)
    
    list(delta = delta_raw, rr = rr, lb = lb, ub = ub)
  })
  
  output$linear_delta_val <- renderUI({
    res <- linear_delta_data()
    req(res)
    val <- paste0(
      "For every <b>", res$delta, "</b> unit increase in dose:<br>",
      "Effect = <b>", round(res$rr, 3), "</b> (95% CI: ", round(res$lb, 3), " to ", round(res$ub, 3), ")"
    )
    HTML(paste0("<span style='font-size: 15px;'>", val, "</span>"))
  })
  
  output$custom_preds_ui <- renderUI({
    req(rv$model)
    tagList(
      textInput("custom_doses", "Doses (comma-separated):", placeholder = "e.g., 5, 10, 25"),
      tableOutput("custom_pred_tbl"),
      downloadButton("download_preds", "Download (CSV)", class = "btn-primary btn-sm", icon = icon("download"))
    )
  })
  
  custom_preds_data <- reactive({
    req(rv$model, rv$fitted_config, input$custom_doses, rv$actual_ref)
    dose_col <- rv$fitted_config$col_dose
    doses <- as.numeric(trimws(unlist(strsplit(input$custom_doses, ","))))
    doses <- doses[!is.na(doses)]
    req(length(doses) > 0)
    scaled_doses <- doses / rv$scale_factor
    pred_data <- data.frame(dose = scaled_doses)
    names(pred_data)[1] <- dose_col
    p_df <- robust_predict(rv$model, newdata = pred_data, dose_col = dose_col, xref = rv$actual_ref)
    data.frame(Dose = doses, Effect_Size = p_df$pred, CI_Lower = p_df$ci.lb, CI_Upper = p_df$ci.ub)
  })
  
  output$custom_pred_tbl <- renderTable({
    custom_preds_data()
  })
  
  # --- CUSTOM LINES LOGIC ---
  observeEvent(input$add_line, {
    req(input$line_intercept)
    line_id <- paste0("line_", as.integer(Sys.time()) + sample(1:1000, 1))
    rv$custom_lines[[line_id]] <- list(
      id = line_id, type = input$line_type, intercept = input$line_intercept,
      color = input$line_color, style = input$line_style, label = input$line_label
    )
  })
  
  output$remove_line_ui <- renderUI({
    if (length(rv$custom_lines) == 0) {
      return(selectInput("line_to_remove", "Active Lines:", choices = c("No custom lines added" = "")))
    }
    choices <- setNames(names(rv$custom_lines), sapply(rv$custom_lines, function(x) paste(ifelse(x$type == "vline", "X =", "Y ="), x$intercept, ifelse(x$label != "", paste("-", x$label), ""))))
    selectInput("line_to_remove", "Active Lines:", choices = choices)
  })

  observeEvent(input$remove_line_btn, {
    req(isTruthy(input$line_to_remove))
    rv$custom_lines[[input$line_to_remove]] <- NULL
  })
  
  observeEvent(input$clear_lines, {
    rv$custom_lines <- list()
  })
  
  # --- PLOT PREDICTIONS REACTIVE (custom range) ---
  plot_predictions <- reactive({
    req(rv$model, rv$dose_col_name, rv$actual_ref, !is.null(rv$dose_range_raw))
    use_min <- rv$dose_range_raw[1]
    use_max <- rv$dose_range_raw[2]
    if (!is.null(input$pred_min) && !is.na(input$pred_min)) use_min <- input$pred_min
    if (!is.null(input$pred_max) && !is.na(input$pred_max)) use_max <- input$pred_max
    if (use_min >= use_max) { use_min <- rv$dose_range_raw[1]; use_max <- rv$dose_range_raw[2] }
    d_seq <- seq(use_min / rv$scale_factor, use_max / rv$scale_factor, length.out = 200)
    pred_data <- data.frame(dose = d_seq)
    names(pred_data)[1] <- rv$dose_col_name
    tryCatch({
      p_df <- robust_predict(rv$model, newdata = pred_data, dose_col = rv$dose_col_name, xref = rv$actual_ref)
      data.frame(dose = d_seq * rv$scale_factor, pred = p_df$pred, ci.lb = p_df$ci.lb, ci.ub = p_df$ci.ub)
    }, error = function(e) rv$predictions)
  })
  
  # --- LEAVE-ONE-OUT SENSITIVITY ANALYSIS ---
  observeEvent(input$run_loo, {
    req(rv$model, rv$fitted_config)
    cfg <- rv$fitted_config
    studies <- unique(cfg$clean_data[[cfg$col_id]])
    if (length(studies) < 3) {
      showNotification("Leave-One-Out sensitivity analysis requires at least 3 included studies.", type = "warning")
      return()
    }
    
    active_preds <- plot_predictions()
    req(active_preds)
    d_seq_scaled <- active_preds$dose / rv$scale_factor
    pred_data <- data.frame(dose = d_seq_scaled)
    names(pred_data)[1] <- cfg$col_dose
    
    curve_list <- list()
    row_list   <- list()
    
    for (sid in studies) {
      sub_df <- cfg$clean_data[cfg$clean_data[[cfg$col_id]] != sid, , drop = FALSE]
      Slist_sub <- if (!is.null(cfg$Slist)) cfg$Slist[as.character(unique(sub_df[[cfg$col_id]]))] else NULL
      
      m_sub <- tryCatch(
        dosresmeta(
          formula = cfg$fmla,
          id = sub_df[[cfg$col_id]],
          type = sub_df$type_calc,
          v = sub_df$se_calc^2,
          cases = if (cfg$cov_method != "indep") sub_df[[cfg$col_cases]] else NULL,
          n = if (cfg$cov_method != "indep") sub_df[[cfg$col_n]] else NULL,
          data = sub_df,
          proc = cfg$proc,
          method = cfg$est_method,
          covariance = cfg$cov_method,
          Slist = Slist_sub
        ),
        error = function(e) NULL
      )
      
      if (!is.null(m_sub)) {
        m_sub$formula_fixed <- cfg$fmla
        p_sub <- tryCatch(
          robust_predict(m_sub, newdata = pred_data, dose_col = cfg$col_dose, xref = rv$actual_ref),
          error = function(e) NULL
        )
        if (!is.null(p_sub)) {
          sub_preds <- data.frame(
            dose = active_preds$dose,
            pred = p_sub$pred,
            ci.lb = p_sub$ci.lb,
            ci.ub = p_sub$ci.ub,
            Excluded_Study = as.character(sid),
            stringsAsFactors = FALSE
          )
          curve_list[[as.character(sid)]] <- sub_preds
          
          idx_min <- which.min(sub_preds$pred)
          min_str <- paste0(
            round(sub_preds$pred[idx_min], 3), " (",
            round(sub_preds$ci.lb[idx_min], 3), "\u2013",
            round(sub_preds$ci.ub[idx_min], 3), ") @ ",
            round(sub_preds$dose[idx_min], 2)
          )
          
          cf_sub <- coef(m_sub)
          V_sub  <- vcov(m_sub)
          wald_p <- if (cfg$model_type == "Linear" || length(cf_sub) < 2) {
            "N/A"
          } else {
            tryCatch({
              idx_w <- 2:length(cf_sub)
              W <- as.numeric(t(cf_sub[idx_w]) %*% solve(V_sub[idx_w, idx_w, drop = FALSE]) %*% cf_sub[idx_w])
              pv <- 1 - pchisq(W, df = length(idx_w))
              if (pv < 0.001) "< 0.001" else as.character(signif(pv, 3))
            }, error = function(e) "Error")
          }
          
          het_sub <- extract_heterogeneity(m_sub, cfg$proc)
          row_list[[as.character(sid)]] <- data.frame(
            Excluded_Study = as.character(sid),
            K_Remaining = length(unique(sub_df[[cfg$col_id]])),
            Min_Effect_95CI_at_Dose = min_str,
            Nonlin_Wald_p = wald_p,
            I2 = het_sub$i2,
            Het_p = het_sub$p_val,
            AIC = round(AIC(m_sub), 2),
            stringsAsFactors = FALSE
          )
        }
      }
    }
    
    if (length(row_list) == 0) {
      showNotification("Leave-One-Out models failed to converge.", type = "error")
      return()
    }
    
    rv$loo_results <- list(
      curves = do.call(rbind, curve_list),
      table  = do.call(rbind, row_list)
    )
    updateTabsetPanel(session, "diag_tabs", selected = "loo_tab")
    showNotification(paste0("Leave-One-Out sensitivity analysis completed across ", length(row_list), " studies."), type = "message")
  })
  
  output$loo_status_badge_ui <- renderUI({
    req(!is.null(rv$loo_results), !is.null(rv$loo_results$table))
    n_loo <- nrow(rv$loo_results$table)
    div(
      style = "margin: 8px 0; padding: 8px 10px; background: rgba(243, 156, 18, 0.14); border-left: 4px solid #f39c12; border-radius: 4px; font-size: 13px;",
      icon("check-circle", style = "color: #f39c12;"),
      tags$b(paste0(" LOO completed across ", n_loo, " studies.")),
      br(),
      tags$span(style = "font-size: 12px; opacity: 0.85;", "Dashed grey curves on the main plot show each leave-one-out fit; see the full table in the Diagnostics box below.")
    )
  })
  
  output$loo_table_container_ui <- renderUI({
    if (is.null(rv$loo_results) || is.null(rv$loo_results$table)) {
      return(
        div(
          style = "padding: 25px 20px; text-align: center; opacity: 0.8; border: 1px dashed #666; border-radius: 6px;",
          icon("filter", style = "font-size: 26px; color: #f39c12; margin-bottom: 8px;"),
          p(style = "margin: 0 0 10px 0; font-size: 14px;", "No Leave-One-Out sensitivity analysis has been run for the active model yet."),
          p(style = "margin: 0; font-size: 13px; opacity: 0.85;", "Open the ", tags$b("Sensitivity"), " tab in the ", tags$b("Plot, Sensitivity & Export"), " box (top-right) and click ", tags$b("Run Leave-One-Out Analysis"), ".")
        )
      )
    }
    DTOutput("loo_table")
  })
  
  output$loo_table <- renderDT({
    req(!is.null(rv$loo_results), !is.null(rv$loo_results$table))
    datatable(rv$loo_results$table, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  output$download_loo_ui <- renderUI({
    req(!is.null(rv$loo_results), !is.null(rv$loo_results$table))
    downloadButton("download_loo", "Download LOO (.csv)", class = "btn-info btn-block", icon = icon("download"))
  })
  
  output$download_loo <- downloadHandler(
    filename = function() {
      paste0("loo_sensitivity_analysis_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(!is.null(rv$loo_results), !is.null(rv$loo_results$table))
      write.csv(rv$loo_results$table, file, row.names = FALSE)
    }
  )
  
  # --- MAIN PLOT BUILDER ---
  build_plot <- reactive({
    preds <- plot_predictions()
    req(!is.null(preds) && nrow(preds) > 0)
    selected_theme <- get_ggplot_theme(input$plot_theme, input$plot_font, input$plot_fontsize)
    plot_lt <- if (!is.null(input$plot_linetype)) input$plot_linetype else "solid"
    
    p <- ggplot(preds, aes(x = dose, y = pred)) +
      geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), fill = input$plot_color, alpha = input$plot_alpha)
    
    if (isTruthy(input$show_loo_curves) && !is.null(rv$loo_results) && !is.null(rv$loo_results$curves) && nrow(rv$loo_results$curves) > 0) {
      p <- p + geom_line(
        data = rv$loo_results$curves,
        aes(x = dose, y = pred, group = Excluded_Study),
        inherit.aes = FALSE,
        color = "#7f8c8d",
        alpha = 0.45,
        linewidth = 0.6,
        linetype = "dashed"
      )
    }
    
    p <- p +
      geom_line(color = input$plot_color, linewidth = input$plot_lwd, linetype = plot_lt) +
      selected_theme +
      labs(title = input$plot_title, x = input$plot_xlab, y = input$plot_ylab) +
      theme(
        plot.title      = element_text(hjust = as.numeric(input$plot_title_loc)),
        legend.position = "none"
      )
    
    # Conditionally add reference line at RR = 1
    if (isTruthy(input$show_ref_one)) {
      p <- p + geom_hline(yintercept = 1, linetype = "dashed", color = "black")
    }
    
    # Optional Rug Plot addition
    if (isTruthy(input$show_rug) && !is.null(rv$rug_data)) {
      rug_c <- if (isTruthy(input$rug_auto_color)) input$plot_color else input$rug_color
      p <- p + geom_rug(
        data = rv$rug_data,
        aes(x = dose),
        inherit.aes = FALSE,
        sides = "b",
        color = rug_c,
        linewidth = input$rug_width,
        alpha = input$rug_alpha,
        length = unit(input$rug_length, "npc")
      )
    }
    
    p <- add_custom_lines_to_plot(p, rv$custom_lines, log_y = isTruthy(input$log_y_scale))
    
    custom_y_ticks <- NULL
    if (!is.null(input$y_axis_ticks) && trimws(input$y_axis_ticks) != "") {
      parsed_y <- suppressWarnings(as.numeric(unlist(strsplit(input$y_axis_ticks, ","))))
      custom_y_ticks <- parsed_y[!is.na(parsed_y)]
    }
    
    custom_x_ticks <- NULL
    if (!is.null(input$x_axis_ticks) && trimws(input$x_axis_ticks) != "") {
      parsed_x <- suppressWarnings(as.numeric(unlist(strsplit(input$x_axis_ticks, ","))))
      custom_x_ticks <- parsed_x[!is.na(parsed_x)]
    }
    
    if (isTruthy(input$log_y_scale)) {
      custom_y_ticks <- custom_y_ticks[custom_y_ticks > 0]
      if (length(custom_y_ticks) > 0) {
        p <- p + scale_y_continuous(trans = "log10", breaks = custom_y_ticks, minor_breaks = NULL, oob = scales::squish_infinite)
      } else {
        p <- p + scale_y_continuous(trans = "log10", minor_breaks = NULL, oob = scales::squish_infinite)
      }
    } else {
      if (length(custom_y_ticks) > 0) p <- p + scale_y_continuous(breaks = custom_y_ticks, minor_breaks = NULL)
    }
    
    if (length(custom_x_ticks) > 0) p <- p + scale_x_continuous(breaks = custom_x_ticks, minor_breaks = NULL)
    
    x_lim <- NULL
    if (!is.null(input$plot_xlim) && trimws(input$plot_xlim) != "") {
      parsed_xl <- suppressWarnings(as.numeric(unlist(strsplit(input$plot_xlim, ","))))
      parsed_xl <- parsed_xl[!is.na(parsed_xl)]
      if (length(parsed_xl) == 2) x_lim <- sort(parsed_xl)
    }
    y_lim <- NULL
    if (!is.null(input$plot_ylim) && trimws(input$plot_ylim) != "") {
      parsed_yl <- suppressWarnings(as.numeric(unlist(strsplit(input$plot_ylim, ","))))
      parsed_yl <- parsed_yl[!is.na(parsed_yl)]
      if (isTruthy(input$log_y_scale)) parsed_yl <- parsed_yl[parsed_yl > 0]
      if (length(parsed_yl) == 2) y_lim <- sort(parsed_yl)
    }
    if (!is.null(x_lim) || !is.null(y_lim)) {
      p <- p + coord_cartesian(xlim = x_lim, ylim = y_lim)
    }
    return(p)
  })
  
  output$dr_plot <- renderPlot({
    shiny::validate(
      shiny::need(rv$data,  "Please upload a dataset first."),
      shiny::need(rv$model, "Please click 'Run Analysis' to generate the plot."),
      shiny::need(!is.null(rv$dose_range_raw), "Running analysis...")
    )
    tryCatch(
      build_plot(),
      error = function(e) shiny::validate(shiny::need(FALSE, paste("Plot error:", conditionMessage(e))))
    )
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("dr_plot.", input$plot_fmt)
    },
    content = function(file) {
      save_plot_file(file, plot_obj = build_plot(), fmt = input$plot_fmt, width = input$plot_w, height = input$plot_h, dpi = input$plot_dpi)
    }
  )
  
  output$download_full_curve <- downloadHandler(
    filename = function() {
      paste0("dr_curve_predictions_", Sys.Date(), ".csv")
    },
    content = function(file) {
      preds <- plot_predictions()
      req(preds)
      out_df <- data.frame(
        Dose = preds$dose,
        Effect_Size = preds$pred,
        CI_Lower = preds$ci.lb,
        CI_Upper = preds$ci.ub
      )
      write.csv(out_df, file, row.names = FALSE)
    }
  )
  
  # --- REPORT GENERATION ---
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("DR_Analysis_Report_", Sys.Date(), ".", ifelse(input$report_format == "word", "docx", input$report_format))
    },
    content = function(file) {
      req(rv$model, rv$fitted_config, rv$data)
      active_preds <- plot_predictions()
      req(active_preds)
      tempReport <- file.path(tempdir(), "report.Rmd")
      out_fmt <- switch(input$report_format,
                        "html" = "html_document",
                        "pdf" = "pdf_document",
                        "word" = "word_document"
      )
      
      c_preds <- tryCatch(
        {
          if (isTruthy(input$custom_doses)) custom_preds_data() else NULL
        },
        error = function(e) NULL
      )
      
      l_delta <- tryCatch(
        {
          if (rv$fitted_config$model_type == "Linear" && isTruthy(input$linear_delta)) linear_delta_data() else NULL
        },
        error = function(e) NULL
      )
      
      comp_p <- tryCatch(
        {
          if (length(rv$stored_models) > 0) build_comp_plot() else NULL
        },
        error = function(e) NULL
      )
      
      nadir_md <- compute_nadir_summary(active_preds, rv$fitted_config$model_type)$md
      het_md   <- extract_heterogeneity(rv$model, rv$fitted_config$proc)$md
      knot_loc <- rv$fitted_config$knot_locations
      loo_tbl  <- if (!is.null(rv$loo_results) && !is.null(rv$loo_results$table) && nrow(rv$loo_results$table) > 0) rv$loo_results$table else NULL
      
      rmd_lines <- c(
        "---", "title: 'Dose-Response Meta-Analysis Report'", paste0("date: '", Sys.Date(), "'"), paste0("output: ", out_fmt),
        "params:", "  model: NA", "  model_type: NA", "  plot: NA", "  data_head: NA", "  nadir_md: NA", "  het_md: NA", "  scale_factor: NA", "  knot_locations: NA", "  custom_preds: NA", "  linear_delta: NA", "  comp_plot: NA", "  loo_tbl: NA", "---", "",
        "## 1. Data Overview (First 10 rows)", "```{r echo=FALSE}", "knitr::kable(params$data_head)", "```", "",
        "## 2. Model Summary", "```{r echo=FALSE}", "summary(params$model)", "```", "",
        "## 3. Statistical Details", "```{r echo=FALSE, results='asis'}",
        "if (params$scale_factor != 1) {", "  cat('- **Scaling:** Dose data was scaled by a factor of', params$scale_factor, '\\n')", "} else {", "  cat('- **Scaling:** Dose data was not scaled.\\n')", "}", "",
        "if (!is.null(params$knot_locations) && !is.na(params$knot_locations)) {", "  cat('- **Spline Knot Doses (Unscaled):**', params$knot_locations, '\\n')", "}", "",
        "cat('- **NADIR (Minimum Risk):**', params$nadir_md, '\\n')", "",
        "cat('- **Between-Study Heterogeneity:**', params$het_md, '\\n')", "",
        "cf <- coef(params$model)", "V <- vcov(params$model)", "n_coef <- length(cf)",
        "if (params$model_type == 'Linear' || n_coef < 2) {", "  cat('- **Non-Linearity:** Not Applicable (Linear Model)\\n')", "} else {",
        "  idx_wald <- 2:n_coef", "  cf_nonlin <- cf[idx_wald]", "  V_nonlin <- V[idx_wald, idx_wald, drop = FALSE]",
        "  out <- tryCatch({", "    W <- as.numeric(t(cf_nonlin) %*% solve(V_nonlin) %*% cf_nonlin)", "    df <- length(cf_nonlin)", "    p_val <- 1 - pchisq(W, df = df)", "    paste0('p = ', signif(p_val, 4), ' (df = ', df, ')')", "  }, error = function(e) 'Could not compute')",
        "  cat('- **Test for Non-Linearity (Wald Test):**', out, '\\n')", "}", "```", "",
        "```{r echo=FALSE, results='asis'}",
        "if (!is.null(params$linear_delta)) {",
        "  cat('## 4. Linear Effect Estimate\\n\\n')",
        "  cat('For a dose increment of **', params$linear_delta$delta, '**, the estimated effect is:\\n\\n')",
        "  cat('**RR:** ', round(params$linear_delta$rr, 3), ' (95% CI: ', round(params$linear_delta$lb, 3), ' to ', round(params$linear_delta$ub, 3), ')\\n\\n')",
        "}", "```", "",
        "## 5. Dose-Response Plot", "```{r echo=FALSE, warning=FALSE, message=FALSE, fig.width=8, fig.height=6}", "print(params$plot)", "```", "",
        "```{r echo=FALSE, results='asis'}", "if (!is.null(params$custom_preds) && is.data.frame(params$custom_preds)) {", "  cat('## 6. Custom Predictions\\n\\n')", " print(knitr::kable(params$custom_preds))", "  cat('\\n\\n')", "}", "```", "",
        "```{r echo=FALSE, results='asis'}", "if (!is.null(params$comp_plot)) {", "  cat('## 7. Model Comparison Plot\\n\\n')", "}", "```",
        "```{r echo=FALSE, warning=FALSE, message=FALSE, fig.width=8, fig.height=6}", "if (!is.null(params$comp_plot)) {", "  print(params$comp_plot)", "}", "```", "",
        "```{r echo=FALSE, results='asis'}", "if (!is.null(params$loo_tbl) && is.data.frame(params$loo_tbl) && nrow(params$loo_tbl) > 0) {", "  cat('## 8. Leave-One-Out Sensitivity Analysis\\n\\n')", "  print(knitr::kable(params$loo_tbl, row.names = FALSE))", "  cat('\\n\\n')", "}", "```"
      )
      writeLines(rmd_lines, tempReport)
      params <- list(model = rv$model, model_type = rv$fitted_config$model_type, plot = build_plot(), data_head = head(rv$data, 10), nadir_md = nadir_md, het_md = het_md, scale_factor = rv$scale_factor, knot_locations = knot_loc, custom_preds = c_preds, linear_delta = l_delta, comp_plot = comp_p, loo_tbl = loo_tbl)
      tryCatch(
        rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv())),
        error = function(e) {
          showNotification(
            paste0("Report generation failed: ", e$message, ". If exporting to PDF, ensure a LaTeX distribution (e.g., tinytex::install_tinytex()) is installed, or choose HTML/Word format."),
            type = "error", duration = 12
          )
          stop(e)
        }
      )
    }
  )
  
  output$download_r_code <- downloadHandler(
    filename = function() {
      paste0("reproducible_dosresmeta_analysis_", Sys.Date(), ".R")
    },
    content = function(file) {
      req(rv$model, rv$fitted_config)
      cfg <- rv$fitted_config
      fmla_str <- paste(deparse(cfg$fmla, width.cutoff = 500L), collapse = " ")
      cases_arg <- if (!is.null(cfg$col_cases)) paste0("data[[\"", cfg$col_cases, "\"]]") else "NULL"
      n_arg     <- if (!is.null(cfg$col_n))     paste0("data[[\"", cfg$col_n, "\"]]")     else "NULL"
      
      slist_block <- if (cfg$cov_method == "gl") {
        c(
          "# Compute Greenland & Longnecker covariance matrices per study",
          paste0("studies <- unique(data[[\"", cfg$col_id, "\"]])"),
          "Slist <- lapply(studies, function(sid) {",
          paste0("  study_data <- data[data[[\"", cfg$col_id, "\"]] == sid, ]"),
          "  dosresmeta::covar.logrr(",
          "    y = study_data$es_log, v = study_data$se_calc^2, type = study_data$type_calc,",
          paste0("    cases = study_data[[\"", cfg$col_cases, "\"]], n = study_data[[\"", cfg$col_n, "\"]]"),
          "  )",
          "})",
          "names(Slist) <- as.character(studies)"
        )
      } else {
        "Slist <- NULL"
      }
      
      script_lines <- c(
        "# ==========================================================================",
        "# Reproducible Dose-Response Meta-Analysis Script",
        paste0("# Generated by EasyDoseMeta on ", Sys.Date()),
        paste0("# Model Configuration: ", cfg$model_label),
        paste0("# Procedure: ", cfg$proc, " | Estimation: ", toupper(cfg$est_method), " | Covariance: ", cfg$cov_method),
        if (!is.null(cfg$knot_locations)) paste0("# Unscaled Spline Knot Doses: ", cfg$knot_locations) else NULL,
        "# ==========================================================================",
        "",
        "library(dosresmeta)",
        "library(rms)",
        "library(splines)",
        "library(ggplot2)",
        "",
        "# 1. Load your cleaned dataset (replace with your file path)",
        "# data <- readxl::read_excel('your_data.xlsx')",
        "",
        paste0("# 2. Dose Scaling (Scale Factor = ", rv$scale_factor, ")"),
        paste0("scale_factor <- ", rv$scale_factor),
        paste0("data[[\"", cfg$col_dose, "\"]] <- as.numeric(data[[\"", cfg$col_dose, "\"]]) / scale_factor"),
        paste0("xref_scaled <- ", signif(rv$actual_ref, 10), " # Reference dose (scaled)"),
        "",
        slist_block,
        "",
        "# 3. Fit Dose-Response Meta-Analysis Model",
        paste0("fmla <- as.formula(\"", gsub("\"", "\\\\\"", fmla_str), "\")"),
        "model <- dosresmeta::dosresmeta(",
        "  formula    = fmla,",
        paste0("  id         = data[[\"", cfg$col_id, "\"]],"),
        "  type       = data$type_calc,",
        "  v          = data$se_calc^2,",
        paste0("  cases      = ", cases_arg, ","),
        paste0("  n          = ", n_arg, ","),
        "  data       = data,",
        paste0("  proc       = \"", cfg$proc, "\","),
        paste0("  method     = \"", cfg$est_method, "\","),
        paste0("  covariance = \"", cfg$cov_method, "\","),
        "  Slist      = Slist",
        ")",
        "",
        "summary(model)",
        "",
        "# 4. Generate Dose-Response Predictions",
        paste0("d_seq_scaled <- seq(min(data[[\"", cfg$col_dose, "\"]], na.rm = TRUE), max(data[[\"", cfg$col_dose, "\"]], na.rm = TRUE), length.out = 200)"),
        "pred_df <- data.frame(dose_tmp = d_seq_scaled)",
        paste0("names(pred_df)[1] <- \"", cfg$col_dose, "\""),
        "preds <- predict(model, newdata = pred_df, xref = xref_scaled, expo = TRUE)",
        "preds$dose_unscaled <- d_seq_scaled * scale_factor",
        "",
        "# 5. Plot Dose-Response Curve",
        "ggplot(preds, aes(x = dose_unscaled, y = pred)) +",
        paste0("  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), fill = \"", input$plot_color, "\", alpha = ", input$plot_alpha, ") +"),
        paste0("  geom_line(color = \"", input$plot_color, "\", linewidth = ", input$plot_lwd, ") +"),
        "  geom_hline(yintercept = 1, linetype = \"dashed\", color = \"black\") +",
        paste0("  labs(title = \"", gsub("\"", "\\\\\"", input$plot_title), "\", x = \"", gsub("\"", "\\\\\"", input$plot_xlab), "\", y = \"", gsub("\"", "\\\\\"", input$plot_ylab), "\") +"),
        paste0("  ", input$plot_theme, "(base_size = ", input$plot_fontsize, ", base_family = \"", input$plot_font, "\")")
      )
      writeLines(script_lines, con = file)
    }
  )
  
  output$download_preds <- downloadHandler(
    filename = function() {
      "custom_predictions.csv"
    },
    content = function(file) {
      write.csv(custom_preds_data(), file, row.names = FALSE)
    }
  )
  
  # --- MODELS MANAGEMENT & MULTI-MODEL REPORT LOGIC ---
  observeEvent(input$store_model, {
    req(rv$model, rv$fitted_config)
    active_preds <- plot_predictions()
    req(active_preds)
    rv$model_counter <- rv$model_counter + 1
    
    m_type     <- rv$fitted_config$model_label
    sheet_name <- rv$fitted_config$sheet
    proc_name  <- if (rv$fitted_config$proc == "1stage") "1-stage" else "2-stage"
    est_label  <- toupper(if (!is.null(rv$fitted_config$est_method)) rv$fitted_config$est_method else "reml")
    
    raw_name <- if (!is.null(input$model_name)) trimws(input$model_name) else ""
    if (raw_name == "") {
      raw_name <- paste0(m_type, " [", proc_name, ", ", est_label, "]")
    }
    existing_names <- sapply(rv$stored_models, function(m) m$name)
    final_name <- raw_name
    dup_idx <- 2L
    while (final_name %in% existing_names) {
      final_name <- paste0(raw_name, " (", dup_idx, ")")
      dup_idx <- dup_idx + 1L
    }
    cov_name <- switch(rv$fitted_config$cov_method,
                       "gl" = "Greenland & Longnecker",
                       "h" = "Hamling",
                       "indep" = "Independent"
    )
    
    scale_str <- if (rv$scale_factor == 1) {
      "Dose data was not scaled."
    } else {
      paste0("Dose data was scaled by a factor of ", rv$scale_factor, ".")
    }
    
    nadir_str <- compute_nadir_summary(active_preds, rv$fitted_config$model_type)$md
    het_info  <- extract_heterogeneity(rv$model, rv$fitted_config$proc)
    
    cf <- coef(rv$model)
    V <- vcov(rv$model)
    n_coef <- length(cf)
    if (rv$fitted_config$model_type == "Linear" || n_coef < 2) {
      wald_str <- "Not Applicable (Linear Model)"
    } else {
      idx_w <- 2:n_coef
      cf_nonlin <- cf[idx_w]
      V_nonlin <- V[idx_w, idx_w, drop = FALSE]
      wald_str <- tryCatch(
        {
          W <- as.numeric(t(cf_nonlin) %*% solve(V_nonlin) %*% cf_nonlin)
          df_w <- length(cf_nonlin)
          p_val <- 1 - pchisq(W, df = df_w)
          sig_note <- if (p_val < 0.05) "**Statistically significant non-linearity**" else "No significant non-linearity"
          paste0("p = ", signif(p_val, 4), " (df = ", df_w, ") \u2014 ", sig_note)
        },
        error = function(e) "Could not compute Wald test (Covariance matrix issue)"
      )
    }
    
    linear_delta_str <- ""
    if (rv$fitted_config$model_type == "Linear" && isTruthy(input$linear_delta)) {
      ld_res <- tryCatch(linear_delta_data(), error = function(e) NULL)
      if (!is.null(ld_res)) {
        linear_delta_str <- paste0(
          "- **Linear Effect (Delta = ", ld_res$delta, "):** RR = **",
          round(ld_res$rr, 3), "** (95% CI: ", round(ld_res$lb, 3), " to ", round(ld_res$ub, 3), ")"
        )
      }
    }
    
    custom_pred_str <- ""
    if (isTruthy(input$custom_doses)) {
      cp_df <- tryCatch(custom_preds_data(), error = function(e) NULL)
      if (!is.null(cp_df) && nrow(cp_df) > 0) {
        cp_md <- paste(knitr::kable(cp_df, format = "markdown"), collapse = "\n")
        custom_pred_str <- paste0("\n#### Custom Dose Predictions\n\n", cp_md, "\n")
      }
    }
    
    loo_md_str <- ""
    if (!is.null(rv$loo_results) && !is.null(rv$loo_results$table) && nrow(rv$loo_results$table) > 0) {
      loo_md <- paste(knitr::kable(rv$loo_results$table, format = "markdown", row.names = FALSE), collapse = "\n")
      loo_md_str <- paste0("\n#### Leave-One-Out Sensitivity Analysis\n\n", loo_md, "\n")
    }
    
    knot_md_line <- if (!is.null(rv$fitted_config$knot_locations)) {
      paste0("- **Spline Knot Doses (Unscaled):** ", rv$fitted_config$knot_locations)
    } else {
      ""
    }
    
    term_summary <- capture.output(summary(rv$model))
    term_summary_md <- paste0("```text\n", paste(term_summary, collapse = "\n"), "\n```")
    
    md_text <- paste(
      paste0("### Model Name: ", final_name),
      paste0("- **Data Sheet:** ", sheet_name),
      paste0("- **Model Type:** ", m_type),
      paste0("- **Procedure:** ", proc_name, " (", est_label, ")"),
      paste0("- **Covariance Method:** ", cov_name),
      "",
      "#### Summary Metrics",
      paste0("- **Scaling Info:** ", scale_str),
      if (knot_md_line != "") knot_md_line else "",
      paste0("- **NADIR (Minimum Effect):** ", nadir_str),
      paste0("- **Between-Study Heterogeneity:** ", het_info$md),
      paste0("- **Test for Non-Linearity (Wald Test):** ", wald_str),
      if (linear_delta_str != "") linear_delta_str else "",
      custom_pred_str,
      loo_md_str,
      "",
      "#### Terminal Output Summary",
      term_summary_md,
      "",
      sep = "\n"
    )
    
    current_stored_plot <- build_plot()
    
    rv$stored_models[[as.character(rv$model_counter)]] <- list(
      id = rv$model_counter,
      name = final_name,
      type = m_type,
      preds = active_preds,
      color     = input$plot_color,
      alpha     = input$plot_alpha,
      lwd       = input$plot_lwd,
      linetype = if (!is.null(input$plot_linetype)) input$plot_linetype else "solid",
      aic = round(AIC(rv$model), 2),
      bic = round(BIC(rv$model), 2),
      loglik = round(as.numeric(logLik(rv$model)), 2),
      i2 = het_info$i2,
      het_p = het_info$p_val,
      md_text = md_text,
      rug_data = rv$rug_data,
      show_rug = input$show_rug,
      rug_width = input$rug_width,
      rug_alpha = input$rug_alpha,
      rug_length = input$rug_length,
      rug_color = if (isTruthy(input$rug_auto_color)) input$plot_color else input$rug_color,
      plot = current_stored_plot
    )
    showNotification(paste("Model '", final_name, "' stored."), type = "message")
  })
  
  output$subgroup_store_ui <- renderUI({
    req(rv$columns)
    def_sg <- guess_col(rv$columns, c("subgroup", "^group$", "^sex$", "^gender$", "^region$", "^stratum", "study[_\\.]?type", "^type$"))
    tagList(
      selectInput(
        "subgroup_col", "Stratify by Subgroup:",
        choices  = c("Select Column" = "", rv$columns),
        selected = def_sg
      ),
      actionButton(
        "run_subgroup_store", "Fit & Store by Subgroup",
        class = "btn-warning btn-block btn-sm",
        icon  = icon("layer-group")
      )
    )
  })
  
  observeEvent(input$run_subgroup_store, {
    if (is.null(rv$model) || is.null(rv$fitted_config)) {
      showNotification("Please click 'Run Analysis' first to establish the base model configuration.", type = "warning", duration = 8)
      return()
    }
    cfg <- rv$fitted_config
    sg_col <- input$subgroup_col
    if (!isTruthy(sg_col) || !(sg_col %in% names(cfg$clean_data))) {
      showNotification("Please select a valid categorical subgroup column.", type = "warning", duration = 6)
      return()
    }
    
    df_all <- cfg$clean_data
    raw_sg <- trimws(as.character(df_all[[sg_col]]))
    sg_vals <- unique(raw_sg[!is.na(raw_sg) & raw_sg != ""])
    if (length(sg_vals) < 1) {
      showNotification("No non-empty subgroup levels found in the selected column.", type = "warning", duration = 6)
      return()
    }
    if (length(sg_vals) > 15) {
      showNotification(
        paste0("Column '", sg_col, "' has ", length(sg_vals), " unique values (> 15). Please select a categorical subgroup column."),
        type = "warning", duration = 8
      )
      return()
    }
    
    sg_palette <- c(
      "#0073C2", "#EFC000", "#CD534C", "#00A087", "#8F7700",
      "#3C5488", "#F39B7F", "#7E6148", "#4DBBD5", "#605CA8",
      "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"
    )
    
    stored_count <- 0L
    skipped_levels <- character(0)
    dose_col <- cfg$col_dose
    dose_col_bt <- paste0("`", gsub("`", "", dose_col, fixed = TRUE), "`")
    
    for (idx_sg in seq_along(sg_vals)) {
      sg_val <- sg_vals[idx_sg]
      sub_df <- df_all[!is.na(raw_sg) & raw_sg == sg_val, , drop = FALSE]
      
      if (cfg$proc == "2stage") {
        s_counts <- table(sub_df[[cfg$col_id]])
        valid_s <- names(s_counts[s_counts >= 2])
        sub_df <- sub_df[sub_df[[cfg$col_id]] %in% valid_s, , drop = FALSE]
      }
      if (nrow(sub_df) < 2) {
        skipped_levels <- c(skipped_levels, paste0(sg_val, " (insufficient rows)"))
        next
      }
      
      Slist_sub <- if (!is.null(cfg$Slist)) {
        cfg$Slist[as.character(unique(sub_df[[cfg$col_id]]))]
      } else {
        NULL
      }
      
      fit_with_formula <- function(f_use) {
        m_try <- dosresmeta(
          formula = f_use,
          id = sub_df[[cfg$col_id]],
          type = sub_df$type_calc,
          v = sub_df$se_calc^2,
          cases = if (!is.null(cfg$col_cases)) sub_df[[cfg$col_cases]] else NULL,
          n = if (!is.null(cfg$col_n)) sub_df[[cfg$col_n]] else NULL,
          data = sub_df,
          proc = cfg$proc,
          method = cfg$est_method,
          covariance = cfg$cov_method,
          Slist = Slist_sub
        )
        m_try$formula_fixed <- f_use
        m_try
      }
      
      m_sg <- tryCatch(fit_with_formula(cfg$fmla), error = function(e) NULL)
      fmla_used <- cfg$fmla
      
      # Fallback for Spline models if subgroup has narrower dose range than full-sample knots
      if (is.null(m_sg) && cfg$model_type == "Spline" && !is.null(cfg$spline_type)) {
        m_sg <- tryCatch({
          x_sg <- sub_df[[dose_col]]
          nk_sg <- if (!is.null(cfg$knots)) max(3L, as.integer(cfg$knots)) else 3L
          if (cfg$spline_type == "rcs") {
            probs_sg <- seq(0.10, 0.90, length.out = nk_sg)
            k_sg <- sort(unique(unname(quantile(x_sg, probs = probs_sg, na.rm = TRUE))))
            if (length(k_sg) < 3) k_sg <- sort(unique(unname(quantile(unique(x_sg), probs = probs_sg, na.rm = TRUE))))
            k_str_sg <- paste0("c(", paste(signif(k_sg, 10), collapse = ", "), ")")
            fmla_used <<- as.formula(paste("es_log ~ rcs(", dose_col_bt, ", ", k_str_sg, ")"))
          } else {
            tmp_b <- if (cfg$spline_type == "bs") splines::bs(x_sg, df = nk_sg) else splines::ns(x_sg, df = nk_sg)
            ik_sg <- as.numeric(attr(tmp_b, "knots"))
            bk_sg <- as.numeric(attr(tmp_b, "Boundary.knots"))
            b_str_sg <- paste0("Boundary.knots = c(", paste(signif(bk_sg, 10), collapse = ", "), ")")
            k_str_sg <- if (length(ik_sg) > 0) {
              paste0("knots = c(", paste(signif(ik_sg, 10), collapse = ", "), "), ", b_str_sg)
            } else {
              b_str_sg
            }
            fmla_used <<- as.formula(paste("es_log ~ ", cfg$spline_type, "(", dose_col_bt, ", ", k_str_sg, ")"))
          }
          fit_with_formula(fmla_used)
        }, error = function(e) NULL)
      }
      
      if (is.null(m_sg)) {
        skipped_levels <- c(skipped_levels, paste0(sg_val, " (failed to converge)"))
        next
      }
      
      min_sg <- min(sub_df[[dose_col]], na.rm = TRUE)
      max_sg <- max(sub_df[[dose_col]], na.rm = TRUE)
      xref_sg <- if (is.na(input$ref_dose)) min_sg else (input$ref_dose / rv$scale_factor)
      d_seq_sg <- seq(min_sg, max_sg, length.out = 100)
      pred_in_sg <- data.frame(dose = d_seq_sg)
      names(pred_in_sg)[1] <- dose_col
      
      p_df_sg <- tryCatch(
        robust_predict(m_sg, newdata = pred_in_sg, dose_col = dose_col, xref = xref_sg),
        error = function(e) NULL
      )
      if (is.null(p_df_sg)) {
        skipped_levels <- c(skipped_levels, paste0(sg_val, " (prediction error)"))
        next
      }
      
      preds_sg <- data.frame(
        dose  = d_seq_sg * rv$scale_factor,
        pred  = p_df_sg$pred,
        ci.lb = p_df_sg$ci.lb,
        ci.ub = p_df_sg$ci.ub
      )
      
      rv$model_counter <- rv$model_counter + 1
      sg_color <- sg_palette[((idx_sg - 1L) %% length(sg_palette)) + 1L]
      
      base_sg_name <- paste0(cfg$model_label, " [", sg_col, ": ", sg_val, "]")
      existing_names <- sapply(rv$stored_models, function(m) m$name)
      final_sg_name <- base_sg_name
      dup_idx <- 2L
      while (final_sg_name %in% existing_names) {
        final_sg_name <- paste0(base_sg_name, " (", dup_idx, ")")
        dup_idx <- dup_idx + 1L
      }
      
      het_sg   <- extract_heterogeneity(m_sg, cfg$proc)
      nadir_sg <- compute_nadir_summary(preds_sg, cfg$model_type)$md
      
      cf_sg <- coef(m_sg)
      V_sg  <- vcov(m_sg)
      if (cfg$model_type == "Linear" || length(cf_sg) < 2) {
        wald_sg <- "Not Applicable (Linear Model)"
      } else {
        idx_w <- 2:length(cf_sg)
        wald_sg <- tryCatch({
          W <- as.numeric(t(cf_sg[idx_w]) %*% solve(V_sg[idx_w, idx_w, drop = FALSE]) %*% cf_sg[idx_w])
          df_w <- length(idx_w)
          p_val <- 1 - pchisq(W, df = df_w)
          paste0("p = ", signif(p_val, 4), " (df = ", df_w, ")")
        }, error = function(e) "Could not compute Wald test")
      }
      
      rug_sg <- data.frame(dose = sub_df[[dose_col]] * rv$scale_factor)
      p_sg <- ggplot(preds_sg, aes(x = dose, y = pred)) +
        geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), fill = sg_color, alpha = input$plot_alpha) +
        geom_line(color = sg_color, linewidth = input$plot_lwd) +
        labs(
          title = final_sg_name,
          x = if (isTruthy(input$plot_xlab)) input$plot_xlab else "Dose",
          y = if (isTruthy(input$plot_ylab)) input$plot_ylab else "Relative Risk (95% CI)"
        ) +
        get_ggplot_theme(input$plot_theme, input$plot_font, input$plot_fontsize)
      
      term_sg <- paste0("```text\n", paste(capture.output(summary(m_sg)), collapse = "\n"), "\n```")
      proc_name <- if (cfg$proc == "1stage") "1-stage" else "2-stage"
      est_label <- toupper(if (!is.null(cfg$est_method)) cfg$est_method else "reml")
      
      md_text_sg <- paste(
        paste0("### Model Name: ", final_sg_name),
        paste0("- **Subgroup:** ", sg_col, " = ", sg_val, " (", length(unique(sub_df[[cfg$col_id]])), " studies, ", nrow(sub_df), " rows)"),
        paste0("- **Data Sheet:** ", cfg$sheet),
        paste0("- **Model Type:** ", cfg$model_label),
        paste0("- **Procedure:** ", proc_name, " (", est_label, ")"),
        "",
        "#### Summary Metrics",
        paste0("- **NADIR (Minimum Effect):** ", nadir_sg),
        paste0("- **Between-Study Heterogeneity:** ", het_sg$md),
        paste0("- **Test for Non-Linearity (Wald Test):** ", wald_sg),
        "",
        "#### Terminal Output Summary",
        term_sg,
        "",
        sep = "\n"
      )
      
      rv$stored_models[[as.character(rv$model_counter)]] <- list(
        id         = rv$model_counter,
        name       = final_sg_name,
        type       = paste0(cfg$model_label, " (", sg_col, "=", sg_val, ")"),
        preds      = preds_sg,
        color      = sg_color,
        alpha      = input$plot_alpha,
        lwd        = input$plot_lwd,
        linetype   = if (!is.null(input$plot_linetype)) input$plot_linetype else "solid",
        aic        = round(AIC(m_sg), 2),
        bic        = round(BIC(m_sg), 2),
        loglik     = round(as.numeric(logLik(m_sg)), 2),
        i2         = het_sg$i2,
        het_p      = het_sg$p_val,
        md_text    = md_text_sg,
        rug_data   = rug_sg,
        show_rug   = input$show_rug,
        rug_width  = input$rug_width,
        rug_alpha  = input$rug_alpha,
        rug_length = input$rug_length,
        rug_color  = sg_color,
        plot       = p_sg
      )
      stored_count <- stored_count + 1L
    }
    
    if (stored_count > 0L) {
      msg_sg <- paste0("Fitted and stored ", stored_count, " subgroup model(s) by '", sg_col, "'. View them in the Model Comparison tab!")
      if (length(skipped_levels) > 0) {
        msg_sg <- paste0(msg_sg, " Skipped: ", paste(skipped_levels, collapse = "; "), ".")
      }
      showNotification(msg_sg, type = "message", duration = 8)
    } else {
      showNotification(
        paste0("Could not fit any subgroup models for '", sg_col, "'. Skipped: ", paste(skipped_levels, collapse = "; ")),
        type = "error", duration = 10
      )
    }
  })
  
  output$stored_models_badge_ui <- renderUI({
    n_stored <- length(rv$stored_models)
    if (n_stored == 0) return(NULL)
    div(
      style = "margin-top: 8px; padding: 6px 10px; background: rgba(0, 192, 239, 0.12); border-radius: 4px; display: flex; justify-content: space-between; align-items: center;",
      tags$span(style = "font-size: 12px; font-weight: bold;", icon("check-circle"), paste0(" ", n_stored, " model(s) stored")),
      actionButton("go_to_comparison", "Compare \u2192", class = "btn-primary btn-xs", style = "font-weight: bold;")
    )
  })
  
  observeEvent(input$go_to_comparison, {
    updateTabItems(session, "tabs", "comparison")
  })
  
  output$stored_models_container_ui <- renderUI({
    if (length(rv$stored_models) == 0) {
      return(
        div(
          style = "padding: 25px 20px; text-align: center; opacity: 0.75; border: 1px dashed #666; border-radius: 6px;",
          icon("layer-group", style = "font-size: 28px; color: #3c8dbc; margin-bottom: 8px;"),
          p(style = "margin: 0; font-size: 14px;", "No models stored yet. Store models from the ", tags$b("Analysis & Results"), " tab or load a saved ", tags$code(".rds"), " file below.")
        )
      )
    }
    DTOutput("stored_models_tbl")
  })
  
  output$stored_models_tbl <- renderDT({
    req(length(rv$stored_models) > 0)
    df <- do.call(rbind, lapply(rv$stored_models, function(m) {
      data.frame(
        ID = m$id, Name = m$name, Model_Config = m$type,
        AIC = m$aic, BIC = m$bic, LogLik = m$loglik,
        I2 = if (!is.null(m$i2)) m$i2 else "N/A",
        Het_p = if (!is.null(m$het_p)) m$het_p else "N/A",
        Color = m$color, stringsAsFactors = FALSE
      )
    }))
    datatable(df, options = list(pageLength = 5, dom = "t", scrollX = TRUE), rownames = FALSE, selection = "single") %>%
      formatStyle("Color", backgroundColor = styleEqual(df$Color, df$Color), color = "transparent")
  })
  
  output$remove_model_ui <- renderUI({
    if (length(rv$stored_models) == 0) {
      return(selectInput("model_to_remove", "Select Model:", choices = c("No models stored yet" = "")))
    }
    keys <- names(rv$stored_models)
    choices <- setNames(keys, sapply(rv$stored_models, function(x) x$name))
    sel_idx <- input$stored_models_tbl_rows_selected
    selected_key <- if (!is.null(sel_idx) && length(sel_idx) == 1 && sel_idx <= length(keys)) keys[sel_idx] else keys[1]
    selectInput("model_to_remove", "Select Model:", choices = choices, selected = selected_key)
  })
  
  observeEvent(input$stored_models_tbl_rows_selected, {
    sel_idx <- input$stored_models_tbl_rows_selected
    keys <- names(rv$stored_models)
    if (!is.null(sel_idx) && length(sel_idx) == 1 && sel_idx <= length(keys)) {
      updateSelectInput(session, "model_to_remove", selected = keys[sel_idx])
    }
  })
  
  observeEvent(input$remove_model_btn, {
    req(length(rv$stored_models) > 0)
    keys <- names(rv$stored_models)
    sel_idx <- input$stored_models_tbl_rows_selected
    target_key <- if (!is.null(sel_idx) && length(sel_idx) == 1 && sel_idx <= length(keys)) {
      keys[sel_idx]
    } else {
      input$model_to_remove
    }
    req(target_key)
    rv$stored_models[[target_key]] <- NULL
  })
  
  observeEvent(input$clear_models, {
    rv$stored_models <- list()
    rv$model_counter <- 0
  })
  
  output$edit_model_ui <- renderUI({
    req(length(rv$stored_models) > 0)
    keys <- names(rv$stored_models)
    target_key <- if (!is.null(input$model_to_remove) && input$model_to_remove %in% keys) {
      input$model_to_remove
    } else {
      keys[1]
    }
    m <- rv$stored_models[[target_key]]
    req(m)
    tags$details(
      style = "margin-top: 8px; border-top: 1px dashed #ccc; padding-top: 8px;",
      tags$summary(
        style = "cursor:pointer; font-weight:bold; color:#3c8dbc; font-size: 14px;",
        icon("edit"), paste0(" Customize Selected Stored Model (", m$name, ")")
      ),
      br(),
      fluidRow(
        column(4, textInput("edit_model_name", "Display Name:", value = m$name)),
        column(3, colourInput("edit_model_color", "Curve Color:", value = m$color)),
        column(
          3,
          selectInput(
            "edit_model_linetype", "Line Style:",
            choices = c("Solid" = "solid", "Dashed" = "dashed", "Dotted" = "dotted",
                        "Dot-Dash" = "dotdash", "Long-Dash" = "longdash", "Two-Dash" = "twodash"),
            selected = if (!is.null(m$linetype)) m$linetype else "solid"
          )
        ),
        column(
          2,
          actionButton(
            "apply_model_edit", "Apply",
            class = "btn-warning btn-block",
            style = "margin-top: 25px;",
            icon  = icon("check")
          )
        )
      )
    )
  })
  
  observeEvent(input$apply_model_edit, {
    req(length(rv$stored_models) > 0)
    keys <- names(rv$stored_models)
    target_key <- if (!is.null(input$model_to_remove) && input$model_to_remove %in% keys) {
      input$model_to_remove
    } else {
      keys[1]
    }
    m <- rv$stored_models[[target_key]]
    req(m)
    
    new_name <- if (!is.null(input$edit_model_name)) trimws(input$edit_model_name) else ""
    if (new_name == "") new_name <- m$name
    other_names <- sapply(rv$stored_models[setdiff(keys, target_key)], function(x) x$name)
    if (new_name %in% other_names) {
      showNotification("Another stored model already has that name. Please choose a unique name.", type = "warning")
      return()
    }
    
    m$name      <- new_name
    m$color     <- if (!is.null(input$edit_model_color)) input$edit_model_color else m$color
    m$rug_color <- m$color
    m$linetype  <- if (!is.null(input$edit_model_linetype)) input$edit_model_linetype else m$linetype
    if (!is.null(m$md_text)) {
      m$md_text <- sub("^### Model Name: [^\n]*", paste0("### Model Name: ", new_name), m$md_text)
    }
    if (!is.null(m$plot)) {
      m$plot <- m$plot + labs(title = new_name)
    }
    rv$stored_models[[target_key]] <- m
    showNotification(paste0("Updated styling for '", new_name, "'."), type = "message", duration = 4)
  })
  
  output$download_models_rds <- downloadHandler(
    filename = function() {
      paste0("EasyDoseMeta_Stored_Models_", Sys.Date(), ".rds")
    },
    content = function(file) {
      if (length(rv$stored_models) == 0) {
        showNotification("No stored models to save. Please store at least one model first.", type = "warning")
      }
      payload <- list(
        app = "EasyDoseMeta",
        exported_at = Sys.time(),
        model_counter = rv$model_counter,
        stored_models = rv$stored_models
      )
      saveRDS(payload, file = file)
    }
  )
  
  observeEvent(input$upload_models_rds, {
    req(input$upload_models_rds)
    tryCatch({
      loaded_obj <- readRDS(input$upload_models_rds$datapath)
      models_in <- if (is.list(loaded_obj) && !is.null(loaded_obj$stored_models)) {
        loaded_obj$stored_models
      } else if (is.list(loaded_obj)) {
        loaded_obj
      } else {
        stop("Invalid .rds structure.")
      }
      if (length(models_in) == 0) {
        showNotification("The uploaded .rds file contains 0 stored models.", type = "warning")
        return()
      }
      valid_entries <- sapply(models_in, function(m) is.list(m) && !is.null(m$name) && !is.null(m$preds))
      if (!all(valid_entries)) {
        stop("File does not match the EasyDoseMeta stored models format.")
      }
      
      added_n <- 0L
      for (m in models_in) {
        rv$model_counter <- rv$model_counter + 1L
        existing_names <- sapply(rv$stored_models, function(x) x$name)
        nm <- m$name
        dup_i <- 2L
        while (nm %in% existing_names) {
          nm <- paste0(m$name, " (", dup_i, ")")
          dup_i <- dup_i + 1L
        }
        m$id <- rv$model_counter
        m$name <- nm
        rv$stored_models[[as.character(rv$model_counter)]] <- m
        added_n <- added_n + 1L
      }
      showNotification(paste0("Successfully loaded ", added_n, " stored model(s) from .rds file!"), type = "message", duration = 6)
    }, error = function(e) {
      showNotification(paste("Failed to load models (.rds):", e$message), type = "error", duration = 8)
    })
  })
  
  # --- MULTI-MODEL REPORT LOGIC ---
  build_stored_models_summary_df <- function() {
    do.call(rbind, lapply(rv$stored_models, function(m) {
      data.frame(
        `Model Name`    = m$name,
        `Configuration` = m$type,
        `AIC`           = m$aic,
        `BIC`           = m$bic,
        `LogLik`        = m$loglik,
        `I²`            = if (!is.null(m$i2)) m$i2 else "N/A",
        `Het p`         = if (!is.null(m$het_p)) m$het_p else "N/A",
        check.names     = FALSE,
        stringsAsFactors = FALSE
      )
    }))
  }

  output$multi_report_btn_ui <- renderUI({
    if (length(rv$stored_models) == 0) {
      tags$button(
        type = "button",
        class = "btn btn-default btn-block disabled",
        style = "margin-top: 25px; font-weight: bold; cursor: not-allowed; opacity: 0.65;",
        icon("ban"), " Store at least 1 model to generate report"
      )
    } else {
      downloadButton(
        "download_multi_report_btn",
        "Generate & Download Comprehensive Report",
        class = "btn-success btn-block",
        icon = icon("file-export"),
        style = "margin-top: 25px; font-weight: bold;"
      )
    }
  })
  
  output$multi_report_preview_ui <- renderUI({
    if (length(rv$stored_models) == 0) {
      return(
        div(
          style = "padding: 25px 20px; text-align: center; opacity: 0.75; border: 1px dashed #666; border-radius: 6px;",
          icon("file-invoice", style = "font-size: 28px; color: #3c8dbc; margin-bottom: 8px;"),
          p(style = "margin: 0; font-size: 14px;", "No models stored yet. Store at least one model in ", tags$b("Analysis & Results"), " (or load an ", tags$code(".rds"), " workspace in ", tags$b("Model Comparison"), ") to generate a multi-model report.")
        )
      )
    }
    tableOutput("multi_report_preview")
  })
  
  output$multi_report_preview <- renderTable({
    req(length(rv$stored_models) > 0)
    build_stored_models_summary_df()
  })
  
  output$download_multi_report_btn <- downloadHandler(
    filename = function() {
      paste0(
        "Comprehensive_DR_Report_", Sys.Date(), ".",
        ifelse(input$multi_report_format == "word", "docx", input$multi_report_format)
      )
    },
    content = function(file) {
      req(length(rv$stored_models) > 0)
      
      tempReport <- file.path(tempdir(), "multi_report.Rmd")
      out_fmt <- switch(input$multi_report_format,
                        "html" = "html_document",
                        "pdf" = "pdf_document",
                        "word" = "word_document"
      )
      
      summary_tbl_md <- paste(
        knitr::kable(build_stored_models_summary_df(), format = "markdown", row.names = FALSE),
        collapse = "\n"
      )
      
      blocks <- list()
      for (i in seq_along(rv$stored_models)) {
        m <- rv$stored_models[[i]]
        plot_chunk <- paste0(
          "```{r echo=FALSE, warning=FALSE, message=FALSE, fig.width=8, fig.height=5}\n",
          "print(models_list[['", i, "']]$plot)\n",
          "```\n"
        )
        blocks[[i]] <- paste(m$md_text, "\n#### Model Plot\n", plot_chunk, sep = "\n")
      }
      combined_markdown <- paste(blocks, collapse = "\n\n---\n\n")
      
      comp_plot_chunk <- if (length(rv$stored_models) > 1) {
        paste0(
          "\n# Multi-Model Comparison Plot\n\n",
          "```{r echo=FALSE, warning=FALSE, message=FALSE, fig.width=9, fig.height=6}\n",
          "print(comp_plot_obj)\n",
          "```\n"
        )
      } else {
        ""
      }
      
      rmd_lines <- c(
        "---",
        "title: 'Comprehensive Multi-Model Dose-Response Report'",
        paste0("date: '", Sys.Date(), "'"),
        paste0("output: ", out_fmt),
        "---",
        "",
        "This document compiles the configurations, statistical summaries, and dose-response plots for all stored models.",
        "",
        "# Executive Summary: Model Comparison Table",
        "",
        summary_tbl_md,
        "",
        comp_plot_chunk,
        "\n---\n",
        "# Individual Model Details",
        "",
        combined_markdown
      )
      
      writeLines(rmd_lines, tempReport)
      
      render_env <- new.env(parent = globalenv())
      render_env$models_list <- rv$stored_models
      render_env$comp_plot_obj <- tryCatch(build_comp_plot(), error = function(e) NULL)
      
      tryCatch(
        rmarkdown::render(
          tempReport,
          output_file = file,
          envir = render_env
        ),
        error = function(e) {
          showNotification(
            paste0("Report generation failed: ", e$message, ". If exporting to PDF, ensure a LaTeX distribution (e.g., tinytex::install_tinytex()) is installed, or choose HTML/Word format."),
            type = "error", duration = 12
          )
          stop(e)
        }
      )
    }
  )
  
  # --- COMP PLOT BUILDER ---
  build_comp_plot <- reactive({
    req(length(rv$stored_models) > 0)
    all_preds <- do.call(rbind, lapply(rv$stored_models, function(m) {
      p <- m$preds
      p$Model <- m$name
      p$Color <- m$color
      p
    }))
    model_colors <- sapply(rv$stored_models, function(m) m$color)
    names(model_colors) <- sapply(rv$stored_models, function(m) m$name)
    model_alphas <- sapply(rv$stored_models, function(m) m$alpha)
    names(model_alphas) <- sapply(rv$stored_models, function(m) m$name)
    model_lwds <- setNames(
      sapply(rv$stored_models, function(m) if (!is.null(m$lwd)) m$lwd else 1),
      sapply(rv$stored_models, function(m) m$name)
    )
    model_linetypes <- setNames(
      sapply(rv$stored_models, function(m) if (!is.null(m$linetype)) m$linetype else "solid"),
      sapply(rv$stored_models, function(m) m$name)
    )
    
    selected_theme <- get_ggplot_theme(input$comp_plot_theme, input$comp_plot_font, input$comp_plot_fontsize)
    p <- ggplot(all_preds, aes(x = dose, y = pred, color = Model, fill = Model, linetype = Model)) +
      geom_line(aes(linewidth = Model))
    
    if (is.null(input$comp_show_ci) || isTruthy(input$comp_show_ci)) {
      p <- p +
        geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub, alpha = Model), linetype = 0) +
        scale_fill_manual(name = "Model",  values = model_colors) +
        scale_alpha_manual(name = "Model", values = model_alphas, guide = "none")
    }
    
    p <- p +
      scale_color_manual(name = "Model", values = model_colors) +
      scale_linetype_manual(name = "Model", values = model_linetypes) +
      scale_linewidth_manual(name = "Model", values = model_lwds, guide = "none") +
      selected_theme +
      labs(
        title = input$comp_plot_title,
        x = if (!is.null(input$comp_plot_xlab)) input$comp_plot_xlab else input$plot_xlab,
        y = if (!is.null(input$comp_plot_ylab)) input$comp_plot_ylab else input$plot_ylab
      ) +
      theme(legend.position = input$comp_legend_loc, plot.title = element_text(hjust = as.numeric(input$comp_plot_title_loc)))
    
    # Conditionally add reference line at RR = 1
    if (isTruthy(input$comp_show_ref_one)) {
      p <- p + geom_hline(yintercept = 1, linetype = "dashed", color = "black")
    }
    
    # Render Rugs on Comparison Plot if enabled
    if (isTruthy(input$comp_show_rug)) {
      rug_dfs <- lapply(rv$stored_models, function(m) {
        if (!is.null(m$rug_data) && nrow(m$rug_data) > 0) {
          data.frame(dose = m$rug_data$dose, Model = m$name, stringsAsFactors = FALSE)
        } else {
          NULL
        }
      })
      all_rugs <- do.call(rbind, rug_dfs[!sapply(rug_dfs, is.null)])
      
      if (!is.null(all_rugs) && nrow(all_rugs) > 0) {
        if (isTruthy(input$comp_rug_auto_color)) {
          p <- p + geom_rug(
            data = all_rugs,
            aes(x = dose, color = Model),
            inherit.aes = FALSE,
            sides = "b",
            linewidth = input$comp_rug_width,
            alpha = input$comp_rug_alpha,
            length = unit(input$comp_rug_length, "npc")
          )
        } else {
          p <- p + geom_rug(
            data = all_rugs,
            aes(x = dose),
            inherit.aes = FALSE,
            sides = "b",
            color = input$comp_rug_color,
            linewidth = input$comp_rug_width,
            alpha = input$comp_rug_alpha,
            length = unit(input$comp_rug_length, "npc")
          )
        }
      }
    }
    
    p <- add_custom_lines_to_plot(p, rv$custom_lines, log_y = isTruthy(input$comp_log_y_scale))
    
    comp_custom_y_ticks <- NULL
    if (!is.null(input$comp_y_axis_ticks) && trimws(input$comp_y_axis_ticks) != "") {
      parsed_y <- suppressWarnings(as.numeric(unlist(strsplit(input$comp_y_axis_ticks, ","))))
      comp_custom_y_ticks <- parsed_y[!is.na(parsed_y)]
    }
    
    comp_custom_x_ticks <- NULL
    if (!is.null(input$comp_x_axis_ticks) && trimws(input$comp_x_axis_ticks) != "") {
      parsed_x <- suppressWarnings(as.numeric(unlist(strsplit(input$comp_x_axis_ticks, ","))))
      comp_custom_x_ticks <- parsed_x[!is.na(parsed_x)]
    }
    
    if (isTruthy(input$comp_log_y_scale)) {
      comp_custom_y_ticks <- comp_custom_y_ticks[comp_custom_y_ticks > 0]
      if (length(comp_custom_y_ticks) > 0) {
        p <- p + scale_y_continuous(trans = "log10", breaks = comp_custom_y_ticks, minor_breaks = NULL, oob = scales::squish_infinite)
      } else {
        p <- p + scale_y_continuous(trans = "log10", minor_breaks = NULL, oob = scales::squish_infinite)
      }
    } else {
      if (length(comp_custom_y_ticks) > 0) p <- p + scale_y_continuous(breaks = comp_custom_y_ticks, minor_breaks = NULL)
    }
    
    if (length(comp_custom_x_ticks) > 0) p <- p + scale_x_continuous(breaks = comp_custom_x_ticks, minor_breaks = NULL)
    
    comp_x_lim <- NULL
    if (!is.null(input$comp_xlim) && trimws(input$comp_xlim) != "") {
      parsed_xl <- suppressWarnings(as.numeric(unlist(strsplit(input$comp_xlim, ","))))
      parsed_xl <- parsed_xl[!is.na(parsed_xl)]
      if (length(parsed_xl) == 2) comp_x_lim <- sort(parsed_xl)
    }
    comp_y_lim <- NULL
    if (!is.null(input$comp_ylim) && trimws(input$comp_ylim) != "") {
      parsed_yl <- suppressWarnings(as.numeric(unlist(strsplit(input$comp_ylim, ","))))
      parsed_yl <- parsed_yl[!is.na(parsed_yl)]
      if (isTruthy(input$comp_log_y_scale)) parsed_yl <- parsed_yl[parsed_yl > 0]
      if (length(parsed_yl) == 2) comp_y_lim <- sort(parsed_yl)
    }
    if (!is.null(comp_x_lim) || !is.null(comp_y_lim)) {
      p <- p + coord_cartesian(xlim = comp_x_lim, ylim = comp_y_lim)
    }
    return(p)
  })
  
  output$comp_plot <- renderPlot({
    shiny::validate(
      shiny::need(length(rv$stored_models) > 0, "Store one or more models in 'Analysis & Results' (or load a saved .rds file above) to view the comparison plot.")
    )
    tryCatch(
      build_comp_plot(),
      error = function(e) {
        shiny::validate(shiny::need(FALSE, paste("Plot error:", conditionMessage(e))))
      }
    )
  })
  
  output$download_comp_plot <- downloadHandler(
    filename = function() {
      paste0("dr_comparison.", input$comp_plot_fmt)
    },
    content = function(file) {
      save_plot_file(file, plot_obj = build_comp_plot(), fmt = input$comp_plot_fmt, width = input$comp_plot_w, height = input$comp_plot_h, dpi = input$comp_plot_dpi)
    }
  )
}

shinyApp(ui, server)
