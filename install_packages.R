# ==============================================================================
# EasyDoseMeta (v3.7) - Package Installation & Setup Script
# ==============================================================================
# Run this script in R or RStudio prior to launching the Shiny application:
#   source("install_packages.R")
# ==============================================================================

required_packages <- c(
  "shiny",          # Core Shiny web framework
  "shinydashboard", # Dashboard layout & UI components
  "readxl",         # Excel (.xlsx / .xls) file import
  "writexl",        # Excel (.xlsx) file export
  "dplyr",          # Data manipulation & pre-processing
  "ggplot2",        # Publication-ready dose-response plotting
  "dosresmeta",     # Multivariate dose-response meta-analysis engine
  "rms",            # Restricted cubic splines (rcs)
  "splines",        # B-spline basis functions (bs)
  "colourpicker",   # Interactive color pickers for plot customization
  "DT",             # Interactive data tables
  "rmarkdown",      # Dynamic Word (.docx) & HTML report generation
  "pandoc",         # Pandoc integration for R Markdown rendering
  "knitr",          # Report table formatting & knitting
  "shinybusy",      # Busy indicators during model fitting
  "svglite"         # Vector SVG figure export support
)

missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if (length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, dependencies = TRUE)
} else {
  message("All required packages for EasyDoseMeta are already installed!")
}

# Verify all packages load cleanly
loaded_ok <- vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
if (all(loaded_ok)) {
  message("Setup complete! Run `shiny::runApp('app.R')` to launch EasyDoseMeta.")
} else {
  warning(
    "The following packages could not be verified: ",
    paste(required_packages[!loaded_ok], collapse = ", ")
  )
}
