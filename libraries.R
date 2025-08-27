# =============================================================================
# Package Dependencies for Effective Giving AI Persuasion Project
# =============================================================================
# This file installs (if needed) and loads all required packages for the analysis
# Run this file first before running any analysis scripts

# Function to install packages if not already installed
install_if_missing <- function(packages, repos = "https://cran.rstudio.com/") {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      message("Installing ", pkg, "...")
      install.packages(pkg, repos = repos)
    }
  }
}

# Function to install Bioconductor packages if not already installed
install_bioc_if_missing <- function(packages) {
  # Install BiocManager if not already installed
  if (!require("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }
  
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      message("Installing Bioconductor package ", pkg, "...")
      BiocManager::install(pkg, update = FALSE, ask = FALSE)
    }
  }
}

# Function to install GitHub packages if not already installed
install_github_if_missing <- function(packages) {
  # Install remotes if not already installed
  if (!require("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  
  for (pkg in packages) {
    pkg_name <- basename(pkg)  # Extract package name from "user/repo" format
    if (!require(pkg_name, character.only = TRUE, quietly = TRUE)) {
      message("Installing GitHub package ", pkg, "...")
      remotes::install_github(pkg)
    }
  }
}

# =============================================================================
# INSTALL PACKAGES (if missing)
# =============================================================================

# CRAN packages
cran_packages <- c(
  "dplyr", "data.table", "purrr", "stringr", "glue",
  "mgcv", "grf", "estimatr", "marginaleffects", "broom",
  "ggplot2", "patchwork", "ggdist", "ggtext", "ggExtra", 
  "ggsci", "jpeg", "grid", "gridtext", "gridExtra",
  "rvest", "xml2", "here", "fs", "osfr",
  "gt", "kableExtra", "modelsummary", "readr", "tidyr",
  "car", "forcats", "tibble"
)

# Bioconductor packages (if any)
bioc_packages <- c(
  "qvalue"
)

# GitHub packages (if any)
github_packages <- c(
  # Add any GitHub packages here if needed
)

# Install packages
message("Checking and installing packages...")
install_if_missing(cran_packages)
if (length(bioc_packages) > 0) install_bioc_if_missing(bioc_packages)
if (length(github_packages) > 0) install_github_if_missing(github_packages)

# =============================================================================
# LOAD PACKAGES
# =============================================================================

# Core data manipulation and analysis
library(readr)
library(tidyr)
library(dplyr)
library(data.table)
library(purrr)
library(stringr)
library(glue)
library(forcats)
library(tibble)

# Statistical analysis and modeling
library(mgcv)           # Generalized additive models
library(grf)            # Generalized random forests
library(estimatr)       # Estimation with robust standard errors
library(marginaleffects) # Marginal effects and predictions
library(broom)          # Tidy statistical objects
library(car)

# Visualization
library(ggplot2)
library(patchwork)      # Combining plots
library(ggdist)         # Distribution plots
library(ggtext)         # Rich text in plots
library(ggExtra)        # Marginal plots
library(ggsci)          # Scientific color palettes
library(jpeg)           # JPEG image handling
library(grid)           # Grid graphics
library(gridtext)       # Text in grid graphics
library(gridExtra)      # Grid extra utilities

# Web scraping and XML processing
library(rvest)
library(xml2)

# File system and data management
library(here)           # Project-relative file paths
library(fs)             # File system operations
library(osfr)           # Open Science Framework integration

# Tables and reporting
library(gt)             # Grammar of tables
library(kableExtra)     # Enhanced tables
library(modelsummary)   # Model summary tables

# Multiple testing correction
library(qvalue)         # Q-value estimation

# =============================================================================
# VERIFICATION
# =============================================================================

# Check if all packages loaded successfully
cat("All packages loaded successfully!\n")

# Optional: Print session info for reproducibility
cat("\nSession info for reproducibility:\n")
cat("==================================\n")
print(sessionInfo())
