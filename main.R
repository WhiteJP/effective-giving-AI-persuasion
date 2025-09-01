# =============================================================================
# Effective Giving AI Persuasion Project - Main Analysis Script
# =============================================================================

# Set WD
loc <- here::here()
setwd(loc)

# get data 
source("scripts/download_data.R")
download_osf_data_dir("6ya5n")
fs::dir_create("output", "figures") 

# Load config (sets global vars used throughout analysis)
source("scripts/config.R")

# Load functions used for analysis
source("scripts/functions/heterogeneity_functions.R")
source("scripts/functions/agreement_functions.R")
source("scripts/functions/pcs_hierarchy_functions.R")
source("scripts/functions/gam_functions.R")
source("scripts/functions/causal_forest_functions.R")
source("scripts/functions/statistical_helpers.R")

# Analysis
source("scripts/analysis/attrition.R")
source("scripts/analysis/sample_descriptives.R")
source("scripts/analysis/charity_descriptives.R")
source("scripts/analysis/motivation_analysis.R")
source("scripts/analysis/ates.R")
source("scripts/analysis/cat_heterogeneity.R")
source("scripts/analysis/causal_forest.R")
source("scripts/analysis/gams.R")
source("scripts/analysis/persuasive_strategies.R")
source("scripts/analysis/ai_accuracy.R")

# Figures
source("scripts/figures/charity_descriptive_plot.R") # Fig 2
source("scripts/figures/motivation_plots.R") # Fig S1
source("scripts/figures/main_plot.R") # Fig 3
source("scripts/figures/cat_het_plot_cates.R") # Fig 4
source("scripts/figures/binned_het_cates.R") # Fig S2
source("scripts/figures/vip_and_gam_plot.R") # Fig 5
source("scripts/figures/causal_forest_plots.R") # Fig S3 and S4
source("scripts/figures/strategy_plot.R") # Fig 6
source("scripts/figures/accuracy_figure.R") # Fig S5

# Tables
source("scripts/tables/gam_tables.R") # Tables S2 and S3
source("scripts/tables/strategy_regression_table.R") # Table S4

#Note other tables and figures were not generated programatically.

cat("\nSession info for reproducibility:\n")
cat("==================================\n")
print(sessionInfo())
