

# run gams on these variables
important_covars <- c(
  "revenue_log10", "cents_to_amf_pre", "mean_aot", "ous_impartial_beneficence"
)

important_covar_names <- c(
  "revenue_log10" = "Charity 2024 Revenue (log10)",
  "cents_to_amf_pre" = "Pre-trt Cents to AMF", 
  "mean_aot" = "Open-Minded Thinking (AOT-E)",
  "ous_impartial_beneficence" = "Utilitarianism: Impartial Beneficence (OUS)"
)

# run gams for each
gams <- lapply(
  important_covars, 
  \(cov) run_gam_simple(d, cov, include_data = FALSE, add_hist = TRUE, x_label = important_covar_names[cov], include_legend = FALSE)
)
