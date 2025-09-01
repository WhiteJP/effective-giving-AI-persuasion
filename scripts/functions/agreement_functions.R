# =============================================================================
# AGREEMENT ANALYSIS FUNCTIONS
# =============================================================================
# Functions for analyzing inter-rater agreement and reliability
# =============================================================================

#' Agreement analysis by group variable
#' 
#' Computes ICC and Cronbach's alpha for each group level
#' 
#' @param d Data frame. Data with items as columns and cases as rows
#' @param group_var Character. Name of grouping variable
#' @return List containing ICC and alpha results by group
agreement_analysis_by_group <- function(d, group_var) {
  group_sym <- rlang::sym(group_var)
  
  iccs_by_variable <- d |> 
    group_by(!!group_sym) |> 
    nest() |> 
    mutate(
      icc = map(data, psych::ICC),
      icc_df = map(icc, ~ tibble::rownames_to_column(.x$results, "icc_name"))
    ) |> 
    unnest(icc_df)
  
  grand_icc <- d |> 
    select(-!!group_sym) |>  # drop group var to compute ICC across items
    psych::ICC()
  
  alpha_by_variable <- d |> 
    group_by(!!group_sym) |> 
    nest() |> 
    mutate(
      alpha_obj = map(data, ~ psych::alpha(.x, check.keys = TRUE)),
      alpha = map(alpha_obj, ~ .x$total)
    ) |> 
    unnest(alpha)
  
  cat("Grand ICC:\n")
  print(grand_icc)
  
  cat("\nIndividual ICCs (ICC2k):\n")
  iccs_by_variable |> 
    select(-data, -icc) |> 
    filter(type == "ICC2k") |> 
    print(n = Inf)
  
  cat("\nmean ICCs (ICC):\n")
  icc_summary <- iccs_by_variable |> 
    select(-data, -icc) |> 
    filter(type == "ICC2k") |>
    ungroup() |> 
    summarise(
      mean_icc2k = mean(ICC, na.rm = TRUE),
      min_icc2k = min(ICC, na.rm = TRUE),
      max_icc2k = max(ICC, na.rm = TRUE)
    ) 
  print(icc_summary)
  
  cat("\nCronbach's Alpha:\n")
  alpha_by_variable |> 
    select(-data, -alpha_obj) |> 
    print(n = Inf)
  
  invisible(
    list(
      iccs_by_variable = iccs_by_variable,
      icc_summary = icc_summary,
      grand_icc = grand_icc,
      alpha_by_variable = alpha_by_variable
    )
  )
}

#' Simple agreement analysis for a data frame
#' 
#' Computes ICC and Cronbach's alpha across all items
#' 
#' @param d Data frame or matrix. Numeric items (columns = items, rows = cases)
#' @return List containing ICC and alpha results
agreement_analysis <- function(d) {
  # d: a data.frame or matrix of numeric items (columns = items, rows = cases)
  
  # 1. Compute ICC
  icc_res <- psych::ICC(d)
  
  # 2. Compute Cronbach's alpha
  alpha_res <- psych::alpha(d, check.keys = TRUE)
  
  # 3. Print results
  cat("=== Intraclass Correlation (ICC) ===\n")
  print(icc_res)
  
  cat("\n=== Cronbach's Alpha ===\n")
  print(alpha_res$total)
  
  # 4. Return invisibly for further inspection
  invisible(list(
    icc   = icc_res,
    alpha = alpha_res
  ))
}
