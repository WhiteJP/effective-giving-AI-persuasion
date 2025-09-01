# =============================================================================
# HETEROGENEITY ANALYSIS FUNCTIONS
# =============================================================================
# Functions for analyzing treatment effect heterogeneity across different
# categorical and continuous variables
# =============================================================================

#' Heterogeneity analysis by categorical variable
#' 
#' Runs regression with treatment-categorical variable interactions and returns
#' omnibus tests, marginal effects, and comparison plots
#' 
#' @param cat_var Character. Name of categorical variable for heterogeneity analysis
#' @param d Data frame. Analysis dataset
#' @param out_var Character. Outcome variable name (default: "cents_to_amf_change")
#' @param control_var Character. Control variable name (default: "cents_to_amf_pre_cat")
#' @return List containing model, tests, comparisons, predictions, and plot
het_by_cat <- function(
    cat_var, 
    d, 
    out_var = "cents_to_amf_change", 
    control_var = "cents_to_amf_pre_cat"
) {
  # run lm 
  mod <- estimatr::lm_robust(
    as.formula(paste(out_var, "~ condition*", cat_var, "+ condition*", control_var)),
    data = d
  )
  
  ## get omnibus F-test
  omnibus_tests <- run_ombinus_tests(mod, cat_var)
  
  ## get CATE comparisons
  comps <- marginaleffects::avg_comparisons(
    mod,
    variables = list("condition" = "pairwise"),
    newdata = "balanced",
    by = cat_var
  )
  
  ## get model predictions
  preds <- marginaleffects::avg_predictions(
    mod,
    variables = control_var,
    newdata = "balanced",
    by = c(cat_var, "condition")
  ) 
  
  marg_plot <- preds |> 
    ggplot(aes(x = !!sym(cat_var), y = estimate, color = condition, fill = condition)) +
    geom_point(position = position_dodge(width = 0.4)) +
    geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.4)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      fill = "Condition",
      color = "Condition",
      x = cat_var,
      y = "Donation Change"
    )
  
  ## return objects
  list(
    mod = mod, 
    omnibus_tests = omnibus_tests,
    comparisons = comps, 
    preds = preds,
    marg_plot = marg_plot
  )
}

#' Heterogeneity analysis by binned categorical variables
#' 
#' Handles multiple binary variables (that together are non-mutually exclusive categorization) 
#' with minimum sample size requirements
#' 
#' @param prefix Character. Prefix for binary variables to analyze
#' @param d Data frame. Analysis dataset
#' @param out_var Character. Outcome variable name (default: "cents_to_amf_change")
#' @param control_var Character. Control variable name (default: "cents_to_amf_pre_cat")
#' @param min_n Numeric. Minimum sample size per condition (default: 10)
#' @return List containing model, tests, predictions, contrasts, and plots
het_by_bins_cond <- function(
    prefix, 
    d,
    out_var = "cents_to_amf_change", 
    control_var = "cents_to_amf_pre_cat", 
    min_n = 10
) {
  obj <- run_bin_het_mod(prefix, d, out_var, control_var, min_n)
  counts <- obj$counts
  mod <- obj$mod
  d <- obj$newd
  het_vars <- obj$het_vars
  
  ## get omnibus F-tests
  omnibus_tests <- run_ombinus_tests(mod, prefix)
  
  # Get predictions, for each condition, with level on (1), and off (0),
  # with all other levels off (0)
  preds_all <- map_dfr(
    het_vars,
    function(var) {
      preds <- marginaleffects::avg_predictions(
        mod,
        newdata = get_bin_newdata(var, het_vars, d, control_var),
        by = c(var, "condition")
      )
      
      preds |> 
        mutate(
          var = var,
          var_value = .data[[var]],  # Extract the moderator's value
          .keep = "unused"
        ) |> 
        select(where(~ !all(is.na(.x))))
    }
  )
  
  # plot this
  pred_plot <- preds_all %>% 
    ggplot(
      aes(x = factor(var_value), y = estimate, color = condition, group = condition)
    ) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_linerange(
      aes(ymin = conf.low, ymax = conf.high), 
      position = position_dodge(width = 0.5)
    ) +
    facet_wrap(~ var, scales = "free_x") +
    labs(title = "Predicted Outcome by Treatment Condition and Binary Moderator",
         x = "Moderator Level (0/1)",
         y = "Predicted Outcome (Change Score)")
  
  ## now get these 0-1 contrasts
  ## computes diff in prediction, for each group, if level is on (1) v off (0)
  contrasts_all <- map_dfr(
    het_vars,
    function(var) {
      marginaleffects::avg_comparisons(
        mod,
        newdata = get_bin_newdata(var, het_vars, d, control_var),
        variables = set_names(list(c(0, 1)), var),
        by = "condition"
      )  |> 
        select(where(~ !all(is.na(.x))))
    }
  )
  
  # plot showing this
  contrasts_plot <- contrasts_all %>% 
    ggplot(aes(x = term, y = estimate, color = condition, group = condition)) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5)) +
    labs(title = "Change in treament effect by moderator",
         x = "Moderator Level",
         y = "Change in prediction when mod = 1")
  
  # now get comparisons of these between conditions
  dd_contrasts <- map_dfr(
    het_vars,
    function(var) {
      marginaleffects::avg_comparisons(
        mod,
        newdata   = get_bin_newdata(var, het_vars, d, control_var),
        # compare treat vs control:
        variables = set_names(list(c(0, 1)), var),
        # do one ΔΔ per moderator:
        by        = "condition",
        hypothesis = difference ~ revpairwise # gets correct ordering of conditions
      ) %>%
        mutate(var = var)
    }
  )
  
  # Plot these
  cates_plot <- dd_contrasts %>% 
    ggplot(aes(x = var, y = estimate, color = term, group = term)) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5)) +
    labs(title = "Change in treament effect by moderator",
         x = "Moderator",
         y = "ATE")
  
  ## Profile Treatment Effects -- thing reported in paper
  ## contrasts when level on v off, with all others off.
  te_data <- map_dfr(
    het_vars,
    function(var) {
      get_bin_newdata_profile_te(var, het_vars, d, control_var) |> 
        mutate(profile_var = var, .keep = "unused")
    }
  )
  
  
  profile_tes <- marginaleffects::avg_comparisons(
    mod,
    newdata = te_data,
    variables = "condition",
    by = c("profile_var"),
  )
  
  profile_tes_diff <-  marginaleffects::avg_comparisons(
    mod,
    newdata = te_data,
    variables = list("condition" = "minmax"), # just conv v control
    by = "profile_var",
    hypothesis = difference ~ pairwise
  ) |> 
    marginaleffects::hypotheses(multcomp = "holm")
  
  # return a list of results
  list(
    counts = counts,
    mod = mod,
    omnibus_tests = omnibus_tests,
    preds = preds_all,
    preds_plot = pred_plot,
    contrasts = contrasts_all,
    contrasts_plot = contrasts_plot,
    cates = dd_contrasts,
    cates_plot = cates_plot,
    profile_tes = profile_tes,
    profile_tes_diff = profile_tes_diff
  )
  
}

#' Helper function to create balanced newdata for binary heterogeneity analysis
#' 
#' @param var Character. Variable name to vary
#' @param vars Character vector. All heterogeneity variables
#' @param d Data frame. Analysis dataset
#' @param control_var Character. Control variable name
#' @return Data frame with balanced newdata
get_bin_newdata <- function(var, vars, d, control_var = "cents_to_amf_pre_cat") {
  other_vars <- setdiff(vars, var)
  new_data <- expand_grid(
    condition = unique(d$condition),
    !!var := c(0, 1), 
    !!control_var := if ( is.numeric(d[[control_var]]) ) {
      mean(d[[control_var]], na.rm = TRUE) 
    } else {
      unique(d[[control_var]])
    }
  ) |> 
    bind_cols(
      setNames(
        as.list(rep(0, length(other_vars))),
        other_vars
      )
    )
}

#' Helper function to create newdata for profile treatment effects
#' 
#' @param var Character. Variable name to vary
#' @param vars Character vector. All heterogeneity variables
#' @param d Data frame. Analysis dataset
#' @param control_var Character. Control variable name
#' @return Data frame with profile treatment effect newdata
get_bin_newdata_profile_te <- function(var, vars, d, control_var = "cents_to_amf_pre_cat") {
  other_vars <- setdiff(vars, var)
  new_data <- expand_grid(
    condition = unique(d$condition),
    !!var := 1,
    !!control_var := if ( is.numeric(d[[control_var]]) ) {
      mean(d[[control_var]], na.rm = TRUE) 
    } else {
      unique(d[[control_var]])
    }
  ) |> 
    bind_cols(
      setNames(
        as.list(rep(0, length(other_vars))),
        other_vars
      )
    )
}

#' Run binary heterogeneity model with minimum sample size filtering
#' 
#' @param prefix Character. Prefix for binary variables
#' @param d Data frame. Analysis dataset
#' @param out_var Character. Outcome variable name
#' @param control_var Character. Control variable name
#' @param min_n Numeric. Minimum sample size per condition
#' @return List containing counts, model, updated data, and heterogeneity variables
run_bin_het_mod <- function(prefix, d, out_var = "cents_to_amf_change", control_var = "cents_to_amf_pre_cat", min_n = 10) {
  # get counts for each each variable with prefix
  counts <- d |> 
    group_by(condition) |>
    summarise(across(starts_with(prefix), sum))
  
  # keep vars that have > x for all conditions
  het_vars <- counts |>
    select(starts_with(prefix)) |>
    select(where(~ all(.x >= min_n))) |>
    names()
  
  # separate into well-supported and sparse vars
  all_vars <- counts |> select(starts_with(prefix)) |> names()
  sparse_vars <- setdiff(all_vars, het_vars)
  
  # if there are sparse vars, create "Other" var
  if (length(sparse_vars) > 0) {
    d <- d |> mutate(
      !!paste0(prefix, "_other_categorization") := rowSums(across(all_of(sparse_vars)))
    )
    het_vars <- c(het_vars, paste0(prefix, "_other_categorization"))
  }
  
  
  # run lm
  mod <- estimatr::lm_robust(
    as.formula(paste(out_var, "~ condition*(", paste(het_vars, collapse = "+"), ") + condition*", control_var)),
    data = d
  )
  
  list(counts = counts, mod  = mod, newd = d, het_vars = het_vars)
}

#' Run omnibus F-tests for interaction terms
#' 
#' @param mod Model object. Fitted regression model
#' @param cat_var Character. Categorical variable name
#' @return List containing F-test results for different interaction sets
run_ombinus_tests <- function(mod, cat_var) {
  # 1. pull out all the interaction names
  all_int   <- grep(
    paste0("^(conditionstatic_treatment|conditionconv_treatment):", cat_var),
    names(coef(mod)),
    value = TRUE
  )
  # 2. split into the two treatment arms
  static_int <- grep("static", all_int, value = TRUE)
  conv_int   <- grep("conv",   all_int, value = TRUE)
  
  # 3. build the four sets of hypothesis strings
  all_hyp    <- paste0(all_int,    " = 0")       #  all (static and conv) = 0
  static_hyp <- paste0(static_int, " = 0")       # static only = 0
  conv_hyp   <- paste0(conv_int,   " = 0")       # conv only = 0
  eq_hyp     <- paste0(conv_int,   " = ", static_int)  # conv = static
  
  # 4. run the F–tests
  list(
    all           = car::linearHypothesis(mod, all_hyp,    test = "F"),
    static_only   = car::linearHypothesis(mod, static_hyp, test = "F"),
    conv_only     = car::linearHypothesis(mod, conv_hyp,   test = "F"),
    conv_vs_static= car::linearHypothesis(mod, eq_hyp,     test = "F")
  )
}
