# =============================================================================
# FUNCTIONS FOR EFFECTIVE GIVING AI PERSUASION PROJECT
# =============================================================================
# This file contains all custom functions used throughout the analysis.
# Functions are organized by purpose and functionality.
# =============================================================================

# =============================================================================
# HETEROGENEITY ANALYSIS FUNCTIONS
# =============================================================================

## Heterogeneity analysis by categorical variable
## Runs regression with treatment-categorical variable interactions and returns
## omnibus tests, marginal effects, and comparison plots
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

## Heterogeneity analysis by binned categorical variables
## Handles multiple binary variables (that together are non-mutually exclusive categorization) 
## with minimum sample size requirements
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
    hypotheses(multcomp = "holm")
  
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

## Helper function to create balanced newdata for binary heterogeneity analysis
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

## Helper function to create newdata for profile treatment effects
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

## Run binary heterogeneity model with minimum sample size filtering
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

## Run omnibus F-tests for interaction terms
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

# =============================================================================
# AGREEMENT ANALYSIS FUNCTIONS
# =============================================================================

## Agreement analysis by group variable
## Computes ICC and Cronbach's alpha for each group level
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

## Simple agreement analysis for a data frame
## Computes ICC and Cronbach's alpha across all items
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

# =============================================================================
# PCS (PHILANTHROPIC CLASSIFICATION SYSTEM) FUNCTIONS
# =============================================================================

## Find missing values with fuzzy matching
## Uses Levenshtein distance to find closest matches for missing values
find_missing_with_match <- function(vec, df) {
  # flatten df to unique character values
  df_vals <- unique(na.omit(as.character(unlist(df))))
  
  # which in vec are truly missing
  missing <- setdiff(vec, df_vals)
  if (length(missing) == 0) {
    return(data.frame(original = character(0),
                      closest  = character(0),
                      distance = integer(0),
                      stringsAsFactors = FALSE))
  }
  
  # for each missing element, find the df_val with minimal adist
  res <- lapply(missing, function(m) {
    dists <- adist(m, df_vals)      # base R Levenshtein distances
    i_min <- which.min(dists)
    data.frame(
      original = m,
      closest  = df_vals[i_min],
      distance = dists[i_min],
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, res)
}

## Replace terms using a named vector of replacements
## Handles semicolon-separated values and deduplicates results
replace_terms_named <- function(vec, replacements) {
  sapply(vec, function(entry) {
    if (is.na(entry)) return(NA_character_)
    
    parts <- str_split(entry, ";\\s*")[[1]]
    
    # Replace if in names of the replacement vector/list
    parts_new <- sapply(parts, function(part) {
      if (part %in% names(replacements)) {
        replacements[[part]]
      } else {
        part
      }
    }, USE.NAMES = FALSE)
    
    # Deduplicate and reassemble
    paste(unique(parts_new), collapse = "; ")
  }, USE.NAMES = FALSE)
}

## Extract unique PCS labels from a vector
## Handles semicolon-separated values and sorts results
extract_unique_pcs_labels <- function(vec) {
  vec |>
    replace_na("Unknown or not classified") |>  # Treat NA as a label
    str_split(";\\s*") |>
    unlist() |>
    unique() |>
    sort()
}

## Convert character vector to one-hot-encoded matrix
## Creates binary variables for each unique PCS label
make_pcs_matrix <- function(vec, prefix = "pcs") {
  # 1. Extract all unique labels including "NA"
  unique_labels <- extract_unique_pcs_labels(vec)
  label_lookup <- tibble(
    label = unique_labels,
    label_clean = label |>
      str_to_lower() |>
      str_replace_all("[^a-z0-9]+", "_") |>
      str_remove("_$")
  )
  
  # 2. Expand to long format (treat NA as "NA")
  long <- tibble(row_id = seq_along(vec), raw = vec) %>%
    mutate(raw = replace_na(raw, "NA")) %>%
    separate_rows(raw, sep = ";\\s*") %>%
    rename(label = raw)
  
  # 3. Generate variable names
  long_labeled <- long %>%
    left_join(label_lookup, by = "label") %>%
    mutate(varname = paste0(prefix, "_", label_clean)) %>%
    distinct(row_id, varname) %>%
    mutate(value = 1)
  
  # 4. Pivot to wide matrix
  long_labeled %>%
    pivot_wider(names_from = varname, values_from = value, values_fill = 0) %>%
    arrange(row_id) %>%
    select(-row_id)
}

## Set unknown category for rows with missing PCS data
## Creates an "unknown" column and sets other columns to 0 for missing rows
set_unknown_on_na <- function(data, prefix, unknown_col) {
  # 1. Identify all subj_ columns (after possibly adding unknown_col)
  if (!unknown_col %in% names(data)) {
    data[[unknown_col]] <- 0L
  }
  subj_cols  <- names(data)[startsWith(names(data), prefix)]
  other_cols <- setdiff(subj_cols, unknown_col)
  
  data %>%
    # 2. Flag rows where any *other* subj_ col is NA
    mutate(
      .row_na_flag = if_any(all_of(other_cols), is.na)
    ) %>%
    # 3. For flagged rows: set all other subj_ cols to 0, and unknown_col to 1
    mutate(
      across(all_of(other_cols),
             ~ if_else(.row_na_flag, 0L, .)),
      !!unknown_col := if_else(.row_na_flag, 1L, .data[[unknown_col]])
    ) %>%
    select(-.row_na_flag)
}

## Reduce PCS matrix to columns with sufficient sample size
## Filters out sparse categories and creates "other" category if needed
reduce_pcs_matrix <- function(d, prefix, min_n = 5) {
  # 1. Count occurrences of each variable
  counts <- d |> 
    group_by(condition) |>
    summarise(across(starts_with(prefix), sum))
  
  # 2. Keep vars that have > min_n for all conditions
  het_vars <- counts |>
    select(starts_with(prefix)) |>
    select(where(~ all(.x >= min_n))) |>
    names()
  
  # 3. Separate into well-supported and sparse vars
  all_vars <- counts |> select(starts_with(prefix)) |> names()
  sparse_vars <- setdiff(all_vars, het_vars)
  
  # 4. If there are sparse vars, create "Other" var
  if (length(sparse_vars) > 0) {
    d <- d |> mutate(
      !!paste0(prefix, "_other_categorization") := rowSums(across(all_of(sparse_vars)))
    )
    het_vars <- c(het_vars, paste0(prefix, "_other_categorization"))
  }
  
  d |> select(-all_of(sparse_vars))
}

## Format EIN numbers with dash separator
format_ein <- function(eins) {
  sub("^(\\d{2})(\\d{7})$", "\\1-\\2", eins)
}

# =============================================================================
# HIERARCHY PARSING FUNCTIONS
# =============================================================================

## Parse HTML hierarchy file into nested list structure
parse_hierarchy <- function(html_file_path, leaves_as_vec = FALSE) {
  html <- read_html(html_file_path)
  
  # Recursive helper
  rec_parse_hierarchy <- function(node) {
    lis <- html_elements(node, xpath = "./li")
    
    map(lis, function(li) {
      span <- html_element(li, "span")
      label <- html_text(span) |>
        str_trim()
      
      # Skip empty labels
      if (is.na(label) || label == "") return(NULL)
      
      children <- html_elements(li, "ul")
      
      if (length(children) > 0) {
        set_names(list(rec_parse_hierarchy(children[[1]])), label)
      } else {
        label
      }
    }) |>
      compact() |>  # Remove NULLs from skipped empty labels
      (\(x) {
        if (leaves_as_vec && all(map_lgl(x, is.character))) {
          unlist(x)
        } else {
          x
        }
      })()
  }
  
  top_ul <- html_element(html, "#browser")
  rec_parse_hierarchy(top_ul)
}

## Convert hierarchy list to data frame with level columns
hierarchy_to_df <- function(hierarchy) {
  collect_paths <- function(node, path = character(), acc = list()) {
    if (is.character(node)) {
      # leaf labels (atomic character vectors too)
      for (lbl in node) {
        acc[[length(acc) + 1]] <- c(path, lbl)
      }
    } else if (is.list(node)) {
      nm <- names(node)
      for (i in seq_along(node)) {
        child  <- node[[i]]
        name_i <- if (!is.null(nm)) nm[i] else NA_character_
        next_path <- if (!is.na(name_i) && nzchar(name_i)) c(path, name_i) else path
        acc <- collect_paths(child, next_path, acc)
      }
    }
    acc
  }
  
  all_paths <- list()
  for (entry in hierarchy) {
    if (is.list(entry) && !is.null(names(entry))) {
      # named subtree → dive in with lvl_1 = the name
      for (branch in names(entry)) {
        all_paths <- c(all_paths,
                       collect_paths(entry[[branch]], path = branch))
      }
    } else if (is.character(entry)) {
      # top‐level atomic labels → treat as a depth‐1 path
      all_paths <- c(all_paths,
                     collect_paths(entry, path = character()))
    }
  }
  
  # now pad to uniform depth
  max_depth <- max(vapply(all_paths, length, integer(1)))
  mat <- do.call(rbind, lapply(all_paths, function(p) {
    length(p) <- max_depth
    p
  }))
  
  # to tibble with lvl_1…lvl_n
  df <- as_tibble(mat)
  colnames(df) <- paste0("lvl_", seq_len(ncol(df)))
  df
}

## Replace labels with their level-k equivalents in hierarchy
replace_with_level <- function(vec, hierarchy_df, k = 1, na_replacement = "Unknown or not classified") {
  lvl_cols <- grep("^lvl_", names(hierarchy_df), value = TRUE)
  lvl_col  <- paste0("lvl_", k)
  if (!lvl_col %in% lvl_cols) {
    stop("No column ", lvl_col, " in hierarchy_df")
  }
  
  sapply(vec, USE.NAMES = FALSE, function(entry) {
    if (is.na(entry)) return(na_replacement)
    
    parts <- str_split(entry, ";\\s*")[[1]]
    new_parts <- vapply(parts, FUN.VALUE = character(1), function(lbl) {
      hits <- hierarchy_df %>%
        filter(if_any(all_of(lvl_cols), ~ .x == lbl))
      
      if (nrow(hits) == 0) {
        # not in hierarchy: leave as is
        return(lbl)
      }
      
      row <- hits[1, ]
      # try lvl_k, then lvl_{k-1}, … down to lvl_1
      for (j in seq(k, 1)) {
        candidate <- row[[paste0("lvl_", j)]]
        if (!is.na(candidate) && nzchar(candidate)) {
          return(candidate)
        }
      }
      # if somehow all are NA, fall back to the original label
      lbl
    })
    
    # collapse back, remove duplicates, clean whitespace
    result <- paste(unique(new_parts), collapse = "; ")
    str_squish(str_replace_all(result, "[\r\n]", " "))
  })
}

# =============================================================================
# GAM (GENERALIZED ADDITIVE MODEL) FUNCTIONS
# =============================================================================

## Run simple GAM between moderator and outcome variable
## Creates smooth plots with confidence intervals and optional histogram
run_gam_simple <- function(
    d, cov, dv = "cents_to_amf_change",
    include_data = TRUE, add_hist = TRUE, x_label = cov, y_label = "Donation change",
    include_legend = TRUE) {
  
  # Get number of unique values
  num_unique <- length(unique(d[[cov]]))
  
  # Determine how to model the covariate
  if (num_unique <= 10) {
    cov_term <- paste("s(", cov, ", k=", num_unique - 1, ",  by=condition)")  # Limit basis dimension
  } else {
    cov_term <- paste("s(", cov, ", by=condition)")  # Use default smooth
  }
  
  form <- as.formula(
    paste(dv, "~ condition +", cov_term)
  )
  
  gam_model <- gam(form, data = d, method = "REML")
  
  gam_sum <- summary(gam_model)
  gam_gratia_plot <- gratia::draw(gam_model)
  
  avg_pred_plot <- marginaleffects::avg_predictions(
    gam_model,
    by = c("condition", cov),
    newdata = "balanced"
  ) |> 
    ggplot(aes(x = !!sym(cov), y = estimate, ymin = conf.low, ymax = conf.high, col = condition)) +
    geom_line() +
    geom_ribbon(alpha = 0.2) +
    geom_jitter(aes(y = !!sym(dv), ymin = NULL, ymax = NULL), data = d) 
  
  preds <- marginaleffects::predictions(
    gam_model,
    newdata = expand_grid(
      !!sym(cov) := seq(min(d[[cov]], na.rm = TRUE), max(d[[cov]], na.rm = TRUE), length.out = 100),
      condition = unique(d$condition)
    )
  )
  
  
  # 0) compute ribbon extents + a little 2% padding
  lims <- preds %>% 
    summarise(
      x_min = min(!!sym(cov)),
      x_max = max(!!sym(cov)),
      y_min = min(conf.low),
      y_max = max(conf.high))
  x_rng <- lims$x_max - lims$x_min
  y_rng <- lims$y_max - lims$y_min
  
  pad_x <- 0.02 * x_rng
  pad_y <- 0.02 * y_rng
  
  # 1) Build base ggplot + layers
  p <- ggplot() +
    # a) (possibly invisible) jittered data points
    geom_jitter(
      data        = d,
      inherit.aes = FALSE,
      aes(
        x      = !!sym(cov),
        y      = !!sym(dv),
        colour = condition
      ),
      width = 0.1,
      height = 0,
      alpha = if (include_data) 0.3 else 0
    ) +
    geom_hline(
      yintercept = 0,
      linetype   = "dashed",
      colour     = "black",
      lwd = 1
    ) +
    # b) ribbon & line
    geom_ribbon(
      data   = preds,
      aes(
        x    = !!sym(cov),
        ymin = conf.low,
        ymax = conf.high,
        fill = condition
      ),
      alpha  = 0.2,
      colour = NA
    ) +
    geom_line(
      data = preds,
      aes(
        x      = !!sym(cov),
        y      = estimate,
        colour = condition
      ),
      linewidth = 1.25
    ) +
    scale_y_continuous(
      labels = scales::label_number(suffix = "c")
    ) +
    ggsci::scale_color_locuszoom(labels = cond_names) +
    ggsci::scale_fill_locuszoom(labels = cond_names) +
    labs(
      fill = "Condition",
      color = "Condition",
      x = x_label,
      y = y_label
    ) +
    theme(
      panel.grid = element_blank(),
      axis.title.x = element_text(size = 10),
      axis.text.x = element_text(size = 8),
      axis.title.y = element_text(size = 10)
    )
  
  ## if the covaraite is log10 transformed, add a log scale
  if (grepl("log10", cov)) {
    log10_breaks <- seq(4, 10)
    p <- p +
      scale_x_continuous(
        breaks = log10_breaks ,
        labels = scales::label_number(
          scale_cut = scales::cut_short_scale(),
          prefix = "$",
          accuracy = 1
        )(10^log10_breaks )  # convert log10 back to original values for labels
      )
  } else {
    p <- p + scale_x_continuous()
  }
  
  # 2) If we're *not* showing the raw data, lock the limits to the ribbon ± padding
  if (!include_data) {
    p <- p + 
      coord_cartesian(
        xlim = c(lims$x_min - pad_x, lims$x_max + pad_x),
        ylim = c(lims$y_min - pad_y, lims$y_max + pad_y)
      )
  }
  
  # 3) strip legend if requested
  if (!include_legend) {
    p <- p + theme(legend.position = "none")
  }
  
  # 4) Finally, if you want the marginal histogram, wrap in ggMarginal
  if (add_hist) {
    p <- ggMarginal(
      p,
      type    = "histogram",
      margins = "x",
      size    = 5,
      fill    = "grey80",
      alpha   = 0.4
    )
  }
  
  list(
    mod = gam_model,
    mod_summary = summary(gam_model),
    gam_gratia_plot = gam_gratia_plot,
    preds = preds,
    pred_plot = p
  )
}

# =============================================================================
# STATISTICAL HELPER FUNCTIONS
# =============================================================================

## Get q-values for family of hypothesis tests
## Replaces p-values with q-values for specified terms
get_qvals <- function(mod, terms_to_q_swap) {
  requireNamespace('qvalue')
  nms <- terms_to_q_swap
  ps <- mod$p.value
  qs <- set_names(qvalue::qvalue(ps[nms])$qvalues, nms)
  ps[nms] <- qs
  ps
}

# =============================================================================
# CAUSAL FOREST PLOTTING FUNCTIONS
# =============================================================================

## Plot unadjusted heterogeneity for a single variable
## Creates GAM smooth plots of individual treatment effects
plot_uhp <- function(ites_conv, ites_static, d, var, label) {
  #get k for number of bases for GAM
  default_k <- 10
  # default unless less unique vals, then uniq vals - 1
  k_val    <- min(default_k, n_distinct(d[[var]]) - 1) 
  
  d %>% 
    mutate(
      ite_conv = ites_conv,
      ite_static = ites_static
    ) |> 
    pivot_longer(
      cols = starts_with("ite_"),
      names_to = "comparison",
      values_to = "ite"
    ) |>
    ggplot(aes(y = ite, x = !!sym(var), fill = comparison, col = comparison)) +
    geom_point(alpha = 0.025) +
    geom_smooth(
      method  = "gam",
      formula = y ~ s(x, bs = "tp", k = k_val)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    labs(
      x = label,
      y = "Causal-forest estimated Individual Treatment Effect"
    ) +
    scale_color_manual(
      values = c("ite_conv" = "#5CB85CFF", "ite_static" = "#EEA236FF"),
      name = "Treatment Effect",
      labels = c(
        "ite_conv" = "LLM Conversation - Control",
        "ite_static" ="Static Message - Control"
      )
    ) +
    scale_fill_manual(
      values = c("ite_conv" = "#5CB85CFF", "ite_static" = "#EEA236FF"),
      name = "Treatment Effect",
      labels = c(
        "ite_conv" = "LLM Conversation - Control",
        "ite_static" = "Static Message - Control"
      )
    ) +
    theme(
      axis.title.x = element_text(size = 8),
      legend.position = "bottom",
      axis.text.x = element_text(size = 6),
    )
}

## Plot all unadjusted heterogeneity plots
## Creates a grid of UHP plots for multiple variables
plot_all_uhps <- function(ites_conv, ites_static, d, vars, labels) {
  uhps <- map(seq_along(vars), ~ plot_uhp(ites_conv, ites_static, d, vars[.x], labels[.x]))
  patchwork::wrap_plots(uhps, guides = 'collect', ncol = 5)  +
    plot_layout(axes = "collect") &
    theme(legend.position = "bottom")
}

## Wrapper function for partial dependence plots
plot_pdp <- function(...) pdp(...)$fig

## Plot all partial dependence plots
## Creates a grid of PDP plots for multiple variables
plot_all_pdps <- function(forest, X, vars, labels, n = 250, midpoint_func = median,
                          method = "centered", se_type = "sample",
                          include_hline0 = TRUE) {
  pdps <- map(vars, ~ plot_pdp(
    forest        = forest,
    X             = X,
    cov_name      = .x,
    label         = labels[.x],
    n             = n,
    midpoint_func = midpoint_func,
    method        = method,
    se_type       = se_type,
    include_hline0= include_hline0
  ))
  patchwork::wrap_plots(pdps, guides = "collect", ncol = 5) +
    plot_layout(axes = "collect_y") &
    theme(
      axis.title.x = element_text(size = 8),
      legend.position = "bottom",
      axis.text.x = element_text(size = 6),
    )
}

# =============================================================================
# PARTIAL DEPENDENCE PLOT FUNCTIONS
# =============================================================================

## Create partial dependence plots from causal forest
## Supports both binary and multi-armed forests
pdp <- function(forest, X, cov_name, label = cov_name, 
                n = 250, midpoint_func = median,
                method = c("centered", "average"),
                se_type = c("sample", "model"),
                include_hline0 = FALSE) {
  
  method  <- match.arg(method)
  se_type <- match.arg(se_type)
  
  # ensure data.table
  if (!data.table::is.data.table(X)) X <- data.table::as.data.table(X)
  
  # set up grid of values for covariate
  x_all <- X[[cov_name]]
  if (data.table::uniqueN(x_all) <= 2) {
    pred_vals <- sort(unique(x_all))
  } else {
    fr <- stats::fivenum(x_all, na.rm = TRUE)
    pred_vals <- seq(fr[1], fr[5], length.out = n)
  }
  
  message(glue::glue("feature: {cov_name} → plotting {length(pred_vals)} points"))
  
  # build newdata for predictions
  if (method == "centered") {
    center_row <- X[, lapply(.SD, midpoint_func)]
    X_pred <- center_row[rep(1, length(pred_vals))]
    X_pred[[cov_name]] <- pred_vals
  } else {
    X_pred <- data.table::rbindlist(
      lapply(pred_vals, function(v) { tmp <- copy(X); tmp[[cov_name]] <- v; tmp })
    )
  }
  
  # get CF predictions & variances
  cf <- predict(forest, newdata = X_pred, estimate.variance = TRUE)
  preds_array <- cf$predictions
  vars_array  <- cf$variance.estimates
  
  # drop trailing dimensions to get matrix
  preds <- drop(preds_array)
  vars  <- drop(vars_array)
  
  # detect multi-armed vs. binary
  if (is.matrix(preds) && ncol(preds) > 1) {
    arms <- colnames(preds)
    if (is.null(arms) || length(arms) != ncol(preds)) {
      arms <- paste0("arm", seq_len(ncol(preds)))
    }
    
    # stack into long table
    n_pred <- length(pred_vals)
    dt <- data.table::data.table(
      v           = rep(pred_vals, times = ncol(preds)),
      treatment   = factor(rep(arms, each = n_pred), levels = arms),
      prediction  = as.vector(preds),
      variance    = as.vector(vars)
    )
    
    # summarise
    if (method == "average") {
      pd_sum <- dt[, {
        if (se_type == "sample") {
          se <- sd(prediction)/sqrt(.N)
        } else {
          se <- sqrt(mean(variance)/.N)
        }
        .(prediction = mean(prediction), se = se)
      }, by = .(v, treatment)]
    } else {
      pd_sum <- dt[, .(
        prediction = mean(prediction),
        se         = sqrt(mean(variance))
      ), by = .(v, treatment)]
    }
    
    pd_sum[, `:=`(
      lower = prediction - 1.96 * se,
      upper = prediction + 1.96 * se
    )]
    
    plot_data <- pd_sum
    
    p <- ggplot(pd_sum, aes(x = v, y = prediction,
                            colour = treatment, fill = treatment)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) +
      geom_line(linewidth = 1) +
      scale_color_manual(
        values = c(
          "conv_treatment - control" = "#5CB85CFF", 
          "static_treatment - control" = "#EEA236FF"
        ),
        name = "Treatment Effect",
        labels = c(
          "conv_treatment - control" = "LLM Conversation - Control",
          "static_treatment - control" ="Static Message - Control"
        )
      ) +
      scale_fill_manual(
        values = c(
          "conv_treatment - control" = "#5CB85CFF", 
          "static_treatment - control" = "#EEA236FF"
        ),
        name = "Treatment Effect",
        labels = c(
          "conv_treatment - control" = "LLM Conversation - Control",
          "static_treatment - control" ="Static Message - Control"
        ) 
      ) +
      labs(x = label, y = "Causal-forest estimated CATE") +
      theme(legend.position = "bottom")
    
  } else {
    # single-arm fallback
    pd_summary <- if (method == "average") {
      tmp <- data.table::data.table(
        v           = rep(pred_vals, each = nrow(X)),
        pred        = as.vector(preds),
        variance    = as.vector(vars)
      )
      if (se_type == "sample") {
        tmp[, .(
          prediction = mean(pred),
          se         = sd(pred)/sqrt(.N)
        ), by = v]
      } else {
        tmp[, .(
          prediction = mean(pred),
          se         = sqrt(mean(variance)/.N)
        ), by = v]
      }
    } else {
      data.table::data.table(
        v           = pred_vals,
        prediction  = as.vector(preds),
        se          = sqrt(as.vector(vars))
      )
    }
    pd_summary[, `:=`(
      lower = prediction - 1.96 * se,
      upper = prediction + 1.96 * se
    )]
    
    plot_data <- pd_summary
    
    p <- ggplot2::ggplot(plot_data,
                         ggplot2::aes(x = v, y = prediction)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                           alpha = 0.1, fill = "grey70", colour = NA) +
      ggplot2::geom_line(size = 1) +
      labs(x = cov_name, y = "Causal-forest estimated CATE") +
      theme_bw()
  }
  
  if (include_hline0) {
    p <- p + ggplot2::geom_hline(yintercept = 0, linetype = "dashed")
  }
  if (length(pred_vals) <= 2) {
    p <- p + ggplot2::geom_point(size = 2, show.legend = FALSE)
  }
  
  return(list(fig = p, pd_data = plot_data))
}
