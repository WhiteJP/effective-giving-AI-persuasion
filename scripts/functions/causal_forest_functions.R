# =============================================================================
# CAUSAL FOREST FUNCTIONS
# =============================================================================
# Functions for causal forest analysis and visualization
# =============================================================================

#' Plot unadjusted heterogeneity for a single variable
#' 
#' Creates GAM smooth plots of individual treatment effects
#' 
#' @param ites_conv Numeric vector. Individual treatment effects for conversation condition
#' @param ites_static Numeric vector. Individual treatment effects for static condition
#' @param d Data frame. Analysis dataset
#' @param var Character. Variable name for x-axis
#' @param label Character. Label for x-axis
#' @return ggplot object
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

#' Plot all unadjusted heterogeneity plots
#' 
#' Creates a grid of UHP plots for multiple variables
#' 
#' @param ites_conv Numeric vector. Individual treatment effects for conversation condition
#' @param ites_static Numeric vector. Individual treatment effects for static condition
#' @param d Data frame. Analysis dataset
#' @param vars Character vector. Variable names for x-axes
#' @param labels Character vector. Labels for x-axes
#' @return patchwork object
plot_all_uhps <- function(ites_conv, ites_static, d, vars, labels) {
  uhps <- map(seq_along(vars), ~ plot_uhp(ites_conv, ites_static, d, vars[.x], labels[.x]))
  patchwork::wrap_plots(uhps, guides = 'collect', ncol = 5)  +
    plot_layout(axes = "collect") &
    theme(legend.position = "bottom")
}

#' Wrapper function for partial dependence plots
#' 
#' @param ... Arguments passed to pdp function
#' @return ggplot object
plot_pdp <- function(...) pdp(...)$fig

#' Plot all partial dependence plots
#' 
#' Creates a grid of PDP plots for multiple variables
#' 
#' @param forest Causal forest object
#' @param X Data frame. Feature matrix
#' @param vars Character vector. Variable names
#' @param labels Character vector. Variable labels
#' @param n Numeric. Number of prediction points (default: 250)
#' @param midpoint_func Function. Function to compute midpoint (default: median)
#' @param method Character. Method for PDP computation (default: "centered")
#' @param se_type Character. Standard error type (default: "sample")
#' @param include_hline0 Logical. Whether to include horizontal line at 0 (default: TRUE)
#' @return patchwork object
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

#' Create partial dependence plots from causal forest
#' 
#' Supports both binary and multi-armed forests
#' 
#' @param forest Causal forest object
#' @param X Data frame. Feature matrix
#' @param cov_name Character. Name of covariate to plot
#' @param label Character. Label for x-axis (default: cov_name)
#' @param n Numeric. Number of prediction points (default: 250)
#' @param midpoint_func Function. Function to compute midpoint (default: median)
#' @param method Character. Method for PDP computation (default: "centered")
#' @param se_type Character. Standard error type (default: "sample")
#' @param include_hline0 Logical. Whether to include horizontal line at 0 (default: FALSE)
#' @return List containing plot and data
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
