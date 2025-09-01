# =============================================================================
# GAM (GENERALIZED ADDITIVE MODEL) FUNCTIONS
# =============================================================================
# Functions for fitting and visualizing generalized additive models
# =============================================================================

#' Run simple GAM between moderator and outcome variable
#' 
#' Creates smooth plots with confidence intervals and optional histogram
#' 
#' @param d Data frame. Analysis dataset
#' @param cov Character. Name of covariate variable
#' @param dv Character. Name of dependent variable (default: "cents_to_amf_change")
#' @param include_data Logical. Whether to include raw data points (default: TRUE)
#' @param add_hist Logical. Whether to add marginal histogram (default: TRUE)
#' @param x_label Character. X-axis label (default: cov)
#' @param y_label Character. Y-axis label (default: "Donation change")
#' @param include_legend Logical. Whether to include legend (default: TRUE)
#' @return List containing model, summary, predictions, and plot
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
  
  gam_model <- mgcv::gam(form, data = d, method = "REML")
  
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
    p <- ggExtra::ggMarginal(
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
