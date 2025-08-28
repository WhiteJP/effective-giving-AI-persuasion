




# PDPs from causal forest ------------------------------------------------
# can take either a binary or multi armed forest

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
