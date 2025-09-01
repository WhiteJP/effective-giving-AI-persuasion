# =============================================================================
# STATISTICAL HELPER FUNCTIONS
# =============================================================================
# Utility functions for statistical analysis and multiple testing corrections
# =============================================================================

#' Get q-values for family of hypothesis tests
#' 
#' Replaces p-values with q-values for specified terms
#' 
#' @param mod Model object. Fitted model with p-values
#' @param terms_to_q_swap Character vector. Names of terms to apply q-value correction
#' @return Numeric vector. P-values with q-values substituted for specified terms
get_qvals <- function(mod, terms_to_q_swap) {
  nms <- terms_to_q_swap
  ps <- mod$p.value
  qs <- set_names(qvalue::qvalue(ps[nms])$qvalues, nms)
  ps[nms] <- qs
  ps
}
