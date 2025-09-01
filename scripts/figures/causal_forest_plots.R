## Now do unadjusted heterogeneity plots and partial dependence plots for all variables 
uhps <- plot_all_uhps(
  conv_ites, static_ites, d, 
  importance_tbl$variable_dirty, importance_tbl$variable
)
uhps
ggsave(
  "output/figures/uhps.png",
  uhps,
  width = 8.25, height = 9.3, dpi = FIGURE_DPI
)

## now get PDPs
pdps <- plot_all_pdps(
  ma_cf, as.matrix(d[, covars_vec]), 
  importance_tbl$variable_dirty, labels_map
)
pdps
ggsave(
  "output/figures/pdps.png",
  pdps,
  width = 8.25, height = 9.3, dpi = FIGURE_DPI
)
