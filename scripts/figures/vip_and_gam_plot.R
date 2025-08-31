## causal forest and GAM plot. 
## 

# do one gam plot to extract legend
plot_w_legend <- run_gam_simple(d, "age", add_hist = FALSE, include_legend = TRUE)$pred_plot +
  theme(legend.position = "bottom") 
legend <- cowplot::get_legend(plot_w_legend)

gam_plots <- lapply(gams, function(g) g$pred_plot)
all_plots <- c(list(importance_plot), gam_plots[c(1, 3, 4)], list(legend))

layout <- "
ABB
ACC
ADD
EEE
"

wrap_plots(all_plots, design = layout, heights = c(1, 1, 1, 0.1)) +
  plot_annotation(tag_levels = list(c("A", "B", "C", "D", " "))) &
  theme(
    plot.tag.position = c(0.0, 0.975),  # top-left corner
    plot.tag = element_text(size = 10, hjust = .5, vjust = 0, face = "bold")
  )
ggsave("output/figures/htes_cf.png",
       width = FIGURE_WIDTH, height = 6, dpi = FIGURE_DPI
)
