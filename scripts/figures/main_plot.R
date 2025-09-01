# main plot

# Define condition labels
cond_names <- c(
  control = "Control conversation", 
  static_treatment = "Static message", 
  conv_treatment = "Persuasive LLM conversation"
)

cond_names_short <- c(
  control = "Control", 
  static_treatment = "Static Msg", 
  conv_treatment = "LLM Conv."
)

# --- Create Individual Plots ---
# --- Plot A, change by condition ---
a <- marginaleffects::plot_predictions(
  estimatr::lm_robust(cents_to_amf_change ~ condition * cents_to_amf_pre, data = d),
  by = c("condition"),
  newdata = "balanced", 
) +
  aes(color = condition, fill = condition) +
  labs(
    x = NULL,
    color = "Condition",
    fill = "Condition",
    y = "Donation change"
  ) +
  scale_x_discrete(labels = cond_names_short) +
  scale_y_continuous(
    labels = scales::label_number(suffix = "c")
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  guides(color = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE)) +
  theme(
    #panel.grid.major.y = element_blank(),
    legend.position = "bottom"
    )

## Plot B, change by pre (binned)
nl_preds <- marginaleffects::plot_predictions(
  estimatr::lm_robust(cents_to_amf_change ~ condition * cents_to_amf_pre_cat, data = d),
  by = c("cents_to_amf_pre_cat", "condition"),
  newdata = "balanced",
  draw = FALSE
) |> 
  ggplot(
    aes(x = cents_to_amf_pre_cat, 
        y = estimate, ymin = conf.low, ymax = conf.high,
        colour = condition, fill = condition)
  ) +
  geom_pointrange(size = 0.25, position = position_dodge(width = 0.25), show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Pre-treatment donation to AMF",
    y = "Donation change"
  ) +
  scale_y_continuous(
    labels = scales::label_number(suffix = "c")
  ) +
  scale_x_discrete(labels = function(x) paste0(x, "c")) +
  ggsci::scale_color_locuszoom(labels = cond_names) +
  ggsci::scale_fill_locuszoom(labels = cond_names) +
  theme(
    panel.grid.major.x = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
    )

# get n counts and have histogram
n_counts <- d %>%
  count(cents_to_amf_pre_cat) %>%
  mutate(label = paste0("n = ", n))

histogram_plot <- ggplot(n_counts, aes(x = as.factor(cents_to_amf_pre_cat), y = n)) +
  geom_bar(stat = "identity", fill = "darkgray", alpha = 0.4) +
  theme_void() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())  +
  geom_text(
    data = n_counts, 
    aes(label = label), 
    y = 100,
    size = 2.5
  ) +
  theme(plot.margin = margin(0, 0, 0, 0))

b <- (histogram_plot / 
        (nl_preds + plot_layout(tag_level = "new"))
) + 
  plot_layout(heights = c(0.3, 1))




# --- Apply Consistent Scales After Creating Plots ---

color_scale <- ggsci::scale_color_locuszoom(labels = cond_names)
fill_scale <- ggsci::scale_fill_locuszoom(labels = cond_names)

a <- a + color_scale + fill_scale
b <- b + color_scale + fill_scale
l <- guide_area() # specify legend space

# --- Arrange Plots in Patchwork ---

# Define layout
design <- "
ABB
LLL
"


# Combine plots with shared legend
final_plot <- wrap_plots(a, b, l, design = design) + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect", heights = c(1, 0.1)) &
  theme(
    panel.grid = element_blank(),
    plot.tag.position = c(0.0, 0.95),  # top-left corner
    plot.tag = element_text(size = 10, hjust = 0, vjust = 0, face = "bold")
  )

final_plot

shared_theme <- theme(
  plot.title = element_text(size = 10),
  axis.title = element_text(size = 8.5),
  axis.text  = element_text(size = 7),
  plot.tag.position = c(0.0, 0.95),  # top-left corner
  plot.tag = element_text(size = 10, hjust = 0, vjust = 0, face = "bold"),
  panel.grid = element_blank()
)

# Save and Display
ggsave(
  "output/figures/main_plot.png",
   final_plot, width = FIGURE_WIDTH, height = 3, units = "in", dpi = FIGURE_DPI
)

