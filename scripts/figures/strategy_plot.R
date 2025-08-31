# strategy plot


# Plot distributions -------------------------------------------------------

# 1. Pull out and rank your strategy coefficients
coef_df <- broom::tidy(mod_lm_donation) |> 
  filter(term %in% names(STRATEGY_DESCRIPTIONS)) |> 
  mutate(
    strategy = str_remove(term, "^strategy"),
    strategy = factor(strategy, levels = strategy[order(-estimate)]),
    sig = case_when(
      p.value < .10 & estimate >  0 ~ "pos",
      p.value < .10 & estimate <  0 ~ "neg",
      TRUE                           ~ "ns"
    )
  )

ranked_strats <- levels(coef_df$strategy)

# 2. Build HTML labels using centralized strategy descriptions
label_map <- imap_chr(SHORT_STRAT_DESCRIPTIONS, ~ {
  clean_name <- str_to_sentence(str_replace_all(.y, "(?<=[a-z])(?=[A-Z])", " "))
  paste0(
    "<b>", clean_name, "</b><br>",
    "<span style='font-size:8pt;'>", .x, "</span>"
  )
})

# I could just do it as a boxplot with dots if these are causing too much trouble 
# probably just need to separate out the point interval and the slab and it should all work: https://github.com/mjskay/ggdist/issues/93 
# 4. Plot, ordering by your OLS rank
p1  <- d_strategy_agg_long %>%
  mutate(
    strategy = factor(strategy, levels = ranked_strats)
  ) %>%
  ggplot(aes(x = rating, y = strategy)) +
  stat_slab(
    density = "histogram",
    breaks = seq(-0.25, 3.25, by = 0.5),
    fill = amf_blue,
    height = 0.5, 
    justification = 0.5
  ) +
  stat_pointinterval(
    point_interval = "mean_qi",
   .width  = 0.5, #IQR
   interval_size_domain = c(0, 20),
   position = position_nudge(y = -.2),
   interval_color = NA
  ) +
  scale_y_discrete(
    labels = label_map,
    #expand = expansion(add = c(0.5, 0))
  ) +
  scale_x_continuous(
    breaks = seq(0, 3, 1),
    #expand = expansion(add = c(0.25, 0.25)),
    labels = c("Not\nused", "Used\nminimally", "Used\nmoderately", "Used\nextensively"),
  ) +
  labs(
    x = "Strategy Rating",
    y = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.y      = element_markdown(lineheight = 0.9),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# try p1 as a boxplot?

# ──────────────────────────────────────────────────────────────────────────────
# 3. Panel B: horizontal bar plot of coefficients ± SE
p2 <- ggplot(coef_df, aes(x = estimate, y = strategy, fill = sig)) +
  geom_col() +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error),
                 height = 0) +
  scale_y_discrete(labels = NULL,
                   expand = expansion(add = c(.6, .6))
                   ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(
    values = c(
      pos = amf_blue,     # significant & positive
      neg = amf_red,      # significant & negative
      ns  = "grey80"      # non-significant
    ),
    guide = "none"
  ) +
  labs(x = "OLS Coefficient", y = NULL) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.y      = element_blank(),
    #axis.ticks.y     = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

# Combine -----------------------------------------------------------------

shared_theme <- theme(
  plot.title = element_text(size = 10),
  axis.title = element_text(size = 8.5),
  axis.text  = element_text(size = 7),
  plot.tag.position = c(0.0, 1),  # top-left corner
  plot.tag = element_text(size = 10, hjust = 1, vjust = 0, face = "bold"),
  panel.grid = element_blank()
)

combined <- p1 + p2 +
  plot_layout(ncol = 2, widths = c(1.6, 1)) +
  plot_annotation(tag_levels = "A") & 
  shared_theme

# Print it
combined

ggsave(
  "output/figures/strategy_plot.png",
  combined,
  width = FIGURE_WIDTH,
  height = 7,
  dpi = FIGURE_DPI
)
