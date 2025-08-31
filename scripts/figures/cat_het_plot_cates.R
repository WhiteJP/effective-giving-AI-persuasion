#categorical heterogeneity plot


# ───────────────────────────────────────────────────────────────────────────────
# POSITION PARAMETERS FOR SIG DIFFERENCES (fractions of panel CI‐range)
initial_offset_frac  <- 0.05  # how far beyond the max CI the first bracket stem sits
bracket_spacing_frac <- 0.04  # spacing between successive bracket stems
leg_length_frac      <- 0.02  # horizontal “leg” tick length
symbol_offset_frac   <- 0.5  # distance from the bracket stem to the star
symbol_size <- 1.5
# ──────────────────────────────────────────────────────────────────────────────


## FOR CHARITY HETEROGENEITY

# get treatment effect for each charity
df_plot <- char_het$comparisons |> 
  filter(contrast == "conv_treatment - control") |>
  mutate(
    charity_fct = fct_reorder(charity_fct, estimate, \(x) median(abs(x))),
    #charity_fct = fct_rev(charity_fct),
    contrast = factor(contrast, levels = unique(contrast))
  )

# get (adjusted) p values for all the comparisons
# none, are significange, so this is empty
manual_p <- char_cate_comparisons %>%
  tidyr::extract(
    term,
    into = c("group1","group2"),
    regex = "^\\((.+)\\)\\s*-\\s*\\((.+)\\)$"
  ) %>%
  mutate(
    contrast = "conv_treatment - control",
    code1 = as.integer(factor(group1, levels = levels(df_plot$charity_fct))),
    code2 = as.integer(factor(group2, levels = levels(df_plot$charity_fct))),
    group1 = factor(levels(df_plot$charity_fct)[pmax(code1, code2)],
                    levels = levels(df_plot$charity_fct)),
    group2 = factor(levels(df_plot$charity_fct)[pmin(code1, code2)],
                    levels = levels(df_plot$charity_fct)) 
  ) |> 
  select(-code1, -code2) |> 
  mutate(
    p_str  = as.character(p.value),
    p_num  = readr::parse_number(p_str),
    signif = case_when(
      str_detect(p_str, "^<") & p_num <= 0.001 ~ "***",
      p_num < 0.001                          ~ "***",
      p_num < 0.01                           ~ "**",
      p_num < 0.05                           ~ "*",
      p_num < 0.1                            ~ "+",
      TRUE                                   ~ NA_character_
    ),
    shape = case_when(
      p_num < 0.001        ~ "triangle",
      p_num < 0.01         ~ "square",
      p_num < 0.05         ~ "circle",
      p_num < 0.10         ~ "diamond",
      TRUE             ~ NA_character_
    )
  ) %>%
  filter(!is.na(signif)) %>%
  mutate(
    contrast = factor(contrast, levels = levels(df_plot$contrast)),
    y1       = as.numeric(factor(group1, levels = levels(df_plot$charity_fct))),
    y2       = as.numeric(factor(group2, levels = levels(df_plot$charity_fct))),
    y        = (y1 + y2) / 2
  ) %>%
  group_by(contrast) |> 
  arrange(desc(group1), desc(group2)) %>%
  mutate(idx = row_number()) %>%
  ungroup()

# 4. Compute per‐panel CI stats & scaled offsets
panel_info <- df_plot %>%
  group_by(contrast) %>%
  summarise(
    max_hi = max(conf.high),
    min_lo = min(conf.low)
  ) %>%
  mutate(
    range           = max_hi - min_lo,
    offset          = initial_offset_frac  * range,
    spacing         = bracket_spacing_frac * range,
    leg_length      = leg_length_frac      * range,
    symbol_offset   = symbol_offset_frac   * range
  )

# 5. Join & turn those fractions into absolute positions
manual_p <- manual_p %>%
  left_join(panel_info, by = "contrast") %>%
  mutate(
    x_start   = max_hi + offset,
    x         = x_start + (idx - 1) * spacing,
    x_leg_end = x - leg_length,
    labelx    = x + symbol_offset
  )

# 6. Plot everything
p1 <- ggplot(df_plot, aes(x = estimate, y = charity_fct, color = contrast)) +
  # forest plot
  geom_point(position = position_dodge(0.4)) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(0.4)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~contrast, ncol = 1,
             labeller = labeller(contrast = comparison_names_cate)) +
  scale_color_manual(
    values = contrast_colors,
    labels = comparison_names_short
  ) +
  scale_y_discrete(labels = short_names) +
  labs(x = "Conditional Average Treatment Effect (95% CI)", y = NULL) +
  theme(legend.position = "bottom") +
  # bracket stems
  geom_segment(data = manual_p,
               aes(x = x, xend = x, y = y1, yend = y2),
               inherit.aes = FALSE) +
  # inward legs
  geom_segment(data = manual_p,
               aes(x = x, xend = x_leg_end, y = y1, yend = y1),
               inherit.aes = FALSE) +
  geom_segment(data = manual_p,
               aes(x = x, xend = x_leg_end, y = y2, yend = y2),
               inherit.aes = FALSE) +
  # stars
  geom_text(data = manual_p,
            aes(x = x, y = y, label = signif),
            inherit.aes = FALSE,
            hjust = 0.5, vjust = 0.25, size = 4)
  #shapes
  # geom_point(
  #   data      = manual_p,
  #   aes(x = x, y = y, shape = shape),
  #   inherit.aes = FALSE,
  #   size      = symbol_size
  # ) +
  # scale_shape_manual(
  #   values = c(
  #     circle   = 16,  # filled circle
  #     square   = 15,  # filled square
  #     triangle = 17,   # filled triangle-up
  #     diamond  = 18    # filled diamond
  #   ),
  #   na.translate = FALSE
  # )


## LOCATION HETEROGENEITY

# 2. Prepare forest‐plot data
df_plot <- loc_het$comparisons %>%
  filter(contrast == "conv_treatment - control") %>%
  mutate(
    location = fct_rev(location_cat3),
    contrast = factor(contrast, levels = unique(contrast))
  )

# 3. Parse & filter only within‐contrast significant tests
manual_p <- loc_cate_comparisons %>%
  tidyr::extract(
    hypothesis,
    into = c("group1", "group2"),
    regex = "^\\(([^)]*)\\)\\s*-\\s*\\(([^)]*)\\)$"
  ) %>%
  mutate(
    p_str  = as.character(p.value),
    p_num  = readr::parse_number(p_str),
    signif = case_when(
      str_detect(p_str, "^<") & p_num <= 0.001 ~ "***",
      p_num < 0.001                          ~ "***",
      p_num < 0.01                           ~ "**",
      p_num < 0.05                           ~ "*",
      p_num < 0.1                            ~ "+",
      TRUE                                   ~ NA_character_
    ),
    shape = case_when(
      p_num < 0.001        ~ "triangle",
      p_num < 0.01         ~ "square",
      p_num < 0.05         ~ "circle",
      p_num < 0.10         ~ "diamond",
      TRUE             ~ NA_character_
    ),
    contrast = "conv_treatment - control"
  ) %>%
  filter(!is.na(signif)) %>%
  mutate(
    contrast = factor(contrast, levels = levels(df_plot$contrast)),
    y1       = as.numeric(factor(group1, levels = levels(df_plot$location))),
    y2       = as.numeric(factor(group2, levels = levels(df_plot$location))),
    y        = (y1 + y2) / 2
  ) %>%
  group_by(contrast) %>%
  mutate(idx = row_number()) %>%
  ungroup()

# 4. Compute per‐panel CI stats & scaled offsets
panel_info <- df_plot %>%
  group_by(contrast) %>%
  summarise(
    max_hi = max(conf.high),
    min_lo = min(conf.low)
  ) %>%
  mutate(
    range           = max_hi - min_lo,
    offset          = initial_offset_frac  * range,
    spacing         = bracket_spacing_frac * range,
    leg_length      = leg_length_frac      * range,
    symbol_offset   = symbol_offset_frac   * range
  )

# 5. Join & turn those fractions into absolute positions
manual_p <- manual_p %>%
  left_join(panel_info, by = "contrast") %>%
  mutate(
    x_start   = max_hi + offset,
    x         = x_start + (idx - 1) * spacing,
    x_leg_end = x - leg_length,
    labelx    = x + symbol_offset
  )

# 6. Plot everything
p2 <- ggplot(df_plot, aes(x = estimate, y = location, color = contrast)) +
  # forest plot
  geom_point(position = position_dodge(0.4)) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(0.4)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~contrast, ncol = 1,
             labeller = labeller(contrast = comparison_names_cate)) +
  scale_color_manual(
    values = contrast_colors,
    labels = comparison_names_short
  ) +
  labs(x = "Conditional Average Treatment Effect (95% CI)", y = NULL) +
  theme(legend.position = "bottom") +
  # bracket stems
  geom_segment(data = manual_p,
               aes(x = x, xend = x, y = y1, yend = y2),
               inherit.aes = FALSE) +
  # inward legs
  geom_segment(data = manual_p,
               aes(x = x, xend = x_leg_end, y = y1, yend = y1),
               inherit.aes = FALSE) +
  geom_segment(data = manual_p,
               aes(x = x, xend = x_leg_end, y = y2, yend = y2),
               inherit.aes = FALSE) +
  # stars
  geom_text(data = manual_p,
            aes(x = x, y = y, label = signif),
            inherit.aes = FALSE,
            hjust = -.35, vjust = 0.5, size = 4)

  #shapes
  # geom_point(
  #   data      = manual_p,
  #   aes(x = x, y = y, shape = shape),
  #   inherit.aes = FALSE,
  #   size      = symbol_size
  # ) +
  # scale_shape_manual(
  #   values = c(
  #     circle   = 16,  # filled circle
  #     square   = 15,  # filled square
  #     triangle = 17   # filled triangle-up
  #   ),
  #   na.translate = FALSE
  # )

# PUT TOGETHER

(p1 + p2) +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect", axes = "collect", widths = c(3/3, 1)) &
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") &
  scale_x_continuous(label = scales::label_number(suffix = "c")) &
  theme(
    legend.position = "none",
    #axis.title.x = element_blank(),
    panel.grid = element_blank(),
    #plot.tag.position = c(0.0, 1),  # top-left corner
    plot.tag = element_text(size = 10, vjust = 2, face = "bold")
  )

ggsave(
  "output/figures/cat_het_cates.png",
  width = FIGURE_WIDTH, 
  height = 3.5
)
