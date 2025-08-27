

# Add long label to your dataframe
m_dat_long <- m_dat %>%
  pivot_longer(AwarenessOfNeed:Efficacy, names_to = "variable", values_to = "value") |> 
  filter(variable != "DoesntGive") %>%
  mutate(
    label = fct_rev(factor(variable)),
    label_formatted = fct_rev(factor(glue(
      "**{variable}**\n<span style='font-weight:normal; font-size:9pt'>{MOTIVATION_DESCRIPTIONS[variable]}</span>"
    ), levels = unique(variable)))
  )

# 1.  Build HTML labels with <b> and <br>
label_map <- imap_chr(MOTIVATION_DESCRIPTIONS, ~ {
  clean_name <- str_to_sentence(str_replace_all(.y, "(?<=[a-z])(?=[A-Z])", " "))
  paste0(
    "<b>", clean_name, "</b><br>",
    "<span style='font-size:8pt;'>", .x, "</span>"
  )
})
# Ensure names(label_map) match your factor levels

# 3. Plot
m_dat_long %>%
  filter(variable != "DoesntGive") %>%
  mutate(variable = factor(variable, levels = names(label_map))) %>%
  ggplot(aes(x = value, y = variable)) +
  stat_histinterval(
    point_interval = "mean_qi",
    breaks        = seq(0.75, 5.25, by = 0.5),
    slab_color    = NA,
    fill          = amf_blue,
    alpha         = 0.9,
    linewidth     = 2,
    size          = 3,
    .width        = 0.5,
    interval_color = NA # REMOVE interavl
  ) +
  scale_y_discrete(
    labels = label_map,
    expand = expansion(add = c(0.5, 0))
  ) +
  scale_x_continuous(
    breaks = seq(1, 5, 1),
    expand = expansion(add = c(0.25, 0.25))
  ) +
  labs(
    x     = "Rating",
    y     = NULL#,
    #title = "Distribution of Motivation Ratings (Histogram Slabs)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.y      = element_markdown(lineheight = 0.9),
    plot.title       = element_text(face = "bold", size = 13, margin = margin(b = 4)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "output/figures/motivation-descriptives.png",
  width    = 7,
  height   = 3.5,
  dpi      = FIGURE_DPI
)
