## binned categorical variable plots


subj1_cond$mod |> 
  broom::tidy() |> 
  dplyr::filter(grepl("^conditionconv_treatment:subj", term)) |> 
  mutate(
    cause_area = gsub("conditionconv_treatment:subj_", "", term),
    cause_area = clean_names(cause_area)
  ) |> 
  ggplot(aes(x = estimate, y = cause_area)) +
  geom_point() +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high)) +
  labs(x = "Estimated CATE", y = "Cause Area") +
  theme_minimal()


## Profile TEs
# no sig differences between any groups
#subj1_cond$profile_tes_diff 
#pop2_cond$profile_tes_diff

p1 <- subj1_cond$profile_tes |>  
  filter(contrast == "mean(conv_treatment) - mean(control)") |> 
  mutate(
    cause_area = clean_names(gsub("subj_", "",  profile_var)),
    cause_area = fct_reorder(cause_area, estimate)
  ) |> 
  ggplot(aes(x = estimate, y = cause_area, color = contrast)) +
  # forest plot
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~contrast, ncol = 1,
             labeller = labeller(contrast = comparison_names_cate)) +
  scale_color_manual(
    values = contrast_colors,
    labels = comparison_names_short
  ) +
  labs(
    title = "Treatment Effect Heterogeneity by Charity Cause Area",
    x = "Conditional Average Treatment Effect (95% CI)", 
    y = NULL) +
  theme(legend.position = "bottom")




p2 <- pop2_cond$profile_tes |>  
  filter(contrast == "mean(conv_treatment) - mean(control)") |> 
  mutate(
    population_served = clean_names(gsub("pop_", "",  profile_var)),
    population_served = fct_reorder(population_served, estimate)
  ) |> 
  ggplot(aes(x = estimate, y = population_served, color = contrast)) +
  # forest plot
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~contrast, ncol = 1,
             labeller = labeller(contrast = comparison_names_cate)) +
  scale_color_manual(
    values = contrast_colors,
    labels = comparison_names_short
  ) +
  labs(
    title = "Treatment Effect Heterogeneity by Charity Population Served",
    x = "Conditional Average Treatment Effect (95% CI)", 
    y = NULL) +
  theme(legend.position = "bottom")



(p1 / p2) +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect", heights = c(1, 1.5)) & #axis_titles = "collect",
  scale_x_continuous(
    label = scales::label_number(suffix = "c"),
    limits = c(-18, 45),
    breaks = seq(-20, 40, by = 10)
  ) &
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11),
    #axis.title.x = element_blank(),
    panel.grid = element_blank(),
    #plot.tag.position = c(0.0, 1),  # top-left corner
    plot.tag = element_text(size = 10, vjust = 2, face = "bold")
  )

ggsave(
  "output/figures/binned_het_cates.png",
  width = FIGURE_WIDTH, 
  height = 6
)
