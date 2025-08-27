# accuracy of factual claim increases over rounds
plot_a <- d_fact_ave |> 
  ggplot(aes(x = round, y = accuracy)) +
  geom_violin() +
  geom_jitter(alpha = 0.1) +
  stat_summary(fun = mean, geom = "point", col = "red", size = 1) +
  stat_summary(fun.data = ~ mean_se(.x), geom = "errorbar", width = 0.2,  col = "red") +
  #geom_smooth(method = "lm",  se = TRUE, col = "black", aes(x = round_num)) +
  #geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = TRUE, col = "blue", aes(x = round_num)) +
  labs(x = "Round", y = "Factual Accuracy Rating") 


# correlation between accuracy and donation change
plot_b <- d_acc %>%
  ggplot(aes(x = accuracy, y = cents_to_amf_change)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_jitter(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(col = "red", se = FALSE) +
  scale_y_continuous(labels = scales::label_number(suffix = "c")) +
  labs(x = "Factual Accuracy Rating", y = "Donation Change") 

## plot
(plot_a + plot_b) +
  plot_annotation(
    tag_levels = "A",
  )

ggsave("output/figures/accuracy_plot.png",
       width = FIGURE_WIDTH, height = 4, dpi = FIGURE_DPI
)
