# charity descriptives plot

## charites plot?
top10 <- d |> 
  drop_na(charity_name_final) |> 
  count(charity_name_final) |> 
  arrange(desc(n)) |> 
  head(n = 10) |> 
  pull(charity_name_final)

# 1) Compute overall counts and total mentions
overall_counts <- d %>%
  drop_na(charity_name_final) %>%
  count(charity_name_final, sort = TRUE)

total_mentions <- sum(overall_counts$n)

# 2) Keep top 10
top_df <- overall_counts %>%
  slice_head(n = 10) %>%                      # top 10 by n
  mutate(
    prop  = n / total_mentions,               # proportion of all mentions
    short = dplyr::recode(charity_name_final, !!!short_names)
  )

charity_tab <-
  top_df %>%
  mutate(
    charity = short,
    prop = scales::percent(prop, accuracy = 0.1),
  ) |> 
  select(charity, prop)

charity_tab_gt <-
  charity_tab |> 
  gt() |> 
  tab_header(
    title = "Top 10 Favorite Charities"
  ) |> 
  cols_label(
    charity = "Charity",
    prop = "Proportion of Sample"
  ) |> 
  fmt_percent(
    columns = vars(prop),
    decimals = 1
  ) |> 
  tab_options(
    table.font.size = px(10),
    data_row.padding = px(2.5)
  )

# 3) Plot percentages
charity_plot <- ggplot(top_df, aes(x = fct_reorder(short, prop), y = prop)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(
    labels    = scales::percent_format(accuracy = 0.1),
    expand    = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x     = "Charity",
    y     = "Proportion"
  ) +
  theme_bw()
  
## population plot
pop_plot <- pop2_counts |> 
  filter(count >= 20) |> 
  filter(population_area != "adults") |> 
  ggplot(
    aes(x = reorder(population_area, count), y = count, fill = amf_blue)
  ) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Population served", 
    y = "Count",
    title = "Population served by chosen favorite charities"
    ) +
  scale_x_discrete(labels = clean_names) + 
  theme_bw()  +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  scale_fill_identity()

  
## subject plot
subject_plot <- subj1_counts |> 
  ggplot(
    aes(x = reorder(subject_area, count), y = count, 
        fill = amf_blue
        # fill = ifelse(
        #   subject_area == "health",
        #   amf_red,
        #   amf_blue
        #)
      )
    ) +
  geom_col() +
  scale_x_discrete(labels = clean_names) + 
  coord_flip() +
  labs(
    x = "Subject area", 
    y = "Count",
    title = "Cause area of chosen favorite charities") +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  scale_fill_identity()

  
## where plot (could probably inset this on3
where_plot <- d_where |> 
  ggplot(
    aes(x = location_cat3,
        fill = amf_blue
        # fill = ifelse(
        #    location_cat3 == "International" & !is.na(location_cat3),
        #    amf_red,
        #    amf_blue
        #  )
        ) 
    ) +
  geom_bar(aes(y = after_stat(count/sum(count)))) +
  #geom_bar() + #for count
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(title = "Where do the charities operate?",
       x = NULL,
       y = NULL) +
  theme_bw() +
  theme(
    title = element_text(size = 10),
    panel.grid = element_blank(),
    axis.title.x = element_blank()
    ) +
  scale_fill_identity()

  

## size plot?
d_size <- d |> 
  select(ResponseId, condition, starts_with("cents_to_amf"), ein, revenue)

size_plot <- ggplot(d_size, aes(x = revenue)) +
  stat_bin(
    bins = 15,
    aes(
     #fill = ifelse(after_stat(x) > 5e7 & after_stat(x) < 6.5e7, amf_red, amf_blue)
     fill = amf_blue
    ),
    geom = "bar",
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  scale_x_log10(
    breaks = scales::breaks_log(n = 7),
    labels = scales::label_number(
      scale_cut = scales::cut_short_scale(),
      prefix = "$",
      accuracy = 1
    )
  ) +
  labs(
    title = "Charity size",
    x = "Charity 2024 Revenue (log10)",
    y = "Count"
  )

### NOW LETS PUT IT ALL TOGETHER

p1 <- subject_plot +
  #inset_element(charity_plot, .1, .1, 1, .9)
  inset_element(charity_tab_gt, .35, .20, .975, .75)

p2 <- pop_plot +
  inset_element(where_plot, .23, 0.025, .975, .55)



# ── 1) Shared theme ─────────────────────────────────────────────────────────────
shared_theme <- theme(
  plot.title = element_text(size = 10),
  axis.title = element_text(size = 8.5),
  axis.text  = element_text(size = 7),
  plot.tag.position = c(0.0, 0.95),  # top-left corner
  plot.tag = element_text(size = 10, hjust = 0, vjust = 0, face = "bold"),
  panel.grid = element_blank()
)

# ── 2) Read & style the JPEG logo ──────────────────────────────────────────────
img <- jpeg::readJPEG("logo_amf.jpg")
img_grob <- rasterGrob(
  img,
  x      = unit(0.20, "npc"),    # 2% in from left
  y      = unit(0.90, "npc"),
  just = c("left", "top")  # align top-left
  #width  = unit(1, "npc"),        # fill its cell
  #height = unit(1, "npc"),
  #just   = c("left", "top")       # align top-left
)

# colours from logo
amf_blue <- "#0193CF"
amf_red <- "#CB1031"


# ── 3) Build your rich HTML text ──────────────────────────────────────────────
info_html <- paste0(
  "<b>Against Malaria Foundation (AMF)</b><br/>",
  "<i>“We fund and provide long-lasting insecticidal<br/>",
  "nets to protect those at risk from malaria.”</i><br/>",
  "<b>Cause area</b>: Health<br/>",
  "<b>Population</b>: People with diseases and illnesses<br/>",
  "<b>Operation</b>: Internationally<br/>",
  "<b>Size (2024 Revenue)</b>: $62.3M"
)

info_txt <- paste0(
  "**Against Malaria Foundation (AMF)**\n\n",
  "_“We fund and provide long-lasting insecticidal nets  \n",
  "to protect those at risk from malaria.”_\n\n",
  "**Cause area**: Health  \n",
  "**Population**: People with diseases and illnesses  \n",
  "**Operation**: Internationally  \n",
  "**Size (2024 Revenue)**: $62.3M"
)

text_grob <- richtext_grob(
  info_html,
  x      = unit(0.05, "npc"), 
  y      = unit(0.5, "npc"),
  hjust  = 0, 
  vjust  = 0.5,
  halign = 0,  # left-align text
  align_widths = TRUE,  # align text width
  gp     = gpar(fontsize = 8, lineheight = 1.1)  # smaller text
)

# ── 4) Arrange image + text in 1:4 ratio with null units ───────────────────────
info_inner <- arrangeGrob(
  img_grob,
  nullGrob(), # spacer column
  text_grob,
  ncol   = 3,
  widths = unit(c(1, 0.05, 4), "null"),  # 0.05 = small space
  heights = unit(1, "null")
)

# ── 5) Draw a white background + border around that two-column block ──────────
#info_bordered <- grobTree(
#  rectGrob(gp = gpar(fill = "white", col = "black", lwd = 0.8)),
#  info_inner
#)

# ── 6) Turn into one patchwork element ────────────────────────────────────────
info_elem <- wrap_elements(full = info_inner)


# plot altgether
plots <- list(
  A = p1,
  B = p2,
  C = free(size_plot),
  D = free(info_elem)
)

final_plot <- wrap_plots(
  plots,
  design  = "
    AA
    BB
    CD
  ",
  heights = c(2.5, 2.5, 1),
  widths  = c(1, 4) # this only works on the aligned part of the plots, not the whole space!
) +
  plot_annotation(tag_levels = "A") & 
  shared_theme

final_plot

# ── 9) Save ───────────────────────────────────────────────────────────────────
ggsave("output/figures/charity_descriptive_plot_no_adults.png",
       final_plot,
       width  = 6.25, # max width according to NHB is 7.1
       height = 7.1,
       dpi    = FIGURE_DPI,
       create.dir = TRUE
)




