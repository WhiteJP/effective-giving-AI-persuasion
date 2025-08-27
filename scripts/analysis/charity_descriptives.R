
d_candid <- read_csv("data/guidestar_data.csv") |> 
  janitor::clean_names() 

pcs_population <- extract_unique_pcs_labels(d_candid$population_served)
pcs_subjects <- extract_unique_pcs_labels(d_candid$subject_area)
pcs_location <- extract_unique_pcs_labels(d_candid$where_we_work)

pop_hierarchy <- parse_hierarchy("data/pcs_pop_hierarchy.html", leaves_as_vec = FALSE)
subj_hierarchy <- parse_hierarchy("data/pcs_subj_hierarchy.html", leaves_as_vec = FALSE)

pop_hierarchy_df <- hierarchy_to_df(pop_hierarchy)
subj_hierarchy_df <- hierarchy_to_df(subj_hierarchy)


find_missing_with_match(pcs_population, pop_hierarchy_df) 
find_missing_with_match(pcs_subjects, subj_hierarchy_df) 

## replace them manually after looking at hierarchy for closest match
pop_replacements <- c(
  "American Indians" = "American Indians/Native Americans",
  "At-risk youth" = "Out-of-home youth",
  "Dropouts" = "Out-of-school youth",
  "Ex-offenders" = "Formerly incarcerated people",
  "Extremely poor people" = "People living in extreme poverty",
  "LGBTQ people" = "LGBTQIA+ people",
  "Multiracial people" = "Multi-racial/Multi-ethnic people",
  "Offenders" = "Incarcerated people",
  "People of African descent" = "Black/African people",
  "People of Asian descent" = "Asian people",
  "People of Middle Eastern descent" = "Middle Eastern/North African people",
  "People with HIV/AIDS" = "People living with HIV or AIDS",
  "Seniors" = "Older adults",
  "Sexual identity" = "LGBTQIA+ people",
  "Substance abusers" = "People with substance use disorder",
  "Victims and oppressed people" = "Victims of violence or disasters"
)

subj_replacemnents <- c(
  "Abuse prevention"  = "Abuse prevention and services",
  "Ethnic and racial minority rights" = "Ethnic and racial group rights",
  "HIV/AIDS" = "HIV and AIDS",
  "Housing for the homeless" = "Housing for homeless people",
  "Human rights" = "International human rights",
  "Immigrant services" = "Immigrant and refugee services",
  "Individual liberties" = "International human rights",
  "Justice rights" = "Criminal justice system rights",
  "LGBTQ rights" = "LGBTQIA+ rights",
  "Nursing education" = "Nursing Education",
  "Public safety" = "Public safety and disaster management",
  "Right to life" = "Anti-abortion",
  "Senior assisted living" = "Assisted living for older adults",
  "Senior services" = "Services for older adults",
  "Social rights" = "International human rights",
  "Temporary accomodations" = "Temporary accommodations"
)

  
# add higher levels of categores
d_candid <- d_candid |> 
  mutate(
    population_served = replace_terms_named(population_served, pop_replacements),
    subject_area = replace_terms_named(subject_area, subj_replacemnents),
    pop_lvl1 = replace_with_level(population_served, pop_hierarchy_df, k = 1),
    pop_lvl2 = replace_with_level(population_served, pop_hierarchy_df, k = 2),
    subj_lvl1 = replace_with_level(subject_area, subj_hierarchy_df, k = 1),
    subj_lvl2 = replace_with_level(subject_area, subj_hierarchy_df, k = 2)
  )


pcs_subj1_mat <- make_pcs_matrix(d_candid$subj_lvl1, "subj") |> mutate(ein = d_candid$ein) 
pcs_subj2_mat <- make_pcs_matrix(d_candid$subj_lvl2, "subj") |> mutate(ein = d_candid$ein)
pcs_pop1_mat <- make_pcs_matrix(d_candid$pop_lvl1, "pop") |> mutate(ein = d_candid$ein)
pcs_pop2_mat <- make_pcs_matrix(d_candid$pop_lvl2, "pop") |> mutate(ein = d_candid$ein)

# subject 1 counts
d_subj1 <- d_all |> 
  select(ResponseId, condition, starts_with("cents_to_amf"), ein) |>
  filter(ResponseId %in% ps_final) |> 
  mutate(ein = format_ein(ein)) |> 
  left_join(pcs_subj1_mat, by = "ein") |> 
  set_unknown_on_na("subj", "subj_unknown_or_not_classified")

subj1_counts <- d_subj1 |> 
  summarise(across(starts_with("subj"), ~ sum(.x))) |> 
  pivot_longer(cols = everything(), names_to = "subject_area", values_to = "count") |> 
  mutate(subject_area = str_remove(subject_area, "^subj_")) |>
  arrange(desc(count))

#plot
subj1_counts |> 
  ggplot(aes(x = reorder(subject_area, count), y = count)) +
  geom_col() +
  coord_flip() +
  labs(x = "Subject area", y = "Count") +
  theme_minimal()

# population 1 counts
d_pop1 <- d_all |> 
  select(ResponseId, condition, starts_with("cents_to_amf"), ein) |>
  filter(ResponseId %in% ps_final) |> 
  mutate(ein = format_ein(ein)) |> 
  left_join(pcs_pop1_mat, by = "ein") |> 
  set_unknown_on_na("pop", "pop_unknown_or_not_classified")

pop1_counts <- d_pop1 |>
  summarise(across(starts_with("pop"), ~ sum(.x))) |> 
  pivot_longer(cols = everything(), names_to = "population_area", values_to = "count") |> 
  mutate(population_area = str_remove(population_area, "^pop_")) |>
  arrange(desc(count))

#plot
pop1_counts |> 
  ggplot(aes(x = reorder(population_area, count), y = count)) +
  geom_col() +
  coord_flip() +
  labs(x = "Population area", y = "Count") +
  theme_minimal()

# subject 2 counts
d_subj2 <- d_all |> 
  select(ResponseId, condition, starts_with("cents_to_amf"), ein) |>
  filter(ResponseId %in% ps_final) |> 
  mutate(ein = format_ein(ein)) |> 
  left_join(pcs_subj2_mat, by = "ein") |> 
  set_unknown_on_na("subj", "subj_unknown_or_not_classified")


subj2_counts <- d_subj2 |> 
  summarise(across(starts_with("subj"), ~ sum(.x))) |> 
  pivot_longer(cols = everything(), names_to = "subject_area", values_to = "count") |> 
  mutate(subject_area = str_remove(subject_area, "^subj_")) |>
  arrange(desc(count))

#plot
subj2_counts |> 
  ggplot(aes(x = reorder(subject_area, count), y = count)) +
  geom_col() +
  coord_flip() +
  labs(x = "Subject area", y = "Count") +
  theme_minimal()

# population 2 counts
d_pop2 <- d_all |> 
  select(ResponseId, condition, starts_with("cents_to_amf"), ein) |>
  filter(ResponseId %in% ps_final) |> 
  mutate(ein = format_ein(ein)) |> 
  left_join(pcs_pop2_mat, by = "ein") |> 
  set_unknown_on_na("pop", "pop_unknown_or_not_classified")

pop2_counts <- d_pop2 |>
  summarise(across(starts_with("pop"), ~ sum(.x))) |> 
  pivot_longer(cols = everything(), names_to = "population_area", values_to = "count") |> 
  mutate(population_area = str_remove(population_area, "^pop_")) |>
  arrange(desc(count))

#plot
pop2_counts |> 
  ggplot(aes(x = reorder(population_area, count), y = count)) +
  geom_col() +
  coord_flip() +
  labs(x = "Population area", y = "Count") +
  theme_minimal()


## NOW lets look at the location data
d_where_orig <- read_csv("data/location_categories.csv") |> 
  janitor::clean_names() |> 
  mutate(
    location_cat = str_to_lower(category)
  )

## summarizi
d_where_mod_agreement <- d_where_orig %>%
  group_by(ein) %>%
  summarise(
    # total non-NA location_cat responses
    n_responses = sum(!is.na(location_cat)),
    n_unique = length(unique(location_cat)),
    # largest count of any single location_cat value
    max_same    = max(table(location_cat)),
    all_valid = n_responses == 3,
    perf_agreement = max_same == 3,
    all_agreement = n_responses == max_same,
    majority_cat = names(which.max(table(location_cat))),
    minority_cat = names(which.min(table(location_cat))),
    .groups = "drop"
  )

## now lets get proportions of all of these, first for the whole dataset
d_where_mod_agreement |> 
  summarise(
    n_total = n(),
    n_valid = sum(all_valid),
    n_perf_agreement = sum(perf_agreement),
    n_all_agreement = sum(all_agreement),
    n_complete_disagreement = sum(n_unique == 3),
    prop_valid = n_valid / n_total,
    prop_perf_agreement = n_perf_agreement / n_total,
    prop_all_agreement = n_all_agreement / n_total
  )

# now by majority answer
d_where_mod_agreement |> 
  group_by(majority_cat) %>%
  summarise(
    n_total = n(),
    n_valid = sum(all_valid),
    n_perf_agreement = sum(perf_agreement),
    n_all_agreement = sum(all_agreement),
    n_complete_disagreement = sum(n_unique == 3),
    prop_valid = n_valid / n_total,
    prop_perf_agreement = n_perf_agreement / n_total,
    prop_all_agreement = n_all_agreement / n_total
  )

# now lets look at when there is disagreement, what is the majority and minority answers, what proportion of the total category
d_where_mod_agreement |> 
  count(majority_cat, minority_cat)

## lets look at results with cat4, and cat3 (collapsing local and state)
d_where_agg <- d_where_orig |> 
  group_by(ein) |>
  summarise(
    location_cat4 = names(which.max(table(location_cat))),
    location_cat3 = case_when(
      location_cat4 %in% c("local", "state") ~ "local/state",
      TRUE ~ location_cat4
    ),
  )

d_where <- d_all |> 
  select(ResponseId, condition, starts_with("cents_to_amf"), ein) |>
  filter(ResponseId %in% ps_final) |> 
  mutate(ein = format_ein(ein)) |> 
  left_join(d_where_agg, by = "ein") |> 
  mutate(
    location_cat4 = replace_na(location_cat4, "Unknown or not classified"),
    location_cat3 = replace_na(location_cat3, "Unknown or not classified"),
    location_cat4 = factor(
      location_cat4, 
      levels = c("international", "national", "state", "local", "Unknown or not classified"),
      labels = c("International", "National", "State", "Local", "Unknown or not classified")
    ),
    location_cat3 = factor(
      location_cat3, 
      levels = c("international", "national", "local/state", "Unknown or not classified"),
      labels = c("International", "National", "Local/State", "NA")
    ),
    is_international = if_else(location_cat3 == "International", 1L, 0L),
  )

# plot cents_to_amf_change by location cats
d_where |> 
  ggplot(aes(x = location_cat4, y = cents_to_amf_change, col = condition)) +
  #geom_jitter(position = position_dodge(width = 0.5), alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.5))

d_where |> 
  ggplot(aes(x = location_cat3, y = cents_to_amf_change, col = condition)) +
  #geom_jitter(position = position_dodge(width = 0.5), alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.5))