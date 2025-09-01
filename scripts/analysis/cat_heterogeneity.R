
d <- d %>%
  mutate(
    charity_fct = charity_name_final %>%
      forcats::fct_na_value_to_level("Other") %>%
      forcats::fct_lump_min(min = 40, other_level = "Other") %>%
      forcats::fct_infreq()
  )


# CHARITY
char_het <- het_by_cat("charity_fct", d, control_var = "cents_to_amf_pre_cat")
char_het$omnibus_tests

# Just want comparisons of conv - control, with Holms correction
char_cate_comparisons <- marginaleffects::avg_comparisons(
  char_het$mod,
  variables = list(condition = "minmax"), # conv - control (max - min)
  newdata = "balanced",
  by = "charity_fct",
  hypothesis = difference ~ pairwise
) |> 
  marginaleffects::hypotheses(multcomp = "holm") # holm comparisons

# none less than .05 after holm
char_cate_comparisons |> 
  filter(p.value < .05)

# Location
loc_het <- het_by_cat("location_cat3", d_where, control_var = "cents_to_amf_pre_cat") 
loc_het$omnibus_tests

# Just want comparisons of conv - control, with Holms correction
loc_cate_comparisons <- marginaleffects::avg_comparisons(
  loc_het$mod,
  variables = list(condition = "minmax"), # conv - control (max - min)
  newdata = "balanced",
  by = "location_cat3",
  hypothesis = difference ~ pairwise
) |> 
  marginaleffects::hypotheses(multcomp = "holm")

# sig comparisons
loc_cate_comparisons |> 
  arrange(desc(abs(estimate))) |> 
  as_tibble() |> 
  filter(p.value < .05)

## Now for binned variables, cause area (subj) 
subj1_cond <- het_by_bins_cond("subj", d_subj1) #Level 1 of PCS
subj1_cond$omnibus_tests

## and population served (pop)
pop2_cond <- het_by_bins_cond("pop", d_pop2) #Lvl 2 of PCS
pop2_cond$omnibus_tests