
d <- d %>%
  mutate(
    charity_fct = charity_name_final %>%
      fct_na_value_to_level("Other") %>%
      fct_lump_min(min = 40, other_level = "Other") %>%
      fct_infreq()
  )


# CHARITY
char_het <- het_by_cat("charity_fct", d, control_var = "cents_to_amf_pre_cat")
char_het

# look at comparisons between everyhing here
char_cate_comparisons <- marginaleffects::avg_comparisons(
  char_het$lm_mod,
  variables = list("condition" = "minmax"), # just conv v control
  newdata = "balanced",
  by = c("charity_fct"),
  hypothesis = "pairwise",
  p_adjust = "holm"
) 

char_cate_comparisons 

# none less thatn .05 after holm
char_cate_comparisons |> 
  arrange(desc(abs(estimate))) |> 
  as_tibble() |> 
  filter(p.value < .05) |> 
  print(n = Inf)

# Location
loc_het <- het_by_cat("location_cat3", d_where, control_var = "cents_to_amf_pre_cat") 
loc_het

loc_cate_comparisons <- marginaleffects::avg_comparisons(
  loc_het$lm_mod,
  variables = "condition",#list("condition" = "minmax"), # just conv v control
  newdata = "balanced",
  by = c("location_cat3"),
  hypothesis = "pairwise",
  p_adjust = "holm"
) 


## NOW for binned variables, cause area (subj) and population served (pop)
subj1_cond <- het_by_bins_cond("subj", d_subj1) #Level 1 of PCS
pop2_cond <- het_by_bins_cond("pop", d_pop2) #Lvl 2 fo PCS
