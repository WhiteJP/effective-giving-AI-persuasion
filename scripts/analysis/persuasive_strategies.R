d_strategy1 <- read_csv("data/strategy_ratings_GPT4Sonnet3Deepseek.csv")
d_strategy3.7 <- read_csv("data/strategy_ratings_GPT4oSonnet3.7.csv") # sonnet 3.7

#combine all
d_strategy <- d_strategy1 |> 
  bind_rows(d_strategy3.7) 

d_strategy_long <- d_strategy |> 
  pivot_longer(
    cols = EffectivenessFraming:GuiltAppeals,
    names_to = "strategy",
    values_to = "rating"
  ) |> 
  mutate(
    rating = dplyr::recode(
      rating,
      "None" = 0,
      "Low" = 1,
      "Moderate" = 2,
      "High" = 3
    ),
  ) 

## MODEL agreement
d_strat_agree_wide <- d_strategy_long |> 
  filter(ResponseId %in% ps_final) |> # yes want this just for real sample
  pivot_wider(names_from = model, values_from = rating) |> 
  select(strategy, `deepseek/deepseek-chat-v3-0324`:`anthropic/claude-3.7-sonnet`) |> 
  rename_with(~ str_remove(.x, "^.*/")) 

strat_agreement <- agreement_analysis_by_group(d_strat_agree_wide, group_var = "strategy")

d_strat_agree_wide

d_strategy_agg_long  <- d_strategy_long |> 
  group_by(ResponseId, strategy) |>
  summarise(
    rating = mean(rating, na.rm = TRUE)
  ) 

d_strategy_agg_wide <- d_strategy_agg_long |> 
  pivot_wider(
    names_from = strategy,
    values_from = rating
  ) |> 
  ungroup()

# Link wide data with whole dataset
d_strat_all_wide <- d |> 
  filter(treatment == "conv_treatment") |> 
  select(ResponseId, 
         cents_to_amf_change, cents_to_amf_pre, cents_to_amf_post, cents_to_amf_pre_cat, ## donation change 
         charity_wrong_pre, charity_wrong_post, charity_wrong_change, link_clicked, ## other dvs
         
         
         # control for these other factors that might differ
         AwarenessOfNeed, Solicitation, CostsAndBenefits, 
         Altruism, Reputation, PsychologicalBenefits, Values, Efficacy, 
         DoesntGive) |>
  
  ## add new data
  left_join(d_where[, c("ResponseId", "is_international", "location_cat3")], by = "ResponseId") |> 
  left_join(
    reduce_pcs_matrix(d_subj1 |> filter(condition == "conv_treatment"), "subj", min_n = 30) |> 
      select(ResponseId, starts_with("subj")),
    by = "ResponseId"
  ) |> 
  left_join(
    reduce_pcs_matrix(d_pop2 |> filter(condition == "conv_treatment"), "pop", min_n = 30) |> 
      select(ResponseId, starts_with("pop")),
    by = "ResponseId"
  ) |> 
  left_join(d_strategy_agg_wide, by = "ResponseId") 


# Models  -----------------------------------------------------------------

## for Donation change
# with the categorical control for pre score -- the exact same vars are significant.
mod_lm_donation <- d_strat_all_wide |> 
  select(
    -ResponseId, 
    -location_cat3,
    -cents_to_amf_post,-cents_to_amf_pre, 
    -charity_wrong_pre, -charity_wrong_post, -charity_wrong_change, -link_clicked
  ) |>
  estimatr::lm_robust(
    cents_to_amf_change ~ . ,
    data = _
  ) 

# hacky update p values with q values
mod_lm_donation$p.value <- get_qvals(mod_lm_donation, strat_names)
summary(mod_lm_donation)

## For moral belief change
mod_lm_char_wrong <- d_strat_all_wide |> 
  select(
    -ResponseId, 
    -location_cat3,
    -cents_to_amf_post,-cents_to_amf_pre, -cents_to_amf_change, -cents_to_amf_pre_cat,
    -charity_wrong_post, -link_clicked
  ) |>
  estimatr::lm_robust(
    charity_wrong_change ~ . ,
    data = _
  ) 

# hacky update p values with q values
mod_lm_char_wrong$p.value <- get_qvals(mod_lm_char_wrong, strat_names)
mod_lm_char_wrong |> summary()

## For link clicked
mod_lm_clicked <- d_strat_all_wide |> 
  select(
    -ResponseId, 
    -location_cat3,
    -cents_to_amf_post,-cents_to_amf_pre, -cents_to_amf_change,
    -charity_wrong_pre, -charity_wrong_post, -charity_wrong_change,
  ) |>
  estimatr::lm_robust(
    link_clicked ~ . ,
    data = _
  )

mod_lm_clicked$p.value <- get_qvals(mod_lm_clicked, strat_names)
mod_lm_clicked |>
  summary()
