## correlation between accuracy and donation change

d_fact <- read_csv("data/fact_check_results.csv") |> 
  mutate(
    round = ordered(
      round, 
      levels = c("round1", "round2", "round3", "round4"),
      labels = c("Round 1", "Round 2", "Round 3", "Round 4")
    ),
    round_num = as.numeric(gsub("Round", "", round)),
  )

## agreement analysis
d_fact_wide <- d_fact  %>%
  filter(ResponseId %in% ps_final) |> # yes want this just for real sample
  select( -explanation) |> 
  pivot_wider(names_from = model, values_from = rating) |> 
  select(-round, -round_num, -text, -ResponseId) |> 
  rename_with(~ stringr::str_remove(.x, "^.*/")) 

d_fact_wide_by_round <- d_fact  %>%
  filter(ResponseId %in% ps_final) |> # yes want this just for real sample
  select( -explanation) |> 
  pivot_wider(names_from = model, values_from = rating) |> 
  select(-round_num, -text, -ResponseId) |> 
  rename_with(~ stringr::str_remove(.x, "^.*/")) 

agreement_analysis(d_fact_wide)
agreement_analysis_by_group(d_fact_wide_by_round, group_var = "round")

# average across models
d_fact_ave <- d_fact %>%
  group_by(ResponseId, round, round_num, text) %>%
  summarise(accuracy = mean(rating)) %>%
  ungroup()

# likilihood of being factual claim reduces throughout rounds
trt_n <- d |> filter(condition == "conv_treatment") |> nrow()
d_fact_ave |> 
  count(round) |> 
  mutate(pct = n / trt_n * 100)

## get factual accuracy over rounds
d_fact_ave |> 
  group_by(round) %>%
  summarise(mean = mean(accuracy),
            sd = sd(accuracy),
            n = n(),
            se = sd/sqrt(n),
            min = min(accuracy),
            max = max(accuracy),
  ) |> 
  ungroup()

# model this increase over time
# linearly, reported in paper (but same general result regardless of specification)
d_fact_ave |> lm(accuracy ~ round_num, data = _) |> summary()

## as successive differences (adjacent category comparison)
d_fact_ave |>
  within(contrasts(round) <- MASS::contr.sdif(nlevels(round))) |>
  lm(accuracy ~ round, data = _) |>
  summary()

# now with quadratic term
d_fact_ave |> lm(accuracy ~ round_num + I(round_num^2), data = _) |> summary()

## get overall totals
d_fact_ave |> 
  summarise(
    mean = mean(accuracy),
    sd = sd(accuracy),
    n = n(),
    min = min(accuracy),
    max = max(accuracy)
  )

## NOW lets get one accuracy per conversation, to get overall correlation with donation change
d_fact_subj_ave <- d_fact_ave %>%
  group_by(ResponseId) %>%
  summarise(accuracy = mean(accuracy)) %>%
  ungroup()

d_acc <- d |> 
  select(ResponseId, cents_to_amf_change, cents_to_amf_pre, cents_to_amf_pre_cat, cents_to_amf_post) |> 
  inner_join(d_fact_subj_ave, by = "ResponseId")

## regressions
estimatr::lm_robust(cents_to_amf_change ~ accuracy, data = d_acc) ## sig just by itself -- reported in paper
estimatr::lm_robust(cents_to_amf_change ~ accuracy + cents_to_amf_pre_cat, data = d_acc) ## also sig when controlling for pre score. 
