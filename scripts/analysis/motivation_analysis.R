m_dat1 <- read_csv("data/motivation_ratings_GPT4oSonnet3Deepseek.csv") # original 3, accidentally with earlier version of sonnet
m_dat_3.7 <- read_csv("data/motivation_ratings_Sonnet3.7.csv") # sonnet 3.7

m_dat_all <- m_dat1 |> 
  bind_rows(m_dat_3.7) |> 
  filter(model != "anthropic/claude-3-sonnet") |> 
  arrange(ResponseId)

m_dat_all_long <- m_dat_all |> 
  pivot_longer(AwarenessOfNeed:DoesntGive, names_to = "variable", values_to = "value")

#1. ICCs for agreement 2k (agreement)
d_ratings_wide <- m_dat_all_long |> 
  filter(ResponseId %in% ps_final) |> 
  filter(variable != "DoesntGive") |> 
  pivot_wider(names_from = model, values_from = value) |> 
  select(-charity_reasons, -why_charity_general, -ResponseId) |> 
  rename_with(~ stringr::str_remove(.x, "^.*/")) 

agreement_analysis_by_group(d_ratings_wide, group_var ="variable")

## final data for use in analysis
m_dat <- d |> 
  select(AwarenessOfNeed:Efficacy, DoesntGive)
