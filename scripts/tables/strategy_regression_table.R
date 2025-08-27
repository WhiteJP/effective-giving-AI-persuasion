
options("modelsummary_format_numeric_latex" = "plain")

# need to get clean names. 
strat_names <- names(SHORT_STRAT_DESCRIPTIONS)
strat_names_clean <- paste0(
  "Strategy: ", 
  str_to_sentence(str_replace_all(strat_names, "(?<=[a-z])(?=[A-Z])", " "))
)


motivations <- names(MOTIVATION_DESCRIPTIONS)
motivations_clean <- paste0(
  "Motivation: ", 
  str_to_sentence(str_replace_all(motivations, "(?<=[a-z])(?=[A-Z])", " "))
)
pop_served <- mod_lm_donation |> coef() |> names() |> grep("^pop_", x = _, value = TRUE) 
pop_served_clean <- pop_served |>
  str_remove("^pop_")                |>  # drop the pop_ prefix
  str_replace_all("_", " ")          |>  # turn underscores into spaces
  str_to_sentence()                  |>  # capitalize first letter
  (\(x) paste0("Population: ", x))()

subjects <- mod_lm_donation |> coef() |> names() |>  grep("^subj_", x = _, value = TRUE) 
subjects_clean <- subjects |>
  str_remove("^subj_")               |>  # drop the subj_ prefix
  str_replace_all("_", " ")          |>  # turn underscores into spaces
  str_to_sentence()                  |>  # capitalize first letter
  (\(x) paste0("Subject Area: ", x))()  # prepend label


# rename some vars
coef_map  = c(
  `(Intercept)` = "Intercept",
  `cents_to_amf_pre_cat1-10` = "Pre-treatment Donation, 1-10c",
  `cents_to_amf_pre_cat11-20` = "Pre-treatment Donation, 11-20c",
  `cents_to_amf_pre_cat21-30` = "Pre-treatment Donation, 21-30c",
  `cents_to_amf_pre_cat31-40` = "Pre-treatment Donation, 31-40c",
  `cents_to_amf_pre_cat41-50` = "Pre-treatment Donation, 41-50c",
  `cents_to_amf_pre_cat51-100` = "Pre-treatment Donation, 51-100c",
  charity_wrong_pre = "Pre-treatment Moral Belief",
  setNames(strat_names_clean, strat_names),
  setNames(motivations_clean, motivations),
  `DoesntGiveTRUE` = "Motivation: Doesn't Give",
  is_international = "Scope: International",
  setNames(pop_served_clean, pop_served),
  setNames(subjects_clean, subjects)
)


# 2) Move controls to top
coef_order = c(
  "Strategy",
  "Intercept",
  "cents_to_amf_pre_cat1-10",
  "cents_to_amf_pre_cat11-20",
  "cents_to_amf_pre_cat21-30",
  "cents_to_amf_pre_cat31-40",
  "cents_to_amf_pre_cat41-50",
  "cents_to_amf_pre_cat51-100",
  "charity_wrong_pre",
  ".*"
)

table_latex <- modelsummary(
  list(
    "Donation Change" = mod_lm_donation,
    "Moral Belief Change" = mod_lm_char_wrong,
    "Click-through" = mod_lm_clicked
  ),
  output     = "kableExtra", #you need to go via kable to get longtable
  format     = "latex",
  float = TRUE,
  booktabs   = TRUE,
  longtable  = TRUE,
  title      = "\\label{tab:strategy-regressions}Persuasive Strategy Regression Results",
  escape     = FALSE,
  gof_omit   = ".*",     # drop goodness-of-fit rows if you like
  estimate  = "{estimate} ({std.error}){stars}",
  statistic = NULL,                            
  notes     = c(                          
    "*** p<0.001, ** p<0.01; * p<0.05; + p<0.10. Standard errors (HC2) in parentheses.",
    "Strategy coef. p values replaced with q values to maintain pFDR < .05 (Storey)"
  ),
  coef_map = coef_map,
  coef_order = coef_order
)

table_latex <- kableExtra::column_spec(table_latex, 1, width = "6cm") 
table_latex

