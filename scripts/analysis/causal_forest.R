## Causal forests final

# input -------------------------------------------------------------------

covars_vec <- c(
  "age", "education", 
  "Social_Conserv", "Economic_Conserv", #"pol_party",
  "EA_familiarity",
  "ous_instrumental_harm", "ous_impartial_beneficence", 
  "crt_prop_cor", "mean_aot",
  "ai_use_1", "trust_ai_pre",
  "income_for_denominator",
  "charity_times_per_year",
  "charity_amount_per_year_log10", 
  "charity_proportion_log10",
  "charity_wrong_pre",
  "dogs_v_cats_pre_1",
  "cents_to_amf_pre",
  "AwarenessOfNeed", "Solicitation", "Altruism", "Reputation", "PsychologicalBenefits",
  "Values", "Efficacy", "DoesntGive",
  "encompass_score", 
  "revenue_log10"
)

covars_labels <- c(
  "Age", "Education", 
  "Social Conservatism", "Economic Conservatism", #"Political Party",
  "EA Familiarity",
  "OUS: Instrumental Harm", "OUS: Impartial Beneficence", 
  "CRT % Correct", "Mean AOT-E",
  "AI Use", "Trust in AI",
  "Income",
  "# Donations/yr",
  "$ Donations/yr (log10)", 
  "Rel. $/yr (log10)",
  "Not Giving Wrong",
  "Dogs vs Cats", 
  "Pre-trt Cents to AMF ",
  "Motivation: Awareness of Need", "Motivation: Solicitation", "Motivation: Altruism", 
  "Motivation: Reputation", "Motivation: Psychological Benefits",
  "Motivation: Values", "Motivation: Efficacy", "Motivation: Doesn't Give",
  "Charity Navigator Encompass Score",
  "Revenue (log10)"
)

labels_map <- covars_labels
names(labels_map) <- covars_vec


# Run multi-arm causal forest
set.seed(2882025) #results basically the same for all seed, but fix one for reproducibility
ma_cf <- grf::multi_arm_causal_forest(
  X = as.matrix(d[, covars_vec]),
  Y = d$cents_to_amf_change,
  W = d$condition,
  W.hat = rep(1/3, 3)
)

## ATE
ate <- grf::average_treatment_effect(ma_cf) #ATE
ites <- predict(ma_cf)$predictions

static_ites <- ites[, 1, ]
conv_ites <- ites[, 2, ]

sd(conv_ites)
sd(static_ites)

## plot variable importance
importance_tbl <- tibble(
  variable = covars_labels,
  variable_dirty = covars_vec,
  importance = grf::variable_importance(ma_cf)[, 1]
) |> 
  arrange(desc(importance))

importance_plot <- importance_tbl %>% 
  ggplot(aes(x = importance, y = reorder(variable, importance))) +
  geom_bar(
    stat = "identity",
    fill = amf_blue
  ) +
  labs(
    x = "Variable importance",
    y = NULL
  ) +
  theme(
    panel.grid = element_blank()
    #panel.grid.minor = element_blank(),
    #panel.grid.major.y = element_blank(),
    #panel.grid.minor.x = element_blank()
  )
