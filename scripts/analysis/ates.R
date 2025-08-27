## T-tests

## For the conversation group
t.test(
  d$cents_to_amf_post[d$condition == "conv_treatment"], 
  d$cents_to_amf_pre[d$condition == "conv_treatment"], paired = TRUE
)

## for the static treatment group
t.test(
  d$cents_to_amf_post[d$condition == "static_treatment"], 
  d$cents_to_amf_pre[d$condition == "static_treatment"], paired = TRUE
)

## for the control treatment group
t.test(
  d$cents_to_amf_post[d$condition == "control"], 
  d$cents_to_amf_pre[d$condition == "control"], paired = TRUE
)

## get means and SDs
d |> 
  group_by(condition) |> 
  summarise(
    mean_pre = mean(cents_to_amf_pre),
    sd_pre = sd(cents_to_amf_pre),
    mean_post = mean(cents_to_amf_post),
    sd_post = sd(cents_to_amf_post),
    mean_change = mean(cents_to_amf_change),
    sd_change = sd(cents_to_amf_change)
  ) 

# standardizers 
pooled_pre_mean <- mean(d$cents_to_amf_pre) # for percent change
pooled_pre_sd <- sd(d$cents_to_amf_pre) # for cohens d

## get percentage increase for each from pre-treatment
mean(d$cents_to_amf_change[d$condition == "conv_treatment"])/pooled_pre_mean
mean(d$cents_to_amf_change[d$condition == "static_treatment"])/pooled_pre_mean 
mean(d$cents_to_amf_change[d$condition == "control"])/pooled_pre_mean

## get cohens d (mean change in donation amount / SD pre donation amount (pooled))
mean(d$cents_to_amf_change[d$condition == "conv_treatment"])/pooled_pre_sd
mean(d$cents_to_amf_change[d$condition == "static_treatment"])/pooled_pre_sd
mean(d$cents_to_amf_change[d$condition == "control"])/pooled_pre_sd

### ATES
d_cov_scaled <- d |> 
select(cents_to_amf_change, charity_wrong_change, trust_ai_change,
       cents_to_amf_pre, charity_wrong_pre, trust_ai_pre, condition) |>
  mutate(
    cents_to_amf_pre_c = cents_to_amf_pre - mean(cents_to_amf_pre),
    charity_wrong_pre_c = charity_wrong_pre - mean(charity_wrong_pre),
    trust_ai_pre_c = trust_ai_pre - mean(trust_ai_pre)
  )

## All relevant comparisons treatments
## same as preregistered 
## estimatr::lm_lin(cents_to_amf_change ~ condition, covariates = ~ cents_to_amf_pre, data = d)
lh_mod <- estimatr::lh_robust(
  cents_to_amf_change ~ condition*cents_to_amf_pre_c, data = d_cov_scaled, 
  linear_hypothesis = "conditionconv_treatment = conditionstatic_treatment"
)
lh_mod

# Cohens d
lh_mod$lm_robust$coefficients["conditionconv_treatment"]/pooled_pre_sd #conv v control
lh_mod$lm_robust$coefficients["conditionstatic_treatment"]/pooled_pre_sd #static v control
coef(lh_mod$lh)/pooled_pre_sd # conv v static

# percent change
lh_mod$lm_robust$coefficients["conditionconv_treatment"]/pooled_pre_mean #conv v control
lh_mod$lm_robust$coefficients["conditionstatic_treatment"]/pooled_pre_mean #static v control
coef(lh_mod$lh)/pooled_pre_mean # conv v static

## click-through

#preregistered analysis
d %>% 
  group_by(condition) %>%
  summarise(
    mean_clicked = mean(link_clicked)
  )

logreg <- glm(link_clicked ~ condition, data = d, family = binomial)

logreg |> summary()
exp(logreg$coefficients)

## Moral Belief change
  
#Now lets do a comparison of the conditions
d_cov_scaled <- d |> 
  select(cents_to_amf_change, charity_wrong_change, trust_ai_change,
         cents_to_amf_pre, charity_wrong_pre, trust_ai_pre, condition) |>
  mutate(
    cents_to_amf_pre_c = cents_to_amf_pre - mean(cents_to_amf_pre),
    charity_wrong_pre_c = charity_wrong_pre - mean(charity_wrong_pre),
    trust_ai_pre_c = trust_ai_pre - mean(trust_ai_pre)
  )

## All relevant comparisons treatments
lh_mod <- estimatr::lh_robust(
  charity_wrong_change ~ condition*charity_wrong_pre_c, data = d_cov_scaled, 
  linear_hypothesis = "conditionconv_treatment = conditionstatic_treatment"
)
lh_mod

# Cohens d
lh_mod$lm_robust$coefficients["conditionconv_treatment"]/pooled_pre_sd #conv v control
lh_mod$lm_robust$coefficients["conditionstatic_treatment"]/pooled_pre_sd #static v control
coef(lh_mod$lh)/pooled_pre_sd # conv v static


