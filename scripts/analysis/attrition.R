d_all <- read_rds("data/d_all.rds")

# d_all <- readr::read_csv("data/data_ai-eg-persuasion.csv") |> 
#   mutate(
#     condition = factor(
#       condition, 
#       levels = c('control', "static_treatment", "conv_treatment")
#     )
#   )

conv_errors <- c("R_7M4lCO8AvzFNtA2", "R_1DPc7kbjHlQHOVS", "R_5173KJIiwXvV5oJ")
d_treated <- d_all |> filter(!is.na(condition))
d_itt <- d_treated |> filter(attn == 5)
d <- d_itt |> filter(Finished == 1) |> 
  filter(!ResponseId %in% conv_errors)

ps_final <- d$ResponseId

nrow(d_all) #Total N
nrow(d_all) - nrow(d_treated) # withdrew before randomization
nrow(d_treated) - nrow(d_itt) # failed attention check (but were randomized)
nrow(d_itt) - nrow(d) #28 of ITT withdrew, 3 had technical issues with chatbot
nrow(d) #final 

## Is there differential attrition from ITT to analysis sample?
d_itt |> count(condition)
d |> count(condition)

attrition_matrix <- d_itt |> count(condition) |>
  left_join(d |> count(condition), by = "condition", suffix = c("_pre", "_post")) |>
  transmute(dropped = n_pre - n_post, stayed = n_post) |>
  as.matrix()
attrition_matrix

chisq.test(attrition_matrix)