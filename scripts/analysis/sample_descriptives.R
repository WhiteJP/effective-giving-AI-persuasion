# Gender and age for total sample
d_all |>  count(gender)
d_all |> 
  summarise(
    m_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
  ) |> 
  as.data.frame()

# for final sample
d |>  count(gender)
d |> 
  summarise(
    m_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
  ) |> 
  as.data.frame()
