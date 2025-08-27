

m1 <- gams[[1]]$mod
m2 <- gams[[3]]$mod
m3 <- gams[[4]]$mod

mods <- list(
  "Charity Revenue" = m1,
  "AOT-E" = m2,
  "Impartial Beneficence" = m3
)

coef_map  = c(
  `(Intercept)` = "Control",
  conditionstatic_treatment = "Static Treatment",
  conditionconv_treatment = "LLM Conv. Treatment"
)

## ---------- 1) Parametric-only model table ----------
param_tbl <- msummary(
  mods,
  output   = "kableExtra",
  format   = "latex",
  float    = TRUE,
  booktabs = TRUE,
  escape   = FALSE,
  estimate = "{estimate}{stars} ({std.error})",
  title    = "\\label{tab:gam-params}Fixed Parameters from General Additive Models",
  statistic = NULL,
  gof_map   = c("r.squared", "nobs"),
  coef_omit = "^s\\(",
  coef_rename = c(
    "(Intercept)"               = "Intercept",
    "conditionstatic_treatment" = "Static treatment",
    "conditionconv_treatment"   = "LLM Conv. Treatment"
  ),
  
  notes = c(
    "Each column shows separate GAM predicting donation change with a different covariate.",
    "*** p<0.001, standard errors in parentheses."
  )
)

param_tbl

## ---------- 2) Smooth terms table ----------

# optional: clean smooth labels (turn "s(revenue_log10):conditionconv_treatment"
# into "s(revenue_log10) × LLM Conv.Treatment")
pretty_condition <- function(x) {
  x |>
    str_replace_all("conditioncontrol", "Control") |>
    str_replace_all("conditionstatic_treatment", "Static Treatment") |>
    str_replace_all("conditionconv_treatment",   "LLM Conv. Treatment") |> 
    str_replace_all("s\\(revenue_log10\\)", "Revenue (Log10)")  |> 
    str_replace_all("s\\(ous_impartial_beneficence\\)", "Impartial Beneficence") |>
    str_replace_all("s\\(mean_aot\\)", "AOT-E")
}

smooth_tbl <- purrr::imap_dfr(mods, ~{
  st <- summary(.x)$s.table
  # coerce to dataframe with named columns
  st_df <- as.data.frame(st)
  st_df$Term <- rownames(st)
  st_df |>
    transmute(
      Model  = .y,
      Smooth = Term |>
        pretty_condition() |>
        # replace ":" with " × " for readability
        str_replace(":", " × "),
      edf    = sprintf("%.2f", `edf`),
      Ref.df = sprintf("%.2f", `Ref.df`),
      F      = sprintf("%.2f", `F`),
      p  = ifelse(`p-value` < .001, "<0.001***", sprintf("%.3f", `p-value`))
    ) |>
    select(Smooth, edf, Ref.df, F, p) %>%  
    remove_rownames()
})


smooth_tbl_kbl <- smooth_tbl %>%
  kbl(
    format   = "latex",
    booktabs = TRUE,
    escape   = TRUE,  # we already escaped text; keep stars and symbols
    align    = c("l","l","r","r","r","r"),
    caption  = "\\label{tab:gam-smooths}Approximate significance of smooth terms",
    col.names = c("Smooth","Edf","Ref.df","F","p-value")
  ) |> 
  kable_styling(latex_options = "hold_position") 

smooth_tbl_kbl
