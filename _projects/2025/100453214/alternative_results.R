# 1. Label Preparation
alt_country_labels <- c(
  "Benin", "Botswana", "Burkina Faso", "Cabo Verde", "Cameroon", 
  "Côte d'Ivoire", "eSwatini", "Gabon", "Gambia", "Ghana", 
  "Guinea", "Kenya", "Lesotho", "Liberia", "Madagascar", 
  "Malawi", "Mali", "Mauritius", "Morocco", "Mozambique", 
  "Namibia", "Niger", "Nigeria", "São Tomé and Príncipe", 
  "Senegal", "Sierra Leone", "South Africa", "Sudan", 
  "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")
alt_country_nivels <- 1:length(alt_country_labels)

# 2. Raw Data Cleaning
alt_round_7 <- read_sav(file = "./round7.sav") |> 
  select(COUNTRY, URBRUR, Q68A, Q8A, Q1, Q101, withinwt, REGION) |> 
  mutate(
    across(c(Q68A, Q8A), ~ case_when(.x %in% c(8, 9, -1) ~ NA_real_, TRUE ~ as.numeric(.x))), 
    Q68A_bin = if_else(Q68A == 3, 1, 0),
    Q8A_bin = if_else(Q8A > 0, 1, 0),
    Q1_cleaned = case_when(Q1 %in% c(998, 999, -1) ~ NA_real_, TRUE ~ as.numeric(Q1)),
    gender_factor = factor(Q101, levels = c(1, 2), labels = c("Male", "Female")),
    country_name = factor(COUNTRY, levels = alt_country_nivels, labels = alt_country_labels))

# 3. Nigeria Sub-group Integration
alt_data_nigeria_total <- alt_round_7 |> filter(COUNTRY == 23)

alt_data_urbana <- alt_data_nigeria_total |>
  filter(URBRUR == 1) |>
  mutate(COUNTRY = 231, country_name = "Urban Nigeria")

alt_data_rural <- alt_data_nigeria_total |>
  filter(URBRUR == 2) |>
  mutate(COUNTRY = 232, country_name = "Rural Nigeria")

alt_round_7_integrado <- alt_round_7 |>
  filter(COUNTRY != 23) |>
  bind_rows(alt_data_nigeria_total, alt_data_urbana, alt_data_rural)

# 4. Firth Logit Function with CI Extraction
alt_ejecutar_logit_firth <- function(alt_data_country_raw) {
  alt_resultado <- tryCatch({
    alt_modelo <- logistf(
      Q68A_bin ~ Q8A_bin + gender_factor + Q1_cleaned, 
      data = alt_data_country_raw,
      weights = alt_data_country_raw$withinwt,
      firth = TRUE,
      conflev = 0.95
    )
    data.frame(
      alt_OR = exp(alt_modelo$coefficients["Q8A_bin"]),
      alt_CI_Lower = exp(alt_modelo$ci.lower["Q8A_bin"]),
      alt_CI_Upper = exp(alt_modelo$ci.upper["Q8A_bin"]),
      alt_P_Value = alt_modelo$prob["Q8A_bin"]
    )
  }, error = function(e) {
    data.frame(alt_OR = NA, alt_CI_Lower = NA, alt_CI_Upper = NA, alt_P_Value = NA)
  })
  return(alt_resultado)
}

# 5. Iterative execution across all countries
alt_paises_lista <- unique(alt_round_7_integrado$COUNTRY)

alt_resultados_or <- alt_paises_lista |>
  set_names() |>
  map_dfr(.id = "COUNTRY_CODE", function(alt_p_code) {
    alt_df_pais <- alt_round_7_integrado |> filter(COUNTRY == alt_p_code)
    alt_ejecutar_logit_firth(alt_df_pais)
  })

# 6. Mapping names and cleaning results
alt_mapeo_nombres <- alt_round_7_integrado |>
  select(COUNTRY, country_name) |>
  distinct() |>
  rename(COUNTRY_CODE = COUNTRY, Country = country_name)

alt_full_results <- alt_resultados_or |>
  mutate(COUNTRY_CODE = as.numeric(COUNTRY_CODE)) |>
  left_join(alt_mapeo_nombres, by = "COUNTRY_CODE") |>
  mutate(
    across(c(alt_OR, alt_CI_Lower, alt_CI_Upper), ~round(.x, 3)),
    alt_P_Value = if_else(alt_P_Value < 0.001, 0.001, round(alt_P_Value, 3))
  ) |>
  select(Country, alt_OR, alt_CI_Lower, alt_CI_Upper, alt_P_Value)

# 7. Add remittances results
alt_data_graph <- alt_full_results %>%
  left_join(df_remittances, by = "Country") %>%
  rename(alt_Remittances = Share) %>%
# 8. Creating a new variable in case of using facets
  mutate(
    alt_is_significant = if_else(alt_CI_Lower > 1 | alt_CI_Upper < 1, 
                                 "Statistically Significant", 
                                 "Not Significant")
  )


