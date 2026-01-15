library(haven)
library(dplyr)
library(purrr)
library(broom)
library(logistf)
library(survey) 
library(tidyr)

country_labels <- c(
  "Benin", "Botswana", "Burkina Faso", "Cabo Verde", "Cameroon", 
  "Côte d'Ivoire", "eSwatini", "Gabon", "Gambia", "Ghana", 
  "Guinea", "Kenya", "Lesotho", "Liberia", "Madagascar", 
  "Malawi", "Mali", "Mauritius", "Morocco", "Mozambique", 
  "Namibia", "Niger", "Nigeria", "São Tomé and Príncipe", 
  "Senegal", "Sierra Leone", "South Africa", "Sudan", 
  "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")
country_nivels <- 1:length(country_labels)


full_data <- read_sav(file = "./round7.sav")
round_7 <- read_sav(file = "./round7.sav") |> 
  select(COUNTRY, URBRUR, Q68A, Q8A, Q1, Q101, withinwt, REGION, RESPNO) |> 
  mutate(
    across(
      c(Q68A, Q8A),
      ~ case_when(.x %in% c(8, 9, -1) ~ NA_real_, TRUE ~ as.numeric(.x)),
      .names = "{.col}_cleaned"), 
    Q68A_bin = if_else(Q68A_cleaned == 3, 1, 0),
    Q8A_bin = if_else(Q8A_cleaned > 0, 1, 0),
    Q1_cleaned = case_when(Q1 %in% c(998, 999, -1) ~ NA_real_, TRUE ~ as.numeric(Q1)),
    Q101_cleaned = if_else(Q101 == -1, NA_integer_, Q101),
    gender_factor = factor(Q101_cleaned, levels = c(1, 2), labels = c("Male", "Female")),
    across(c(COUNTRY, REGION, URBRUR), as.factor),
    country_name = factor(COUNTRY, levels = country_nivels, labels = country_labels)) |>
  select(-Q68A, -Q8A, -Q1, -Q101)

data_nigeria_total <- round_7 |> filter(COUNTRY == 23)

data_urbana <- data_nigeria_total |>
  filter(URBRUR == 1) |>
  mutate(COUNTRY = as.factor(231), country_name = "Urban Nigeria")

data_rural <- data_nigeria_total |>
  filter(URBRUR == 2) |>
  mutate(COUNTRY = as.factor(232), country_name = "Rural Nigeria")

round_7_integrado <- round_7 |>
  filter(COUNTRY != 23) |>
  bind_rows(data_nigeria_total, data_urbana, data_rural)

round_7_svy_integrado <- svydesign(
  ids = ~REGION,      
  strata = ~URBRUR,    
  weights = ~withinwt, 
  data = round_7_integrado,
  nest = TRUE) |> 
  subset(COUNTRY != 231 | (COUNTRY == 231 & URBRUR == 1)) |>
  subset(COUNTRY != 232 | (COUNTRY == 232 & URBRUR == 2))

ejecutar_logit_firth <- function(data_country_raw) {
  
  resultado <- tryCatch({
    modelo_firth <- logistf(
      Q68A_bin ~ Q8A_bin + gender_factor + Q1_cleaned, 
      data = data_country_raw,
      weights = data_country_raw$withinwt,
      firth = TRUE)
    results <- data.frame(
      Odds_Ratio = exp(modelo_firth$coefficients["Q8A_bin"]),
      P_Value = modelo_firth$prob["Q8A_bin"])
    if (is.na(results$Odds_Ratio) || is.null(results$Odds_Ratio)) {
      stop("Resultado de Firth nulo.")}
    return(results)
  }, error = function(e) {
    message(paste("Fallo en logistf para el país/subgrupo. Error:", e$message))
    return(data.frame(
      Odds_Ratio = NA,
      P_Value = NA))})
  return(resultado)}

remit_base <- full_data |>
  select(COUNTRY, URBRUR, Q9) |> 
  mutate(
    Country = factor(COUNTRY, levels = country_nivels, labels = country_labels)
  ) |>
  filter(Q9 != -1) |>
  mutate(
    Q9_cleaned = case_when(
      Q9 %in% c(8, 9, -1) ~ NA_real_, 
      TRUE ~ as.numeric(Q9)
    ),
    Q9_bin = if_else(Q9_cleaned > 0, 1, 0),
    Q9_bin = replace_na(Q9_bin, 1)
  ) |>
  select(Country, URBRUR, Q9_bin)

data_nigeria <- remit_base |> filter(Country == "Nigeria")

data_urbana <- data_nigeria |>
  filter(URBRUR == 1) |>
  mutate(Country = "Urban Nigeria")

data_rural <- data_nigeria |>
  filter(URBRUR == 2) |>
  mutate(Country = "Rural Nigeria")

remit_integrado <- remit_base |>
  filter(Country != "Nigeria") |> 
  bind_rows(data_nigeria, data_urbana, data_rural)

Country_remit <- remit_integrado |> 
  group_by(Country) |> 
  summarise(
    Yes = sum(Q9_bin, na.rm = TRUE), 
    Observations = n()) |> 
  mutate(
    Share = round((Yes / Observations) * 100, digits = 0)) |>
  ungroup()

df_remittances <- Country_remit |> 
  select(Country, Share) |>
  arrange(Country)


pop <- full_data |>
  select(COUNTRY, URBRUR) |>
  mutate(
    Country = factor(COUNTRY, levels = country_nivels, labels = country_labels)
  )

answers_pop <- pop |>
  group_by(Country) |>
  summarise(people = n())

nigeria_split <- pop |>
  filter(Country == "Nigeria") |>
  group_by(URBRUR) |>
  summarise(people = n()) |>
  mutate(
    Country = case_when(
      URBRUR == 1 ~ "Urban Nigeria",
      URBRUR == 2 ~ "Rural Nigeria"
    )
  ) |>
  select(Country, people)

df_answers <- bind_rows(answers_pop, nigeria_split) |>
  arrange(Country)

total_encuestas <- nrow(pop)


df_pop <- tibble(
  Country = c("Nigeria", "South Africa", "Tanzania", "Kenya", "Sudan", 
              "Uganda", "Morocco", "Ghana", "Cameroon", "Madagascar", 
              "Mali", "Malawi", "Burkina Faso", "Niger", "Mozambique", 
              "Côte d'Ivoire", "Tunisia", "Senegal", "Zambia", "Guinea", 
              "Zimbabwe", "Togo", "Sierra Leone", "Lesotho", "Liberia", 
              "Namibia", "Gambia", "Benin", "Botswana", "Gabon", 
              "eSwatini", "Mauritius", "Cabo Verde", "São Tomé and Príncipe"),
  Population = c(201135, 58587, 58015, 52574, 42813, 
                 44269, 36472, 30418, 25877, 26969, 
                 19655, 18629, 20321, 23311, 30227, 
                 25717, 11708, 16296, 17861, 12504, 
                 14645, 8082, 7813, 2126, 4937, 
                 2501, 2416, 11802, 2303, 2216, 
                 1155, 1266, 551, 216))

paises_a_ejecutar <- unique(round_7_integrado$COUNTRY)

mapeo_nombres <- round_7_integrado |>
  select(COUNTRY, country_name) |>
  distinct() |>
  rename(COUNTRY_CODE = COUNTRY, Country = country_name)

resultados_or <- paises_a_ejecutar |>
  set_names() |>
  map_dfr(.id = "COUNTRY_CODE", function(pais_code) {
    data_raw_country <- round_7_integrado |> filter(COUNTRY == pais_code)
    resultado <- ejecutar_logit_firth(data_raw_country)
    return(resultado)})

tabla_final_resultados <- resultados_or |>
  mutate(COUNTRY_CODE = as.factor(COUNTRY_CODE)) |>
  left_join(mapeo_nombres, by = "COUNTRY_CODE") |>
  select(Country, Odds_Ratio, P_Value) 


full_results <- tabla_final_resultados |>
  left_join(df_remittances, by = "Country") |>
  left_join(df_answers, by = "Country") |>
  left_join(df_pop, by = "Country") |>
  rename(
    OR = Odds_Ratio,
    P_Value = P_Value,
    Remittances = Share,
    Respondents = people,
    Population = Population)

full_results <- full_results |>
  mutate(
    P_Value = round(P_Value, digits = 3),
    OR = round(OR, digits = 3),
    P_Value = case_when(
      P_Value == 0.000 ~ 0.001,
      TRUE ~ P_Value
    ),
  )
full_results <- full_results |> 
  mutate(
    Population = as.numeric(gsub(",", "", Population)),
    Population = case_when(
      Country == "Urban Nigeria" ~ 85000, 
      Country == "Rural Nigeria" ~ 85000,   
      is.na(Population) ~ 0,               
      TRUE ~ Population                    
    )
  )


