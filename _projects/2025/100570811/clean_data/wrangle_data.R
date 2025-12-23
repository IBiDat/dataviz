# Wrangle data

# R/wrangle_data.R

library(readxl)
library(dplyr)

# Load raw data
battledeaths <- read.csv("raw_data/BattleDeaths_v25_1_conf.csv")
battledeaths_b <- read_excel("raw_data/PRIO Battle Deaths Dataset 3.0.xls")

# Uppsala (UCDP) deaths
uppsaladeaths <- battledeaths |>
  filter(type_of_conflict %in% c(3, 4), year <= 2012) |>
  group_by(year) |>
  summarise(
    totaldeaths = sum(bd_best, na.rm = TRUE) / 1000,
    .groups = "drop"
  )

# PRIO deaths
priodeaths <- battledeaths_b |>
  filter(type %in% c(3, 4), between(year, 1946, 1988)) |>
  group_by(id, year) |>
  summarise(
    conflict_deaths = max(bdeadbes, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(year) |>
  summarise(
    totaldeaths = sum(ifelse(conflict_deaths > 0, conflict_deaths, 0)) / 1000,
    .groups = "drop"
  )

# Save processed data
write.csv(
  uppsaladeaths,
  "uppsaladeaths.csv",
  row.names = FALSE
)

write.csv(
  priodeaths,
  "priodeaths.csv",
  row.names = FALSE
)
