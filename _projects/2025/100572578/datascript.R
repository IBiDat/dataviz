# data

library(readxl)
library(dplyr)
library(tidyr)

assess_data_1 <- read_xlsx("CAT_AssessmentData_AUS.xlsx",
                           skip = 23)
effort_share <- read_xlsx(path = "CAT_AssessmentData_AUS.xlsx",
                          skip = 21,
                          sheet = "EffortSharing")

country_1 <- read_xlsx(path = "Countryemissions_AUSdata.xlsx")


# Read the data with proper header
global <- read_xlsx("CAT_19122025_CountryAssessmentData_DataExplorer.xlsx",
                     skip = 18,
                     sheet = "Assessment")

#extract fair share data needed
country_1 <- country_1 |>
  select(scenario, sector, value, year, indicator) |>
  rename(emissions = value)

##############################################################
### FIRST PLOT - MODELLED PATHWAYS GRAPH
##############################################################

# data for rating bands
country <-  country_1 |>
  filter(indicator == "Modelled Domestic Pathways boundaries",
         year <= 2030) |>
  pivot_wider(names_from = scenario,
              values_from = emissions)


##############################################################
## Processed data
#############################################################

# Modelled pathways and target points
# (sector, series, year and emissions)
assess_data <- assess_data_1 |>
  mutate_if(is.character, list(~na_if(., "-"))) |>
  mutate(`2030` = as.numeric(`2030`)) |>
  rename(series = `Graph label`,
         sector = `Sector/Type`) |>
  pivot_longer(cols = -c(series,
                         sector),
               names_to = "year",
               values_to = "emissions") |>
  filter(!is.na(emissions)) |>
  mutate (year = as.numeric(year))


# Add missing data points from country dataset
assess_data <- assess_data |>
  add_row(
    series = "Policies and action",
    year = 2023,
    emissions = 530.4000,
    sector = "Total, excl LULUCF, Min",
    .after = 68) |>
  add_row(
    series = "Policies and action",
    year = 2023,
    emissions = 530.4000,
    sector = "Total, excl LULUCF, Max",
    .after = 81) |>
  add_row(
    series = "Planned policies",
    year = 2023,
    emissions = 530.4000,
    sector = "Total, excl LULUCF, Min",
    .before = 95)



# (p4) extract historical data - History line
historical_exc <- assess_data |>
  filter(series == "Historical emissions, excl forestry")


# (p5) extract LULUCF data - green line
lulucf <- assess_data |>
  filter(series == "Historical emissions/removals from forestry")

# (p6 & p8) Announced/planned policies line and geom point
planned <- assess_data |>
  filter(series == "Planned policies",
         year <= 2030) |>
  pivot_wider(names_from = sector,
              values_from = emissions)

# (p7) data for annotation lines
# Land use and forests line
line1p <- tibble(
  year = c(2013, 2013, 2030),
  emissions = c(16,45,45))
# Announced policies line
line2p <- tibble(
  year = c(2029.5, 2028, 2021.5),
  emissions = c(400, 377, 377))

# (p8) calculate policies & action min/max for ribbon
policies <- assess_data |>
  filter(series == "Policies and action",
         year <= 2030) |>
  pivot_wider(names_from = sector,
              values_from = emissions) |>
  rowwise() |>
  mutate(mid_range = mean(c(`Total, excl LULUCF, Min`,
                            `Total, excl LULUCF, Max`))) |>
  ungroup()


# (p9) Get NDC target (using 'Min' though there is no difference)
ndc_value <- assess_data |>
  filter(series == "Unconditional",
         sector == "Min",
         year == 2030) |>
  select(year, emissions)


# (p10) Add 1.5C shapes
fair <- effort_share |>
  pivot_longer(!`Upper end of`,
               names_to = "year",
               values_to = "emissions") |>
  filter(year == 2030,
         `Upper end of` == "1.5C Paris Agreement compatible") |>
  mutate (year = as.numeric(year))

mod_1.5 <- country |>
  select(`1.5C compatible`, year) |>
  filter(year == 2030) |>
  pivot_longer(`1.5C compatible`,
               names_to = "modelled",
               values_to = "emissions")


##############################################################
### SECOND PLOT - FAIR SHARE GRAPH
##############################################################

# (f2) Add rating column
fair_bars <- country_1 |>
  filter(year == 2030,
         indicator == "Equity boundaries",
         emissions > 100,
         scenario != "Upper limit") |>
  select(year, emissions, scenario) |>
  add_row(year = 2030,
          emissions = 600,
          scenario = "Critically insufficient") |>
  arrange(emissions) |>
  mutate(ymin = lag(emissions, default = -100),
         height = emissions - ymin)


# (f4) extract historical data > 2010
hist_fair <- historical_exc |>
  filter(year >= 2010)
hist_fair

# (f5) extract LULUCF data > 2010
lulucf_hist <- lulucf |>
  filter(year >= 2010)

# (f6) data for annotation lines
# Land use and forests line
line1f <- tibble(
  year = c(2026, 2030),
  emissions = c(432,432))
# Announced policies line
line2f <- tibble(
  year = c(2030, 2027.5, 2026),
  emissions = c(410.3128, 377, 377))


##############################################################
### LABEL PLOT FOR FIRST GRAPH - LEFT
##############################################################

# (l4) data for annotation lines
# BLUE PnA y axis dot = 432.5334
line1 <- tibble(
  year = c(2030.55, 2035.28, 2035.28),
  emissions = c(432.5334, 432.5334, 501))
# BLACK NDC y axis point = 412.507
line2 <- tibble(
  year = c(2030.95, 2045.13, 2045.13),
  emissions = c(412.507, 412.507, 501))
# SOURCE BOX
line3 <- tibble(
  year = c(2030, 2040, 2040),
  emissions = c(45,45,14.60))

##############################################################
### LABEL PLOT FOR SECOND GRAPH - RIGHT
##############################################################

# (r4) data for annotation lines
# BLACK NDC y axis point = 412.507
line1_r <- tibble(
  year = c(2030.83, 2035.9, 2035.9),
  emissions = c(412.507, 412.507, 501))


##############################################################
##############################################################
### IMPROVEMENT
##############################################################
##############################################################

## PLOT 1 - MAIN ##
# panels for 1.5 compatible - domestic
dom_data <- country_1 |>
  filter(indicator == "Modelled Domestic Pathways boundaries",
         year <= 2030,
         year >= 2024,
         scenario == "1.5C compatible") |>
  select(year, emissions, scenario)

# panels for 1.5 compatible - equity/ fair share
fair_data <- country_1 |>
  filter(indicator == "Equity boundaries",
         year <= 2030,
         scenario == "1.5C compatible") |>
  select(year, emissions, scenario) |>
  add_row(
    year = 2024,
    emissions = 294.41,
    scenario = "1.5C compatible - missing data",
    .before = 1)

# Policies data
policies_r <- country_1 |>
  filter(scenario == "Current Policy, Max" |
           scenario == "Current Policy, Min",
         year <= 2030) |>
  pivot_wider(names_from = scenario,
              values_from = emissions) |>
  rowwise() |>
  mutate(mid_range = mean(c(`Current Policy, Min`,
                            `Current Policy, Max`))) |>
  ungroup()

## PLOT 2 - LEFT (RATINGS) ##

# ratings for the year 2030 (dom)
bar_ratings <-  country_1 |>
  filter(indicator == "Modelled Domestic Pathways boundaries",
         year == 2030,
         emissions < 600,
         scenario != "Lower limit") |>
  select(scenario, emissions) |>
  add_row(emissions = 600,
          scenario = "Critically insufficient") |>
  arrange(emissions) |>
  mutate(ymin = lag(emissions, default = 0)) |>
  rowwise() |>
  mutate(mid = mean(c(ymin,emissions))) |>
  ungroup()


###############################################################
# Improvement 2 data


# Relevant variables for fair share and emissions
current_policy <- global |>
  filter(Scenario == "Current Policy, Max" |
           Scenario == "Current Policy, Min" |
           Scenario == "Historical" & #Historical for 2021
           Sector == "Economy-wide, excluding LULUCF") |>
  select(Country, Scenario, `2021`:`2030`) |>
  pivot_longer(cols = `2021`:`2030`,
               names_to = "year",
               values_to = "emissions") |>
  pivot_wider(names_from = Scenario,
              values_from = emissions) |>
  mutate(year = as.numeric(year)) |>
  group_by(Country, year) |>
  mutate(
    midpoint = coalesce((`Current Policy, Max` + `Current Policy, Min`) / 2,
                        Historical)) |>
  ungroup()
# Get boundaries
boundaries <- global |>
  filter(Scenario %in% c("1.5C compatible", "Almost sufficient",
                         "Insufficient", "Highly insufficient")) |>
  filter(Indicator == "Equity boundaries (absolute)") |>
  select(Country, Scenario, `2030`) |>
  pivot_wider(names_from = Scenario,
              values_from = `2030`)
# Combine and apply rating cats
ratings <- current_policy |>
  left_join(boundaries, by = "Country") |>
  filter(Country != "UKR") |> #Removed due to ongoing war
  group_by(Country, year) |>
  mutate(
    Rating = factor(case_when(
      midpoint <= `1.5C compatible` ~ "1.5C compatible",
      midpoint <= `Almost sufficient` ~ "Almost sufficient",
      midpoint <= `Insufficient` ~ "Insufficient",
      midpoint <= `Highly insufficient` ~ "Highly insufficient",
      .default = as.character("Critically insufficient")),
      levels = c("1.5C compatible", "Almost sufficient",
                 "Insufficient", "Highly insufficient",
                 "Critically insufficient"),
      ordered = TRUE),
    within_cat = case_when(
      Rating == "1.5C compatible" ~
        (midpoint/`1.5C compatible`) * 100,
      Rating == "Almost sufficient" ~
        (midpoint - `1.5C compatible`) /
        (`Almost sufficient` - `1.5C compatible`) * 100,
      Rating == "Insufficient" ~
        (midpoint - `Almost sufficient`) /
        (`Insufficient` - `Almost sufficient`)* 100,
      Rating == "Highly insufficient" ~
        (midpoint - `Insufficient`) /
        (`Highly insufficient` - `Insufficient`) * 100,
      Rating == "Critically insufficient" ~
        (midpoint - `Highly insufficient`) /
        ((midpoint + 100) - `Highly insufficient`))) |>
  ungroup()
# Arrange country and limit dataset
final_ratings <- ratings |>
  filter(
    Country %in% (ratings |>
                    filter(year == 2030) |>
                    slice_max(order_by = midpoint, n = 30) |>
                    pull(Country))) |>
  group_by(year) |>
  mutate(ranked_emitters = rank(-midpoint)) |>
  ungroup() |>
  group_by(Country) |>
  mutate(rating_num = first(as.numeric(Rating)[year == 2030]),
         within_cat2030 = within_cat[year == 2030]) |>
  arrange(rating_num, within_cat2030) |>
  ungroup() |>
  select(!within_cat2030)
# apply order to countries for plotting
factor <- final_ratings |>
  mutate(Country = forcats::fct_inorder(Country))
# Function to create gradient colors
create_gradient_colour <- function(rating, within_cat) {
  # Normalise to 0-1 scale (darker = worse within category)
  intensity <- within_cat/100
  if (rating == "1.5C compatible") {
    colorRampPalette(c("#E8F5E9",
                       "#C8E6C9",
                       "#A5D6A7",
                       "#FFF9C4"))(1000)[round(intensity * 999) + 1]
  } else if (rating == "Almost sufficient") {
    colorRampPalette(c("#FFF9C4",
                       "#FFE082",
                       "#FFCC80"))(1000)[round(intensity * 999) + 1]
  } else if (rating == "Insufficient") {
    colorRampPalette(c("#FFCC80",
                       "#FFB74D",
                       "#EF9A9A"))(1000)[round(intensity * 999) + 1]
  } else if (rating == "Highly insufficient") {
    colorRampPalette(c("#EF9A9A",
                       "#E57373",
                       "#EF5350"))(1000)[round(intensity * 999) + 1]
  } else (
    colorRampPalette(c("#EF5350",
                       "#C62828"))(1000)[round(intensity * 999) + 1])
}
# calculate column width and plot values to show width
positions <- factor |>
  filter(year == 2030) |>
  mutate(sqrt_rank = sqrt(ranked_emitters),
         column_width = scales::rescale(sqrt_rank,
                                        to = c(1.8, 0.5),
                                        from = c(sqrt(1), sqrt(30))),
         x_end = cumsum(column_width),
         x_start = lag(x_end, default = 0),
         x_center = (x_start + x_end) / 2) |>
  select(Country, column_width, x_end, x_start, x_center)
# Apply gradient colors
factor_pos <- factor |>
  rowwise() |>
  mutate(
    gradient_colour = create_gradient_colour(Rating, within_cat)) |>
  ungroup() |>
  left_join(positions, by = "Country")

# Select relevant variables for plotting
final_global <- factor_pos |>
  select(c(Country, year, Rating,
           gradient_colour,
           ranked_emitters,
           x_start, x_end, x_center))
