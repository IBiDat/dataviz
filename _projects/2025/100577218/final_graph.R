library(giscoR)
library(sf)
library(ggplot2)
library(ggforce)
library(patchwork)
library(dplyr)
library(grid)
library(png)
library(tidyverse)
library(lubridate)
##### CLEAN DATA #####
data <- read_csv("data/Africa_aggregated_data_up_to-2025-11-29.csv")

data <- 
  data |> 
  mutate(WEEK = dmy(WEEK))

filtered_data <- 
  data |> 
  filter(WEEK >= as.Date("2021-03-01") & WEEK <= as.Date("2022-04-30"))

country_list <- c("Mali", "Burkina Faso", "Niger", "Chad", 
                  "Sudan", "Nigeria", "Cameroon", "Central African Republic", 
                  "South Sudan", "Ethiopia", "Somalia", 
                  "Democratic Republic of Congo", "Uganda", "Mozambique")

filtered_data <- 
  filtered_data |>
  filter(COUNTRY %in% country_list) |> 
  group_by(COUNTRY) |> 
  summarise(total_fatalities = sum(FATALITIES))

# lookup table
values_lookup <- 
  tribble(
    ~COUNTRY, ~human_impact, ~incidence, ~geopolitical_impact,
    "Burkina Faso", 10, 15, 17,
    "Cameroon", 6, 7, 7,
    "Central African Republic", 8, 5, 44,
    "Chad", 2, 1, 23,
    "Democratic Republic of Congo", 32, 25, 42,
    "Ethiopia", 24, 13, 8,
    "Mali", 5, 10, 48,
    "Mozambique", 4, 4, 24,
    "Niger", 2, 3, 33,
    "Nigeria", 26, 27, 12,
    "Somalia", 20, 24, 42,
    "South Sudan", 19, 7, 45,
    "Sudan",19, 6, 30,
    "Uganda", 0, 3, 7
)

filtered_data <- 
  filtered_data |> 
  left_join(values_lookup, by = "COUNTRY")
##### BASE MAP #####
# # Get African countries from giscoR
# # You can adjust the resolution: "60" (1:60m), "20" (1:20m), "10" (1:10m), "03" (1:3m)
# countries <- gisco_get_countries(
#   year = "2020",
#   epsg = "4326",
#   resolution = "20"  # Medium resolution
# )
# 
# # Filter to your region
# bbox <- st_bbox(c(xmin = -27, xmax = 64, ymin = -31, ymax = 41), 
#                 crs = st_crs(4326))
# bbox_poly <- st_as_sfc(bbox)
# 
# countries_in_view <- countries[st_intersects(countries, bbox_poly, sparse = FALSE), ]
# 
# # Create union for coastline
# all_countries_union <- st_union(countries_in_view)
# coastline <- st_cast(st_boundary(all_countries_union), "LINESTRING")
# 
# 
# ##### scaleable
# 
# # Auto-labeled countries (using centroids)
# auto_countries <- 
#   c("Mali",
#     "Niger",
#     "Chad",
#     "Nigeria",
#     "Cameroon",
#     "Cape Verde",
#     "Gambia",
#     "Mauritania",
#     "Senegal",
#     "Guinea",
#     "Sierra Leone",
#     "Liberia",
#     "Côte D’Ivoire",
#     "Ghana",
#     "Togo",
#     "Benin",
#     "Equatorial Guinea",
#     "Gabon",
#     "Congo",
#     "Angola",
#     "Namibia",
#     "South Africa",
#     "Botswana",
#     "Zimbabwe",
#     "Zambia",
#     "Malawi",
#     "Madagascar",
#     "Tanzania",
#     "Burundi",
#     "Rwanda",
#     "Kenya",
#     "Djibouti",
#     "Eritrea")
# 
# countries_auto <- countries_in_view |> 
#   filter(NAME_ENGL %in% auto_countries)
# 
# # Precisely positioned countries
# custom_positions <- tribble(
#   ~name,                          ~lon,   ~lat,
#   "South\nSudan",                 31.5,   8.0,
#   "Central African\nRepublic",    20.0,   6.5,
#   "Somalia",                      45.5,   5.2,
#   "Ethiopia",                     40.5,   9.0,
#   "Burkina\nFaso",                -1.5,   12.5,
#   "Mozambique",                   35.0,   -18.0,
#   "Uganda",                       32.5,   1.5,
#   "Sudan",                        30.0,   15.0
# ) |> 
#   st_as_sf(coords = c("lon", "lat"), crs = 4326)
# 
# # Final map
# base_map <- ggplot() +
#   geom_sf(data = countries_in_view, 
#           fill = "#F0EDE3", color = "#B8C0BE", linewidth = 0.2) +
#   geom_sf(data = coastline, color = "#387298", linewidth = 0.2) +
#   
#   # Auto labels
#   geom_sf_text(data = countries_auto,
#                aes(label = NAME_ENGL),
#                fontface = "bold") +
#   
#   # Custom positioned labels
#   geom_sf_text(data = custom_positions,
#                aes(label = name),
#                fontface = "bold",
#                lineheight = 0.9) +
#   
#   coord_sf(xlim = c(-27, 64), ylim = c(-31, 41), expand = FALSE) +
#   theme_void() +
#   theme(panel.background = element_rect(fill = "#DCE8F2"),
#         plot.background = element_rect(fill = "white"))
# 
##### COLOURS #####
color_points <- tibble(
  value = c(0, 5, 10, 15, 17, 20, 23, 30, 32, 42, 44, 48),
  color = c("#FAF8B2", "#FAF3B0", "#F6E5A8", "#F0DA9F", "#EFD69C",
            "#EDCF97", "#E9C790", "#E4B585", "#E3B080", "#D89670",
            "#D9956D", "#CF8A69")
) %>%
  arrange(value)

value_to_color <- function(val) {
  if (val %in% color_points$value) {
    return(color_points$color[color_points$value == val])
  } else {
    lower_idx <- max(which(color_points$value < val))
    upper_idx <- min(which(color_points$value > val))

    lower_val <- color_points$value[lower_idx]
    upper_val <- color_points$value[upper_idx]
    lower_col <- color_points$color[lower_idx]
    upper_col <- color_points$color[upper_idx]

    weight <- (val - lower_val) / (upper_val - lower_val)
    return(colorRamp(c(lower_col, upper_col))(weight) %>%
             rgb(maxColorValue = 255))
  }
}
##### BASE MAP #####

# Get African countries from giscoR
countries <- gisco_get_countries(
  year = "2020",
  epsg = "4326",
  resolution = "20"
)

# Filter to your region
bbox <- st_bbox(c(xmin = -27, xmax = 64, ymin = -31, ymax = 41), 
                crs = st_crs(4326))
bbox_poly <- st_as_sfc(bbox)

countries_in_view <- countries[st_intersects(countries, bbox_poly, sparse = FALSE), ]

# Define country colors
country_colors <- tribble(
  ~COUNTRY, ~color,
  "Mali", "#ED8758",
  "Burkina Faso", "#EC8657",
  "Niger", "#F29464",
  "Chad", "#F7B88D",
  "Sudan", "#F18B5C",
  "South Sudan", "#F08A5B",
  "Ethiopia", "#DD6F4B",
  "Somalia", "#ED8556",
  "Uganda", "#F6D1A5",
  "Democratic Republic of The Congo", "#E67D52",
  "Mozambique", "#F19F70",
  "Nigeria", "#D46443",
  "Cameroon", "#F3A172",
  "Central African Republic", "#F28C5D",
  "Mauritania", "#FFFFFF",
  "Senegal", "#FFFFFF",
  "Gambia", "#FFFFFF",
  "Guinea-Bissau", "#FFFFFF",
  "Cape Verde", "#FFFFFF",
  "Guinea", "#FFFFFF",
  "Sierra Leone", "#FFFFFF",
  "Côte D'Ivoire", "#FFFFFF",
  "Ghana", "#FFFFFF",
  "Togo", "#FFFFFF",
  "Benin", "#FFFFFF",
  "Gabon", "#FFFFFF",
  "Congo", "#FFFFFF",
  "Angola", "#FFFFFF",
  "Zambia", "#FFFFFF",
  "Namibia", "#FFFFFF",
  "Botswana", "#FFFFFF",
  "Zimbabwe", "#FFFFFF",
  "South Africa", "#FFFFFF",
  "Madagascar", "#FFFFFF",
  "Malawi", "#FFFFFF",
  "United Republic of Tanzania", "#FFFFFF",
  "Burundi", "#FFFFFF",
  "Rwanda", "#FFFFFF",
  "Kenya", "#FFFFFF",
  "Eritrea", "#FFFFFF",
  "Djibouti", "#FFFFFF"
)

# Merge colors with map data
countries_in_view_colored <- countries_in_view %>%
  left_join(country_colors, by = c("NAME_ENGL" = "COUNTRY")) %>%
  mutate(
    # If no specific color assigned, use default beige
    fill_color = ifelse(is.na(color), "#F0EDE3", color)
  )

# Create union for coastline
all_countries_union <- st_union(countries_in_view)
coastline <- st_cast(st_boundary(all_countries_union), "LINESTRING")

##### scaleable

# Auto-labeled countries (using centroids)
auto_countries <- 
  c("Mali",
    "Niger",
    "Chad",
    "Nigeria",
    "Cameroon",
    "Cape Verde",
    "Gambia",
    "Mauritania",
    "Senegal",
    "Guinea",
    "Sierra Leone",
    "Liberia",
    "Côte D'Ivoire",
    "Ghana",
    "Togo",
    "Benin",
    "Equatorial Guinea",
    "Gabon",
    "Congo",
    "Angola",
    "Namibia",
    "South Africa",
    "Botswana",
    "Zimbabwe",
    "Zambia",
    "Malawi",
    "Madagascar",
    "Tanzania",
    "Burundi",
    "Rwanda",
    "Kenya",
    "Djibouti",
    "Eritrea")

countries_auto <- countries_in_view |> 
  filter(NAME_ENGL %in% auto_countries)

# Precisely positioned countries
custom_positions <- tribble(
  ~name,                          ~lon,   ~lat,
  "South\nSudan",                 31.5,   8.0,
  "Central African\nRepublic",    20.0,   6.5,
  "Somalia",                      45.5,   5.2,
  "Ethiopia",                     40.5,   9.0,
  "Burkina\nFaso",                -1.5,   12.5,
  "Mozambique",                   35.0,   -18.0,
  "Uganda",                       32.5,   1.5,
  "Sudan",                        30.0,   15.0
) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Final map with colors
base_map <- ggplot() +
  geom_sf(data = countries_in_view_colored, 
          aes(fill = fill_color),
          color = "#B8C0BE", 
          linewidth = 0.2) +
  scale_fill_identity() +  # Use the hex colors directly
  
  geom_sf(data = coastline, color = "#387298", linewidth = 0.2) +
  
  # Auto labels
  geom_sf_text(data = countries_auto,
               aes(label = NAME_ENGL),
               fontface = "bold") +
  
  # Custom positioned labels
  geom_sf_text(data = custom_positions,
               aes(label = name),
               fontface = "bold",
               lineheight = 0.9) +
  
  coord_sf(xlim = c(-27, 64), ylim = c(-31, 41), expand = FALSE) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#DCE8F2"),
        plot.background = element_rect(fill = "white"))

##### CAMEROON GRAPH #####
cameroon_data <- filtered_data |> 
  filter(COUNTRY == "Cameroon") |> 
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) |> 
  mutate(original_segment = row_number()) |> 
  arrange(desc(row_number())) |> 
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),
    original_start = pi - ((original_segment - 1) / 3 * pi),
    original_end = pi - (original_segment / 3 * pi),
    start_angle = (pi - ((segment - 1) / 3 * pi)) - pi/2,
    end_angle = (pi - (segment / 3 * pi)) - pi/2,
    mid_angle = (original_start + original_end) / 2,
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Scalable version
cameroon_plot <- ggplot(cameroon_data) +
  annotate("rect", 
           xmin = -1.2, xmax = 1.2, 
           ymin = -0.6, ymax = 1,
           fill = alpha("white", 0.5), color = "black", linewidth = 0.2) +  # Changed 'size' to 'linewidth'
  geom_arc_bar(aes(x0 = 0, y0 = -0.3, r0 = 0.6, r = 0.9,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", linewidth = 0.2) +  # Changed 'size' to 'linewidth'
  scale_fill_identity() +
  # Use geom_text instead of size parameter - scales with plot
  geom_text(aes(x = label_x * 1.05, y = (label_y - 0.3) * 1.05, label = value),
            fontface = "bold") +  # Removed fixed size
  annotate("text", x = 0, y = -0.42, label = "Cameroon", 
           fontface = "bold") +  # Removed fixed size
  coord_fixed(xlim = c(-1.3, 1.3), ylim = c(-0.6, 1.0)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = margin(0, 0, 0, 0),
    text = element_text(size = rel(1))  # Text scales with plot size
  )
##### CAR GRAPH #####
central_african_republic_data <- filtered_data |> 
  filter(COUNTRY == "Central African Republic") |> 
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) |> 
  mutate(original_segment = row_number()) |> 
  arrange(desc(row_number())) |> 
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),
    original_start = pi - ((original_segment - 1) / 3 * pi),
    original_end = pi - (original_segment / 3 * pi),
    start_angle = (pi - ((segment - 1) / 3 * pi)) - pi/2,
    end_angle = (pi - (segment / 3 * pi)) - pi/2,
    mid_angle = (original_start + original_end) / 2,
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

central_african_republic_plot <- ggplot(central_african_republic_data) +
  annotate("rect", 
           xmin = -1.2, xmax = 1.2, 
           ymin = -0.8, ymax = 1,
           fill = alpha("white", 0.5), color = "black", linewidth = 0.2) +
  geom_arc_bar(aes(x0 = 0, y0 = -0.3, r0 = 0.6, r = 0.9,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", linewidth = 0.2) +
  scale_fill_identity() +
  geom_text(aes(x = label_x * 1.05, y = (label_y - 0.3) * 1.05, label = value),
            fontface = "bold", size = 2.75) +
  annotate("text", x = 0, y = -0.55, label = "Central African\nRepublic",  # Moved text down
           fontface = "bold", size = 2.75, lineheight = 0.9) +
  coord_fixed(xlim = c(-1.3, 1.3), ylim = c(-0.8, 1.0)) +  # Extended downward
  theme_void() +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = margin(0, 0, 0, 0),
    text = element_text(size = rel(1))
  )

##### CHAD GRAPH #####
chad_data <- filtered_data |> 
  filter(COUNTRY == "Chad") |> 
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) |> 
  mutate(original_segment = row_number()) |> 
  arrange(desc(row_number())) |> 
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),
    original_start = pi - ((original_segment - 1) / 3 * pi),
    original_end = pi - (original_segment / 3 * pi),
    start_angle = (pi - ((segment - 1) / 3 * pi)) - pi/2,
    end_angle = (pi - (segment / 3 * pi)) - pi/2,
    mid_angle = (original_start + original_end) / 2,
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Scalable version
chad_plot <- ggplot(chad_data) +
  annotate("rect", 
           xmin = -1.2, xmax = 1.2, 
           ymin = -0.6, ymax = 1,
           fill = alpha("white", 0.5), color = "black", linewidth = 0.2) +  # Changed 'size' to 'linewidth'
  geom_arc_bar(aes(x0 = 0, y0 = -0.3, r0 = 0.6, r = 0.9,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", linewidth = 0.2) +  # Changed 'size' to 'linewidth'
  scale_fill_identity() +
  # Use geom_text instead of size parameter - scales with plot
  geom_text(aes(x = label_x * 1.05, y = (label_y - 0.3) * 1.05, label = value),
            fontface = "bold") +  # Removed fixed size
  annotate("text", x = 0, y = -0.42, label = "Chad", 
           fontface = "bold") +  # Removed fixed size
  coord_fixed(xlim = c(-1.3, 1.3), ylim = c(-0.6, 1.0)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = margin(0, 0, 0, 0),
    text = element_text(size = rel(1))  # Text scales with plot size
  )
##### DRC GRAPH #####
democratic_republic_of_congo_data <- filtered_data |> 
  filter(COUNTRY == "Democratic Republic of Congo") |> 
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) |> 
  mutate(original_segment = row_number()) |> 
  arrange(desc(row_number())) |> 
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),
    original_start = pi - ((original_segment - 1) / 3 * pi),
    original_end = pi - (original_segment / 3 * pi),
    start_angle = (pi - ((segment - 1) / 3 * pi)) - pi/2,
    end_angle = (pi - (segment / 3 * pi)) - pi/2,
    mid_angle = (original_start + original_end) / 2,
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

democratic_republic_of_congo_plot <- ggplot(democratic_republic_of_congo_data) +
  annotate("rect", 
           xmin = -1.2, xmax = 1.2, 
           ymin = -0.8, ymax = 1,
           fill = alpha("white", 0.5), color = "black", linewidth = 0.2) +
  geom_arc_bar(aes(x0 = 0, y0 = -0.3, r0 = 0.6, r = 0.9,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", linewidth = 0.2) +
  scale_fill_identity() +
  geom_text(aes(x = label_x * 1.05, y = (label_y - 0.3) * 1.05, label = value),
            fontface = "bold", size = 2.75) +
  annotate("text", x = 0, y = -0.55, label = "Democratic Rep.\nof the Congo",  # Moved text down
           fontface = "bold", size = 2.75, lineheight = 0.9) +
  coord_fixed(xlim = c(-1.3, 1.3), ylim = c(-0.8, 1.0)) +  # Extended downward
  theme_void() +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = margin(0, 0, 0, 0),
    text = element_text(size = rel(1))
  )

##### ETHIOPIA GRAPH #####
ethiopia_data <- filtered_data |> 
  filter(COUNTRY == "Ethiopia") |> 
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) |> 
  mutate(original_segment = row_number()) |> 
  arrange(desc(row_number())) |> 
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),
    original_start = pi - ((original_segment - 1) / 3 * pi),
    original_end = pi - (original_segment / 3 * pi),
    start_angle = (pi - ((segment - 1) / 3 * pi)) - pi/2,
    end_angle = (pi - (segment / 3 * pi)) - pi/2,
    mid_angle = (original_start + original_end) / 2,
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Scalable version
ethiopia_plot <- ggplot(ethiopia_data) +
  annotate("rect", 
           xmin = -1.2, xmax = 1.2, 
           ymin = -0.6, ymax = 1,
           fill = alpha("white", 0.5), color = "black", linewidth = 0.2) +  # Changed 'size' to 'linewidth'
  geom_arc_bar(aes(x0 = 0, y0 = -0.3, r0 = 0.6, r = 0.9,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", linewidth = 0.2) +  # Changed 'size' to 'linewidth'
  scale_fill_identity() +
  # Use geom_text instead of size parameter - scales with plot
  geom_text(aes(x = label_x * 1.05, y = (label_y - 0.3) * 1.05, label = value),
            fontface = "bold") +  # Removed fixed size
  annotate("text", x = 0, y = -0.42, label = "Ethiopia", 
           fontface = "bold") +  # Removed fixed size
  coord_fixed(xlim = c(-1.3, 1.3), ylim = c(-0.6, 1.0)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = margin(0, 0, 0, 0),
    text = element_text(size = rel(1))  # Text scales with plot size
  )
##### MALI GRAPH #####
mali_data <- filtered_data |> 
  filter(COUNTRY == "Mali") |> 
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) |> 
  mutate(original_segment = row_number()) |> 
  arrange(desc(row_number())) |> 
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),
    original_start = pi - ((original_segment - 1) / 3 * pi),
    original_end = pi - (original_segment / 3 * pi),
    start_angle = (pi - ((segment - 1) / 3 * pi)) - pi/2,
    end_angle = (pi - (segment / 3 * pi)) - pi/2,
    mid_angle = (original_start + original_end) / 2,
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Scalable version
mali_plot <- ggplot(mali_data) +
  annotate("rect", 
           xmin = -1.2, xmax = 1.2, 
           ymin = -0.6, ymax = 1,
           fill = alpha("white", 0.5), color = "black", linewidth = 0.2) +  # Changed 'size' to 'linewidth'
  geom_arc_bar(aes(x0 = 0, y0 = -0.3, r0 = 0.6, r = 0.9,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", linewidth = 0.2) +  # Changed 'size' to 'linewidth'
  scale_fill_identity() +
  # Use geom_text instead of size parameter - scales with plot
  geom_text(aes(x = label_x * 1.05, y = (label_y - 0.3) * 1.05, label = value),
            fontface = "bold") +  # Removed fixed size
  annotate("text", x = 0, y = -0.42, label = "Mali", 
           fontface = "bold") +  # Removed fixed size
  coord_fixed(xlim = c(-1.3, 1.3), ylim = c(-0.6, 1.0)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = margin(0, 0, 0, 0),
    text = element_text(size = rel(1))  # Text scales with plot size
  )
##### MOZAMBIQUE GRAPH #####
mozambique_data <- filtered_data |> 
  filter(COUNTRY == "Mozambique") |> 
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) |> 
  mutate(original_segment = row_number()) |> 
  arrange(desc(row_number())) |> 
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),
    original_start = pi - ((original_segment - 1) / 3 * pi),
    original_end = pi - (original_segment / 3 * pi),
    start_angle = (pi - ((segment - 1) / 3 * pi)) - pi/2,
    end_angle = (pi - (segment / 3 * pi)) - pi/2,
    mid_angle = (original_start + original_end) / 2,
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Scalable version
mozambique_plot <- ggplot(mozambique_data) +
  annotate("rect", 
           xmin = -1.2, xmax = 1.2, 
           ymin = -0.6, ymax = 1,
           fill = alpha("white", 0.5), color = "black", linewidth = 0.2) +  # Changed 'size' to 'linewidth'
  geom_arc_bar(aes(x0 = 0, y0 = -0.3, r0 = 0.6, r = 0.9,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", linewidth = 0.2) +  # Changed 'size' to 'linewidth'
  scale_fill_identity() +
  # Use geom_text instead of size parameter - scales with plot
  geom_text(aes(x = label_x * 1.05, y = (label_y - 0.3) * 1.05, label = value),
            fontface = "bold") +  # Removed fixed size
  annotate("text", x = 0, y = -0.42, label = "Mozambique", 
           fontface = "bold") +  # Removed fixed size
  coord_fixed(xlim = c(-1.3, 1.3), ylim = c(-0.6, 1.0)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = margin(0, 0, 0, 0),
    text = element_text(size = rel(1))  # Text scales with plot size
  )
##### NIGER GRAPH #####
niger_data <- filtered_data |> 
  filter(COUNTRY == "Niger") |> 
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) |> 
  mutate(original_segment = row_number()) |> 
  arrange(desc(row_number())) |> 
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),
    original_start = pi - ((original_segment - 1) / 3 * pi),
    original_end = pi - (original_segment / 3 * pi),
    start_angle = (pi - ((segment - 1) / 3 * pi)) - pi/2,
    end_angle = (pi - (segment / 3 * pi)) - pi/2,
    mid_angle = (original_start + original_end) / 2,
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Scalable version
niger_plot <- ggplot(niger_data) +
  annotate("rect", 
           xmin = -1.2, xmax = 1.2, 
           ymin = -0.6, ymax = 1,
           fill = alpha("white", 0.5), color = "black", linewidth = 0.2) +  # Changed 'size' to 'linewidth'
  geom_arc_bar(aes(x0 = 0, y0 = -0.3, r0 = 0.6, r = 0.9,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", linewidth = 0.2) +  # Changed 'size' to 'linewidth'
  scale_fill_identity() +
  # Use geom_text instead of size parameter - scales with plot
  geom_text(aes(x = label_x * 1.05, y = (label_y - 0.3) * 1.05, label = value),
            fontface = "bold") +  # Removed fixed size
  annotate("text", x = 0, y = -0.42, label = "Niger", 
           fontface = "bold") +  # Removed fixed size
  coord_fixed(xlim = c(-1.3, 1.3), ylim = c(-0.6, 1.0)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = margin(0, 0, 0, 0),
    text = element_text(size = rel(1))  # Text scales with plot size
  )
##### NIGERIA GRAPH #####
nigeria_data <- filtered_data |> 
  filter(COUNTRY == "Nigeria") |> 
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) |> 
  mutate(original_segment = row_number()) |> 
  arrange(desc(row_number())) |> 
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),
    original_start = pi - ((original_segment - 1) / 3 * pi),
    original_end = pi - (original_segment / 3 * pi),
    start_angle = (pi - ((segment - 1) / 3 * pi)) - pi/2,
    end_angle = (pi - (segment / 3 * pi)) - pi/2,
    mid_angle = (original_start + original_end) / 2,
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Scalable version
nigeria_plot <- ggplot(nigeria_data) +
  annotate("rect", 
           xmin = -1.2, xmax = 1.2, 
           ymin = -0.6, ymax = 1,
           fill = alpha("white", 0.5), color = "black", linewidth = 0.2) +  # Changed 'size' to 'linewidth'
  geom_arc_bar(aes(x0 = 0, y0 = -0.3, r0 = 0.6, r = 0.9,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", linewidth = 0.2) +  # Changed 'size' to 'linewidth'
  scale_fill_identity() +
  # Use geom_text instead of size parameter - scales with plot
  geom_text(aes(x = label_x * 1.05, y = (label_y - 0.3) * 1.05, label = value),
            fontface = "bold") +  # Removed fixed size
  annotate("text", x = 0, y = -0.42, label = "Nigeria", 
           fontface = "bold") +  # Removed fixed size
  coord_fixed(xlim = c(-1.3, 1.3), ylim = c(-0.6, 1.0)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = margin(0, 0, 0, 0),
    text = element_text(size = rel(1))  # Text scales with plot size
  )
##### SOMALIA GRAPH #####
somalia_data <- filtered_data |> 
  filter(COUNTRY == "Somalia") |> 
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) |> 
  mutate(original_segment = row_number()) |> 
  arrange(desc(row_number())) |> 
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),
    original_start = pi - ((original_segment - 1) / 3 * pi),
    original_end = pi - (original_segment / 3 * pi),
    start_angle = (pi - ((segment - 1) / 3 * pi)) - pi/2,
    end_angle = (pi - (segment / 3 * pi)) - pi/2,
    mid_angle = (original_start + original_end) / 2,
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Scalable version
somalia_plot <- ggplot(somalia_data) +
  annotate("rect", 
           xmin = -1.2, xmax = 1.2, 
           ymin = -0.6, ymax = 1,
           fill = alpha("white", 0.5), color = "black", linewidth = 0.2) +  # Changed 'size' to 'linewidth'
  geom_arc_bar(aes(x0 = 0, y0 = -0.3, r0 = 0.6, r = 0.9,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", linewidth = 0.2) +  # Changed 'size' to 'linewidth'
  scale_fill_identity() +
  # Use geom_text instead of size parameter - scales with plot
  geom_text(aes(x = label_x * 1.05, y = (label_y - 0.3) * 1.05, label = value),
            fontface = "bold") +  # Removed fixed size
  annotate("text", x = 0, y = -0.42, label = "Somalia", 
           fontface = "bold") +  # Removed fixed size
  coord_fixed(xlim = c(-1.3, 1.3), ylim = c(-0.6, 1.0)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = margin(0, 0, 0, 0),
    text = element_text(size = rel(1))  # Text scales with plot size
  )
##### SOUTH SUDAN GRAPH #####
south_sudan_data <- filtered_data |> 
  filter(COUNTRY == "South Sudan") |> 
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) |> 
  mutate(original_segment = row_number()) |> 
  arrange(desc(row_number())) |> 
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),
    original_start = pi - ((original_segment - 1) / 3 * pi),
    original_end = pi - (original_segment / 3 * pi),
    start_angle = (pi - ((segment - 1) / 3 * pi)) - pi/2,
    end_angle = (pi - (segment / 3 * pi)) - pi/2,
    mid_angle = (original_start + original_end) / 2,
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Scalable version
south_sudan_plot <- ggplot(south_sudan_data) +
  annotate("rect", 
           xmin = -1.2, xmax = 1.2, 
           ymin = -0.6, ymax = 1,
           fill = alpha("white", 0.5), color = "black", linewidth = 0.2) +  # Changed 'size' to 'linewidth'
  geom_arc_bar(aes(x0 = 0, y0 = -0.3, r0 = 0.6, r = 0.9,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", linewidth = 0.2) +  # Changed 'size' to 'linewidth'
  scale_fill_identity() +
  # Use geom_text instead of size parameter - scales with plot
  geom_text(aes(x = label_x * 1.05, y = (label_y - 0.3) * 1.05, label = value),
            fontface = "bold") +  # Removed fixed size
  annotate("text", x = 0, y = -0.42, label = "South Sudan", 
           fontface = "bold") +  # Removed fixed size
  coord_fixed(xlim = c(-1.3, 1.3), ylim = c(-0.6, 1.0)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = margin(0, 0, 0, 0),
    text = element_text(size = rel(1))  # Text scales with plot size
  )
##### SUDAN GRAPH #####
sudan_data <- filtered_data |> 
  filter(COUNTRY == "Sudan") |> 
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) |> 
  mutate(original_segment = row_number()) |> 
  arrange(desc(row_number())) |> 
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),
    original_start = pi - ((original_segment - 1) / 3 * pi),
    original_end = pi - (original_segment / 3 * pi),
    start_angle = (pi - ((segment - 1) / 3 * pi)) - pi/2,
    end_angle = (pi - (segment / 3 * pi)) - pi/2,
    mid_angle = (original_start + original_end) / 2,
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Scalable version
sudan_plot <- ggplot(sudan_data) +
  annotate("rect", 
           xmin = -1.2, xmax = 1.2, 
           ymin = -0.6, ymax = 1,
           fill = alpha("white", 0.5), color = "black", linewidth = 0.2) +  # Changed 'size' to 'linewidth'
  geom_arc_bar(aes(x0 = 0, y0 = -0.3, r0 = 0.6, r = 0.9,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", linewidth = 0.2) +  # Changed 'size' to 'linewidth'
  scale_fill_identity() +
  # Use geom_text instead of size parameter - scales with plot
  geom_text(aes(x = label_x * 1.05, y = (label_y - 0.3) * 1.05, label = value),
            fontface = "bold") +  # Removed fixed size
  annotate("text", x = 0, y = -0.42, label = "Sudan", 
           fontface = "bold") +  # Removed fixed size
  coord_fixed(xlim = c(-1.3, 1.3), ylim = c(-0.6, 1.0)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = margin(0, 0, 0, 0),
    text = element_text(size = rel(1))  # Text scales with plot size
  )
##### UGANDA GRAPH #####
uganda_data <- filtered_data |> 
  filter(COUNTRY == "Uganda") |> 
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) |> 
  mutate(original_segment = row_number()) |> 
  arrange(desc(row_number())) |> 
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),
    original_start = pi - ((original_segment - 1) / 3 * pi),
    original_end = pi - (original_segment / 3 * pi),
    start_angle = (pi - ((segment - 1) / 3 * pi)) - pi/2,
    end_angle = (pi - (segment / 3 * pi)) - pi/2,
    mid_angle = (original_start + original_end) / 2,
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Scalable version
uganda_plot <- ggplot(uganda_data) +
  annotate("rect", 
           xmin = -1.2, xmax = 1.2, 
           ymin = -0.6, ymax = 1,
           fill = alpha("white", 0.5), color = "black", linewidth = 0.2) +  # Changed 'size' to 'linewidth'
  geom_arc_bar(aes(x0 = 0, y0 = -0.3, r0 = 0.6, r = 0.9,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", linewidth = 0.2) +  # Changed 'size' to 'linewidth'
  scale_fill_identity() +
  # Use geom_text instead of size parameter - scales with plot
  geom_text(aes(x = label_x * 1.05, y = (label_y - 0.3) * 1.05, label = value),
            fontface = "bold") +  # Removed fixed size
  annotate("text", x = 0, y = -0.42, label = "Uganda", 
           fontface = "bold") +  # Removed fixed size
  coord_fixed(xlim = c(-1.3, 1.3), ylim = c(-0.6, 1.0)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = margin(0, 0, 0, 0),
    text = element_text(size = rel(1))  # Text scales with plot size
  )
##### BURKINA FASO GRAPH #####
burkina_faso_data <- filtered_data |> 
  filter(COUNTRY == "Burkina Faso") |> 
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) |> 
  mutate(original_segment = row_number()) |> 
  arrange(desc(row_number())) |> 
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),
    original_start = pi - ((original_segment - 1) / 3 * pi),
    original_end = pi - (original_segment / 3 * pi),
    start_angle = (pi - ((segment - 1) / 3 * pi)) - pi/2,
    end_angle = (pi - (segment / 3 * pi)) - pi/2,
    mid_angle = (original_start + original_end) / 2,
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Scalable version
burkina_faso_plot <- ggplot(burkina_faso_data) +
  annotate("rect", 
           xmin = -1.2, xmax = 1.2, 
           ymin = -0.6, ymax = 1,
           fill = alpha("white", 0.5), color = "black", linewidth = 0.2) +  # Changed 'size' to 'linewidth'
  geom_arc_bar(aes(x0 = 0, y0 = -0.3, r0 = 0.6, r = 0.9,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", linewidth = 0.2) +  # Changed 'size' to 'linewidth'
  scale_fill_identity() +
  # Use geom_text instead of size parameter - scales with plot
  geom_text(aes(x = label_x * 1.05, y = (label_y - 0.3) * 1.05, label = value),
            fontface = "bold") +  # Removed fixed size
  annotate("text", x = 0, y = -0.42, label = "Burkina Faso", 
           fontface = "bold") +  # Removed fixed size
  coord_fixed(xlim = c(-1.3, 1.3), ylim = c(-0.6, 1.0)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = margin(0, 0, 0, 0),
    text = element_text(size = rel(1))  # Text scales with plot size
  )
##### BASE MAP + LINES #####
# Helper function to convert pixels to lon/lat coordinates
pixel_to_lonlat <- function(x_pixels, y_pixels, 
                            map_width = 2232, map_height = 1783,
                            lon_min = -27, lon_max = 64,
                            lat_min = -31, lat_max = 41) {
  x_prop <- x_pixels / map_width
  y_prop <- y_pixels / map_height
  lon <- lon_min + x_prop * (lon_max - lon_min)
  lat <- lat_max - y_prop * (lat_max - lat_min)
  return(list(lon = lon, lat = lat))
}

# ALL CONNECTION LINES - EASY TO EDIT
connections <- list(
  mali = list(
    x = c(298, 298, 445, 445),
    y = c(212, 250, 250, 670)
  ),
  burkina_faso = list(
    x = c(586, 586),
    y = c(209, 714)
  ),
  niger = list(
    x = c(869, 869),
    y = c(209, 531)
  ),
  chad = list(
    x = c(1131, 1131),
    y = c(209, 600)
  ),
  sudan = list(
    x = c(1379, 1379),
    y = c(209, 576)
  ),
  south_sudan = list(
    x = c(1630, 1630, 1459, 1459),
    y = c(330, 519, 519, 886)
  ),
  ethiopia = list(
    x = c(1967, 1612, 1612),
    y = c(627, 627, 768)
  ),
  somalia = list(
    x = c(1967, 1871, 1871, 1800),
    y = c(810, 810, 887, 887)
  ),
  uganda = list(
    x = c(1967, 1450),
    y = c(992, 992)
  ),
  drc = list(
    x = c(1967, 1351),
    y = c(1185, 1185)
  ),
  mozambique = list(
    x = c(1967, 1477),
    y = c(1593, 1593)
  ),
  central_african_republic = list(
    x = c(1090, 1126, 1126, 1090),
    y = c(1197, 1197, 885, 885)
  ),
  cameroon = list(
    x = c(557, 874, 874, 955, 955),
    y = c(1051, 1051, 940, 940, 921)
  ),
  nigeria = list(
    x = c(308, 840, 840),
    y = c(924, 924, 807)
  )
)

# Process all lines (don't need to touch this)
lines_df <- map_dfr(names(connections), function(country) {
  coords <- pixel_to_lonlat(
    x_pixels = connections[[country]]$x,
    y_pixels = connections[[country]]$y
  )
  data.frame(
    x = coords$lon,
    y = coords$lat,
    country = country
  )
})

# Process all endpoints (don't need to touch this)
endpoints_df <- map_dfr(names(connections), function(country) {
  n <- length(connections[[country]]$x)
  coords <- pixel_to_lonlat(
    x_pixels = connections[[country]]$x[n],
    y_pixels = connections[[country]]$y[n]
  )
  data.frame(x = coords$lon, y = coords$lat, country = country)
})

# Add ALL lines and endpoints to map
base_map <- base_map +
  geom_path(data = lines_df,
            aes(x = x, y = y, group = country),
            color = "black",
            linewidth = 0.3) +
  geom_point(data = endpoints_df,
             aes(x = x, y = y),
             color = "black",
             fill = "black",
             shape = 21,
             size = 2,
             stroke = 0.3)


##### BASE MAP + ARC GRAPHS #####

# Standard arc dimensions (215 x 156)
arc_w <- 215 / 2232
arc_h <- 156 / 1783

# CAR and DRC dimensions (215 x 183 - same width, taller)
car_drc_h <- 183 / 1783

# Map dimensions
map_width <- 2232
map_height <- 1783

# Helper function for positioning
pixel_to_prop <- function(left_px, top_px, height_px = 156) {
  list(
    left = left_px / map_width,
    bottom = 1 - (top_px + height_px) / map_height
  )
}

# Calculate positions for standard arcs (215 x 156)
mali_pos <- pixel_to_prop(190, 77)
burkina_pos <- pixel_to_prop(487, 77)
niger_pos <- pixel_to_prop(754, 77)
chad_pos <- pixel_to_prop(1020, 77)
sudan_pos <- pixel_to_prop(1272, 77)
south_sudan_pos <- pixel_to_prop(1528, 196)
ethiopia_pos <- pixel_to_prop(1947, 547)
somalia_pos <- pixel_to_prop(1947, 733)
uganda_pos <- pixel_to_prop(1947, 914)
mozambique_pos <- pixel_to_prop(1947, 1517)
nigeria_pos <- pixel_to_prop(114, 836)
cameroon_pos <- pixel_to_prop(364, 943)

# Calculate positions for CAR and DRC (215 x 183 - taller)
drc_pos <- pixel_to_prop(1947, 1108, height_px = 183)
car_pos <- pixel_to_prop(897, 1104, height_px = 183)

# Create final map with precise positioning
base_map <- base_map +
  # Mali
  inset_element(mali_plot,
                left = mali_pos$left, right = mali_pos$left + arc_w,
                bottom = mali_pos$bottom, top = mali_pos$bottom + arc_h) +
  
  # Burkina Faso
  inset_element(burkina_faso_plot,
                left = burkina_pos$left, right = burkina_pos$left + arc_w,
                bottom = burkina_pos$bottom, top = burkina_pos$bottom + arc_h) +
  
  # Niger
  inset_element(niger_plot,
                left = niger_pos$left, right = niger_pos$left + arc_w,
                bottom = niger_pos$bottom, top = niger_pos$bottom + arc_h) +
  
  # Chad
  inset_element(chad_plot,
                left = chad_pos$left, right = chad_pos$left + arc_w,
                bottom = chad_pos$bottom, top = chad_pos$bottom + arc_h) +
  
  # Sudan
  inset_element(sudan_plot,
                left = sudan_pos$left, right = sudan_pos$left + arc_w,
                bottom = sudan_pos$bottom, top = sudan_pos$bottom + arc_h) +
  
  # South Sudan
  inset_element(south_sudan_plot,
                left = south_sudan_pos$left, right = south_sudan_pos$left + arc_w,
                bottom = south_sudan_pos$bottom, top = south_sudan_pos$bottom + arc_h) +
  
  # Ethiopia
  inset_element(ethiopia_plot,
                left = ethiopia_pos$left, right = ethiopia_pos$left + arc_w,
                bottom = ethiopia_pos$bottom, top = ethiopia_pos$bottom + arc_h) +
  
  # Somalia
  inset_element(somalia_plot,
                left = somalia_pos$left, right = somalia_pos$left + arc_w,
                bottom = somalia_pos$bottom, top = somalia_pos$bottom + arc_h) +
  
  # Uganda
  inset_element(uganda_plot,
                left = uganda_pos$left, right = uganda_pos$left + arc_w,
                bottom = uganda_pos$bottom, top = uganda_pos$bottom + arc_h) +
  
  # Democratic Republic of Congo (TALLER - 215 x 183)
  inset_element(democratic_republic_of_congo_plot,
                left = drc_pos$left, right = drc_pos$left + arc_w,
                bottom = drc_pos$bottom, top = drc_pos$bottom + car_drc_h) +
  
  # Mozambique
  inset_element(mozambique_plot,
                left = mozambique_pos$left, right = mozambique_pos$left + arc_w,
                bottom = mozambique_pos$bottom, top = mozambique_pos$bottom + arc_h) +
  
  # Nigeria
  inset_element(nigeria_plot,
                left = nigeria_pos$left, right = nigeria_pos$left + arc_w,
                bottom = nigeria_pos$bottom, top = nigeria_pos$bottom + arc_h) +
  
  # Cameroon
  inset_element(cameroon_plot,
                left = cameroon_pos$left, right = cameroon_pos$left + arc_w,
                bottom = cameroon_pos$bottom, top = cameroon_pos$bottom + arc_h) +
  
  # Central African Republic (TALLER - 215 x 183)
  inset_element(central_african_republic_plot,
                left = car_pos$left, right = car_pos$left + arc_w,
                bottom = car_pos$bottom, top = car_pos$bottom + car_drc_h)

##### BASE MAP + SAHEL BOX #####
create_sahel_box <- function() {
  
  # Read images
  france_flag <- readPNG("flags_and_logos/france.png")
  italy_flag <- readPNG("flags_and_logos/italy.png")
  usa_flag <- readPNG("flags_and_logos/us.png")
  
  eu_logo <- readPNG("flags_and_logos/eu.png")
  un_logo <- readPNG("flags_and_logos/un.png")
  au_logo <- readPNG("flags_and_logos/au.png")
  g5_logo <- readPNG("flags_and_logos/sahel_g5.png")
  
  # Flag and logo dimensions in box coordinates
  flag_width <- 65 / 245
  flag_height <- 43 / 205
  logo_width <- 65 / 245
  logo_height <- 43 / 205
  logo_circle_width <- 43 / 245
  logo_circle_height <- 43 / 205
  
  # Create the plot
  p <- ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
             fill = "white", color = NA) +
    
    annotate("text", x = 0.5, y = 0.92, label = "The Sahel",
             hjust = 0.5, fontface = "bold") +
    
    # === FLAGS ROW (top) - MIDDLE ONE CENTERED ===
    # France (left)
    annotation_raster(france_flag, 
                      xmin = 0.08, 
                      xmax = 0.08 + flag_width, 
                      ymin = 0.62, 
                      ymax = 0.62 + flag_height) +
    
    # Italy (center - CENTERED)
    annotation_raster(italy_flag, 
                      xmin = 0.5 - (flag_width / 2), 
                      xmax = 0.5 + (flag_width / 2), 
                      ymin = 0.62, 
                      ymax = 0.62 + flag_height) +
    
    # USA (right)
    annotation_raster(usa_flag, 
                      xmin = 0.92 - flag_width, 
                      xmax = 0.92, 
                      ymin = 0.62, 
                      ymax = 0.62 + flag_height) +
    
    # === LOGOS ROW (middle) - MIDDLE ONE CENTERED ===
    # EU (left)
    annotation_raster(eu_logo, 
                      xmin = 0.08, 
                      xmax = 0.08 + logo_width, 
                      ymin = 0.30, 
                      ymax = 0.30 + logo_height) +
    
    # UN (center - CENTERED)
    annotation_raster(un_logo, 
                      xmin = 0.5 - (logo_width / 2), 
                      xmax = 0.5 + (logo_width / 2), 
                      ymin = 0.30, 
                      ymax = 0.30 + logo_height) +
    
    # African Union (right)
    annotation_raster(au_logo, 
                      xmin = 0.92 - logo_width, 
                      xmax = 0.92, 
                      ymin = 0.30, 
                      ymax = 0.30 + logo_height) +
    
    # === BOTTOM LOGO (CIRCULAR - CENTERED) ===
    # For a circular logo, use same pixel dimensions (43x43 px)

    annotation_raster(g5_logo, 
                     xmin = 0.5 - (logo_circle_width / 2), 
                     xmax = 0.5 + (logo_circle_width / 2), 
                     ymin = 0.02, 
                     ymax = 0.02 + logo_circle_height) +
    
    coord_fixed(ratio = 205/245, xlim = c(0, 1), ylim = c(0, 1)) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = "black", linewidth = 2),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  # Convert to grob and add rounded corners
  g <- ggplotGrob(p)
  bg <- g$grobs[[1]]
  
  round_bg <- roundrectGrob(
    x = bg$x, 
    y = bg$y, 
    width = bg$width, 
    height = bg$height,
    r = unit(0.1, "snpc"),
    just = bg$just, 
    name = bg$name, 
    gp = gpar(fill = "white", col = "black", lwd = 2),
    vp = bg$vp
  )
  
  g$grobs[[1]] <- round_bg
  
  return(g)
}

sahel_box <- create_sahel_box()

sahel_w <- 245 / 2232  # 0.1098
sahel_h <- 205 / 1783  # 0.1150

# Add Sahel box to your map
base_map <- base_map +
  inset_element(
    sahel_box,
    left = 0.27,              # Position from left edge
    right = 0.27 + sahel_w,   # Left + width
    bottom = 0.68,            # Position from bottom
    top = 0.68 + sahel_h      # Bottom + height
  )

##### BASE MAP + LAKE CHAD BOX #####
create_lake_chad_box <- function() {
  
  # Read images
  france_flag <- readPNG("flags_and_logos/france.png")
  uk_flag <- readPNG("flags_and_logos/uk.png")
  usa_flag <- readPNG("flags_and_logos/us.png")
  multi_jtf_logo <- readPNG("flags_and_logos/multi.png")
  
  # Box width: 235px
  # Layout: 50px, 65px (flag), 5px, 65px (flag), 50px = 235px total
  
  # Calculate positions in box coordinates (0-1)
  left_margin <- 50 / 235      # 0.2128
  flag_width <- 65 / 235       # 0.2766
  gap <- 5 / 235               # 0.0213
  
  flag_height <- 43 / 145
  logo_width <- 65 / 235
  logo_height <- 43 / 145
  
  # Create the plot
  p <- ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
             fill = "white", color = NA) +
    
    annotate("text", x = 0.5, y = 0.88, label = "Lake Chad Basin",
             hjust = 0.5, fontface = "bold") +
    
    # === FLAGS ROW 1 (top) - 50, 65, 5, 65, 50 ===
    # France (left): starts at 50px
    annotation_raster(france_flag, 
                      xmin = left_margin, 
                      xmax = left_margin + flag_width, 
                      ymin = 0.4, 
                      ymax = 0.4 + flag_height) +
    
    # UK (right): starts at 50 + 65 + 5 = 120px
    annotation_raster(uk_flag, 
                      xmin = left_margin + flag_width + gap, 
                      xmax = left_margin + flag_width + gap + flag_width, 
                      ymin = 0.4, 
                      ymax = 0.4 + flag_height) +
    
    # === FLAGS/LOGOS ROW 2 (bottom) - 50, 65, 5, 65, 50 ===
    # USA (left): starts at 50px
    annotation_raster(usa_flag, 
                      xmin = left_margin, 
                      xmax = left_margin + flag_width, 
                      ymin = 0.06, 
                      ymax = 0.06 + flag_height) +
    
    # Multi JTF logo (right): starts at 50 + 65 + 5 = 120px
    annotation_raster(multi_jtf_logo, 
                      xmin = left_margin + flag_width + gap, 
                      xmax = left_margin + flag_width + gap + logo_width, 
                      ymin = 0.06, 
                      ymax = 0.06 + logo_height) +
    
    coord_fixed(ratio = 145/235, xlim = c(0, 1), ylim = c(0, 1)) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = "black", linewidth = 2),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  # Convert to grob and add rounded corners
  g <- ggplotGrob(p)
  bg <- g$grobs[[1]]
  
  round_bg <- roundrectGrob(
    x = bg$x, 
    y = bg$y, 
    width = bg$width, 
    height = bg$height,
    r = unit(0.1, "snpc"),
    just = bg$just, 
    name = bg$name, 
    gp = gpar(fill = "white", col = "black", lwd = 2),
    vp = bg$vp
  )
  
  g$grobs[[1]] <- round_bg
  
  return(g)
}

lake_chad_box <- create_lake_chad_box()

lake_chad_w <- 235 / 2232  # 0.1098
lake_chad_h <- 145 / 1783  # 0.1150

# Add Sahel box to your map
base_map <- base_map +
  inset_element(
    lake_chad_box,
    left = 0.4,              # Position from left edge
    right = 0.4 + lake_chad_w,   # Left + width
    bottom = 0.55,            # Position from bottom
    top = 0.55 + lake_chad_h      # Bottom + height
  )

##### BASE MAP + GREAT LAKES BOX #####

create_great_lakes_box <- function() {
  
  # Read images - UPDATE THESE WITH YOUR NEEDED FLAGS/LOGOS
  un_logo <- readPNG("flags_and_logos/un.png")
  au_logo <- readPNG("flags_and_logos/au.png")
  rwanda_flag <- readPNG("flags_and_logos/rwanda.png")
  burundi_flag <- readPNG("flags_and_logos/burundi.png")
  
  
  # Box dimensions - SAME AS LAKE CHAD OR DIFFERENT?
  box_width <- 177  # Change if needed
  box_height <- 177  # Change if needed
  
  # Calculate positions in box coordinates (0-1)
  left_margin <- 13 / box_width
  flag_width <- 65 / box_width
  gap <- 25 / box_width
  
  flag_height <- 43 / box_height
  logo_width <- 65 / box_width
  logo_height <- 43 / box_height
  
  # Create the plot
  p <- ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
             fill = "white", color = NA) +
    
    annotate("text", x = 0.5, y = 0.88, label = "Great\nLakes Region",  # CHANGED TITLE
             hjust = 0.5, fontface = "bold") +
    
    # === FLAGS ROW 1 (top) ===
    annotation_raster(au_logo, 
                      xmin = left_margin, 
                      xmax = left_margin + flag_width, 
                      ymin = 0.4, 
                      ymax = 0.4 + flag_height) +
    
    annotation_raster(un_logo, 
                      xmin = left_margin + flag_width + gap, 
                      xmax = left_margin + flag_width + gap + flag_width, 
                      ymin = 0.4, 
                      ymax = 0.4 + flag_height) +
    
    # === FLAGS/LOGOS ROW 2 (bottom) ===
    annotation_raster(rwanda_flag, 
                      xmin = left_margin, 
                      xmax = left_margin + flag_width, 
                      ymin = 0.06, 
                      ymax = 0.06 + flag_height) +
    
    annotation_raster(burundi_flag, 
                      xmin = left_margin + flag_width + gap, 
                      xmax = left_margin + flag_width + gap + logo_width, 
                      ymin = 0.06, 
                      ymax = 0.06 + logo_height) +
    
    coord_fixed(ratio = box_height/box_width, xlim = c(0, 1), ylim = c(0, 1)) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = "black", linewidth = 2),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  # Convert to grob and add rounded corners
  g <- ggplotGrob(p)
  bg <- g$grobs[[1]]
  
  round_bg <- roundrectGrob(
    x = bg$x, y = bg$y, 
    width = bg$width, height = bg$height,
    r = unit(0.1, "snpc"),
    just = bg$just, name = bg$name, 
    gp = gpar(fill = "white", col = "black", lwd = 2),
    vp = bg$vp
  )
  
  g$grobs[[1]] <- round_bg
  
  return(g)
}

great_lakes_box <- create_great_lakes_box()

great_lakes_w <- 177 / 2232
great_lakes_h <- 177 / 1783

# Add to map
base_map <- base_map +
  inset_element(
    great_lakes_box,
    left = 0.52,  # ADJUST POSITION
    right = 0.52 + great_lakes_w,
    bottom = 0.4,  # ADJUST POSITION
    top = 0.4 + great_lakes_h
  )

##### BASE MAP + RWANDA EU SADCMM #####
create_two_row_box <- function() {
  
  # Read images - UPDATE WITH YOUR FLAG FILES
  rwanda_flag <- readPNG("flags_and_logos/rwanda.png")  # Center flag
  eu_logo <- readPNG("flags_and_logos/eu.png")
  sadcmm_flag <- readPNG("flags_and_logos/sadcmm.png")
  
  # Box dimensions
  box_width <- 195  # 65 + 65 + 65 = 195
  box_height <- 135  # Adjust if needed
  
  # Top row: centered flag (65px margin, 65px flag, 65px margin)
  top_margin <- 65 / box_width
  top_flag_width <- 65 / box_width
  
  # Bottom row: 22, 65, 21, 65, 22 = 195
  bottom_left_margin <- 22 / box_width
  bottom_flag_width <- 65 / box_width
  bottom_gap <- 21 / box_width
  
  flag_height <- 43 / box_height  # Standard flag height
  
  # Create the plot
  p <- ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
             fill = "white", color = NA) +
    
    # === TOP ROW - CENTERED FLAG ===
    annotation_raster(rwanda_flag, 
                      xmin = top_margin, 
                      xmax = top_margin + top_flag_width, 
                      ymin = 0.55, 
                      ymax = 0.55 + flag_height) +
    
    # === BOTTOM ROW - TWO FLAGS ===
    # Left flag
    annotation_raster(eu_logo, 
                      xmin = bottom_left_margin, 
                      xmax = bottom_left_margin + bottom_flag_width, 
                      ymin = 0.15, 
                      ymax = 0.15 + flag_height) +
    
    # Right flag
    annotation_raster(sadcmm_flag, 
                      xmin = bottom_left_margin + bottom_flag_width + bottom_gap, 
                      xmax = bottom_left_margin + bottom_flag_width + bottom_gap + bottom_flag_width, 
                      ymin = 0.15, 
                      ymax = 0.15 + flag_height) +
    
    coord_fixed(ratio = box_height/box_width, xlim = c(0, 1), ylim = c(0, 1)) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = "black", linewidth = 2),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  # Convert to grob and add rounded corners
  g <- ggplotGrob(p)
  bg <- g$grobs[[1]]
  
  round_bg <- roundrectGrob(
    x = bg$x, y = bg$y, 
    width = bg$width, height = bg$height,
    r = unit(0.1, "snpc"),
    just = bg$just, name = bg$name, 
    gp = gpar(fill = "white", col = "black", lwd = 2),
    vp = bg$vp
  )
  
  g$grobs[[1]] <- round_bg
  
  return(g)
}

two_row_box <- create_two_row_box()

two_row_w <- 195 / 2232
two_row_h <- 135 / 1783

# Add to map
base_map <- base_map +
  inset_element(
    two_row_box,
    left = 0.71,  # ADJUST POSITION
    right = 0.71 + two_row_w,
    bottom = 0.22,  # ADJUST POSITION
    top = 0.22 + two_row_h
  )
##### BASE MAP + TURK UK US AU EU #####
create_three_two_box <- function() {
  
  # Read images - UPDATE WITH YOUR FLAG FILES
  top_flag1 <- readPNG("flags_and_logos/turkey.png")
  top_flag2 <- readPNG("flags_and_logos/uk.png")
  top_flag3 <- readPNG("flags_and_logos/us.png")
  bottom_flag1 <- readPNG("flags_and_logos/au.png")
  bottom_flag2 <- readPNG("flags_and_logos/eu.png")
  
  # Box dimensions
  box_width <- 245
  box_height <- 138  # Adjust if needed
  
  # Top row: equal spacing with 3 flags
  # Total width = 245, 3 flags × 65 = 195, leaving 50 for 4 spaces = 12.5 each
  top_space <- 12.5 / box_width
  top_flag_width <- 65 / box_width
  
  # Bottom row: flag, 10px gap, flag (centered)
  bottom_flag_width <- 65 / box_width
  bottom_gap <- 10 / box_width
  # Total = 65 + 10 + 65 = 140, centered means (245-140)/2 = 52.5 margin each side
  bottom_margin <- 52.5 / box_width
  
  flag_height <- 43 / box_height
  
  # Create the plot
  p <- ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
             fill = "white", color = NA) +
    
    # === TOP ROW - 3 FLAGS ===
    # Flag 1 (left)
    annotation_raster(top_flag1, 
                      xmin = top_space, 
                      xmax = top_space + top_flag_width, 
                      ymin = 0.55, 
                      ymax = 0.55 + flag_height) +
    
    # Flag 2 (center)
    annotation_raster(top_flag2, 
                      xmin = top_space * 2 + top_flag_width, 
                      xmax = top_space * 2 + top_flag_width * 2, 
                      ymin = 0.55, 
                      ymax = 0.55 + flag_height) +
    
    # Flag 3 (right)
    annotation_raster(top_flag3, 
                      xmin = top_space * 3 + top_flag_width * 2, 
                      xmax = top_space * 3 + top_flag_width * 3, 
                      ymin = 0.55, 
                      ymax = 0.55 + flag_height) +
    
    # === BOTTOM ROW - 2 FLAGS (CENTERED) ===
    # Left flag
    annotation_raster(bottom_flag1, 
                      xmin = bottom_margin, 
                      xmax = bottom_margin + bottom_flag_width, 
                      ymin = 0.15, 
                      ymax = 0.15 + flag_height) +
    
    # Right flag
    annotation_raster(bottom_flag2, 
                      xmin = bottom_margin + bottom_flag_width + bottom_gap, 
                      xmax = bottom_margin + bottom_flag_width + bottom_gap + bottom_flag_width, 
                      ymin = 0.15, 
                      ymax = 0.15 + flag_height) +
    
    coord_fixed(ratio = box_height/box_width, xlim = c(0, 1), ylim = c(0, 1)) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = "black", linewidth = 2),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  # Convert to grob and add rounded corners
  g <- ggplotGrob(p)
  bg <- g$grobs[[1]]
  
  round_bg <- roundrectGrob(
    x = bg$x, y = bg$y, 
    width = bg$width, height = bg$height,
    r = unit(0.1, "snpc"),
    just = bg$just, name = bg$name, 
    gp = gpar(fill = "white", col = "black", lwd = 2),
    vp = bg$vp
  )
  
  g$grobs[[1]] <- round_bg
  
  return(g)
}

three_two_box <- create_three_two_box()

three_two_w <- 245 / 2232
three_two_h <- 138 / 1783

# Add to map
base_map <- base_map +
  inset_element(
    three_two_box,
    left = 0.76,  # ADJUST POSITION
    right = 0.76 + three_two_w,
    bottom = 0.35,  # ADJUST POSITION
    top = 0.35 + three_two_h
  )

##### BASE MAP + ERITREA #####
create_single_flag_box <- function() {
  
  # Read image
  eritrea_flag <- readPNG("flags_and_logos/eritrea.png")
  
  # Box dimensions
  box_width <- 95
  box_height <- 65
  
  # Center the flag: (95 - 65) / 2 = 15px margin on each side
  flag_margin <- 15 / box_width
  flag_width <- 65 / box_width
  flag_height <- 43 / box_height
  
  # Vertical centering: (65 - 43) / 2 = 11px margin top/bottom
  flag_y_margin <- 11 / box_height
  
  # Create the plot
  p <- ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
             fill = "white", color = NA) +
    
    # === CENTERED FLAG ===
    annotation_raster(eritrea_flag, 
                      xmin = flag_margin, 
                      xmax = flag_margin + flag_width, 
                      ymin = flag_y_margin, 
                      ymax = flag_y_margin + flag_height) +
    
    coord_fixed(ratio = box_height/box_width, xlim = c(0, 1), ylim = c(0, 1)) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = "black", linewidth = 2),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  # Convert to grob and add rounded corners
  g <- ggplotGrob(p)
  bg <- g$grobs[[1]]
  
  round_bg <- roundrectGrob(
    x = bg$x, y = bg$y, 
    width = bg$width, height = bg$height,
    r = unit(0.1, "snpc"),
    just = bg$just, name = bg$name, 
    gp = gpar(fill = "white", col = "black", lwd = 2),
    vp = bg$vp
  )
  
  g$grobs[[1]] <- round_bg
  
  return(g)
}

single_flag_box <- create_single_flag_box()

single_flag_w <- 95 / 2232
single_flag_h <- 65 / 1783

# Add to map
base_map <- base_map +
  inset_element(
    single_flag_box,
    left = 0.70,  # ADJUST POSITION
    right = 0.70 + single_flag_w,
    bottom = 0.50,  # ADJUST POSITION
    top = 0.50 + single_flag_h
  )
##### BASE MAP + UN #####
create_single_flag_box_2 <- function() {
  
  # Read image
  un_logo <- readPNG("flags_and_logos/un.png")
  
  # Box dimensions
  box_width <- 95
  box_height <- 65
  
  # Center the flag: (95 - 65) / 2 = 15px margin on each side
  flag_margin <- 15 / box_width
  flag_width <- 65 / box_width
  flag_height <- 43 / box_height
  
  # Vertical centering: (65 - 43) / 2 = 11px margin top/bottom
  flag_y_margin <- 11 / box_height
  
  # Create the plot
  p <- ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
             fill = "white", color = NA) +
    
    # === CENTERED FLAG ===
    annotation_raster(un_logo, 
                      xmin = flag_margin, 
                      xmax = flag_margin + flag_width, 
                      ymin = flag_y_margin, 
                      ymax = flag_y_margin + flag_height) +
    
    coord_fixed(ratio = box_height/box_width, xlim = c(0, 1), ylim = c(0, 1)) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = "black", linewidth = 2),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  # Convert to grob and add rounded corners
  g <- ggplotGrob(p)
  bg <- g$grobs[[1]]
  
  round_bg <- roundrectGrob(
    x = bg$x, y = bg$y, 
    width = bg$width, height = bg$height,
    r = unit(0.1, "snpc"),
    just = bg$just, name = bg$name, 
    gp = gpar(fill = "white", col = "black", lwd = 2),
    vp = bg$vp
  )
  
  g$grobs[[1]] <- round_bg
  
  return(g)
}

single_flag_box_2 <- create_single_flag_box_2()

single_flag_w <- 95 / 2232
single_flag_h <- 65 / 1783

# Add to map
base_map <- base_map +
  inset_element(
    single_flag_box_2,
    left = 0.58,  # ADJUST POSITION
    right = 0.58 + single_flag_w,
    bottom = 0.577,  # ADJUST POSITION
    top = 0.577 + single_flag_h
  )
##### BASE MAP + RUSSIA RWANDA UN #####
create_three_flag_row_box <- function() {
  
  # Read images
  russia_flag <- readPNG("flags_and_logos/russia.png")
  rwanda_flag <- readPNG("flags_and_logos/rwanda.png")
  un_logo <- readPNG("flags_and_logos/un.png")
  
  # Box dimensions
  box_width <- 205
  box_height <- 84
  
  # Layout: 205 total, 3 flags × 65 = 195, leaving 10px for 4 spaces = 2.5px each
  space <- 2.5 / box_width
  flag_width <- 65 / box_width
  flag_height <- 43 / box_height
  
  # Vertical centering
  flag_y_margin <- (box_height - 43) / 2 / box_height
  
  # Create the plot
  p <- ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
             fill = "white", color = NA) +
    
    # === THREE FLAGS IN A ROW WITH SPACING ===
    # Russia (left) - starts at space
    annotation_raster(russia_flag, 
                      xmin = space, 
                      xmax = space + flag_width, 
                      ymin = flag_y_margin, 
                      ymax = flag_y_margin + flag_height) +
    
    # Rwanda (center) - starts at space + flag + space
    annotation_raster(rwanda_flag, 
                      xmin = space * 2 + flag_width, 
                      xmax = space * 2 + flag_width * 2, 
                      ymin = flag_y_margin, 
                      ymax = flag_y_margin + flag_height) +
    
    # UN (right) - starts at space + flag + space + flag + space
    annotation_raster(un_logo, 
                      xmin = space * 3 + flag_width * 2, 
                      xmax = space * 3 + flag_width * 3, 
                      ymin = flag_y_margin, 
                      ymax = flag_y_margin + flag_height) +
    
    coord_fixed(ratio = box_height/box_width, xlim = c(0, 1), ylim = c(0, 1)) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = "black", linewidth = 2),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  # Convert to grob and add rounded corners
  g <- ggplotGrob(p)
  bg <- g$grobs[[1]]
  
  round_bg <- roundrectGrob(
    x = bg$x, y = bg$y, 
    width = bg$width, height = bg$height,
    r = unit(0.1, "snpc"),
    just = bg$just, name = bg$name, 
    gp = gpar(fill = "white", col = "black", lwd = 2),
    vp = bg$vp
  )
  
  g$grobs[[1]] <- round_bg
  
  return(g)
}

three_flag_row_box <- create_three_flag_row_box()

three_flag_row_w <- 205 / 2232
three_flag_row_h <- 84 / 1783

# Add to map
base_map <- base_map +
  inset_element(
    three_flag_row_box,
    left = 0.515,  # ADJUST POSITION
    right = 0.515 + three_flag_row_w,
    bottom = 0.75,  # ADJUST POSITION
    top = 0.75 + three_flag_row_h
  )
##### BASE MAP + KEY #####
# Create white rectangle function
# Create white rectangle function
# Create white rectangle function
create_white_rectangle <- function() {
  
  # Box dimensions
  box_width <- 833
  box_height <- 582
  
  # Create the plot
  p <- ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
             fill = "white", color = NA) +
    
    coord_fixed(ratio = box_height/box_width, xlim = c(0, 1), ylim = c(0, 1)) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = NA, color = "black", linewidth = 0.5),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  return(ggplotGrob(p))
}

white_rectangle <- create_white_rectangle()

white_rect_w <- 833 / 2232
white_rect_h <- 582 / 1783

# Position: 50px from left, 92px from bottom
white_rect_left <- 50 / 2232
white_rect_bottom <- 92 / 1783

# Add to map
base_map <- base_map +
  inset_element(
    white_rectangle,
    left = white_rect_left,
    right = white_rect_left + white_rect_w,
    bottom = white_rect_bottom,
    top = white_rect_bottom + white_rect_h
  )
##### BASE MAP + BOTTOM TEXT #####
#####
base_map
