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
           fill = "white", color = "black", linewidth = 0.8) +  # Changed 'size' to 'linewidth'
  geom_arc_bar(aes(x0 = 0, y0 = -0.3, r0 = 0.6, r = 0.9,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", linewidth = 1) +  # Changed 'size' to 'linewidth'
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

niger_plot
