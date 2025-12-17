# Redesigned 3-panel layout

# Panel 1: Global deaths (top - context)
global_deaths <- deathplot +
  scale_x_continuous(
    breaks = seq(1950, 2010, 10),
    labels = c("50", "60", "70", "80", "90", "00", "10")
  ) +

  coord_cartesian(
    xlim = c(1942, 2014),
    expand = FALSE
  ) +


  theme(
    # Remove background lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),

    # X-axis (now vertical after flip)
    axis.title.x = element_blank(),
    axis.text.x = element_text(face = "bold", size = 9),
    axis.ticks.x = element_line(color = "black", linewidth = 0.3),

    # Y-axis (now horizontal after flip)
    axis.title.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = 9),
    axis.ticks.y = element_line(color = "black", linewidth = 0.3),

    plot.margin = margin(b = 0)
  )


# Panel 2: Regional timeline with deaths encoded
regional_timeline <- ggplot(duration_data) +
  geom_segment(
    aes(x = start_year, xend = end_year,
        y = location,
        color = deaths_thousands),
    alpha = 0.8
  ) +

  scale_color_gradient(
    low = "#ffcccc",
    high = "#8B0000",
    trans = "log10",
    name = "Deaths\n(thousands)"
  ) +

  scale_linewidth_continuous(range = c(0.5, 3), guide = "none") +

  facet_grid(region ~ ., scales = "free_y", space = "free_y") +

  labs(x = NULL, y = NULL) +

  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),
    strip.text.y = element_text(angle = 0, face = "bold")
  )


duration_data$decade <- floor(duration_data$start_year / 10) * 10


# Panel 3: Duration vs Deaths scatter (bottom - analysis)
scatter_summary <- ggplot(duration_data) +
  aes(x = duration, y = deaths_thousands, size = deaths_thousands, colour = factor(decade)) +
  geom_point(alpha = 0.6) +
    scale_colour_viridis_d(
      option = "plasma",
      end = 0.9,
      name = "Decade"
    ) +
  scale_size_continuous(range = c(2, 15), guide = "none") +
  scale_y_log10(labels = scales::comma) +

  facet_wrap(~region, ncol = 2) +
  labs(
    x = "Duration (years)",
    y = "Deaths (thousands, log scale)"
  ) +
  theme_minimal()

# Combine all three
final_viz <- title_plot / global_deaths +
  scatter_summary
  plot_layout(heights = c(0.05, 0.5, 0.7))

final_viz
