#south_sudan_arc_bar
#ORIENTATION CORRECT
south_sudan_data <- filtered_data %>%
  filter(COUNTRY == "South Sudan") %>%
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) %>%
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),
    # Original angles (for label positions)
    original_start = pi - ((segment - 1) / 3 * pi),
    original_end = pi - (segment / 3 * pi),
    # Rotated angles (for arc drawing)
    start_angle = original_start - pi/2,
    end_angle = original_end - pi/2,
    # Calculate label positions using ORIGINAL angles
    mid_angle = (original_start + original_end) / 2,
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Create semicircle donut
ggplot(south_sudan_data) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", size = 1) +
  scale_fill_identity() +
  geom_text(aes(x = label_x, y = label_y, label = value),
            size = 6, fontface = "bold") +
  annotate("text", x = 0, y = -0.2, label = "South Sudan", 
           size = 10, fontface = "bold") +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

# Check South Sudan's transformation
south_sudan_debug <- filtered_data %>%
  filter(COUNTRY == "South Sudan") %>%
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) %>%
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number()
  ) %>%
  select(category, value, color, segment)

print(south_sudan_debug)

# Check the angles
south_sudan_angles <- filtered_data %>%
  filter(COUNTRY == "South Sudan") %>%
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) %>%
  mutate(
    segment = row_number(),
    original_start = pi - ((segment - 1) / 3 * pi),
    original_end = pi - (segment / 3 * pi),
    start_angle = original_start - pi/2,
    end_angle = original_end - pi/2
  ) %>%
  select(category, value, segment, start_angle, end_angle)

print(south_sudan_angles)


south_sudan_data <- filtered_data %>%
  filter(COUNTRY == "South Sudan") %>%
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) %>%
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),
    # Original angles (for label positions)
    original_start = pi - ((segment - 1) / 3 * pi),
    original_end = pi - (segment / 3 * pi),
    # Rotated angles (for arc drawing)
    start_angle = original_start - pi/2,
    end_angle = original_end - pi/2,
    # Calculate label positions using ORIGINAL angles
    mid_angle = (original_start + original_end) / 2,
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  ) %>%
  arrange(segment)  # Explicitly arrange by segment

# Create semicircle donut
ggplot(south_sudan_data) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1,
                   start = start_angle, end = end_angle, fill = color,
                   group = segment),  # Add group aesthetic
               color = "white", size = 1) +
  scale_fill_identity() +
  geom_text(aes(x = label_x, y = label_y, label = value),
            size = 6, fontface = "bold") +
  annotate("text", x = 0, y = -0.2, label = "South Sudan", 
           size = 10, fontface = "bold") +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )


# Draw ONLY segment 1 (human_impact = 19)
south_sudan_test <- south_sudan_data %>% filter(segment == 1)

ggplot(south_sudan_test) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", size = 1) +
  scale_fill_identity() +
  geom_text(aes(x = label_x, y = label_y, label = value),
            size = 6, fontface = "bold") +
  annotate("text", x = 0, y = -0.2, label = "Segment 1: human_impact = 19", 
           size = 8, fontface = "bold") +
  coord_fixed() +
  theme_void()


south_sudan_data <- filtered_data %>%
  filter(COUNTRY == "South Sudan") %>%
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) %>%
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),
    # Original angles (for label positions)
    original_start = pi - ((segment - 1) / 3 * pi),
    original_end = pi - (segment / 3 * pi),
    # Rotated angles - ADD pi/2 instead of subtracting
    start_angle = original_start + pi/2,
    end_angle = original_end + pi/2,
    # Calculate label positions using ORIGINAL angles (these are correct)
    mid_angle = (original_start + original_end) / 2,
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Create semicircle donut
ggplot(south_sudan_data) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", size = 1) +
  scale_fill_identity() +
  geom_text(aes(x = label_x, y = label_y, label = value),
            size = 6, fontface = "bold") +
  annotate("text", x = 0, y = -0.2, label = "South Sudan", 
           size = 10, fontface = "bold") +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )


south_sudan_data <- filtered_data %>%
  filter(COUNTRY == "South Sudan") %>%
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) %>%
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),
    # Calculate angles that will work correctly with geom_arc_bar
    # We want left-to-right on TOP (like a rainbow)
    start_angle = ((segment - 1) / 3 * pi),
    end_angle = (segment / 3 * pi),
    # Calculate label positions - same angles but for positioning
    mid_angle = (start_angle + end_angle) / 2,
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Create semicircle donut
ggplot(south_sudan_data) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", size = 1) +
  scale_fill_identity() +
  geom_text(aes(x = label_x, y = label_y, label = value),
            size = 6, fontface = "bold") +
  annotate("text", x = 0, y = -0.2, label = "South Sudan", 
           size = 10, fontface = "bold") +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

south_sudan_data <- filtered_data %>%
  filter(COUNTRY == "South Sudan") %>%
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) %>%
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),
    # Original approach but reverse the segment numbering
    reversed_segment = 4 - segment,  # 1->3, 2->2, 3->1
    # Use reversed segment for angles
    start_angle = ((reversed_segment - 1) / 3 * pi),
    end_angle = (reversed_segment / 3 * pi),
    # Calculate label positions
    mid_angle = (start_angle + end_angle) / 2,
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Create semicircle donut
ggplot(south_sudan_data) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", size = 1) +
  scale_fill_identity() +
  geom_text(aes(x = label_x, y = label_y, label = value),
            size = 6, fontface = "bold") +
  annotate("text", x = 0, y = -0.2, label = "South Sudan", 
           size = 10, fontface = "bold") +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )


south_sudan_data <- filtered_data %>%
  filter(COUNTRY == "South Sudan") %>%
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) %>%
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number()
  )

# Manually specify the exact angles we want for each segment
south_sudan_data <- south_sudan_data %>%
  mutate(
    start_angle = case_when(
      segment == 1 ~ 0,        # human_impact (19) - LEFT
      segment == 2 ~ pi/3,     # incidence (7) - MIDDLE
      segment == 3 ~ 2*pi/3    # geopolitical_impact (45) - RIGHT
    ),
    end_angle = case_when(
      segment == 1 ~ pi/3,
      segment == 2 ~ 2*pi/3,
      segment == 3 ~ pi
    ),
    mid_angle = (start_angle + end_angle) / 2,
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Create semicircle donut
ggplot(south_sudan_data) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", size = 1) +
  scale_fill_identity() +
  geom_text(aes(x = label_x, y = label_y, label = value),
            size = 6, fontface = "bold") +
  annotate("text", x = 0, y = -0.2, label = "South Sudan", 
           size = 10, fontface = "bold") +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )







south_sudan_data <- filtered_data %>%
  filter(COUNTRY == "South Sudan") %>%
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) %>%
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number()
  )

# Angles that go LEFT to RIGHT on top
south_sudan_data <- south_sudan_data %>%
  mutate(
    start_angle = case_when(
      segment == 1 ~ pi,       # human_impact (19) - LEFT (9 o'clock)
      segment == 2 ~ 2*pi/3,   # incidence (7) - MIDDLE  
      segment == 3 ~ pi/3      # geopolitical_impact (45) - RIGHT
    ),
    end_angle = case_when(
      segment == 1 ~ 2*pi/3,   # ends at middle
      segment == 2 ~ pi/3,     # ends at right-middle
      segment == 3 ~ 0         # ends at 3 o'clock (right)
    ),
    mid_angle = (start_angle + end_angle) / 2,
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Create semicircle donut
ggplot(south_sudan_data) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", size = 1) +
  scale_fill_identity() +
  geom_text(aes(x = label_x, y = label_y, label = value),
            size = 6, fontface = "bold") +
  annotate("text", x = 0, y = -0.2, label = "South Sudan", 
           size = 10, fontface = "bold") +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )









south_sudan_data <- filtered_data %>%
  filter(COUNTRY == "South Sudan") %>%
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) %>%
  arrange(desc(row_number())) %>%  # Reverse the order: 3, 2, 1
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),  # Now: geopolitical=1, incidence=2, human=3
    # Simple angle calculation
    start_angle = ((segment - 1) / 3 * pi),
    end_angle = (segment / 3 * pi),
    mid_angle = (start_angle + end_angle) / 2,
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Create semicircle donut
ggplot(south_sudan_data) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", size = 1) +
  scale_fill_identity() +
  geom_text(aes(x = label_x, y = label_y, label = value),
            size = 6, fontface = "bold") +
  annotate("text", x = 0, y = -0.2, label = "South Sudan", 
           size = 10, fontface = "bold") +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

















#ORIENTATION CORRECT
south_sudan_data <- filtered_data %>%
  filter(COUNTRY == "South Sudan") %>%
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) %>%
  arrange(desc(row_number())) %>%  # Reverse order: draw row 3, then 2, then 1
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),
    # Original angles (for label positions)
    original_start = pi - ((segment - 1) / 3 * pi),
    original_end = pi - (segment / 3 * pi),
    # Rotated angles (for arc drawing)
    start_angle = original_start - pi/2,
    end_angle = original_end - pi/2,
    # Calculate label positions using ORIGINAL angles
    mid_angle = (original_start + original_end) / 2,
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Create semicircle donut
ggplot(south_sudan_data) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", size = 1) +
  scale_fill_identity() +
  geom_text(aes(x = label_x, y = label_y, label = value),
            size = 6, fontface = "bold") +
  annotate("text", x = 0, y = -0.2, label = "South Sudan", 
           size = 10, fontface = "bold") +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )







####### CORRECT!!!!! ######
#ORIENTATION CORRECT
south_sudan_data <- filtered_data %>%
  filter(COUNTRY == "South Sudan") %>%
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) %>%
  mutate(original_segment = row_number()) %>%  # Save original position BEFORE reversing
  arrange(desc(row_number())) %>%  # Reverse order: draw row 3, then 2, then 1
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),  # This is for arc drawing (1, 2, 3)
    # Use ORIGINAL segment for label positions
    original_start = pi - ((original_segment - 1) / 3 * pi),
    original_end = pi - (original_segment / 3 * pi),
    # Use NEW segment for arc drawing
    start_angle = (pi - ((segment - 1) / 3 * pi)) - pi/2,
    end_angle = (pi - (segment / 3 * pi)) - pi/2,
    # Calculate label positions using ORIGINAL segment angles
    mid_angle = (original_start + original_end) / 2,
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Create semicircle donut
ggplot(south_sudan_data) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", size = 1) +
  scale_fill_identity() +
  geom_text(aes(x = label_x, y = label_y, label = value),
            size = 6, fontface = "bold") +
  annotate("text", x = 0, y = -0.2, label = "South Sudan", 
           size = 10, fontface = "bold") +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )
############

# CLEAN AND SIMPLE VERSION
south_sudan_data <- filtered_data %>%
  filter(COUNTRY == "South Sudan") %>%
  pivot_longer(
    cols = c(geopolitical_impact, incidence, human_impact),  # Reversed order!
    names_to = "category",
    values_to = "value"
  ) %>%
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),
    # Angles for arc
    start_angle = (pi - ((segment - 1) / 3 * pi)) - pi/2,
    end_angle = (pi - (segment / 3 * pi)) - pi/2,
    # Angles for labels
    mid_angle = pi - ((segment - 0.5) / 3 * pi),
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Create semicircle donut
ggplot(south_sudan_data) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", size = 1) +
  scale_fill_identity() +
  geom_text(aes(x = label_x, y = label_y, label = value),
            size = 6, fontface = "bold") +
  annotate("text", x = 0, y = -0.2, label = "South Sudan", 
           size = 10, fontface = "bold") +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

# CLEAN AND CORRECT VERSION
south_sudan_data <- filtered_data %>%
  filter(COUNTRY == "South Sudan") %>%
  pivot_longer(
    cols = c(geopolitical_impact, incidence, human_impact),  # Reversed for arc order
    names_to = "category",
    values_to = "value"
  ) %>%
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),
    # Arc angles (using current segment order)
    start_angle = (pi - ((segment - 1) / 3 * pi)) - pi/2,
    end_angle = (pi - (segment / 3 * pi)) - pi/2,
    # Label angles (need to reverse back: segment 1->3, 2->2, 3->1)
    label_segment = 4 - segment,
    mid_angle = pi - ((label_segment - 0.5) / 3 * pi),
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Create semicircle donut
ggplot(south_sudan_data) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", size = 1) +
  scale_fill_identity() +
  geom_text(aes(x = label_x, y = label_y, label = value),
            size = 6, fontface = "bold") +
  annotate("text", x = 0, y = -0.2, label = "South Sudan", 
           size = 10, fontface = "bold") +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

# CLEANEST VERSION
south_sudan_data <- filtered_data %>%
  filter(COUNTRY == "South Sudan") %>%
  pivot_longer(
    cols = c(human_impact, incidence, geopolitical_impact),
    names_to = "category",
    values_to = "value"
  ) %>%
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),
    # Calculate base angles
    angle_start = pi - ((segment - 1) / 3 * pi) - pi/2,
    angle_end = pi - (segment / 3 * pi) - pi/2,
    # Swap for arc drawing (reverses the visual order)
    start_angle = angle_end,
    end_angle = angle_start,
    # Labels use original angles
    mid_angle = pi - ((segment - 0.5) / 3 * pi),
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Create semicircle donut
ggplot(south_sudan_data) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", size = 1) +
  scale_fill_identity() +
  geom_text(aes(x = label_x, y = label_y, label = value),
            size = 6, fontface = "bold") +
  annotate("text", x = 0, y = -0.2, label = "South Sudan", 
           size = 10, fontface = "bold") +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )




# CLEAN AND WORKING VERSION
south_sudan_data <- filtered_data %>%
  filter(COUNTRY == "South Sudan") %>%
  pivot_longer(
    cols = c(geopolitical_impact, incidence, human_impact),
    names_to = "category",
    values_to = "value"
  ) %>%
  mutate(
    color = map_chr(value, value_to_color),
    segment = row_number(),
    # Arc angles
    start_angle = (pi - ((segment - 1) / 3 * pi)) - pi/2,
    end_angle = (pi - (segment / 3 * pi)) - pi/2,
    # Label angles (reverse the segment number)
    mid_angle = pi - (((4 - segment) - 0.5) / 3 * pi),
    label_x = cos(mid_angle) * 1.08,
    label_y = sin(mid_angle) * 1.08
  )

# Create semicircle donut
ggplot(south_sudan_data) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1,
                   start = start_angle, end = end_angle, fill = color),
               color = "white", size = 1) +
  scale_fill_identity() +
  geom_text(aes(x = label_x, y = label_y, label = value),
            size = 6, fontface = "bold") +
  annotate("text", x = 0, y = -0.2, label = "South Sudan", 
           size = 10, fontface = "bold") +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )
