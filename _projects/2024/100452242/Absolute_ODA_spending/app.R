library(shiny)
library(ggplot2)
library(tidyverse)
library(countrycode)
library(showtext)
library(patchwork)

# Add custom fonts
font_add("Riga DEMO Regular", "RigaDEMO-Regular.otf")
font_add("Riga DEMO Bold", "RigaDEMO-Bold.otf")

# Enable showtext
showtext_auto()

# Read data
oda_data <- read.csv("goal17.oda.csv")

ui <- fluidPage(
  titlePanel("Abosolute Spending for ODA"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("selected_year", "Select Year:",
                  min = 1970, max = 2021,
                  value = max(oda_data$year), step = 1, sep = "")
    ),
    mainPanel(
      uiOutput("dynamicPlotUI")
    )
  )
)

server <- function(input, output, session) {

  output$dynamicPlotUI <- renderUI({
    selected_year <- input$selected_year
    oda_filtered <- oda_data |>
      filter(year == selected_year & iso3 != "DAC") |>
      drop_na() |>
      slice(1:20)


    height_per_row <- 30
    plot_height <- max(nrow(oda_filtered) * height_per_row, 800)

    plotOutput("odaPlot", height = paste0(plot_height, "px"))
  })


  output$odaPlot <- renderPlot({
    selected_year <- input$selected_year
    oda_filtered <- oda_data  |>
      filter(year == selected_year & iso3 != "DAC")  |>
      mutate(
        country_label = countrycode(iso3, "iso3c", "country.name")
      ) |>
      arrange(desc(viz_oda_ldc_pctgni))  |>
      select(country = country_label, oda_total = viz_oda_usdmn,
             oda_pct_gni = viz_odagni_pct,
             oda_ldc_total = viz_oda_ldc_usdmn,
             oda_ldc_pct_gni = viz_oda_ldc_pctgni)  |>
      drop_na()  |>
      slice(1:20)

    oda_filtered <- oda_filtered %>%
      mutate(
        value_label = sprintf("%.1f", oda_total / 1000),
        pct_label = paste0("(", sprintf("%.2f", oda_pct_gni), "%)"),
        font_weight = ifelse(oda_ldc_pct_gni >= 0.15, "bold", "plain"),
        fill = ifelse(oda_ldc_pct_gni >= 0.15, "#e56766", "white"),
        color = ifelse(oda_ldc_pct_gni >= 0.15, "white", "#e56766")
      )

    oda_filtered$country <- factor(oda_filtered$country, levels = oda_filtered$country[order(oda_filtered$oda_ldc_pct_gni)])


    p_left <- ggplot(oda_filtered) +
      geom_col(aes(x = -oda_pct_gni, y = country),
               fill = "#0171bc", color="#0171bc", width = 0.4, alpha = 0.8, size=0.3) +
      geom_col(aes(x = -oda_ldc_pct_gni, y = country),
               fill = oda_filtered$fill, color = oda_filtered$color,
               width = 0.4, alpha = 0.8, size=0.5) +
      geom_vline(xintercept = -0.15 , color = "#e56766", size = 0.5) +
      annotate("text", x = -0.15, y = 0.2, label = "Target: .15% to LDCs",
               color = "gray40", size = 6, hjust = 0.5, family = "sans") +
      coord_cartesian(clip = "off") +
      scale_x_continuous(
        breaks = seq(-1, 0, by = 0.25),
        labels = function(x) paste0(abs(x), "%")
      ) +
      labs(x = "Percentage of Gross National Income (%)", y = NULL) +
      theme_minimal() +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(5, 0, 5, 7),
        panel.spacing = unit(0.05, "lines"),
        axis.title.x = element_text(size = 22, face = "bold", color = "black"),
        axis.text.x = element_text(size = 20, face = "bold", color = "black")
      )





    p_right <- ggplot(oda_filtered) +
      geom_col(aes(x = oda_total, y = country),
               fill = "#0171bc", color = "white", width = 0.4, alpha = 0.8) +
      geom_col(aes(x = oda_ldc_total, y = country),
               fill = "#e56766", color = "white", width = 0.4, alpha = 0.8) +
      scale_x_continuous(
        expand = expansion(mult = c(0, 0.05)),
        labels = function(x) x / 10000  # Convert to scaled units
      ) +
      labs(x = "Total Amount (10k Billion USD)", y = NULL) +  # Adjusted title
      theme_minimal() +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0.05, "lines"),
        axis.title.x = element_text(
          family = "Quicksand",    # Font setting
          size = 22,           # Font size
          face = "bold",       # Bold font
          color = "black"      # Font color
        ),
        axis.text.x = element_text(size = 20, face = "bold", color = "black"),
        plot.margin = margin(5, 7, 5, 0)
      ) +
      scale_y_discrete()

    p_middle <- ggplot(oda_filtered) +
      geom_text(aes(x = -0.1, y = country, label = paste0(sprintf("%.2f", oda_pct_gni), "%")),
                size = 8, hjust = 1, color = "#0171bc") +
      geom_text(aes(x = 0.1, y = country, label = sprintf("%.1f", oda_total / 1000)),
                size = 8, hjust = 0, color = "#0171bc") +
      geom_text(
        data = subset(oda_filtered, font_weight == "plain"),
        aes(x = 0, y = country, label = country),
        fontface = "plain", hjust = 0.5, size = 8, family = "Riga DEMO Regular"
      ) +
      geom_text(
        data = subset(oda_filtered, font_weight == "bold"),
        aes(x = 0, y = country, label = country),
        hjust = 0.5, size = 8, family = "Riga DEMO Bold"
      ) +
      coord_cartesian(clip = "off") +
      theme_void() +
      theme(plot.margin = margin(0, 20, 0, 20),
            panel.spacing = unit(0.05, "lines"))

    p_combined <- p_left + p_middle + p_right +
      plot_layout(widths = c(4, 2, 4))

    p_combined
  })
}

shinyApp(ui, server)

