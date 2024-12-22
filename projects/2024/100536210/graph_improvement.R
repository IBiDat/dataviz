library(dplyr)
library(plotly)
library(tibble)
library(tidyverse)
library(ggplot2)
library(readr)
library(shiny)

goal_12 <- read_delim(file = "~/Documents/dataviz/projects/2024/100536210/goal12.cwon.csv", delim = ";", col_names = TRUE)


income_level <- tibble(
  countryname = c(
    "Albania", "United Arab Emirates", "Argentina",
    "Armenia", "Australia", "Austria",
    "Azerbaijan", "Burundi", "Belgium",
    "Benin", "Burkina Faso", "Bangladesh",
    "Bulgaria", "Bahrain", "Bosnia and Herzegovina",
    "Belarus", "Belize", "Bolivia",
    "Brazil", "Botswana", "Central African Republic",
    "Canada", "Switzerland", "Chile",
    "China", "Côte d'Ivoire", "Cameroon",
    "Dem. Rep. Congo", "Congo", "Colombia",
    "Comoros", "Costa Rica", "Czech Republic",
    "Germany", "Djibouti", "Denmark",
    "Dominican Republic", "Ecuador", "Egypt",
    "Spain", "Estonia", "Ethiopia",
    "Finland", "France", "Gabon",
    "United Kingdom", "Georgia", "Ghana",
    "Guinea", "The Gambia", "Greece",
    "Guatemala", "Guyana", "High income",
    "Honduras", "Croatia", "Haiti",
    "Hungary", "Indonesia", "India",
    "Ireland", "Iran", "Iraq",
    "Iceland", "Italy", "Jamaica",
    "Jordan", "Japan", "Kazakhstan",
    "Kenya", "Kyrgyz Republic", "Cambodia",
    "Korea", "Kuwait", "Lao PDR",
    "Lebanon", "Liberia", "Low income",
    "Sri Lanka", "Lower middle income", "Lesotho",
    "Lithuania", "Luxembourg", "Latvia",
    "Morocco", "Moldova", "Madagascar",
    "Maldives", "Mexico", "North Macedonia",
    "Mali", "Malta", "Mongolia",
    "Mozambique", "Mauritania", "Mauritius",
    "Malawi", "Malaysia", "Namibia",
    "Niger", "Nigeria", "Nicaragua",
    "Netherlands", "Norway", "Nepal",
    "Oman", "Pakistan", "Panama",
    "Peru", "Philippines", "Papua New Guinea",
    "Poland", "Portugal", "Paraguay",
    "West Bank and Gaza", "Qatar", "Romania",
    "Russia", "Rwanda", "Saudi Arabia",
    "Senegal", "Singapore", "Solomon Islands",
    "Sierra Leone", "El Salvador", "Suriname",
    "Slovak Republic", "Slovenia", "Sweden",
    "Eswatini", "Chad", "Togo",
    "Thailand", "Tajikistan", "Turkmenistan",
    "Trinidad and Tobago", "Tunisia", "Turkey",
    "Tanzania", "Uganda", "Ukraine",
    "Upper middle income", "Uruguay", "United States",
    "Venezuela", "Vietnam", "Yemen",
    "South Africa", "Zambia", "Zimbabwe"),
  income_level = c(
    "Upper Middle Income", "High Income", "Upper Middle Income",
    "Upper Middle Income", "High Income", "High Income",
    "Upper Middle Income", "Low Income", "High Income",
    "Low Income", "Low Income", "Lower Middle Income",
    "Upper Middle Income", "High Income", "Upper Middle Income",
    "Upper Middle Income", "Upper Middle Income", "Lower Middle Income",
    "Upper Middle Income", "Upper Middle Income", "Low Income",
    "High Income", "High Income", "Upper Middle Income",
    "Upper Middle Income", "Lower Middle Income", "Lower Middle Income",
    "Low Income", "Lower Middle Income", "Upper Middle Income",
    "Lower Middle Income", "Upper Middle Income", "High Income",
    "High Income", "Lower Middle Income", "High Income",
    "Upper Middle Income", "Upper Middle Income", "Lower Middle Income",
    "High Income", "High Income", "Low Income",
    "High Income", "High Income", "Upper Middle Income",
    "High Income", "Upper Middle Income", "Lower Middle Income",
    "Low Income", "Low Income", "High Income",
    "Upper Middle Income", "Upper Middle Income", "High Income",
    "Lower Middle Income", "High Income", "Lower Middle Income",
    "High Income", "Lower Middle Income", "Lower Middle Income",
    "High Income", "Lower Middle Income", "Upper Middle Income",
    "High Income", "High Income", "Upper Middle Income",
    "Upper Middle Income", "High Income", "Upper Middle Income",
    "Lower Middle Income", "Lower Middle Income", "Lower Middle Income",
    "High Income", "High Income", "Lower Middle Income",
    "Lower Middle Income", "Low Income", "Low Income",
    "Lower Middle Income", "Lower Middle Income", "Lower Middle Income",
    "High Income", "High Income", "High Income",
    "Lower Middle Income", "Upper Middle Income", "Low Income",
    "Upper Middle Income", "Upper Middle Income", "Upper Middle Income",
    "Low Income", "High Income", "Lower Middle Income",
    "Low Income", "Lower Middle Income", "Upper Middle Income",
    "Low Income", "Upper Middle Income", "Upper Middle Income",
    "Low Income", "Lower Middle Income", "Lower Middle Income",
    "High Income", "High Income", "Lower Middle Income",
    "High Income", "Lower Middle Income", "High Income",
    "Upper Middle Income", "Lower Middle Income", "Lower Middle Income",
    "High Income", "High Income", "Upper Middle Income",
    "Lower Middle Income", "High Income", "High Income",
    "Upper Middle Income", "Low Income", "High Income",
    "Lower Middle Income", "High Income", "Lower Middle Income",
    "Low Income", "Lower Middle Income", "Upper Middle Income",
    "High Income", "High Income", "High Income",
    "Lower Middle Income", "Low Income", "Low Income",
    "Upper Middle Income", "Lower Middle Income", "Upper Middle Income",
    "High Income", "Lower Middle Income", "Upper Middle Income",
    "Lower Middle Income", "Low Income", "Lower Middle Income",
    "Upper Middle Income", "High Income", "High Income",
    "Lower Middle Income", "Lower Middle Income", "Low Income",
    "Upper Middle Income", "Low Income", "Lower Middle Income"
  ))

goal_12 <- goal_12 |> left_join(income_level, by = "countryname")


new_breaks_x <- c(-91, -75, -50, 0, 100, 300, 1000)
new_mapped_x <- c(-300, -180, -90, 0, 90, 180, 300)


custom_map_x <- function(value) {
  if (value < min(new_breaks_x)) {
    mapped <- new_mapped_x[1] - 20
  } else if (value > max(new_breaks_x)) {
    mapped <- new_mapped_x[length(new_mapped_x)] + 20
  } else {
    mapped <- approx(x = new_breaks_x, y = new_mapped_x, xout = value, rule = 2)$y
  }
  return(mapped)
}

goal_12 <- goal_12 |> mutate(mapped_growth_nca = purrr::map_dbl(growth_nca, custom_map_x))

new_breaks_y <- c(-45, 0, 100, 300, 1000)
new_mapped_y <- c(-150, 0, 250, 600, 1000)

custom_map_y <- function(value) {
  if (value < min(new_breaks_y)) {
    mapped <- new_mapped_y[1] - 20
  } else if (value > max(new_breaks_y)) {
    mapped <- new_mapped_y[length(new_mapped_y)] + 50
  } else {
    mapped <- approx(x = new_breaks_y, y = new_mapped_y, xout = value, rule = 2)$y
  }
  return(mapped)
}


goal_12 <- goal_12 |> mutate(mapped_growth_gdp = purrr::map_dbl(growth_gdp, custom_map_y))

ui <- fluidPage(
  fluidRow(
    column(
      width = 12,
      h2(
        "Growth in natural capital per and GDP per capita (%), 1996-2018",
        style = "text-align: center; margin-bottom: -50px; font-size: 18px;"))),
  fluidRow(
    column(
      width = 12,
      selectInput("selected_country",
                  "Search for a country:",
                  choices = c("All", unique(goal_12$countryname)),
                  selected = "All",
                  selectize = TRUE))),
  fluidRow(
    column(
      width = 6,
      plotlyOutput("low_income_plot", height = "350px")),
    column(
      width = 6,
      plotlyOutput("lower_middle_income_plot", height = "350px"))),
  fluidRow(
    column(
      width = 6,
      plotlyOutput("upper_middle_income_plot", height = "350px")),
    column(
      width = 6,
      plotlyOutput("high_income_plot", height = "350px"))))



server <- function(input, output, session) {
  render_income_plot <- function(income_level_filter, income_level_name) {
    renderPlotly({
      selected_country <- input$selected_country
      filtered_data <- goal_12 |> filter(income_level == income_level_filter)
      if (selected_country != "All") {
        filtered_data <- filtered_data |> filter(countryname == selected_country)}

      fig <- plot_ly() |>
        add_trace(
          data = filtered_data,
          x = ~mapped_growth_nca,
          y = ~mapped_growth_gdp,
          type = 'scatter',
          mode = 'lines',
          line = list(
            color = ~case_when(
              selected_country == "All" ~ "#cececc",
              income_level == "Low Income" ~ "#aa2626",
              income_level == "Lower Middle Income" ~ "#d36f8e",
              income_level == "Upper Middle Income" ~ "#7b8ad0",
              income_level == "High Income" ~ "#1a607d",
              TRUE ~ "#cececc"),
            width = ~case_when(
              selected_country == "All" ~ 0.75,
              TRUE ~ 3)),
          showlegend = FALSE,
          split = ~countryname,
          hoverinfo = 'none')

      fig <- fig |>
        add_trace(
          data = filtered_data |> filter(year == 2018),
          x = ~mapped_growth_nca,
          y = ~mapped_growth_gdp,
          type = 'scatter',
          mode = 'markers',
          marker = list(
            size = 8.5,
            color = ~case_when(
              selected_country == "All" ~ case_when(
                income_level == "Low Income" ~ "#aa2626",
                income_level == "Lower Middle Income" ~ "#d36f8e",
                income_level == "Upper Middle Income" ~ "#7b8ad0",
                income_level == "High Income" ~ "#1a607d",
                TRUE ~ "grey"),
              income_level == "Low Income" ~ "#aa2626",
              income_level == "Lower Middle Income" ~ "#d36f8e",
              income_level == "Upper Middle Income" ~ "#7b8ad0",
              income_level == "High Income" ~ "#1a607d",
              TRUE ~ "grey"),
            line = list(color = "white", width = 1)),
          text = ~paste(
            "<span style='font-size:12px;'>", toupper(countryname), ", ", year, "</span><br>",
            "<span style='font-size:11px;'>", sprintf("%.0f%%", growth_gdp), "</span><br>",
            "<span style='color:#6c757d; font-size:10px;'>Growth in GDP</span><br>",
            "<span style='font-size:12px;'>", sprintf("%.0f%%", growth_nca), "</span><br>",
            "<span style='color:#6c757d; font-size:10px;'>Growth in natural capital</span>"),
          hoverinfo = 'text',
          showlegend = FALSE)

      fig <- fig |>
        add_trace(
          data = filtered_data |>  filter(year != 2018),
          x = ~mapped_growth_nca,
          y = ~mapped_growth_gdp,
          type = 'scatter',
          mode = 'markers',
          marker = list(
            size = 5.5,
            color = ~case_when(
              selected_country == "All" ~ "rgba(255, 255, 255, 0)",
              income_level == "Low Income" ~ "#aa2626",
              income_level == "Lower Middle Income" ~ "#d36f8e",
              income_level == "Upper Middle Income" ~ "#7b8ad0",
              income_level == "High Income" ~ "#1a607d",
              TRUE ~ "grey"),
            line = list(color = "white", width = 0.8)),
          text = ~case_when(
            selected_country == "All" ~ "",
            TRUE ~ paste(
              "<span style='font-size:12px;'>", toupper(countryname), ", ", year, "</span><br>",
              "<span style='font-size:11px;'>", sprintf("%.0f%%", growth_gdp), "</span><br>",
              "<span style='color:#6c757d; font-size:10px;'>Growth in GDP</span><br>",
              "<span style='font-size:12px;'>", sprintf("%.0f%%", growth_nca), "</span><br>",
              "<span style='color:#6c757d; font-size:10px;'>Growth in natural capital</span>")),
          hoverinfo = 'text',
          showlegend = FALSE,
          visible = ~ifelse(selected_country == "All", "legendonly", "visible"))


      shapes <- list()
      if (income_level_filter == "Low Income") {
        shapes <- list(
          list(
            type = "circle",
            xref = "paper",
            yref = "paper",
            x0 = 0.429,
            y0 = -0.127,
            x1 = 0.44,
            y1 = -0.107,
            fillcolor = "#aa2626",
            line = list(color = "#aa2626")))}
      else if (income_level_filter == "Lower Middle Income") {
        shapes <- list(
          list(
            type = "circle",
            xref = "paper",
            yref = "paper",
            x0 = 0.437,
            y0 = -0.127,
            x1 = 0.448,
            y1 = -0.107,
            fillcolor = "#d36f8e",
            line = list(color = "#d36f8e")))}
      else if (income_level_filter == "Upper Middle Income") {
        shapes <- list(
          list(
            type = "circle",
            xref = "paper",
            yref = "paper",
            x0 = 0.405,
            y0 = -0.13,
            x1 = 0.416,
            y1 = -0.11,
            fillcolor = "#7b8ad0",
            line = list(color = "#7b8ad0")))}
      else if (income_level_filter == "High Income") {
        shapes <- list(
          list(
            type = "circle",
            xref = "paper",
            yref = "paper",
            x0 = 0.444,
            y0 = -0.13,
            x1 = 0.455,
            y1 = -0.11,
            fillcolor = "#1a607d",
            line = list(color = "#1a607d")))}


      fig <- fig |>
        layout(
          plot_bgcolor = "#f6f5f3",
          paper_bgcolor = "#f6f5f3",
          xaxis = list(
            title = list( text = ""),
            tickvals = new_mapped_x,
            ticktext = c("-91%", "-75%", "-50%", "0%", "+100%", "+300%", "+1000%"),
            range = c(min(new_mapped_x) - 50, max(new_mapped_x) + 50),
            showline = FALSE,
            zeroline = TRUE,
            tickangle = 0,
            tickfont = list(size = 8)),
          yaxis = list(
            title = list(text = ""),
            tickvals = new_mapped_y,
            ticktext = c("-50%", "0%", "+100%", "+300%", "+1000%"),
            range = c(min(new_mapped_y) - 20, max(new_mapped_y) + 80),
            showline = FALSE,
            zeroline = TRUE,
            tickfont = list(size = 8)),
          hoverlabel = list(bgcolor = "white", font = list(color = "black"), align = "left"),
          margin = list(t = 10, b = 70, l = 75, r = 0))

      fig <- fig |>
        layout(
          shapes = shapes,
          annotations = list(
            list(
              x = -0.11,
              y = 1,
              text = if (income_level_filter == "Low Income")
                "Growth in GDP →" else "",
              showarrow = FALSE,
              font = list(size = 12, color = "black", weight = "bold"),
              xref = "paper",
              yref = "paper",
              textangle = -90),
            list(
              x = 0.99,
              y = -0.15,
              text = if (income_level_filter == "High Income")
                "Growth in natural capital →" else "",
              showarrow = FALSE,
              font = list(size = 12, color = "black", weight = "bold"),
              xref = "paper",
              yref = "paper"),
            list(
                x = 0.5,
                y = -0.145,
                text = if (income_level_filter == "Low Income")
                  "Low Income" else "",
                showarrow = FALSE,
                font = list(size = 10, color = "black", weight = "bold"),
                xref = "paper",
                yref = "paper"),
             list(
                x = 0.55,
                y = -0.145,
                text = if (income_level_filter == "Lower Middle Income")
                  "Lower Middle Income" else "",
                showarrow = FALSE,
                font = list(size = 10, color = "black", weight = "bold"),
                xref = "paper",
                yref = "paper"),
             list(
                x = 0.52,
                y = -0.15,
                text = if (income_level_filter == "Upper Middle Income")
                  "Upper Middle Income" else "",
                showarrow = FALSE,
                font = list(size = 10, color = "black", weight = "bold"),
                xref = "paper",
                yref = "paper"),
             list(
                x = 0.52,
                y = -0.15,
                text = if (income_level_filter == "High Income")
                "High Income" else "",
                showarrow = FALSE,
                font = list(size = 10, color = "black", weight = "bold"),
                xref = "paper",
                yref = "paper")))

          fig <- fig |>
            layout(
              annotations = list(
             list(
                x = 0.1,
                y = 0.85,
                text = "UNSUSTAINABLE GROWTH",
                showarrow = FALSE,
                font = list(size = 12, color = "#706f7d", weight = "bold"),
                xref = "paper",
                yref = "paper"),
            list(
                x = 0.9,
                y = 0.85,
                text = "SUSTAINABLE GROWTH",
                showarrow = FALSE,
                font = list(size = 12, color = "#706f7d", weight = "bold"),
                xref = "paper",
                yref = "paper"),
            list(
               x = 0.5,
               y = 0.028,
               text = "NEGATIVE GROWTH",
               showarrow = FALSE,
               font = list(size = 12, color = "#706f7d", weight = "bold"),
               xref = "paper",
               yref = "paper")))
      fig
    })}

  output$low_income_plot <- render_income_plot("Low Income", "Low Income")
  output$lower_middle_income_plot <- render_income_plot("Lower Middle Income", "Lower Middle Income")
  output$upper_middle_income_plot <- render_income_plot("Upper Middle Income", "Upper Middle Income")
  output$high_income_plot <- render_income_plot("High Income", "High Income")
}

shinyApp(ui = ui, server = server)
