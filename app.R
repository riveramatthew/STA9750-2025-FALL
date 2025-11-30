# app.R
library(shiny)
library(leaflet)
library(tidyverse)
library(sf)

# Load your processed data
tracts_final <- readRDS("data/tracts_final.rds")

ui <- fluidPage(
  titlePanel("NYC Healthcare Deserts Explorer"),
  sidebarLayout(
    sidebarPanel(
      h4("Filter Desert Tracts"),
      selectInput("borough", "Select Borough:", 
                  choices = c("All", sort(unique(tracts_final$borough)))),
      sliderInput("poverty_min", "Minimum Poverty Rate (%):", 
                  min = 0, max = 100, value = 20, step = 5),
      h5("Desert Tracts Shown:"),
      textOutput("tract_count"),
      hr(),
      p("Total desert tracts: 206 | Total population affected: 535,000")
    ),
    mainPanel(
      leafletOutput("map", height = "600px"),
      hr(),
      h4("Summary Stats"),
      textOutput("summary_stats")
    )
  )
)

server <- function(input, output, session) {
  
  # Filter data reactively
  filtered_data <- reactive({
    data <- tracts_final %>% 
      filter(is_desert == TRUE)
    
    if (input$borough != "All") {
      data <- data %>% filter(borough == input$borough)
    }
    
    data <- data %>% 
      filter(pct_poverty * 100 >= input$poverty_min)
    
    data
  })
  
  # Display count
  output$tract_count <- renderText({
    paste(nrow(filtered_data()), "tracts")
  })
  
  # Summary stats
  output$summary_stats <- renderText({
    data <- filtered_data()
    paste(
      sprintf("Mean Poverty: %.1f%%\n", mean(data$pct_poverty, na.rm = TRUE) * 100),
      sprintf("Mean Nonwhite: %.1f%%\n", mean(data$pct_nonwhite, na.rm = TRUE) * 100),
      sprintf("Population Affected: %s", format(sum(data$population, na.rm = TRUE), big.mark = ","))
    )
  })
  
  # Render interactive map
  output$map <- renderLeaflet({
    data_sf <- filtered_data() %>% st_transform(4326)
    
    pal <- colorFactor(
      palette = c(
        "Low Vulnerability" = "#1f77b4",
        "Single Burden" = "#2ca02c",
        "Double Burden" = "#ff7f0e",
        "Triple Burden" = "#d62728"
      ),
      domain = data_sf$vulnerability_profile
    )
    
    leaflet(data_sf) %>%
      setView(-73.984865, 40.710542, 10.5) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(vulnerability_profile),
        weight = 1,
        color = "#555",
        fillOpacity = 0.7,
        popup = ~paste0(
          "<strong>", NAME, "</strong><br>",
          "Borough: ", borough, "<br>",
          "Vulnerability: ", vulnerability_profile, "<br>",
          "Poverty: ", sprintf("%.1f%%", pct_poverty * 100), "<br>",
          "Nonwhite: ", sprintf("%.1f%%", pct_nonwhite * 100), "<br>",
          "Access: ", sprintf("%.1f%%", perc_covered * 100)
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~vulnerability_profile,
        title = "Vulnerability Profile",
        position = "bottomright"
      ) %>%
      addScaleBar(position = "bottomleft")
  })
}

shinyApp(ui, server)