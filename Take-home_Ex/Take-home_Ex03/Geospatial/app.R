library(shiny)
library(dplyr)
library(readr)
library(tmap)
library(sf)
library(lubridate)
library(spdep)

# Load the data
climate_rainfall3414 <- read_rds("data/climate_rainfall3414.rds")
climate_temperature3414 <- read_rds("data/climate_temperature3414.rds")
climate_windspeed3414 <- read_rds("data/climate_windspeed3414.rds")

# Convert 'date' to Date format
climate_rainfall3414$date <- as.Date(climate_rainfall3414$date, format = "%Y/%m/%d")
climate_temperature3414$date <- as.Date(climate_temperature3414$date, format = "%Y/%m/%d")
climate_windspeed3414$date <- as.Date(climate_windspeed3414$date, format = "%Y/%m/%d")

# Convert to an 'sf' object
climate_rainfall3414 <- st_as_sf(climate_rainfall3414, crs = 3414)
climate_temperature3414 <- st_as_sf(climate_temperature3414, crs = 3414)
climate_windspeed3414 <- st_as_sf(climate_windspeed3414, crs = 3414)

# 1. Function to generate rainfall map
plot_rainfall_map <- function(data, selected_year, selected_month) {
  
  # Aggregate rainfall data by Station, Year, and Month
  aggregated_data <- data %>%
    mutate(Year = year(date), Month = month(date, label = TRUE, abbr = FALSE)) %>%
    group_by(Station, Year, Month) %>%
    summarise(
      Total_Rainfall = sum(`Daily Rainfall Total (mm)`, na.rm = TRUE),
      geometry = first(geometry),  # Preserve unique geometry for each station
      .groups = "drop"
    ) %>%
    st_as_sf(crs = 3414)
  
  # Convert Month to character before filtering
  filtered_data <- aggregated_data %>%
    filter(Year == selected_year, as.character(Month) == as.character(selected_month))
  
  # Handle empty data case
  if (nrow(filtered_data) == 0) {
    return(tm_shape() + tm_title("No data available for the selected year and month."))
  }
  
  # Set interactive map mode
  tmap_mode("view")
  
  # Create map
  map <- tm_shape(filtered_data) +
    tm_bubbles(
      size = "Total_Rainfall",
      fill = "Total_Rainfall",
      col = "black",  # Outline color
      size.scale = tm_scale_continuous(values.scale = c(0.5, 2)),
      fill.scale = tm_scale_intervals(values = "Blues", style = "jenks", n = 5),
      fill.legend = tm_legend(title = "Rainfall Intensity"),
      title.size = "Total Rainfall (mm)"
    ) +
    tm_title(paste("Total Rainfall for", selected_month, selected_year))
  
  return(map)
}

# 2. Function to generate temperature map  
plot_temperature_map <- function(data, selected_year, selected_month) {
  
  # Aggregate temperature data by Station, Year, and Month
  aggregated_data <- data %>%
    mutate(Year = year(date), Month = month(date, label = TRUE, abbr = FALSE)) %>%
    group_by(Station, Year, Month) %>%
    summarise(
      Mean_Temperature = mean(`Mean Temperature (°C)`, na.rm = TRUE),
      geometry = first(geometry),  # Preserve unique geometry for each station
      .groups = "drop"
    ) %>%
    st_as_sf(crs = 3414)  # Ensure it's an sf object
  
  # Filter data for the selected year and month
  filtered_data <- aggregated_data %>%
    filter(Year == selected_year, as.character(Month) == as.character(selected_month))
  
  # Handle empty data case
  if (nrow(filtered_data) == 0) {
    return(tm_shape() + tm_title("No data available for the selected year and month."))
  }
  
  # Set interactive map mode
  tmap_mode("view")
  
  # Create map with bubbles
  map <- tm_shape(filtered_data) +
    tm_bubbles(
      size = "Mean_Temperature",       # Bubble size based on Mean Temperature
      fill = "Mean_Temperature",       # Bubble color based on Mean Temperature
      col = "black",                   # Outline color for bubbles
      size.scale = tm_scale_continuous(
        values.scale = c(0.5, 2)       # Adjust bubble size scaling factor
      ),
      fill.scale = tm_scale_intervals(
        values = "RdYlBu",             # Color palette
        style = "jenks",               # Classification style
        n = 5                          # Number of intervals
      ),
      fill.legend = tm_legend(title = "Temperature (°C)"),
      title.size = "Mean Temperature (°C)"
    ) +
    tm_title(paste("Mean Temperature for", selected_month, selected_year))  # Title for the map
  
  return(map)
}

# 3. Function to generate wind speed map  
plot_windspeed_map <- function(data, selected_year, selected_month) {
  
  # Aggregate wind speed data by Station, Year, and Month
  aggregated_data <- data %>%
    mutate(Year = year(date), Month = month(date, label = TRUE, abbr = FALSE)) %>%
    group_by(Station, Year, Month) %>%
    summarise(
      Mean_Wind_Speed = mean(`Mean Wind Speed (km/h)`, na.rm = TRUE),
      geometry = first(geometry),  # Preserve unique geometry for each station
      .groups = "drop"
    ) %>%
    st_as_sf(crs = 3414)  # Ensure it's an sf object
  
  # Filter data for the selected year and month
  filtered_data <- aggregated_data %>%
    filter(Year == selected_year, as.character(Month) == as.character(selected_month))
  
  # Handle empty data case
  if (nrow(filtered_data) == 0) {
    return(tm_shape() + tm_title("No data available for the selected year and month."))
  }
  
  # Set interactive map mode
  tmap_mode("view")
  
  # Create map with bubbles
  map <- tm_shape(filtered_data) +
    tm_bubbles(
      size = "Mean_Wind_Speed",       # Bubble size based on Mean Wind Speed
      fill = "Mean_Wind_Speed",       # Bubble color based on Mean Wind Speed
      col = "black",                  # Outline color for bubbles
      size.scale = tm_scale_continuous(
        values.scale = c(0.5, 2)      # Adjust bubble size scaling factor
      ),
      fill.scale = tm_scale_intervals(
        values = "YlGnBu",            # Color palette
        style = "jenks",              # Classification style
        n = 5                         # Number of intervals
      ),
      fill.legend = tm_legend(title = "Wind Speed (km/h)"),
      title.size = "Mean Wind Speed (km/h)"
    ) +
    tm_title(paste("Mean Wind Speed for", selected_month, selected_year))  # Title for the map
  
  return(map)
}

# 4. Function to generate total rainfall auto correlation map
localmoran_i_rainfall <- function(data, year, month, k_neighbors = 2) {
  
  data <- data %>%
    mutate(Year = year(date), Month = month(date, label = TRUE, abbr = FALSE)) %>%
    group_by(Station, Year, Month) %>%
    summarise(Total_Rainfall = sum(`Daily Rainfall Total (mm)`, na.rm = TRUE), .groups = "drop")
  
  filtered_data <- data %>%
    filter(Year == year, Month == month)
  
  sf_data <- st_as_sf(filtered_data, coords = c("Longitude", "Latitude"), crs = 3414)
  
  coordinates <- st_coordinates(sf_data)
  coordinates <- as.data.frame(coordinates)
  coordinates[] <- lapply(coordinates, as.numeric)
  
  neighbors <- knearneigh(coordinates, k = k_neighbors)  
  weights <- nb2listw(knn2nb(neighbors), style = "W")  
  
  variable <- filtered_data$Total_Rainfall
  local_moran_result <- localmoran(variable, weights)
  
  filtered_data$Local_Moran_I <- local_moran_result[, 1]  
  
  sf_data$Local_Moran_I <- filtered_data$Local_Moran_I
  
  return(sf_data)
}

plot_rainfall_morani <- function(data, year, month, k_neighbors = 2) {
  tmap_mode("view")
  
  rainfall_morani <- tm_shape(data) +
    tm_bubbles(size = "Total_Rainfall", col = "Local_Moran_I", 
               scale = 3, style = "jenks", palette = "Blues", 
               title.size = "Local Moran's I", 
               title.col = "Local Moran's I",
               popup.vars = c("Station", "Total_Rainfall","Local_Moran_I")) +
    tm_layout(title = paste("Local Indicators of Spatial Association for", month, year))
  
  return(rainfall_morani)
}

# 5. Function to generate mean temperature auto correlation map
localmoran_i_temperature <- function(data, year, month, k_neighbors = 2) {
  
  data <- data %>%
    mutate(Year = year(date), Month = month(date, label = TRUE, abbr = FALSE)) %>%
    group_by(Station, Year, Month) %>%
    summarise(Mean_Temperature = mean(`Mean Temperature (°C)`, na.rm = TRUE), .groups = "drop")
  
  filtered_data <- data %>%
    filter(Year == year, Month == month)
  
  sf_data <- st_as_sf(filtered_data, coords = c("Longitude", "Latitude"), crs = 3414)
  
  coordinates <- st_coordinates(sf_data)
  coordinates <- as.data.frame(coordinates)
  coordinates[] <- lapply(coordinates, as.numeric)
  
  neighbors <- knearneigh(coordinates, k = k_neighbors)  
  weights <- nb2listw(knn2nb(neighbors), style = "W")  
  
  variable <- filtered_data$Mean_Temperature
  local_moran_result <- localmoran(variable, weights)
  
  filtered_data$Local_Moran_I <- local_moran_result[, 1]  
  
  sf_data$Local_Moran_I <- filtered_data$Local_Moran_I
  
  return(sf_data)
}

plot_temperature_morani <- function(data, year, month, k_neighbors = 2) {
  tmap_mode("view")
  
  temperature_morani <- tm_shape(data) +
    tm_bubbles(size = "Mean_Temperature", col = "Local_Moran_I", 
               scale = 3, style = "jenks", palette = "Blues", 
               title.size = "Local Moran's I", 
               title.col = "Local Moran's I",
               popup.vars = c("Station", "Mean_Temperature","Local_Moran_I")) +
    tm_layout(title = paste("Local Indicators of Spatial Association for", month, year))
  
  return(temperature_morani)
}

# 6. Function to generate mean wind speed auto correlation map
localmoran_i_windspeed <- function(data, year, month, k_neighbors = 2) {
  
  data <- data %>%
    mutate(Year = year(date), Month = month(date, label = TRUE, abbr = FALSE)) %>%
    group_by(Station, Year, Month) %>%
    summarise(Mean_Wind_Speed = mean(`Mean Wind Speed (km/h)`, na.rm = TRUE), .groups = "drop")
  
  filtered_data <- data %>%
    filter(Year == year, Month == month)
  
  sf_data <- st_as_sf(filtered_data, coords = c("Longitude", "Latitude"), crs = 3414)
  
  coordinates <- st_coordinates(sf_data)
  coordinates <- as.data.frame(coordinates)
  coordinates[] <- lapply(coordinates, as.numeric)
  
  neighbors <- knearneigh(coordinates, k = k_neighbors)  
  weights <- nb2listw(knn2nb(neighbors), style = "W")  
  
  variable <- filtered_data$Mean_Wind_Speed
  local_moran_result <- localmoran(variable, weights)
  
  filtered_data$Local_Moran_I <- local_moran_result[, 1]  
  
  sf_data$Local_Moran_I <- filtered_data$Local_Moran_I
  
  return(sf_data)
}

plot_windspeed_morani <- function(data, year, month, k_neighbors = 2) {
  tmap_mode("view")
  
  temperature_morani <- tm_shape(data) +
    tm_bubbles(size = "Mean_Temperature", col = "Local_Moran_I", 
               scale = 3, style = "jenks", palette = "Blues", 
               title.size = "Local Moran's I", 
               title.col = "Local Moran's I",
               popup.vars = c("Station", "Mean_Temperature","Local_Moran_I")) +
    tm_layout(title = paste("Local Indicators of Spatial Association for", month, year))
  
  return(windspeed_morani)
}


# UI layout
ui <- fluidPage(
  
  navbarMenu(
    HTML(paste0(
      '<div style="display: inline-flex; align-items: center;">',
      '<img src="google-maps.png" height="15px" style="margin-right: 5px;">',
      'Geospatial Analysis',
      '</div>'
    )),
  
# Exploratory Data Analysis Panel    
  tabPanel("Exploratory Data Analysis",  
    sidebarLayout(
      sidebarPanel(
        selectInput("year", "Select Year:", 
                  choices = unique(year(climate_rainfall3414$date)), 
                  selected = max(year(climate_rainfall3414$date))),
        selectInput("month", "Select Month:", 
                  choices = unique(as.character(month(climate_rainfall3414$date, label = TRUE, abbr = FALSE))), 
                  selected = month.name[1])
      ),
    
      mainPanel(
        tabsetPanel(
          id = "viz_type",
          tabPanel("Total Rainfall", value = "Total Rainfall",
                 tmapOutput("rainfall_map", height = "600px")),
          tabPanel("Mean Temperature", value = "Mean Temperature",
                 tmapOutput("temperature_map", height = "600px")),
          tabPanel("Mean Wind Speed", value = "Mean Wind Speed",
                 tmapOutput("windspeed_map", height = "600px"))
        )
      )
    )
  ),

# Spatial Autocorrelation Panel
  tabPanel("Spatial Autocorrelation",
           sidebarLayout(
             sidebarPanel(
               selectInput("year", "Select Year:", 
                           choices = unique(year(climate_rainfall3414$date)), 
                           selected = max(year(climate_rainfall3414$date))),
               selectInput("month", "Select Month:", 
                           choices = unique(as.character(month(climate_rainfall3414$date, label = TRUE, abbr = FALSE))), 
                           selected = month.name[1])
             ),
             mainPanel(
               tabsetPanel(
                 id = "viz_type",
                 tabPanel("Total Rainfall", value = "Total Rainfall",
                          tmapOutput("rainfall_morani", height = "600px")),
                 tabPanel("Mean Temperature", value = "Mean Temperature",
                          tmapOutput("temperature_morani", height = "600px")),
                 tabPanel("Mean Wind Speed", value = "Mean Wind Speed",
                          tmapOutput("windspeed_morani", height = "600px"))
             )
           )
          )
        ),

))

  
# Server function
server <- function(input, output, session) {
  
  output$rainfall_map <- renderTmap({
    plot_rainfall_map(climate_rainfall3414, as.integer(input$year), input$month)
  })
  
  output$temperature_map <- renderTmap({
    plot_temperature_map(climate_temperature3414, as.integer(input$year), input$month)
  })
  
  output$windspeed_map <- renderTmap({
    plot_windspeed_map(climate_windspeed3414, as.integer(input$year), input$month)
  })
  
  output$rainfall_morani <- renderTmap({
    localmoran_i_rainfall(climate_rainfall3414, as.integer(input$year), input$month)
  })
  
  output$temperature_morani <- renderTmap({
    localmoran_i_temperature(climate_temperature3414, as.integer(input$year), input$month)
  })
  
  output$windspeed_morani <- renderTmap({
    localmoran_i_windspeed(climate_windspeed3414, as.integer(input$year), input$month)
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
