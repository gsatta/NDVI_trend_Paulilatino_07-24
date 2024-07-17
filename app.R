library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(leaflet.providers)
library(terra)
library(sf)
library(ggplot2)
library(MASS)
library(mblm)


NDVI_VALUES <- st_read("./OUTPUT/VECTOR/NDVI_DF/merged_data_ndvi.gpkg")

# Imposta l'opzione scipen su un valore elevato per eliminare la notazione esponenziale dei valori
options(scipen = 999)

# Load the shp file of the crowns coefficients values 
pixel_trend <- st_read("./OUTPUT/VECTOR/TREND/pixels_trend.gpkg")

# Load the shp file of the point sample location
samples <- st_read("./INPUT/VECTOR/sample_points_wgs84.gpkg")

# Load the shp file of the administrative boundaries of Paulilatino municipality
lim_paul_wgs84 <- st_read("./INPUT/VECTOR/limite_amministrativo_paulilatino_wgs84.gpkg")

# Load the shp file of the outbreak in the Paulilatino area
focolai_wgs84 <- st_read("./INPUT/VECTOR/FOCOLAI_wgs84.gpkg")

# Load the shp file of the plots location
plots_wgs84 <- st_read("./INPUT/VECTOR/plot_wgs84.gpkg")

# Define a custom color palette for classes
class_palette <- c("p-value > 0.05" = "#00ff00",  # Green
                   "0.05 > p-value > 0.01" = "yellow",  # Yellow-green
                   "0.01 > p-value > 0.001" = "orange",
                   "0.001 > p-value" = "red")

# Create two color factors with the custom palette and class labels
color_factor1 <- leaflet::colorFactor(palette = class_palette, levels = c("0", "1", "2", "3"))

color_factor2 <- leaflet::colorFactor(palette = class_palette, levels = c("Positive trends or trends not significantly different from the null slope", "Trends significantly negative, 0.05 > p-value > 0.01", "Trends significantly negative, 0.01 > p-value > 0.001", "Trends significantly negative, 0.001 > p-value"))

# Create a coloring function for circles
color_factor_circle <- colorFactor(
  palette = c("green", "red"),
  domain = c("+", "-")
)

ui <- fluidPage(
  titlePanel("NDVI trend analysis"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("plant_code", label="Select the plant code", choices = NULL),
      sliderInput("start_year", "Start Year", min = 2018, max = 2022, value = 2018),
      sliderInput("end_year", "End Year", min = 2018, max = 2023, value = 2023),
      actionButton("update_button", "Update Plot")
    ),
    mainPanel(
      leafletOutput("map"),
      plotOutput("ndvi_plot") 
    )
  )
)

server <- function(input, output, session) {
  # Update the selectize input with unique COD values from pixel_trend
  updateSelectizeInput(session, "plant_code", choices = unique(pixel_trend$COD))
  
  # Function to render the NDVI plot
  renderNDVIPlot <- function(selected_cod, start_year, end_year) {
    selected_plant <- NDVI_VALUES[NDVI_VALUES$COD == selected_cod, ]
    
    if (nrow(selected_plant) > 0) {
      # Filter data based on selected years
      data0 <- selected_plant[selected_plant$date >= as.Date(paste0(start_year, "-01-01")) &
                                selected_plant$date <= as.Date(paste0(end_year, "-12-31")), ]
      
      if (nrow(data0) > 0) {
        min_date <- min(data0$date)
        data0$date_num <- as.numeric(data0$date - min_date)
        
        theil_sen_fit <- mblm(ndvi ~ date_num, data = data0)
        
        slope_ts <- theil_sen_fit$coefficients[2]
        intercept_ts <- theil_sen_fit$coefficients[1]
        
        ts <- ts(data0$ndvi, start = c(2018, 1), end = c(2023, 12), frequency = 12)
        
        mk_test <- smk.test(ts)
        
        data0$trend <- intercept_ts + slope_ts * data0$date_num
        
        p <- ggplot(data0, aes(x = date, y = ndvi)) +
          geom_smooth(method = "loess", color = "green", se = FALSE, span = 0.18) +  
          geom_smooth(method = "loess", color = "blue", se = FALSE) +
          geom_line(aes(y = trend), color = "red") +
          geom_point(color = "black", fill = "blue", alpha = 0.5) +
          labs(title = paste("NDVI Trend for COD:", selected_cod),
               x = "Time",
               y = "NDVI") +
          theme_minimal()
        
        output$ndvi_plot <- renderPlot({
          print(p)
        })
        
        centroid <- st_centroid(pixel_trend[pixel_trend$COD == selected_cod, ])
        
        lng <- as.numeric(centroid[[1]][[1]][1])
        lat <- as.numeric(centroid[[1]][[1]][2])
        
        if (is.finite(lng) && is.finite(lat)) {
          proxy <- leafletProxy("map")
          
          proxy %>%
            clearGroup(group='selected_plant') %>%
            addPolygons(data=pixel_trend[pixel_trend$COD == selected_cod, ],
                        fillColor=NA,
                        color="blue",
                        fillOpacity=0,
                        weight=3,
                        group='selected_plant',
                        stroke=TRUE) %>%
            setView(lng=lng, lat=lat, zoom=10)
        }
      } else {
        proxy <- leafletProxy("map")
        proxy %>% clearGroup(group='selected_plant')
        
        showNotification("No data available for the selected years.", duration = 3000)
      }
    } else {
      proxy <- leafletProxy("map")
      proxy %>% clearGroup(group='selected_plant')
    }
  }
  
  # Observe event for map shape click
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    selected_cod <- click$id
    
    # Update the selectize input
    updateSelectizeInput(session, "plant_code", selected = selected_cod)
    
    # Render the plot with default years
    renderNDVIPlot(selected_cod, input$start_year, input$end_year)
  })
  
  # Observe event for selectize input change
  observeEvent(input$plant_code, {
    selected_cod <- input$plant_code
    
    # Render the plot with default years
    renderNDVIPlot(selected_cod, input$start_year, input$end_year)
  })
  
  # Observe event for update button click
  observeEvent(input$update_button, {
    # Render the plot with selected years
    renderNDVIPlot(input$plant_code, input$start_year, input$end_year)
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(maxZoom = 22)) %>%
      addProviderTiles("Esri.WorldImagery", group = "Imagery", options = providerTileOptions()) %>%
      addPolygons(data = lim_paul_wgs84,
                  fillOpacity = 0,
                  color = "black",
                  weight = 2,
                  group = 'Lim. Amm. Paulilatino') %>%
      addPolygons(data = focolai_wgs84,
                  fillOpacity = 0,
                  color = "red",
                  weight = 2,
                  group = 'Outbreaks') %>%
      addPolygons(data = plots_wgs84,
                  fillOpacity = 0,
                  color = "yellow",
                  weight = 2,
                  group = 'Plots') %>%
      addCircleMarkers(data = samples,
                       lng = ~long,
                       lat = ~lat,
                       fillColor = ~color_factor_circle(samples$positivo),
                       fillOpacity = 0.6,
                       popup = ~paste(":", location, "<br/>SAMPLE:", id_sample, "<br/>SYMPTOMATIC:", sin.asin, "<br/>POSITIVITY:", positivo),
                       radius = 3,
                       group = 'Samples',
                       stroke = FALSE) %>%
      addPolygons(data = pixel_trend,
                  fillColor = ~ifelse(slope > 0, "#00ff00", color_factor1(Trend_Class)),
                  color = "black",
                  fillOpacity = 0.6,
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                  popup = ~paste("COD:", COD, "<br/>slope:", slope, "<br/>class:", Trend_Class, "<br/>Description:", Trend_Description),
                  group = 'Crowns',
                  stroke = TRUE,
                  weight = 1,
                  layerId = ~COD) %>%
      addLegend(pal = color_factor_circle,
                values = samples$positivo,
                title = "Sample Positivity",
                opacity = 0.6,
                position = "bottomright") %>%
      addLegend(title = "Trend: lm(NDVI ~ Month)",
                pal = color_factor2,
                values = pixel_trend$Trend_Description,
                opacity = 0.6,
                position = "topright") %>%
      addFullscreenControl() %>%
      addLayersControl(
        overlayGroups = c("Crowns", "Samples", "Study area Boundaries", "Outbreaks", "Plots"),    
        options = layersControlOptions(collapsed = TRUE)
      )
  })
}

shinyApp(ui = ui, server = server)



# library(rsconnect)
# rsconnect::setAccountInfo(name='gb0bim-gabriele0giuseppe0antonio-satta', token='5C8CC41E9E0CE3FC7E8D0B4B1BAF0DCF', secret='HAJ1qg4xwu5uC4ZH7ikO8h2NZPMzpuZi9bI2vso0')
# 
# deployApp()


