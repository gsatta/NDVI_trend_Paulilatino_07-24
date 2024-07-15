library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(leaflet.providers)
library(terra)
library(sf)


# Load the list of the Theil-Sen coefficients an MK Test results.
results_list <- readRDS("./OUTPUT/LIST/results_list.rds")

# Imposta l'opzione scipen su un valore elevato per eliminare la notazione esponenziale dei valori
options(scipen = 999)

# Load the shp file of the crowns coefficients values 
pixel_trend <- st_read("./OUTPUT/VECTOR/TREND/pixels_trend.gpkg")

# Load the shp file of the point sample location
samples <- st_read("./INPUT/VECTOR/sample_points_wgs84.gpkg")

# Load the shp file of the administrative boundaries of Paulilatino municipality
lim_paul_wgs84 <- st_read("./INPUT/VECTOR/limite_amministrativo_paulilatino_wgs84.gpkg")

# Load the shp file of the outbreack in the Paulilatino area
focolai_wgs84 <- st_read("./INPUT/VECTOR/FOCOLAI_wgs84.gpkg")

# Load the shp file of the plots location
plots_wgs84 <- st_read("./INPUT/VECTOR/plot_wgs84.gpkg")

# Define a custom color palette for classes
class_palette <- c("p-value > 0.05" = "#00ff00",  # Green
                   "0.05 > p-value > 0.01" = "yellow",  # Yellow-green
                   "0.01 > p-value > 0.001" = "orange",
                   "0.001 > p-value" = "red")

# Create two color factor with the custom palette and class labels
color_factor1 <- leaflet::colorFactor(palette = class_palette, levels = c("0", "1", "2", "3"))

color_factor2 <- leaflet::colorFactor(palette = class_palette, levels = c("Positive trends or trends not significantly different from the null slope", "Trends significantly negative, 0.05 > p-value > 0.01", "Trends significantly negative, 0.01 > p-value > 0.001", "Trends significantly negative, 0.001 > p-value"))

# Create a colouring function for circles
color_factor_circle <- colorFactor(
  palette = c("green", "red"),
  domain = c("+", "-")
)

ui <- fluidPage(
  titlePanel("NDVI trend analysis"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("plant_code", label="Select the plant code", choices = NULL),
      plotOutput("plot")  # Sposta questo qui per visualizzare il grafico sotto la selezione
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

server <- function(input, output, session) {
  updateSelectizeInput(session, "plant_code", choices = unique(pixel_trend$COD))
  
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
                  weight = 1
      ) %>%
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
        overlayGroups = c("Crowns", "Samples", "Study area Boundaries", "Outbreaks", "Plots"),    # Add the new group
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  
  observeEvent(input$plant_code,{
    selected_plant <- pixel_trend[pixel_trend$COD == input$plant_code,]
    
    if(nrow(selected_plant) > 0){
      centroid <- st_centroid(selected_plant)
      
      lng <- as.numeric(centroid[[1]][[1]][1])
      lat <- as.numeric(centroid[[1]][[1]][2])
      
      proxy <- leafletProxy("map")
      
      proxy %>% 
        clearGroup(group='selected_plant') %>%
        addPolygons(data=selected_plant,
                    fillColor=NA,
                    color="blue",
                    fillOpacity=0,
                    weight=3,
                    group='selected_plant',
                    stroke=TRUE) %>%
        setView(lng=lng, lat=lat, zoom=10)
    } else {
      # Handle case where selected_plant is empty or not found
      proxy <- leafletProxy("map")
      proxy %>% clearGroup(group='selected_plant')
      
      # Optionally show a message or handle the case in another way
      showNotification("Selected plant not found or has no data.", duration = 3000)
    }
  })
}

shinyApp(ui=ui, server=server)

library(rsconnect)
rsconnect::setAccountInfo(name='gb0bim-gabriele0giuseppe0antonio-satta', token='5C8CC41E9E0CE3FC7E8D0B4B1BAF0DCF', secret='HAJ1qg4xwu5uC4ZH7ikO8h2NZPMzpuZi9bI2vso0')

deployApp()


