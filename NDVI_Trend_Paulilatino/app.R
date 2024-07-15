library(shiny)
library(leaflet)
library(rgeos)

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
                  group = 'Lim. Amm. Paulilatino'
      ) %>% 
      addPolygons(data = focolai_wgs84,
                  fillOpacity = 0, 
                  color = "red", 
                  weight = 2,
                  group = 'Outbreaks'
      ) %>% 
      addPolygons(data = plots_wgs84,
                  fillOpacity = 0, 
                  color = "yellow", 
                  weight = 2,
                  group = 'plots'
      ) %>% 
      addCircleMarkers(data = samples,
                       lng = ~long,
                       lat = ~lat,
                       fillColor = ~color_factor_circle(samples$positivo),
                       fillOpacity = 0.6,
                       popup = ~paste("AREA:", location, "<br/>CAMPIONE:", id_sample, "<br/>SINTOMATICO:", sin_asin, "<br/>POSITIVITÀ:", positivo),
                       radius = 3,
                       group = 'samples',
                       stroke = FALSE
      ) %>%
      addLegend(pal = color_factor_circle,
                values = samples$positivo,
                title = "Sample Positivity",
                opacity = 0.6,
                position = "bottomright"
      ) %>%
      addPolygons(data = pixel_trend, 
                  fillColor = ~ifelse(slope > 0, "#00ff00", color_factor1(Trnd_Cl)),
                  color = "black",
                  fillOpacity = 0.6,
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                  popup = ~paste("COD:", COD, "<br/>slope:", slope, "<br/>class:", Trnd_Cl, "<br/>Description:", Trnd_Ds),
                  group = 'Crowns',
                  stroke = TRUE,
                  weight = 1
      ) %>%
      addLegend(title="Trend: lm(NDVI ~ Month)",
                pal=color_factor2,
                values=pixel_trend$Trnd_Ds,
                opacity=0.6,
                position="topright"
      ) %>% 
      addFullscreenControl() %>%
      addLayersControl(
        overlayGroups=c("Crowns", "samples", "Lim. Amm. Paulilatino", "Outbreaks","plots"),
        options=layersControlOptions(collapsed=TRUE)
      )
  })
  
  observeEvent(input$plant_code,{
    selected_plant <- pixel_trend[pixel_trend$COD == input$plant_code,]
    
    if(nrow(selected_plant) > 0){
      centroid <- st_centroid(selected_plant$geometry)
      
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
    }
  })
  
  # Aggiungi questo al di fuori dell'observeEvent
  output$plot <- renderPlot({
    # Usa l'input reattivo per selezionare il grafico corretto
    results_list[[input$plant_code]]$plot
  })
}

shinyApp(ui=ui, server=server)


## Only run this example in interactive R sessions
if (interactive()) {
  runGitHub( "gsatta/TrendNdviPaulilatino")
  
  
  
}





rsconnect::setAccountInfo(name='gb0bim-gabriele0giuseppe0antonio-satta',
                          token='5C8CC41E9E0CE3FC7E8D0B4B1BAF0DCF',
                          secret='HAJ1qg4xwu5uC4ZH7ikO8h2NZPMzpuZi9bI2vso0')

library(rsconnect)
rsconnect::deployApp('')
