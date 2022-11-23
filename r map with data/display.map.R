#library(shiny)
library(leaflet)
library(leaflegend)
library(dplyr)
library(readr)

free.zone = read.csv("freeZone.csv")
airport <- read_csv("list-of-airports-in-australia.csv", 
                    col_types = cols(latitude_deg = col_double(), 
                                     longitude_deg = col_double()))

mel.airport = airport[airport$iso_region == "AU-VIC" & airport$municipality == "Melbourne", ]
use.airport = mel.airport[mel.airport$type == "large_airport" | mel.airport$type == "medium_airport", ]

free.zone$popup <- paste0('<b>', free.zone$stop, '</b><br>',
                          '<b>Name of the stop: </b>', free.zone$name, '<br>')

use.airport$popup <- paste0('<b>', '<a href =', use.airport$home_link, '>', use.airport$name, '</a>', '</b><br>',
                      '<b>Type of the airport: </b>', use.airport$type, '<br>')

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
     .container-fluid {
			padding-right: 0px;
			padding-left: 0px;
      }"))),
  leafletOutput('map',height="100vh")
  )

server <- function(input, output, session) {
  
  iconSet <- awesomeIconList(
    sign_train=makeAwesomeIcon(icon = 'train',
                              library = "fa",
                              iconColor = 'white',
                              markerColor = 'green',
                              spin = FALSE,
                              squareMarker = TRUE
    ),
    sign_airport=makeAwesomeIcon(icon = 'plane', 
                                 library = "fa",
                                 iconColor = 'white', 
                                 markerColor = 'blue',
                                 spin = FALSE,
                                 squareMarker = TRUE
    )
  )
  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 144.96337, lat = -37.81, zoom = 11) %>%
      addProviderTiles(providers$CartoDB.VoyagerLabelsUnder) %>%
      addPolygons(data = free.zone,
                  lat = ~Latitude, lng = ~Longtitude, 
                  weight = 3,
                  color = "#8ebf88",
                  fillColor = "#d3e3d0",
                  popup=~popup) %>% 
      
      #### airport  ####
      addAwesomeMarkers(data = use.airport,
                        ~longitude_deg, ~latitude_deg,
                          icon=~awesomeIcons(icon='plane', library='fa',
                                             markerColor='blue',
                                             iconColor='#ffffff',
                                             spin = FALSE,
                                             squareMarker = TRUE),
                        label=~name,
                        popup=~popup
        ) %>%
        # #### free zone  ####  
        addAwesomeMarkers(data = free.zone,
                          ~Longtitude, ~Latitude,
                          icon=~awesomeIcons(icon='train', library='fa',
                                             markerColor='green',
                                             iconColor='#ffffff',
                                             spin = FALSE,
                                             squareMarker = TRUE),
                          clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F,color = "green",
                                                                spiderLegPolylineOptions = list(weight = 1.5, color = "green", opacity = 0.5)),
                          label=~name,
                          popup=~popup
        ) %>%
        # Add a vertical legend onto the map
        addLegendAwesomeIcon(iconSet = iconSet,
                             orientation = 'vertical',
                             marker = TRUE,
                             title = htmltools::tags$div(
                               style = 'font-size: 14px;',
                               'Signs of places of interest'),
                             labelStyle = 'font-size: 12px;',
                             position = 'bottomright',
                             group = 'Vertical Legend')
  })
}


# Run the application 
shinyApp(ui = ui, server = server)