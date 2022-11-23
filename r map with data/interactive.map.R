
#library(shiny)
library(leaflet)
library(leaflegend)
library(dplyr)
library(rsconnect)
library(shinyWidgets)

# https://ivassignment3.shinyapps.io/interactiveMap/

free.zone = read.csv("freeZone.csv")
restaurant.data <- read.csv('restaurant.csv')
hotel.data <- read.csv('recommended hotel.csv')
live.events.data <- read.csv('live_music_data_preprocess.csv')
restroom.data <- read.csv('toilet_data_preprocess.csv')


free.zone$popup <- paste0('<b>', free.zone$stop, '</b><br>',
                          '<b>Name of the stop: </b>', free.zone$name, '<br>')

restaurant.data$popup <- paste0('<b>', '<a href =', restaurant.data$website, '>', restaurant.data$name, '</a>', '</b><br>',
                                '<b>Address: </b>', restaurant.data$address, '<br>',
                                '<b>Style: </b>', restaurant.data$restaurant_style, '<br>',
                                '<b>Rating: </b>', restaurant.data$rating, '<br>',
                                '<b>Tel: </b>', restaurant.data$tel, '<br>',
                                '<b>Price: </b>', restaurant.data$price, '<br><br>',
                                "<img src='",restaurant.data$image_url,"' width='300' height='200'>")

hotel.data$popup <- paste0('<b>', '<a href =', hotel.data$website, '>', hotel.data$name, '</a>', '</b><br>',
                           '<b>Address: </b>', hotel.data$address, '<br>',
                           '<b>Rating: </b>', hotel.data$rating, '<br>',
                           '<b>Tel: </b>', hotel.data$tel, '<br>',
                           '<b>Price: </b>', hotel.data$price, '<br><br>',
                           "<img src='",hotel.data$image_url,"' width='300' height='200'>")

live.events.data$popup <- paste0('<b>', '<a href =', live.events.data$website, '>', live.events.data$name, '</a>', '</b><br>',
                                 '<b>Address: </b>', live.events.data$address, '<br>',
                                 '<b>Type: </b>', live.events.data$Accommodation, '<br>')

restroom.data$popup <- paste0('<b>', restroom.data$name, '</b><br>',
                              '<b>Sex use (F): </b>', restroom.data$toilet_female, '<br>',
                              '<b>Sex use (M): </b>', restroom.data$toilet_male, '<br>',
                              '<b>Disabled use: </b>', restroom.data$toilet_wheelchair, '<br>',
                              '<b>Baby facility: </b>', restroom.data$toilet_baby_facil, '<br>')

sidebar_content.map <- sidebarPanel(
  id="sidebar",
  #################
  ## Restaurant ###
  #################
  h4('Restaurant'),
  # Selection of restaurants
  checkboxGroupInput(inputId = "restaurant_style",
                     label = "Dish style",
                     choices = c("Asia" = "Asia",
                                 "Cafe/Brunch" = "cafe/brunch",
                                 "Mordern Australia" = "Mordern Australia",
                                 "Vegeterian" = "Vegeterian",
                                 "West" = "west"),
                     selected = c("Asia", "cafe/brunch", "Mordern Australia",
                                  "Vegeterian", "west")),
  
  # Presents conditions
  checkboxGroupInput(
    inputId = 'rest_price',
    label = 'Price of the restaurant',
    choices = c(sort(unique(restaurant.data$price))),
    selected =  c("$", "$$", "$$$")),
  br(),
  
  #################
  ##### Hotel #####
  #################
  h4('Hotel'),
  # Selection of hotels
  sliderInput(inputId = "hotel_price",
              label = NULL,
              min = min(hotel.data$price),
              max = max(hotel.data$price),
              value = range(hotel.data$price)),
  br(),
  #######################
  ##### Live events #####
  #######################
  h4('Live events'),
  # Selection of events
  selectizeInput(
    inputId = 'events_type',
    label = NULL,
    multiple = TRUE,
    choices = c(sort(unique(live.events.data$Accommodation))),
    selected = c('Bar', 'Hotel'),
    options = list('plugins' = list('remove_button'))),
  br(),
  #######################
  ##### Restrooms #######
  #######################
  h4('Public restrooms'),
  # Selection of restrooms
  checkboxGroupInput(
    inputId = 'baby_facil',
    label = 'Contain baby facility',
    choices = c(sort(unique(restroom.data$toilet_baby_facil))),
    selected = c("yes","no")),
  
  checkboxGroupInput(
    inputId = 'disable',
    label = 'Disable use',
    choices = c(sort(unique(restroom.data$toilet_wheelchair))),
    selected =c("yes","no")),
  width = 2,height="100vh"
)

main_content.map <- mainPanel(
  leafletOutput('map',height="calc(100vh - 50px)"),width = 10,
  h3("Clicking the buttons on the sidebar first, then select the fields on the right-up corner on the map. ")
)

map_tab <- tabPanel(
  br(),
  sidebarLayout(
    sidebar_content.map, main_content.map
  )
)


ui <- fluidPage(
  
  setBackgroundColor(
    color = c("#303333")
  ),
  setSliderColor(
    color = c("#da9d40"),
    sliderId = 1
  ),
  tags$head(
    tags$style(HTML("
      h3{
      height:20px;
      }
      h4 {
        font-size:20px;
        font-weight:800;
        color: #da9d40;
        background-color: #303333;
      }
      #sidebar {
            background-color: #303333;
            opcity:0.5;
            font-size:16px;
            border:none;
            margin:0;
            padding:0;
            padding-top:5;
      }
      #restaurant_style {
        margin-left:30px;
      }
      #rest_price{
        margin-left:30px;
      }
      #hotel_price{
        margin-left:30px;
        padding-left:30px;
      }
      #events_type{
        margin-left:30px;
        padding-left:30px;
      }
      #baby_facil{
        margin-left:30px;
      }
      #disable{
        margin-left:30px;
      }
      .option[data-value=Bar], .item[data-value=Bar] {
        background: #303333 !important;
        color: #da9d40 !important;
      }
      .option[data-value=Hotel], .item[data-value=Hotel] {
        background: #303333 !important;
        color: #da9d40 !important;
      }
      .option[data-value=Multi-use], .item[data-value=Multi-use] {
        background: #303333 !important;
        color: #da9d40 !important;
      }
      .option[data-value=Nightclub], .item[data-value=Nightclub] {
        background: #303333 !important;
        color: #da9d40 !important;
      }
      .option[data-value=Theatre], .item[data-value=Theatre]{
        background: #303333 !important;
        color: #da9d40 !important;
      }
      .option[data-value=Venue], .item[data-value=Venue]{
        background: #303333 !important;
        color: #da9d40 !important;
      }
      #baby_facil {
        background-color: #303333;
      }
      
      #disable {
        background-color: #303333;
      }
      body, label, input, button, select { 
          font-family: 'Times New Roman';
          color: #da9d40;
         
      }
   
      .leaflet-control-layers-base{font-family: 'Times New Roman'; font-size: 16px;color:#303333 !important;padding:10 10 10 0;opacity:0.5;}
      .leaflet-control-layers-overlays{font-family: 'Times New Roman'; font-size: 16px;color:#303333 !important;margin:20px;}
      "))),
  map_tab
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  iconSet <- awesomeIconList(
    # sign_train=makeAwesomeIcon(icon = 'train', 
    #                           library = "fa",
    #                           iconColor = 'white', 
    #                           markerColor = 'black',
    #                           spin = FALSE,
    #                           squareMarker = TRUE,
    # ),
    Restaurant=makeAwesomeIcon(icon = 'cutlery', 
                               library = "fa",
                               iconColor = 'white', 
                               markerColor = 'green',
                               spin = FALSE,
                               squareMarker = TRUE
    ),
    Hotel=makeAwesomeIcon(icon = 'bed', 
                          library = "fa",
                          iconColor = 'white', 
                          markerColor = 'orange',
                          spin = FALSE,
                          squareMarker = TRUE
    ),
    Live_events=makeAwesomeIcon(icon = 'music', 
                                library = "fa",
                                iconColor = 'white', 
                                markerColor = 'pink',
                                spin = FALSE,
                                squareMarker = TRUE
    ),
    Public_restrooms=makeAwesomeIcon(icon = 'transgender', 
                                     library = "fa",
                                     iconColor = 'white', 
                                     markerColor = 'blue',
                                     spin = FALSE,
                                     squareMarker = TRUE
    )
  )
  
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 144.96337, lat = -37.81, zoom = 14) %>%
      addProviderTiles(providers$CartoDB.VoyagerLabelsUnder) %>%
      addPolygons(data = free.zone,
                  lng = ~Longtitude, lat = ~Latitude, 
                  weight = 5,
                  color = "#cdfc7c",
                  fillColor = "transparent", popup=~popup) %>% 
      
      # #### free zone ####  
    # addAwesomeMarkers(data = free.zone,
    #                     ~Longtitude, ~Latitude,
    #                     icon=~awesomeIcons(icon='train', library='fa',
    #                       markerColor='black',
    #                       iconColor='#ffffff',
    #                       spin = FALSE,
    #                       squareMarker = TRUE),
    #                     # clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
    #                     label=~name,
    #                     popup=~popup
    #   ) %>%
    
    #### restaurant ####
    addAwesomeMarkers(data = filtered.restaurant(),
                      ~log, ~lat,
                      icon=~awesomeIcons(icon='cutlery', library='fa',
                                         markerColor='green',
                                         iconColor='#fff',
                                         spin = FALSE,
                                         squareMarker = TRUE),
                      clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                      label=~name,
                      popup=~popup,
                      group = "Restaurant"
    ) %>%

    #   ###### hotel #####
    addAwesomeMarkers(data = filtered.hotel(),
                      ~log, ~lat,
                      icon=~awesomeIcons(icon='bed', library='fa',
                                         markerColor='orange',
                                         iconColor='#fff',
                                         spin = FALSE,
                                         squareMarker = TRUE),
                      clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                      label=~name,
                      popup=~popup,
                      group = "Hotel"
    ) %>%
    
    #   ###### live events #####
    addAwesomeMarkers(data = filtered.events(),
                      ~log, ~lat,
                      icon=~awesomeIcons(icon='music', library='fa',
                                         markerColor='pink',
                                         iconColor='#fff',
                                         spin = FALSE,
                                         squareMarker = TRUE),
                      clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                      label=~name,
                      popup=~popup,
                      group = "Live events"
    ) %>%
      ###### restroom #####
    addAwesomeMarkers(data = filtered.restroom(),
                      ~log, ~lat,
                      icon=~awesomeIcons(icon='transgender', library='fa',
                                         markerColor='blue',
                                         iconColor='#fff',
                                         spin = FALSE,
                                         squareMarker = TRUE),
                      clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                      label=~name,
                      popup=~popup,
                      group = "Restrooms"
    ) %>%
      # Add a vertical legend onto the map
      addLegendAwesomeIcon(iconSet = iconSet,
                           orientation = 'vertical',
                           marker = TRUE,
                           title = htmltools::tags$div(
                             style = "font-size: 18px;margin:15px;
                               font-family: 'Times New Roman'",
                             'Signs of places of interest'),
                           labelStyle = "font-size: 14px;font-family:'Times New Roman'",
                           position = 'bottomright',
                           group = 'Vertical Legend') %>%
      # Layers control
      addLayersControl(
        overlayGroups = c("Restaurant", "Hotel", "Live events", "Restrooms"),
        options = layersControlOptions(collapsed = FALSE)
      )                  
  })
  
  # Filter data for selection
  ###### restaurant #####
  filtered.restaurant <- reactive({
    if ((is.null(input$rest_price) == TRUE) & (is.null(input$restaurant_style) == TRUE)) {
      restaurant.data
    } 
    else if (is.null(input$restaurant_style) == TRUE) {
      print(111)
      restaurant.data[restaurant.data$price %in% input$rest_price, ]
    } 
    else if (is.null(input$rest_price) == TRUE) {
      print(222)
      restaurant.data[restaurant.data$restaurant_style %in% input$restaurant_style, ]
    }
    else {
      restaurant.data[(restaurant.data$restaurant_style %in% input$restaurant_style) & (restaurant.data$price %in% input$rest_price), ]
    }
  })
  
  ###### hotel #####
  filtered.hotel <- reactive({
    hotel.data[(hotel.data$price <= input$hotel_price[2]) & (hotel.data$price >= input$hotel_price[1]), ]
  })
  
  ###### live events #####
  observeEvent(input$sitemap_marker_click, {
    click <- input$sitemap_marker_click
    updateSelectInput(session, "selectedSites", 
                      selected = c(click$id[!click$id %in% input$selectedSites], 
                                   input$selectedSites[input$selectedSites != click$id]))
  })
  
  filtered.events <- reactive({
    
    if (is.null(input$events_type) == TRUE) {
      live.events.data
    } 
    else {
      live.events.data[live.events.data$Accommodation %in% input$events_type, ]
    }
  })
  
  
  ###### restroom #####
  filtered.restroom <- reactive({
    
    if ((is.null(input$baby_facil) == TRUE) & (is.null(input$disable) == TRUE)) {
      restroom.data
    } 
    else if (is.null(input$baby_facil) == TRUE) {
      restroom.data[restroom.data$toilet_wheelchair %in% input$disable, ]
    }
    else if (is.null(input$disable) == TRUE) {
      restroom.data[restroom.data$toilet_baby_facil %in% input$baby_facil, ]
    }
    else {
      restroom.data[(restroom.data$toilet_baby_facil %in% input$baby_facil) | (restroom.data$toilet_wheelchair %in% input$disable), ]
    }
  })}

# Run the application 
shinyApp(ui = ui, server = server)
