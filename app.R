library(shiny)
library(leaflet)
library(tidyverse)

df_sites <- arrow::read_parquet("data/data.parquet")

ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 10, right = 10,
                  textInput("search_sci_name",label = "Enter scientific Name"),
                  gt::gt_output("species_in_area"))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # df <- read_tsv("data/0011257-220831081235567.csv") %>%
  #   select_if(~!all(is.na(.x))) %>%
  #   select_if(~!(length(unique(.x)) == 1)) %>%
  #   drop_na() %>%
  #   mutate(scientificNameshort = map_chr(str_split(scientificName," "),~paste(.x[1:2],collapse = ' '))) %>%
  #   distinct(scientificNameshort,decimalLatitude,decimalLongitude,eventDate,.keep_all=T)
  #
  # df_sites <- df %>%
  #   group_by(decimalLongitude,decimalLatitude,day,month,year) %>%
  #   summarize(species_count = n(),
  #             species_list = paste(scientificNameshort,collapse=','),
  #             gbif_id_list = paste(gbifID,collapse = ',')) %>%
  #   ungroup()
  # species_counts_df <- df %>% count(scientificNameshort,sort=T)

  df_bounds <- reactive({
    if (is.null(input$map_bounds))
      return(df_sites[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(df_sites,
           decimalLatitude >= latRng[1] & decimalLatitude <= latRng[2] &
             decimalLongitude >= lngRng[1] & decimalLongitude <= lngRng[2])
  })

  df_react <- reactive({
    if(!is.null(input$search_sci_name)){
      return(df_sites %>% filter(str_detect(species_list,input$search_sci_name)))
    }else{
      return(df_sites)
    }
  })

  output$map <- renderLeaflet({
      leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addTiles() %>%
      fitBounds(lng1 = 3,lat1 = 4,lng2 = 14,lat2 = 14)
  })

  output$species_in_area <- gt::render_gt({
    df_bound() %>%
      select(species_list) %>%
      separate_rows(species_list,sep = ",") %>%
      count(species_list,sort=T,name = "Count") %>%
      slice_max(Count,n=10)
  })

  observe({
    leafletProxy("map", data = df_react()) %>%
      clearMarkerClusters() %>%
      addMarkers(lng = ~decimalLongitude,
                 lat = ~decimalLatitude,
                 clusterOptions = markerClusterOptions())
  })

}

# Run the application
shinyApp(ui = ui, server = server,options = list(port = 5000))
