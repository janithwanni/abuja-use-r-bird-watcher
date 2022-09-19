library(shiny)
library(leaflet)
library(lubridate)
library(reactlog)
reactlog_enable()

df <- arrow::read_parquet(here::here("data/data.parquet")) %>%
  dplyr::filter(year == 2021)

ui <- bootstrapPage(
  tags$style(type = "text/css",
             "html, body {width:100%;height:100%}"),
  # front end interface
  leafletOutput("map",
                width="100%",
                height="100%"),
  absolutePanel(bottom = 10,left = 10,
                style = "background-color: rgba(255,255,255,0.7);padding: 10px 30px 10px 30px;border-radius: 20px;",
                sliderInput(
                  "day_month",
                  "Select Day of year",
                  min = as.Date("2021-01-01","%Y-%m-%d"),
                  max = as.Date("2021-12-31","%Y-%m-%d"),
                  value=c(as.Date("2021-01-01"),
                          as.Date("2021-12-31")),
                  timeFormat="%Y-%m-%d"))
)

server <- function(input, output, session) {
  # back end logic
  data_reactive <- reactive({
    df %>% tidyr::unite("sight_date",
                        year,month,day,
                        sep="-") %>%
      dplyr::mutate(sight_date = as.Date(sight_date)) %>%
      dplyr::filter(sight_date > input$day_month[1],
                    sight_date < input$day_month[2])
  })

  output$map <- renderLeaflet({
    data_reactive() %>%
    leaflet() %>%
      addTiles() %>%
      fitBounds(lng1 = 3,lat1 = 4,lng2 = 14,lat2 = 14) %>%
      addMarkers(lng = ~decimalLongitude,
                 lat = ~decimalLatitude)
  })

}

shinyApp(ui, server)
