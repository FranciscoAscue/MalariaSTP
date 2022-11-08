### Red Satipo - Plataforma de vigilancia vectorial 


library(shiny)
library(shinythemes)
library(geojsonsf)
library(sf)
library(dplyr)
library(sp)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(plotly)

ui <- bootstrapPage( navbarPage(theme = shinytheme("flatly"), title = "Plataforma de vigilancia Malaria"),
                                tabPanel(title = h5(icon("map")," Maps"), 
                                         tags$style(type = "text/css", "#map {height: calc(100vh - 100px) !important;}"),
                                         leafletOutput("map", width = "100%"),
                                         tags$style("
        #controls {
          background-color: #ddd;
          opacity: 0.5;
        }
        #controls:hover{
          opacity: 1;
        }
               "),
                                         absolutePanel(id = "controls", class = "panel panel-default",
                                                       top = 98, left = 65, width = 350, fixed=TRUE,
                                                       draggable = TRUE, height = "auto",
                                                       
                                                       span(tags$i(h6("Reporte de casos de malaria por distrito")), style="color:#045a8d"),
                                                       column(10,
                                                              selectInput("sele", label = "Seleciona Locacion",
                                                                          choices = c("LLAYLLA","PANGOA","PAMPA HERMOSA","COVIRIALI","MAZAMARI","SATIPO","RIO NEGRO","RIO TAMBO"),
                                                                          selected = "SAN MARTIN")
                                                       ),
                                                       
                                                       column(10,
                                                              plotly::plotlyOutput("bar", height = 200))
                                         ),
                                ),
                                tabPanel(title = h5(icon("chart-pie")," Estadisticas"),
                                         
                                         plotly::plotlyOutput("stack"),
                                         plotly::plotlyOutput("stack2")
                                         
                                         
                                ),
                                tabPanel(title = h5(icon("bullhorn")," Denuncias")),
                                tabPanel(title = h5(icon("info")," Nosotros"))
) 




server <- function(input, output) {
  
  url <- 'https://raw.githubusercontent.com/juaneladio/peru-geojson/master/peru_distrital_simple.geojson'
  Location <- reactive({
    map <- geojsonsf::geojson_sf(geojson = url)
  })
  
  maps <- reactive({
    map <- Location()
    PALETA <- colorNumeric(palette = "Reds", NULL)
    map <- map %>% filter(NOMBDEP %in% "JUNIN")
    map <- map %>% filter(NOMBDIST %in% input$sele)
    return(list(PALETA = PALETA, map = map))
  })
  
  
  output$map <- renderLeaflet({
    basemap <- leaflet(maps()$map, height = "100%") %>% 
      addTiles()%>%
      #addProviderTiles(providers$Esri.WorldImagery) %>%
      #addTiles(urlTemplate = url_forest_watch)%>%
      addPolygons(stroke = TRUE,weight = 1, smoothFactor = 0.4, fillOpacity = 0.3, fillColor = ~maps()$PALETA(AREA_MINAM),
                  label = ~paste0(NOMBDIST, ": ", formatC(AREA_MINAM, big.mark = ","))) %>%
      addLegend(pal = maps()$PALETA, values = ~AREA_MINAM, title = "Densidad vectorial", opacity = 1.0)%>%
      addFullscreenControl()
  })
  
  dataplot <- reactive({
    if(input$sele == "SAN MARTIN"){
      a <- data.frame(N = 1:100, Normal = rnorm(1000,10,4))
    }else{
      a <- data.frame(N = 1:100, Normal = rgamma(1000,1,0.5))
    }
  })
  
  dataplot2 <- reactive({
    a<- read.csv("data.csv")
    a <- a %>% filter(DEPARTAMENTO == "SAN MARTIN")
  })
  
  
  output$bar <- plotly::renderPlotly({
    plotly::plot_ly(dataplot(), x = ~N, y = ~ Normal, type = 'bar')
  })
  
  output$stack <- plotly::renderPlotly({
    plotly::plot_ly(dataplot2(), x = ~DISTRITO, y = ~X2020, z = ~X2019, color = ~PROVINCIA)
  })
  
  output$stack2 <- plotly::renderPlotly({
    plotly::plot_ly(dataplot2(), x = ~DISTRITO, y = ~X2020, name = ~PROVINCIA, groupnorm = "Percentaje", 
                    text = ~paste("CDC week:"), type = 'scatter', mode ="lines", stackgroup = 'two')
  })
}

shinyApp(ui = ui, server = server)
