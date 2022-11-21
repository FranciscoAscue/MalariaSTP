malariaSTP <- bootstrapPage(
  
  navbarPage(theme = shinytheme("flatly"), title = "Plataforma de vigilancia Malaria",
             
             ### Principal Panel
             tabPanel(title = h5(icon("map")," Maps"), 
                      tags$style(type = "text/css", "#map {height: calc(100vh - 100px) !important;}"),
                      leafletOutput("map", width = "100%"),
                      tags$style("
                                          #controls {
                                            background-color: #f1ecec;
                                            opacity: 0.5;
                                          }
                                          #controls:hover{
                                            opacity: 1;
                                          }
                                                 "),
                      absolutePanel(id = "controls", class = "panel panel-default",
                                    top = 100, left = 75, width = 380, fixed=TRUE,
                                    draggable = TRUE, height = "auto",
                                    
                                    span(tags$i(h5("Reporte de casos de malaria por distrito")), style="color:#045a8d", align = "center"),
                                    column(12,
                                           
                                           column( 8,
                                                   selectInput("sele", label = h5("Seleciona Distrito"),
                                                               choices = c("TOTAL","LLAYLLA","PANGOA","PAMPA HERMOSA","COVIRIALI","MAZAMARI",
                                                                           "VIZCATAN DEL ENE","SATIPO","RIO NEGRO","RIO TAMBO" ),
                                                               selected = "TOTAL", multiple = TRUE)),
                                           column( 4, 
                                                   numericInput("year", label = h5("Año"),
                                                                min = 2016, max = 2023, value = 2022
                                                   )
                                           )
                                    ),
                                    column(12,
                                           plotly::plotlyOutput("bar", height = 200))
                      ),
             ),
             
             
             ### Panel de Estadisticas
             tabPanel(title = h5(icon("chart-pie")," Estadisticas"),
                      
                      plotly::plotlyOutput("stack"),
                      plotly::plotlyOutput("stack2")
                      
                      
             ),
             
             ### Panel de manejo de base de datos
             tabPanel(title = h5(icon("upload"),"Base de Datos"))
  ) 
  
)