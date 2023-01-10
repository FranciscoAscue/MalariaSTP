create_btns <- function(x) {
  x %>% purrr::map_chr(~
                         paste0(
                           '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-danger action_button" id="delete_',
                           .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-trash-alt"></i></button></div>'
                         ))
}

create_btnsL <- function(x) {
  x %>% purrr::map_chr(~
                         paste0(
                           '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-danger action_button" id="deleteL_',
                           .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-trash-alt"></i></button></div>'
                         ))
}


malariaSTP <- navbarPage(theme = shinytheme("flatly"),
                         id = "navPrincipal", 
                         title = "Plataforma de Vigilancia Malaria",
                         
                         ### Panel de manejo de base de datos
                         tabPanel(title = h5(icon("upload"),"Base de Datos"), id = "databasetab",
                                  useShinyjs(),
                                  column(12,
                                         column(3,
                                                dateRangeInput("DaterangeA", 
                                                               "Seleccione Fecha", 
                                                               start  = "2022-09-01",
                                                               end    = "2022-10-01")), 
                                         column(3,
                                                uiOutput("distritoG")),
                                  ),
                                  tabsetPanel(type = "tabs",id = "tablesdb",
                                              tabPanel(title = "CONTROL VECTOR ADULTO",
                                                       useShinyjs(),
                                                       column(12, h2(" ")),
                                                       column(12,
                                                              column(3,
                                                                     actionButton(inputId = "IngresarA", 
                                                                                  label =  h4(icon("upload"), "Ingresar Datos"),
                                                                                  width = "200px")),
                                                              
                                                              
                                                              column(3,uiOutput("localidadA")),
                                                              column(3,uiOutput("eessA"))
                                                              
                                                       ),
                                                       column(12, h2(" ")),
                                                       column(12,
                                                              shinycssloaders::withSpinner(
                                                                DT::dataTableOutput("tablaVectoresAdulto"), 
                                                                type = 3, color.background = "white", color = "blue")
                                                       ),
                                                       shiny::includeScript("script.js"),
                                                       
                                                       
                                              ),
                                              tabPanel( title = "CONTROL CRIADEROS",
                                                        useShinyjs(),
                                                        column(12, h2(" ")),
                                                        column(12,
                                                               column(3,
                                                                      actionButton(inputId = "IngresarL", 
                                                                                   label =  h4(icon("upload"), "Ingresar Datos"),
                                                                                   width = "200px")),
                                                               column(3,uiOutput("localidadL")),
                                                               column(3,uiOutput("eessL"))
                                                               
                                                        ),
                                                        column(12, h2(" ")),
                                                        column(12,
                                                               shinycssloaders::withSpinner(
                                                                 DT::dataTableOutput("tablaCriadero"), 
                                                                 type = 3, color.background = "white", color = "blue")
                                                        ),
                                                        shiny::includeScript("script.js"),
                                                        
                                              )
                                              
                                  )
                                  
                         )
                         
                         
) 

##### Conditional tab for admin Users

adminPanelmaps <- tabPanel(title = h5(icon("map")," Maps"), 
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
                                                plotly::plotlyOutput("bar", height = 200)),
                                         column(12, 
                                                dateRangeInput("DaterangePanel", 
                                                               "Seleccione Fecha", 
                                                               start  = "2022-09-01",
                                                               end    = "2022-12-01")
                                                )
                           ),
)

adminPanelStats <- tabPanel(title = h5(icon("chart-pie")," Estadisticas"),
                            plotly::plotlyOutput("stack"),
                            plotly::plotlyOutput("stack2")
)

adminPanelLocalidad <- tabPanel(title = "LOCALIDADES",
                                column(12, h2(" ")),
                                column(12,
                                       
                                       column(3,selectInput(inputId = "prioritarios",
                                                            label = "LOCALIDAD",
                                                            choices = c("PRIORITARIOS","NO PRIORITARIOS"),
                                                            selected = "PRIORITARIOS"))
                                       
                                ),
                                column(12, h2(" ")),
                                column(10,offset = 1,
                                       shinycssloaders::withSpinner(
                                         DT::dataTableOutput("tablaLocalidades"), 
                                         type = 3, color.background = "white", color = "blue")
                                )
)



###### Form input SQL 

sqlLarvaForm <- function(clasificacion){
  return(column(12,
                
                column(12, h1(" ")),
                column(12, style = "background-color:#F4F6F6;",
                       column(6,
                              selectInput(inputId = "clasificacionCriadero",
                                          label = "Clasificacion de Criadero",
                                          choices =  clasificacion$CLASIFICACION,
                                          selected = "ACEQUIA")),
                       column(6,
                              selectInput(inputId = "tipoCriadero",
                                          label = "Tipo de Criadero",
                                          choices =  c("TEMPORAL","PERMANENTE"),
                                          selected = "TEMPORAL")),
                ),
                column(12, style = "background-color:#F4F6F6;",#82E0AA 
                       column(6,
                              numericInput(inputId = "extension",
                                           label = "Ingresar Extensión (m2)",
                                           value = 10, min = 0,
                                           width = "200px")),
                       column(6,
                              selectInput(inputId = "metodo",
                                          label = "Método de Intervención",
                                          choices =  c("FISICO","QUIMICO","BIOLOGICO"),
                                          selected = "FISICO")),
                ),
                column(12, style = "background-color:#F4F6F6;",#82E0AA 
                       column(6,
                              numericInput(inputId = "larvicida",
                                           label = "Ingresar Larvicida (g)",
                                           value = NULL, min = 0,
                                           width = "200px")),
                       column(6,
                              selectInput(inputId = "especieL",
                                          label = "Especie identificada",
                                          choices =  c("Anopheles spp","Culex spp"),
                                          selected = "Culex spp")),
                ),
                column(12, style = "background-color:#F4F6F6;",#82E0AA 
                       column(6,
                              numericInput(inputId = "il_pre",
                                           label = "Indice Larvario Pre-Intervencion",
                                           value = "0.01", min = 0,
                                           step = 0.01 ,
                                           width = "200px")),
                       column(6,
                              numericInput(inputId = "il_post",
                                           label = "Indice Larvario Post-Intervencion",
                                           value = "0.01", min = 0,
                                           step = 0.01 ,
                                           width = "200px")),
                ),column(12, style = "background-color:#F4F6F6;",#82E0AA 
                         column(6,
                                numericInput(inputId = "norteL",
                                             label = "Coordenas de Longitud",
                                             value = -11.0001, max = 0,
                                             step = 0.0001 ,
                                             width = "200px")),
                         column(6,
                                numericInput(inputId = "esteL",
                                             label = "Coordenas de Latitud",
                                             value = -74.0001, max = 0,
                                             step = 0.0001 ,
                                             width = "200px")),
                ),column(12, 
                         column(6, dateInput(inputId = "fecha_l",
                                             label = "Fecha de colecta",
                                             language = "es", min = "2020-01-01", max = "2023-05-28"
                         ))),
                column(12, h3(" ")
                )
  )
  )
}

sqlVectorAdultForm <- function(){
  return(column(12,
                
                column(12, h1(" ")),
                column(12, style = "background-color:#F4F6F6;",
                       column(6,
                              dateInput(inputId = "fecha_tm",
                                        label = "Fecha de toma de muestra",
                                        language = "es", min = "2020-01-01", max = "2023-05-28"
                              )),
                       column(6,
                              selectInput(inputId = "tcolecta",
                                          label = "Selecciona tipo de colecta",
                                          choices = c( "INTRADOMICILIO",
                                                       "PERIDOMICILIO",
                                                       "EXTRADOMICILIO","NULL"),
                                          selected = "INTRADOMICILIO")),
                ),
                column(12, style = "background-color:#F4F6F6;",#82E0AA 
                       column(6,
                              numericInput(inputId = "iphh",
                                           label = "Ingresar IPHH",
                                           value = "0.01", min = 0,
                                           step = 0.01 , 
                                           width = "200px")),
                       column(6,
                              numericInput(inputId = "iphn",
                                           label = "Ingresar IPHN",
                                           value = "0.01", min = 0,
                                           step = 0.01 ,
                                           width = "200px")),
                ),
                column(12, style = "background-color:#F4F6F6;",#82E0AA 
                       column(6,
                              numericInput(inputId = "nespecies",
                                           label = "Ingresar N° Especies",
                                           value = 1, min = 0,
                                           width = "200px")),
                       column(6,
                              numericInput(inputId = "horascolecta",
                                           label = "Ingresar Horas de colecta",
                                           value = 1, min = 0,
                                           width = "200px")),
                ),
                column(12, style = "background-color:#F4F6F6;",#82E0AA 
                       column(6,
                              selectInput(inputId = "especie",
                                          label = "Selecciona especie predominante",
                                          choices = c( "Culex spp",
                                                       "Anopheles spp",
                                                       "NULL"),
                                          selected = "NULL")),
                       column(6,
                              numericInput(inputId = "ncolecta",
                                           label = "Ingresar #N colectados",
                                           value = 1, min = 0,
                                           width = "200px")),
                ),column(12, style = "background-color:#F4F6F6;",#82E0AA 
                         column(6,
                                numericInput(inputId = "temperatura",
                                             label = "Ingresar temperatura(celsius)",
                                             value = 25, min = 0,
                                             width = "200px")),
                         column(6,
                                numericInput(inputId = "hr",
                                             label = "Ingresar humedad relativa (%)",
                                             value = 60, min = 0,
                                             width = "200px")),
                ),column(12, style = "background-color:#F4F6F6;",#82E0AA 
                         column(6,
                                numericInput(inputId = "norteA",
                                             label = "Ingresar Coordenas de Longitud",
                                             value = -11.0001, max = 0,
                                             step = 0.0001 ,
                                             width = "200px")),
                         column(6,
                                numericInput(inputId = "esteA",
                                             label = "Ingresar Coordenas de Latitud",
                                             value = -74.0001, max = 0,
                                             step = 0.0001 ,
                                             width = "200px")),
                ),column(12, h3(" "))
                
  )
  )
}
