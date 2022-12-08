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

