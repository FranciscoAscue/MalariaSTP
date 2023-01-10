source("R/dependencies.R", local = TRUE)
source("R/ui-panel.R", local = TRUE)
source("R/data-mysql.R", local = TRUE)
source("config.R", local = TRUE)

ui <- fluidPage( title = "MalariaSTP",  
                 div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
                 
                 # login section
                 shinyauthr::loginUI(id = "login", title = h3(icon("server"),icon("hospital"),"Plataforma de Malaria Satipo"), 
                                     user_title = "Usuario", pass_title = "Contraseña"),
                 uiOutput("Page") )

#### plot de funciones ####

line_stack_plot <-  function(data, stack){
  data <- ungroup(data)
  if(stack == "stack"){
    plot <- plot_ly(data, x = ~Date, y = ~Frecuency, name = ~Select, groupnorm = "Percentaje", 
                    text = ~paste("CDC week:", Epi.Week), type = 'scatter', mode ="lines", stackgroup = 'two')
  }else{
    plot <- plot_ly(data , x = ~Date, y = ~Frecuency, name = ~Select, groupnorm = "Percentaje", 
                    text = ~paste("CDC week:", Epi.Week), type = 'scatter', mode ="lines")
  }
  return(plot)
}


hist_plot <- function(data, lineage = "AY.102", ndf = 5){
  
  if(is.null(data$Date)){
    return(plot_ly(data.frame(NULL), type = "scatter", mode ="line"))
  }else{
    model <- lm(Frecuency ~ ns(Date, df = ndf), data = data)
    plot <- plot_ly(data, x = ~Date , y = ~Frecuency, name = lineage, type = 'bar', color = I("light blue"),
                    hoverinfo = ~Frecuency, text = ~paste("CDC week:", epi_week)) %>%
      add_trace(x = ~Date, y = ~fitted(model), type = "scatter", mode ="line", color = I("red"))
    
    return(plot)
  }
}


leaflet_plot <- function(data, palette, titleLegend, scale=FALSE, long = FALSE, lat = FALSE, 
                         var = FALSE, total = FALSE){
  basemap <- leaflet(data) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(stroke = FALSE, smoothFactor = 0.4, fillOpacity = 1, fillColor = ~palette(N),
                label = ~paste0(Location, ": ", formatC(N, big.mark = ","))) %>%
    addLegend(pal = palette, values = ~N, title = titleLegend, opacity = 1.0) 
  
  if(!isFALSE(lat) & !isFALSE(long) & !isFALSE(var) & !isFALSE(total)){
    basemap <- basemap %>% addMinicharts(long, lat ,type = "pie", chartdata = var, opacity = 0.8, 
                                         colorPalette = brewer.pal(n = ncol(var), name = "Paired"), 
                                         width = 50 * sqrt(total) / sqrt(max(total)), transitionTime = 0)
    
    return(basemap)
  }
  
  return(basemap)
}

######## 
server <- function(input, output, session) {
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Logout to hide
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  
  Location <- reactive({
    map <- geojsonsf::geojson_sf(geojson = "Data/mapSatipoActual.geojson")
  })
  
  localidades <- reactive({
    
    localidades <- metadata(tabla = "satipolocalidades")
    #localidades <- read.table("Data/CentrosPobladosSatipo.csv", sep = ";", header = TRUE)
    if(input$sele != "TOTAL" ){
      localidades <- localidades %>% filter(DISTRITO %in% input$sele)
    }
    return(localidades)
  })
  
  localidades_tabla <- reactive({
    data <- localidades_mysql(input$prioritarios, input$distrito)
    return(data)
  })
  
  vectorAdulto_tabla <- reactive({
    req(input$localidadAdulto)
    req(input$eessAdulto)
    
    data <- vectorAdulto_mysql(input$distrito, 
                               input$localidadAdulto, 
                               input$eessAdulto, input$DaterangeA[1],
                               input$DaterangeA[2])
    x <- create_btns(data$ID)
    data <- data %>% dplyr::bind_cols(tibble("ACCIONES" = x))
    return(data)
  })
  
  criadero_tabla <- reactive({
    req(input$localidadLarva)
    req(input$eessLarva)
    
    data <- criadero_mysql(input$distrito, 
                           input$localidadLarva, 
                           input$eessLarva, input$DaterangeA[1],
                           input$DaterangeA[2])
    x <- create_btnsL(data$ID)
    data <- data %>% dplyr::bind_cols(tibble("ACCIONES" = x))
    return(data)
  })
  
  eessSatipo <- reactive({
    #eess <- read.csv("Data/EESSsatipo.csv")
    eess <- metadata(tabla = "satipoeess", fxd = TRUE)
    if(input$sele != "TOTAL" ){
      eess <- eess %>% dplyr::filter(DISTRITO %in% input$sele)
    }
    return(eess)
  })
  
  epidemio <- reactive({
    #data <- read.csv("R/SALA_SITUACIONAL_MALARIA.csv")
    #data <- data %>% filter(ANO == input$year)
    data <- metadata_epidemio(input$year)
    return(data)
  })
  
  maps <- reactive({
    req(!is.null(input$sele))
    map <- Location()
    data <- epidemio()# %>% dplyr::group_by(DISTRITO) %>% dplyr::summarise(n = n())
    colnames(data) <- c("NOMBDIST", "N")
    PALETA <- colorNumeric(palette = "Greens", NULL)
    daata <- merge(data, map, by = "NOMBDIST")
    daata <- sf::st_as_sf(daata)
    if(input$sele != "TOTAL"){
      daata <- daata %>% dplyr::filter(NOMBDIST %in% input$sele)
    }
    return(list(PALETA = PALETA, map = daata))
  })
  
  #######
  ####### Observe event
  
  shiny::observeEvent(input$IngresarA, {
    shiny::req(input$localidadAdulto)
    
    shiny::modalDialog(
      
      title = tags$div(tags$h4("LOCALIDAD :", input$localidadAdulto),
                       tags$h5("ESS:", input$eessAdulto)),
      sqlVectorAdultForm(), easyClose = FALSE,
      footer = div(
        shiny::actionButton(inputId = "final_inputA",
                            label   = "Ingresar",
                            icon = shiny::icon("upload"),
                            class = "btn-info"),
        shiny::actionButton(inputId = "dismiss_modal",
                            label   = "Cancelar",
                            class   = "btn-danger")
      )
    ) %>% shiny::showModal()
    
  })
  
  shiny::observeEvent(input$final_inputA, {
    
    tryCatch({
      VA_sql(distrito = input$distrito, fecha = input$fecha_tm, eess = input$eessAdulto,
             localidad = input$localidadAdulto, colecta = input$tcolecta, iphh = input$iphh,
             iphn = input$iphn, cespecie = input$nespecies, hcolecta = input$horascolecta, 
             especie =  input$especie, ncolecta = input$ncolecta ,temperatura = input$temperatura,
             hr = input$hr, norte = input$norteA, este = input$esteA)
      
    },
    
    error = function(e){
      showModal(
        modalDialog(
          title = "Ocurrio un Error!",
          tags$i("Ingrese nuevamente los datos"),br(),br(),
          tags$b("Error:"),br(),
          tags$code(e$message)
        )
      )
    })
    
    tmp1 <- input$DaterangeA[1]
    tmp2 <- input$DaterangeA[2]
    
    updateDateRangeInput(session, "DaterangeA",
                         label = "DaterangeA",
                         start = "2021-09-01",
                         end = "2023-12-31")
    
    updateDateRangeInput(session, "DaterangeA",
                         label = "DaterangeA",
                         start = tmp1,
                         end = tmp2)
    
    shiny::removeModal() 
  })
  
  shiny::observeEvent(input$dismiss_modal, {
    shiny::removeModal()
  })
  
  shiny::observeEvent(input$IngresarL, {
    shiny::req(input$localidadLarva)
    clasificacion <- metadata("clasificacioncriaderos", TRUE)
    shiny::modalDialog(
      
      title = tags$div(tags$h4("LOCALIDAD :", input$localidadLarva),
                       tags$h5("ESS:", input$eessLarva)),
      sqlLarvaForm(clasificacion), easyClose = FALSE,
      footer = div(
        shiny::actionButton(inputId = "final_inputL",
                            label   = "Ingresar",
                            icon = shiny::icon("upload"),
                            class = "btn-info"),
        shiny::actionButton(inputId = "dismiss_modal",
                            label   = "Cancelar",
                            class   = "btn-danger")
      )
    ) %>% shiny::showModal()
    
  })
  
  shiny::observeEvent(input$final_inputL, {
    
    #tryCatch({
    CL_sql(distrito = input$distrito, eess = input$eessLarva,
           localidad = input$localidadLarva, clasificacion = input$clasificacionCriadero,
           tipo =  input$tipoCriadero,extension = input$extension, metodo = input$metodo,
           larvicida =  input$larvicida, especie =  input$especieL, il_pre = input$il_pre,
           il_post = input$il_post,fecha = input$fecha_l, norte = input$norteL, este = input$esteL)
    
    
    tmp1 <- input$DaterangeA[1]
    tmp2 <- input$DaterangeA[2]
    
    shiny::updateDateRangeInput(session, "DaterangeA",
                         label = "DaterangeA",
                         start = "2021-09-01",
                         end = "2023-12-31")
    
    shiny::updateDateRangeInput(session, "DaterangeA",
                         label = "DaterangeA",
                         start = tmp1,
                         end = tmp2)
    
    shiny::removeModal() 
  })
  
  shiny::observeEvent(input$current_id, {
    shiny::req(!is.null(input$current_id) &
                 stringr::str_detect(input$current_id,pattern = "delete"))
    #delet_row <- which(stringr::str_detect(inputData()$ACCIONES, pattern = paste0("\\b", input$current_id, "\\b") ))
    #sql_id <- inputData()[delet_row, ][["NETLAB"]] 
    shiny::modalDialog(
      title = h3("Se borrara permanentemente los datos de vectores adultos!!"),
      div(
        shiny::actionButton(inputId = "final_delete",
                            label   = "Confirmar",
                            icon = shiny::icon("trash"),
                            class = "btn-danger")
      )
    ) %>% shiny::showModal()
  })
  
  shiny::observeEvent(input$final_delete, {
    shiny::req(!is.null(input$current_id) &
                 stringr::str_detect(input$current_id,pattern = "delete"))
    delet_row <- which(stringr::str_detect(vectorAdulto_tabla()$ACCIONES, 
                                           pattern = paste0("\\b", input$current_id, "\\b") ))
    sql_id <- vectorAdulto_tabla()[delet_row, ][["ID"]] 
    query <- paste0("DELETE FROM `controladultos` WHERE `controladultos`.`ID` = \'",sql_id,"\'")
    delete_sql(query)
    tmp1 <- input$DaterangeA[1]
    tmp2 <- input$DaterangeA[2]
    
    shiny::updateDateRangeInput(session, "DaterangeA",
                         label = "DaterangeA",
                         start = "2021-09-01",
                         end = "2023-12-31")
    
    shiny::updateDateRangeInput(session, "DaterangeA",
                         label = "DaterangeA",
                         start = tmp1,
                         end = tmp2)
    shiny::removeModal()
  })
  
  shiny::observeEvent(input$current_id, {
    shiny::req(!is.null(input$current_id) &
                 stringr::str_detect(input$current_id,pattern = "deleteL"))
    #delet_row <- which(stringr::str_detect(inputData()$ACCIONES, pattern = paste0("\\b", input$current_id, "\\b") ))
    #sql_id <- inputData()[delet_row, ][["NETLAB"]] 
    shiny::modalDialog(
      title = h3("Se borrara permanentemente los datos de criaderos!!"),
      div(
        shiny::actionButton(inputId = "final_deleteL",
                            label   = "Confirmar",
                            icon = shiny::icon("trash"),
                            class = "btn-danger")
      )
    ) %>% shiny::showModal()
    
  })
  
  shiny::observeEvent(input$final_deleteL, {
    shiny::req(!is.null(input$current_id) &
                 stringr::str_detect(input$current_id,pattern = "deleteL"))
    delet_row <- which(stringr::str_detect(criadero_tabla()$ACCIONES, pattern = paste0("\\b", input$current_id, "\\b") ))
    sql_id <- criadero_tabla()[delet_row, ][["ID"]] 
    query <- paste0("DELETE FROM `controlcriaderos` WHERE `controlcriaderos`.`ID` = \'",sql_id,"\'")
    delete_sql(query)
    tmp1 <- input$DaterangeA[1]
    tmp2 <- input$DaterangeA[2]
    
    shiny::updateDateRangeInput(session, "DaterangeA",
                         label = "DaterangeA",
                         start = "2021-09-01",
                         end = "2023-12-31")
    
    shiny::updateDateRangeInput(session, "DaterangeA",
                         label = "DaterangeA",
                         start = tmp1,
                         end = tmp2)
    
    shiny::removeModal()
  })
  ### Output graph
  
  output$map <- renderLeaflet({
    tryCatch({
    basemap <- leaflet::leaflet(maps()$map, height = "100%") %>% 
      addTiles(group = "OSM (default)")%>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satelite") %>%
      addOpenweatherTiles(layers = "rain", apikey =  "d67a709b54e08178716c6f441b26233c", group = "Lluvias" ) %>%
      addOpenweatherTiles(layers = "temperature", apikey =  "d67a709b54e08178716c6f441b26233c", group = "Temperatura" ) %>%
      addPolygons(stroke = TRUE,weight = 1, smoothFactor = 0.4, fillOpacity = 0.3, fillColor = ~maps()$PALETA(N),
                  label = ~paste0(NOMBDIST, ": ", formatC(N, big.mark = ","))) %>%
      addCircleMarkers( localidades()$ESTE, localidades()$NORTE, radius = 1, 
                        popup = localidades()$LOCALIDAD,
                        group = "Localidades")%>%
      addCircleMarkers(  eessSatipo()$ESTE,eessSatipo()$NORTE, radius = 1, color = "red",
                         popup = eessSatipo()$NOMBRE,
                         group = "EESS") %>%
      addLegend(pal = maps()$PALETA, values = ~N, title = "Numero de casos", opacity = 1.0)%>%
      addFullscreenControl() %>%
      # Layers control
      addLayersControl(
        baseGroups = c("OSM (default)", "Satelite", "Lluvias","Temperatura"),
        overlayGroups = c("Localidades", "EESS"),
        options = layersControlOptions(collapsed = FALSE)
      )
    }, 
    
    error = function(e){
      showModal(
        modalDialog(
          title = "DNI DUPLICADO!",
          tags$i("Revise si el paciente fue ingresado previamente"),br(),br(),
          tags$code(e$message)
        )
      )
    })
  })
  
  dataplot <- reactive({
    if(input$sele == "TOTAL"){
      data <- metadata_epidemio(year = input$year, map = FALSE, distrito = input$sele)
      #data <- epidemio() %>% group_by(SEMANA) %>% summarise(N = n())
    }else{
      data <- metadata_epidemio(year = input$year, map = FALSE, distrito = input$sele)
      #data <- epidemio() %>% filter(DISTRITO == input$sele) %>%group_by(SEMANA) %>% summarise(N = n ())
    }
  })
  
  dataplot2 <- reactive({
    a<- read.csv("Data/CentrosPobladosSatipo.csv")
    a <- a %>% dplyr::filter(DEPARTAMENTO == "SAN MARTIN")
  })
  
  
  output$bar <- plotly::renderPlotly({
    plotly::plot_ly(dataplot(), x = ~SEMANA, y = ~N, type = 'bar',  text = ~paste("CDC week:", SEMANA)) %>%  
      layout( xaxis = list(title = 'Semana epidemiológica (SE)'), 
              yaxis = list(title = 'Numero de casos (N°)'))
  })
  
  output$tablaLocalidades <- DT::renderDataTable(localidades_tabla(),
                                                 options = list(scrollX = TRUE),
                                                 rownames = FALSE, server = FALSE, escape = FALSE, selection = 'none')
  
  output$tablaVectoresAdulto <- DT::renderDataTable(vectorAdulto_tabla()[c(3,6:18)],
                                                    options = list(scrollX = TRUE),
                                                    rownames = FALSE, server = FALSE, escape = FALSE, selection = 'none')
  
  output$tablaCriadero <- DT::renderDataTable(criadero_tabla()[c(13,4:12,14:16)],
                                              options = list(scrollX = TRUE),
                                              rownames = FALSE, server = FALSE, escape = FALSE, selection = 'none')
  #### render UI 
  output$Page <- renderUI({
    req(credentials()$user_auth)
    if(is.null(credentials()$user_auth)){
      return()
    }
    malariaSTP
  })
  
  output$distritoG <- renderUI({
    
    distritos <- metadata(tabla = "satipodistritos", fxd = TRUE)
    shiny::selectInput(inputId = "distrito",
                label = "Selecciona Distrito",
                choices = distritos$DISTRITOS,
                selected = "RIO TAMBO")
  })
  
  renderUILocalidades <- reactive({
    localidades <- metadata(tabla = "satipolocalidades")
    return(localidades)
  })
  
  output$localidadA <- renderUI({
    if(is.null(input$distrito))
      return()
    localidades <- renderUILocalidades() %>% dplyr::filter(DISTRITO == input$distrito)
    shiny::selectInput(inputId = "localidadAdulto",
                label = "Seleccione Localidad",
                choices = localidades$LOCALIDAD,
                selected = "VALLE ESMERALDA")
  })
  
  output$eessA <- renderUI({
    if(is.null(input$localidadAdulto))
      return()
    req(input$localidadAdulto)
    eess <- renderUILocalidades() %>% dplyr::filter(LOCALIDAD == input$localidadAdulto)
    shiny::selectInput(inputId = "eessAdulto",
                label = "Establecimiento de Salud",
                choices = eess$EESS,
                selected = "VALLE ESMERALDA")
  })
  
  output$localidadL <- renderUI({
    if(is.null(input$distrito))
      return()
    localidades <- renderUILocalidades() %>% dplyr::filter(DISTRITO == input$distrito)
    shiny::selectInput(inputId = "localidadLarva",
                label = "Seleccione Localidad",
                choices = localidades$LOCALIDAD,
                selected = "VALLE ESMERALDA")
  })
  
  output$eessL <- renderUI({
    if(is.null(input$localidadLarva))
      return()
    req(input$localidadLarva)
    eess <- renderUILocalidades() %>% dplyr::filter(LOCALIDAD == input$localidadLarva)
    shiny::selectInput(inputId = "eessLarva",
                label = "Establecimiento de Salud",
                choices = eess$EESS,
                selected = "VALLE ESMERALDA")
  })
  
  #### Add tabset Panel
  observeEvent(input$sele,{
    if(credentials()$info[3] == "admin"){
      appendTab(inputId = "navPrincipal",
                adminPanelStats)
    }
    
  }, once = TRUE)
  
  observeEvent(input$distrito,{
    if(credentials()$info[3] == "admin"){
      appendTab(inputId = "navPrincipal",
                adminPanelmaps)
      }
  }, once = TRUE)
  
  observeEvent(input$sele,{
    if(credentials()$info[3] == "admin"){
      appendTab(inputId = "tablesdb",
                adminPanelLocalidad)
    }
  }, once = TRUE)
  
}

shinyApp(ui = ui, server = server)