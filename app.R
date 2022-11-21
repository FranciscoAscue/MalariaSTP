source("R/dependencies.R", local = TRUE)
source("R/ui-panel.R", local = TRUE)
source("R/data-mysql.R", local = TRUE)
source("config.R", local = TRUE)


create_btns <- function(x) {
  x %>% purrr::map_chr(~
         paste0(
           '<div class = "btn-group">
   <button class="btn btn-default action-button btn-info action_button" id="edit_',
           .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
   <button class="btn btn-default action-button btn-danger action_button" id="delete_',
           .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-trash-alt"></i></button></div>'
         ))
}

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
    map <- geojsonsf::geojson_sf(geojson = "R/mapSatipoActual.geojson")
  })
  
  epidemio <- reactive({
    data <- read.csv("R/SALA_SITUACIONAL_MALARIA.csv")
    data <- data %>% filter(ANO == input$year)
    return(data)
  })
  
  maps <- reactive({
    map <- Location()
    data <- epidemio() %>% group_by(UBIGEO_DESC) %>% summarise(n = n())
    colnames(data) <- c("NOMBDIST", "N")
    PALETA <- colorNumeric(palette = "Purples", NULL)
    daata <- merge(data, map, by = "NOMBDIST")
    
  
    tryCatch({
      if(input$sele != "TOTAL" ){
        daata <- daata %>% filter(NOMBDIST %in% input$sele)
      }
    },
    
    error = function(e){
      showModal(
        modalDialog(
          title = "Debes elegir almenos un Distrito",
          tags$i("Eliga Total o algun distrito"),br(),br(),
          tags$b("Error:"),br(),
          tags$code(e$message)
        )
      )
    })
    
    daata <- sf::st_as_sf(daata)
    return(list(PALETA = PALETA, map = daata))
  })
  
  
  output$map <- renderLeaflet({
    basemap <- leaflet(maps()$map, height = "100%") %>% 
      addTiles(group = "OSM (default)")%>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satelite") %>%
      addOpenweatherTiles(layers = "rain", apikey =  "d67a709b54e08178716c6f441b26233c", group = "Lluvias" ) %>%
      addOpenweatherTiles(layers = "temperature", apikey =  "d67a709b54e08178716c6f441b26233c", group = "Temperatura" ) %>%
      addPolygons(stroke = TRUE,weight = 1, smoothFactor = 0.4, fillOpacity = 0.3, fillColor = ~maps()$PALETA(N),
                  label = ~paste0(NOMBDIST, ": ", formatC(N, big.mark = ","))) %>%
      addLegend(pal = maps()$PALETA, values = ~N, title = "Numero de casos", opacity = 1.0)%>%
      addFullscreenControl() %>%
      # Layers control
      addLayersControl(
        baseGroups = c("OSM (default)", "Satelite", "Lluvias","Temperatura"),
        #overlayGroups = c("Quakes", "Outline"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  dataplot <- reactive({
    if(input$sele == "TOTAL"){
      data <- epidemio() %>% group_by(SEMANA) %>% summarise(N = n())
    }else{
      data <- epidemio() %>% filter(UBIGEO_DESC == input$sele) %>%
        group_by(SEMANA) %>% summarise(N = n ())
      }
  })
  
  dataplot2 <- reactive({
    a<- read.csv("data.csv")
    a <- a %>% filter(DEPARTAMENTO == "SAN MARTIN")
  })
  
  
  output$bar <- plotly::renderPlotly({
    plotly::plot_ly(dataplot(), x = ~SEMANA, y = ~ N, type = 'bar',  text = ~paste("CDC week:", SEMANA)) %>%  
      layout( xaxis = list(title = 'Semana epidemiológica (SE)'), 
      yaxis = list(title = 'Numero de casos (N°)'))
  })
  
  output$stack <- plotly::renderPlotly({
    plotly::plot_ly(dataplot2(), x = ~DISTRITO, y = ~X2020, z = ~X2019, color = ~PROVINCIA)
  })
  
  
  output$Page <- renderUI({
    req(credentials()$user_auth)
    if(is.null(credentials()$user_auth)){
      return()
    }
    malariaSTP
  })
  
}
shinyApp(ui = ui, server = server)