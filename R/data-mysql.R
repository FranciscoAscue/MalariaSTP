if( !is.element("RMySQL",rownames(installed.packages() ) ) ){
  install.packages("RMySQL")
}

source("config.R", local = TRUE)

library(RMySQL)



variant_distribution <- function(map, metadata, epidem,  mindate, maxdate, switch = "VocVoi"){
  
  cities <- as.data.frame(st_coordinates(st_centroid(map)))
  map$Location <- toupper(map$Location)
  cities$location <- map$Location
  metadata <- metadata %>% dplyr::filter(date >= mindate , date <= maxdate)
  metadata$location <- toupper(metadata$location)
  
  if( switch == "VocVoi" ){
    for( var in unique(metadata$VOC.VOI)){
      temp <- metadata %>% filter(VOC.VOI == var) %>% dplyr::group_by(location) %>% dplyr::summarise( !!paste0(var) := n())
      cities <- merge(x  = cities, y = temp, by = 'location', all = TRUE  )
    }
  }else{
    for( var in unique(metadata$lineage)){
      temp <- metadata %>% filter(lineage == var) %>% dplyr::group_by(location) %>% dplyr::summarise( !!paste0(var) := n())
      cities <- merge(x  = cities, y = temp, by = 'location', all = TRUE  )
    }
  }
  
  total <- metadata %>% dplyr::group_by(location) %>% dplyr::summarise(total = n())
  cities <- merge(x  = cities, y = total, by = 'location', all = TRUE )
  cities[is.na(cities)] <- 0
  for(i in 1:length(cities$total)){if(cities$total[i] == 0){cities$total[i] = 1}}
  
  epidem_freq <- epidem %>% dplyr::filter(date >= mindate, date <= maxdate)
  epidem_freq <- epidem_freq %>% dplyr::group_by(Location) %>% dplyr::summarise( N = n())
  epidem_freq$Location <- toupper(epidem_freq$Location)
  Merge_data <- inner_join(map,epidem_freq, by = 'Location' )
  Merge_data$N <- (Merge_data$N/Merge_data$Population)*100000
  
  
  pal <- colorNumeric(  palette = "Greys", NULL)
  long <- cities$X
  lat <- cities$Y
  var <- cities[,4:(length(cities)-1)]
  total <- cities$total
  return( list( df = Merge_data, pal = pal, long = long, lat = lat, var = var, total = total))
}


sampling_distribution <- function(map , metadata, mindate, maxdate, sampling, scale_map){
  
  metadata <- metadata %>% dplyr::filter( date >= mindate, date <= maxdate )
  if(sampling == "Total"){
    metadata <- metadata
    pal <- colorNumeric(palette = "Reds", NULL)
  } else { 
    metadata <- metadata %>% dplyr::filter(VOC.VOI == sampling )
    pal <- colorNumeric(palette = "BuPu", NULL)
  }
  
  if(scale_map == "linear"){
    count_region <- metadata %>% dplyr::group_by(location) %>% dplyr::summarise( n = n())
  } else{
    count_region <- metadata %>% dplyr::group_by(location) %>% dplyr::summarise( n = log10(n()))
  }
  
  count_region$location <- toupper(count_region$location)
  colnames(count_region) <- c("Location", "N")
  Merge_data <- merge(map, count_region , by  = "Location")
  return(list(df = Merge_data, pal = pal))
}


stackvariant <- function(data, mindate, maxdate, ngenomes, varline){
  
  data <- data %>% dplyr::filter(date >= mindate, date <= maxdate)
  
  if( varline == "Lineages"){
    data <- data %>% dplyr::group_by(Date, epi_week,  lineage) %>% dplyr::summarise( n = n()) %>%
      dplyr::mutate(Frecuency = n / sum(n))
    
  }else{
    data <- data %>% dplyr::group_by(Date,epi_week,  VOC.VOI) %>% dplyr::summarise( n = n()) %>%
      dplyr::mutate(Frecuency = n / sum(n))
  }
  
  data$Frecuency = round(data$Frecuency, 2)
  names(data) <- c("Date", "Epi.Week","Select","N", "Frecuency")
  data <- data %>% dplyr::filter(N >= ngenomes)
  
  return(data)
}


matrix_distribution <- function(metadata, type, upload, prov, motivo){
  
  if( type == "SemanaEpidemio"){
    test <- metadata %>% dplyr::group_by(Date, epi_week, lineage) %>% dplyr::summarise( n = n()) %>%
      dplyr::mutate(percentage = n / sum(n))
    new <- test[,c("epi_week","lineage","n")]
    names(new) <- c("SEMANA", "lineage", "abundance")
    new <- as.data.frame(new)
    cuadro_motivo <- create.matrix(new, tax.name = "SEMANA", 
                                   locality = "lineage", abund.col = "abundance", abund = TRUE)
  }else{
    
    if(!is.null(upload)){
      
      
      if( prov == "Region"){
        if(motivo != "Total"){upload <- upload %>% dplyr::filter(MOTIVO == motivo)}
        test <- upload %>% dplyr::group_by(REGION, LINAGES) %>% dplyr::summarise( n = n())
        names(test) <- c("provincia", "lineage", "Freq")
        test <- as.data.frame(test)
        cuadro_motivo <- create.matrix(test, tax.name = "provincia",
                                       locality = "lineage",
                                       abund.col = "Freq",
                                       abund = TRUE)
        cuadro_motivo = as.data.frame(cuadro_motivo)
        
      }else{
        if(motivo != "Total"){upload <- upload %>% dplyr::filter(MOTIVO == motivo)}
        test <- upload %>% dplyr::group_by(PROVINCIA, LINAGES) %>% dplyr::summarise( n = n())
        names(test) <- c("region", "lineage", "Freq")
        test <- as.data.frame(test)
        cuadro_motivo <- create.matrix(test, tax.name = "region",
                                       locality = "lineage",
                                       abund.col = "Freq",
                                       abund = TRUE)
        cuadro_motivo = as.data.frame(cuadro_motivo)
        
      }
      
      
    }else{
      cuadro_motivo <- matrix(0,10,10)
    }
    
  }
  
  return(cuadro_motivo)
}


freq_voc_voi <- function(data, lin){
  
  #if(is.element(lin, unique(data$lineage))){
  dd1 = strsplit(lin, split = ",")
  data <- data %>% filter(lineage == dd1[[1]])
  data <- data %>% dplyr::group_by(Date, epi_week) %>% dplyr::summarise(Frecuency = n())
  return(data)
  #} else{
  # return(data.frame(Date = NULL,  epi_week = NULL,  Frecuency = NULL))
  #}
  
}

## data from MySQL
##SELECT LOCALIDAD,COUNT(LOCALIDAD) AS N FROM `epidemiodb` WHERE YEAR = 2022 GROUP BY LOCALIDAD

metadata <- function(tabla, fxd = FALSE){
  
  con <- dbConnect(MySQL(),
                   user = USER_MYSQL,
                   password = PASSWORD_MYSQL,
                   host = HOST_MYSQL,
                   dbname = DB_MYSQL, port = PORT_MYSQL)
  if(isTRUE(fxd)){
    query = paste0("SELECT * FROM `",tabla,"`;")
  }else{
    query = paste0("SELECT * FROM `",tabla,"` WHERE `PRIORITARIO` LIKE 'SI';")
  }
  
  dbSendQuery(con, "SET NAMES utf8mb4;")
  on.exit(dbDisconnect(con))
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbClearResult(rs)
  return(df)
}

metadata_epidemio <- function(year, map = TRUE, distrito){
  
  con <- dbConnect(MySQL(),
                   user = USER_MYSQL,
                   password = PASSWORD_MYSQL,
                   host = HOST_MYSQL,
                   dbname = DB_MYSQL, port = PORT_MYSQL)
  if(map == TRUE){
    query = paste0("SELECT DISTRITO,COUNT(DISTRITO) AS N FROM `epidemiodb` WHERE YEAR = ",
                   year," GROUP BY DISTRITO;")
    
  }else{
    if(distrito == "TOTAL"){
      query = paste0("SELECT SEMANA,COUNT(SEMANA) AS N FROM `epidemiodb` WHERE YEAR = ",
                     year," GROUP BY SEMANA;")
    }else{
      query = paste0("SELECT SEMANA,COUNT(SEMANA) AS N FROM `epidemiodb` WHERE YEAR = ",
                     year," AND DISTRITO = '",distrito,"' GROUP BY SEMANA;")
    }
  }
  
  dbSendQuery(con, "SET NAMES utf8mb4;")
  on.exit(dbDisconnect(con))
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbClearResult(rs)
  return(df)
}

localidades_mysql <- function(prioritario, distrito){
  
  con <- dbConnect(MySQL(),
                   user = USER_MYSQL,
                   password = PASSWORD_MYSQL,
                   host = HOST_MYSQL,
                   dbname = DB_MYSQL, port = PORT_MYSQL)
  if(prioritario == "PRIORITARIOS"){
    query = paste0("SELECT * FROM `satipolocalidades` WHERE `PRIORITARIO` LIKE 'SI' AND DISTRITO ='", distrito,"';")
  }else{
    query = paste0("SELECT * FROM `satipolocalidades` WHERE `PRIORITARIO` IS NULL AND DISTRITO ='",distrito,"';")
  }
  
  
  dbSendQuery(con, "SET NAMES utf8mb4;")
  on.exit(dbDisconnect(con))
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbClearResult(rs)
  return(df)
}



VA_sql <- function(distrito, fecha, eess, localidad, colecta,
                   iphh, iphn, cespecie, hcolecta, especie,
                   ncolecta, temperatura, hr, norte, este){
  
  
  if(length(fecha) == 0){
    fecha <- 'NULL'
  }
  
  if(is.null(iphh) | is.na(iphh)){
    iphh <- 'NULL'
  }
  
  if(is.null(iphn) | is.na(iphn)){
    iphn <- 'NULL'
  }
  
  if(is.null(cespecie) | is.na(cespecie)){
    cespecie <- 'NULL'
  }
  
  if(is.null(hcolecta) | is.na(hcolecta) | nchar(hcolecta) == 0){
    hcolecta <- 'NULL'
  }
  
  if(is.null(ncolecta) | is.na(ncolecta)){
    ncolecta <- 'NULL'
  }
  
  if(is.null(temperatura) | is.na(temperatura)){
    temperatura <- 'NULL'
  }
  
  if(is.null(hr) | is.na(hr)){
    hr <- 'NULL'
  }
  
  if(is.null(norte) | is.na(norte)){
    norte <- 'NULL'
  }
  
  if(is.null(este) | is.na(este)){
    este <- 'NULL'
  }
  
  con <- dbConnect(MySQL(),
                   user = USER_MYSQL,
                   password = PASSWORD_MYSQL,
                   host = HOST_MYSQL,
                   dbname = DB_MYSQL, port = PORT_MYSQL)
  
  query <- paste0("INSERT INTO `controladultos` ",
                  "(`ID`, `DISTRITO`, `FECHA`, `EESS`,",
                  " `LOCALIDAD`, `COLECTA`, `IPHH`, `IPHN`,",
                  " `CANTIDADESPECIE`, `HORASCOLECTA`, ",
                  " `ESPECIE`, `NCOLECTA`, `TEMPERATURA`,",
                  " `HR`, `VIVIENDA`, `NORTE`, `ESTE`)",
                  " VALUES (NULL, '",distrito,"', '",fecha,
                  "', '",eess,"', '",localidad,"', '",colecta,"', '",iphh,
                  "', '",iphn,"', '",cespecie,"', '",hcolecta,
                  "', '",especie,"', '",ncolecta,"', '",temperatura,
                  "', '",hr,"', NULL, '",norte,"', '",este,"');")
  query <-  gsub("'NULL'", "NULL", query, fixed = TRUE)
  dbSendQuery(con, "SET NAMES utf8mb4;")
  on.exit(dbDisconnect(con))
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbClearResult(rs)
  return(df)
}

CL_sql <- function(distrito, eess, localidad, clasificacion,
                   tipo, extension, metodo, larvicida, especie, 
                   il_pre, il_post,fecha,  norte, este){
  
  
  if(length(fecha) == 0){
    fecha <- 'NULL'
  }

  if(is.null(extension) | is.na(extension) | nchar(extension) == 0){
    extension <- 'NULL'
  }
  
  if(is.null(larvicida) | is.na(larvicida)){
    larvicida <- 'NULL'
  }
  
  if(is.null(il_pre) | is.na(il_pre)){
    il_pre <- 'NULL'
  }
  
  if(is.null(il_post) | is.na(il_post)){
    il_post <- 'NULL'
  }
  
  if(is.null(norte) | is.na(norte)){
    norte <- 'NULL'
  }
  
  if(is.null(este) | is.na(este)){
    este <- 'NULL'
  }
  
  con <- dbConnect(MySQL(),
                   user = USER_MYSQL,
                   password = PASSWORD_MYSQL,
                   host = HOST_MYSQL,
                   dbname = DB_MYSQL, port = PORT_MYSQL)
  
  query <- paste0("INSERT INTO `controlcriaderos` ",
                  "(`ID`, `DISTRITO`, `EESS`,",
                  " `LOCALIDAD`, `CLASIFICACION`, `TIPO`, `EXTENSION`,",
                  " `METODO`, `LARVICIDA`, "," `ESPECIE`,",
                  " `IL-PRE`, `IL-POST`,",
                  " `FECHA`, `NORTE`, `ESTE`)",
                  " VALUES (NULL, '",distrito,"', '",eess,
                  "', '",localidad,"', '",clasificacion,"', '",tipo,"', '",extension,
                  "', '",metodo,"', '",larvicida,"', '",especie,
                  "', '",il_pre,"', '",il_post,"', '",fecha,"', '",norte,"', '",este,"');")
  query <-  gsub("'NULL'", "NULL", query, fixed = TRUE)
  dbSendQuery(con, "SET NAMES utf8mb4;")
  on.exit(dbDisconnect(con))
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbClearResult(rs)
  return(df)
}


vectorAdulto_mysql <- function(distrito, localidad, eess, start, end){
  con <- dbConnect(MySQL(),
                   user = USER_MYSQL,
                   password = PASSWORD_MYSQL,
                   host = HOST_MYSQL,
                   dbname = DB_MYSQL, port = PORT_MYSQL)
  
  query = paste0("SELECT * FROM `controladultos` WHERE `DISTRITO` LIKE '",distrito,"' AND `LOCALIDAD` = '",
                 localidad,"' AND `EESS` = '",eess,"' AND `FECHA` BETWEEN '",start,"' AND '",end,"';")
  
  dbSendQuery(con, "SET NAMES utf8mb4;")
  on.exit(dbDisconnect(con))
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbClearResult(rs)
  return(df)
}

criadero_mysql <- function(distrito, localidad, eess, start, end){
  con <- dbConnect(MySQL(),
                   user = USER_MYSQL,
                   password = PASSWORD_MYSQL,
                   host = HOST_MYSQL,
                   dbname = DB_MYSQL, port = PORT_MYSQL)
  
  query = paste0("SELECT * FROM `controlcriaderos` WHERE `DISTRITO` LIKE '",distrito,"' AND `LOCALIDAD` = '",
                 localidad,"' AND `EESS` = '",eess,"' AND `FECHA` BETWEEN '",start,"' AND '",end,"';")
  
  dbSendQuery(con, "SET NAMES utf8mb4;")
  on.exit(dbDisconnect(con))
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbClearResult(rs)
  return(df)
}

metadata_setnumber <- function(netlab, number){
  con <- dbConnect(MySQL(),
                   user = 'ingreso',
                   password = '123ingreso321',
                   host = HOST_MYSQL,
                   dbname = 'seqcoviddb')
  query <- paste0("UPDATE `metadata` SET `NUMERACION_PLACA` = '",
                  number,"' WHERE `metadata`.`NETLAB` = \'",netlab,"\'")
  query <- gsub("'NULL'", "NULL", query, fixed = TRUE)
  on.exit(dbDisconnect(con))
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbClearResult(rs)
  return(df)
}

delete_sql <- function(query){
  
  con <- dbConnect(MySQL(),
                   user = USER_MYSQL,
                   password = PASSWORD_MYSQL,
                   host = HOST_MYSQL,
                   dbname = DB_MYSQL, port = PORT_MYSQL)
  on.exit(dbDisconnect(con))
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbClearResult(rs)
  return(df)
}

enumerar <- function(netlab_list, max){
  n = max + 1
  for( i in netlab_list){
    metadata_setnumber(i, n)
    n = n + 1
  }
}

metadataAsignar <- function(Corrida, Placa, Oficio){
  
  con <- dbConnect(MySQL(),
                   user = 'ingreso',
                   password = '123ingreso321',
                   host = HOST_MYSQL,
                   dbname = 'seqcoviddb')
  query <- paste0("UPDATE `metadata` SET `CORRIDA` = '",
                  Corrida,"', `PLACA` = '",Placa,"' WHERE `OFICIO` = '",Oficio,
                  "' AND `DNI_CE` IS NOT NULL AND `PLACA` IS NULL AND `CORRIDA` IS NULL ORDER BY `metadata`.`FECHA_INGRESO_BASE` ASC;")
  query <- gsub("'NULL'", "NULL", query, fixed = TRUE)
  on.exit(dbDisconnect(con))
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbClearResult(rs)
  return(df)
}

#DELETE FROM `metadata2` WHERE `metadata2`.`NETLAB` = \'AAA2\'"
#data <- metadata_sql(corrida = 1028, placa = 'placa1')
