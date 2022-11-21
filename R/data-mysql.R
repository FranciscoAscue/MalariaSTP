if( !is.element("RMySQL",rownames(installed.packages() ) ) ){
  install.packages("RMySQL")
}

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

metadata <- function(db, tabla, corrida){
  
  con <- dbConnect(MySQL(),
                   user = 'ingreso',
                   password = '123ingreso321',
                   host = 'localhost',
                   dbname = db)
  query = paste0("SELECT * FROM `",tabla,"` WHERE `CORRIDA` = ",corrida,";")
  dbSendQuery(con, "SET NAMES utf8mb4;")
  on.exit(dbDisconnect(con))
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbClearResult(rs)
  return(df)
}

metadata_sql <- function(oficio = NULL){
  
  con <- dbConnect(MySQL(),
                   user = 'ingreso',
                   password = '123ingreso321',
                   host = 'localhost',
                   dbname = 'seqcoviddb')
  
  
  query = paste0("SELECT NUMERACION_PLACA,NETLAB,OFICIO,DNI_CE,CORRIDA,PLACA FROM `metadata` WHERE `OFICIO` LIKE '",
                 oficio,"' AND `CORRIDA` IS NULL AND `DNI_CE` IS NOT NULL AND `PLACA` IS NULL AND `CORRIDA` IS NULL ORDER BY `metadata`.`FECHA_INGRESO_BASE` ASC;")
  
  dbSendQuery(con, "SET NAMES utf8mb4;")
  on.exit(dbDisconnect(con))
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbClearResult(rs)
  return(df)
}

metadataOrder <- function(corrida, placa, reasig = FALSE){
  
  con <- dbConnect(MySQL(),
                   user = 'root',
                   password = 'maq12345',
                   host = 'localhost',
                   dbname = 'seqcoviddb') ### cambiar a seqcoviddb
  if(isTRUE(reasig)){
    query = paste0("SELECT NUMERACION_PLACA,NETLAB,OFICIO,DNI_CE,CORRIDA,PLACA FROM `metadata` WHERE `CORRIDA` = ",corrida," AND `PLACA` = '",placa,"' ORDER BY `metadata`.`FECHA_INGRESO_BASE` ASC;")  
  }else{
    query = paste0("SELECT NUMERACION_PLACA FROM `metadata` WHERE `CORRIDA` = ",corrida," AND `PLACA` = '",placa,"' ORDER BY `metadata`.`FECHA_INGRESO_BASE` ASC;")
  }
  dbSendQuery(con, "SET NAMES utf8mb4;")
  on.exit(dbDisconnect(con))
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbClearResult(rs)
  return(df)
}



metadataSendquery <- function(netlab, oficio){
  
  con <- dbConnect(MySQL(),
                   user = 'root',
                   password = 'maq12345',
                   host = 'localhost',
                   dbname = 'seqcoviddb')
  query = paste0("INSERT INTO `metadata` (`NETLAB`, `OFICIO`, `CT`, `CT2`, `FECHA_TM`, `REGION`, `PROCEDENCIA`, `PROVINCIA`, `DISTRITO`, `APELLIDO_NOMBRE`, `DNI_CE`, `EDAD`, `SEXO`, `VACUNADO`, `MARCA_PRIMERAS_DOSIS`, `1DOSIS`, `2DOSIS`, `MARCA_3DOSIS`, `3DOSIS`, `HOSPITALIZACION`, `MOTIVO`, `FALLECIDO`, `NUMERACION_PLACA`, `PLACA`, `CORRIDA`, `RECEPCIONADA`, `CODIGO`, `VERIFICADO`, `FECHA_INGRESO_BASE`) VALUES ('",
                 netlab,"','",oficio,"', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, CURRENT_TIMESTAMP);")
  rs = dbSendQuery(con, query);
  #df = fetch(rs, -1);
  dbDisconnect(con)
  #return(df)
}


metadaupdate <- function(oficio){
  
  con <- dbConnect(MySQL(),
                   user = 'root',
                   password = 'maq12345',
                   host = 'localhost',
                   dbname = 'seqcoviddb')
  query = paste0("SELECT NETLAB, OFICIO, CT, CT2, FECHA_TM, MOTIVO, DNI_CE FROM `metadata`  WHERE `OFICIO` = '",oficio,"' ORDER BY `metadata`.`FECHA_INGRESO_BASE` ASC;")
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbDisconnect(con)
  return(df)
}


delete_sql <- function(query){
  
  con <- dbConnect(MySQL(),
                   user = 'root',
                   password = 'maq12345',
                   host = 'localhost',
                   dbname = 'seqcoviddb')
  #query = paste0("SELECT NETLAB, OFICIO, CT, CT2, FECHA_TM, MOTIVO, CODIGO  FROM `metadata`  WHERE `OFICIO` = '",oficio,"' ORDER BY `metadata`.`FECHA_TM` DESC;")
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbDisconnect(con)
  return(df)
}

library(utils)

update_sql <- function(sql_id, ct, ct2, fecha_tm, motivo, dni){
  
  if(is.null(ct) | is.na(ct)){
    ct <- 'NULL'
  }
  
  if(is.null(ct2) | is.na(ct2)){
    ct2 <- 'NULL'
  }
  
  if(length(fecha_tm) == 0){
    fecha_tm <- 'NULL'
  }
  
  if(is.null(dni) | is.na(dni) | nchar(dni) == 0){
    dni <- 'NULL'
  }
  con <- dbConnect(MySQL(),
                   user = 'root',
                   password = 'maq12345',
                   host = 'localhost',
                   dbname = 'seqcoviddb')
  query <- paste0("UPDATE `metadata` SET `CT` = '",
                  ct,"', `CT2` = '",ct2,"', `FECHA_TM` = '",
                  fecha_tm,"', `MOTIVO` = '",motivo,"', `DNI_CE` = '",
                  dni,"' WHERE `metadata`.`NETLAB` = \'",sql_id,"\'")
  query <- gsub("'NULL'", "NULL", query, fixed = TRUE)
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbDisconnect(con)
  return(df)
}


metadata_setnumber <- function(netlab, number){
  con <- dbConnect(MySQL(),
                   user = 'ingreso',
                   password = '123ingreso321',
                   host = 'localhost',
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
                   host = 'localhost',
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
