
library(geojsonio)
library(broom)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(dplyr)
library(viridis)

setwd("~/Documentos/satipo_malaria/")
dataCases <- read.table("PLAN2.csv", sep = ";", header = TRUE)datacount <- dataCases %>%  filter(UBIGEO > 120600, UBIGEO < 120610)

datacount <- dataCases %>% filter(ANO == 2022) %>% group_by(UBIGEO_DESC) %>% summarise(n = n())
#datacount <- dataCases %>% filter(ANO == 2022) %>% group_by(LOCALIDAD) %>% summarise(n = n())
datacount <- datacount %>% filter(n > 16) #%>% group_by(UBIGEO_DESC) %>% summarise(n = n())

datacount <- datacount %>% filter(UBIGEO_DESC %in% c("VIZCATAN DEL ENE","PANGOA","MAZAMARI","RIO NEGRO","CORIVIALI","RIO TAMBO")) 


spdf <- geojson_read("Maps/mapSatipoActual.geojson", what = "sp")
#sddf <- geojson_read("mapRioTamboVizcatan.geojson")
#datacount$n <- log10(datacount$n)

spdf_fortified <- tidy(spdf, region = "NOMBDIST")

spdf_fortified = spdf_fortified %>%
  left_join(. , datacount, by=c("id"="UBIGEO_DESC"))

# Note that if the number of restaurant is NA, it is in fact 0
#spdf_fortified$nb_equip[ is.na(spdf_fortified$nb_equip)] = 0.001



eess <- read.csv("GeoPeru-instituciones_salud.csv")

eess["LOCALIDAD"] <- eess$mretdscl 
dataCases$mretdscl <- dataCases$LOCALIDAD
eessSatipo <- eess %>% filter(nom_prov == "SATIPO")
eessSatipo <- eessSatipo %>% filter(nom_dist %in% c("VIZCATAN DEL ENE","RIO TAMBO")) 



ccnn <- read.table("CentrosPobladosSatipo.csv", sep = ";", header = TRUE)
ccnn <- ccnn %>% filter(DISTRITO %in% c("VIZCATAN DEL ENE","RIO TAMBO")) 



p <- ggplot() +
  geom_polygon(data = spdf_fortified, 
               aes(fill = n, x = long, y = lat, group = group) , 
               size=5, alpha=0.9) +
  #geom_point(data=ccnn, 
  #           aes(x=Latitud...coord.X., y = Longitud...coord.Y., fill = NULL),
  #           color = "black")+
  #geom_point(data=eessSatipo, 
  #           aes(x=este_sig, y=norte_sig, fill=NULL), 
  #           color = "red")+
  
  theme_economist()+
  scale_fill_gradient2( low = "#229954",
                        mid = "#CDE94C",
                        high = "#E74C3C", 
                        breaks=c(100,500,1000,1500),
                        name="Numero de casos de Malaria",
                        guide = guide_legend( keyheight = unit(3, units = "mm"),
                                              keywidth=unit(12, units = "mm"),
                                              label.position = "bottom", 
                                              title.position = 'top', nrow=1) )+
  # scale_fill_viridis(trans = "log", 
  #                    breaks=c(10,100,500,1000,1500), 
  #                    name="Numero de casos de Malaria", 
  #                    guide = guide_legend( keyheight = unit(3, units = "mm"),
  #                                          keywidth=unit(12, units = "mm"), 
  #                                          label.position = "bottom", 
  #                                          title.position = 'top', nrow=1),direction = -1 ) +
  labs(
    title = "PROVINCIA SATIPO",
    subtitle = "Casos de malaria x distrito",
    caption = "Red de Salud Satipo "#| Creation:  "
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.2, 0.09)
  ) +
  coord_map()
p



library(ggmap)
ggmap::register_google()
register_google(key = "AIzaSyD8dCdhd7qmaDHRboUH7yZKiTSmkOu91SI", write = TRUE)


mapa <- get_map("Peru", source = "google", maptype = "roadmap" , zoom = 6)
ruta <- route(from = "Puerta del Sol, Madrid", to = "Plaza de Castilla, Madrid")
ggmap(mapa) + 
  geom_point(aes(x = -74.00241465, y =-11.99612569, colour = "Quempiri"), size = 2, shape = 9) +  
  guides(color = guide_legend(ncol=5
        , title = "Ubicación Geográfica")
        , fill = guide_legend(title="Legend" , nrow = 1) )+
  theme(
    legend.key = element_blank(),
    legend.title = element_text(size = 7 , face = "bold"),
    legend.margin = margin(0, 0, 0, 0),
    legend.text = element_text(size = 6),
    panel.background = element_blank(),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank())


runif(1,-1,1)

library(ggmap)
library(dplyr)
library(ggplot2)
library(ggthemes)
ggmap::register_google()
register_google(key = "AIzaSyD8dCdhd7qmaDHRboUH7yZKiTSmkOu91SI", write = TRUE)

col1 = "#011f4b"
  
col2 = "#6497b1"
  
col3 = "#b3cde0"
  
col4 = "#CC0000"
  
mapa <- get_map(c(lat = -11.627312, lon =-74.319166), 
                source = "google", maptype = "roadmap" , zoom = 9,language = "spanish" )

setwd("~/Documentos/satipo_malaria/")

eessSatipo <- read.csv("BaseDatos/EESSsatipo2.csv", header = FALSE)

eessSatipo <- eessSatipo %>% filter(V4 %in% c("VIZCATAN DEL ENE",
                                              "RIO TAMBO",
                                              "PANGOA",
                                              "MAZAMARI",
                                              "CORIVIALI",
                                              "RIO NEGRO"))
colnames(eessSatipo) <- c("V1","V2","eess_DESC","V4","V5","V6")

epidemio <- read.csv("BaseDatos/epidemio2022.csv")
epidemio_trimestre <- epidemio %>% filter(SEMANA >= 44)

epidemio_trimestre <- epidemio %>% filter(SEMANA >= 14, SEMANA <= 26)
epidemio_trimestre <- epidemio %>% filter(SEMANA >= 27, SEMANA <= 39)



count <- epidemio_trimestre %>% group_by(eess_DESC) %>% summarise(n = n())

count[count$n > 1]





plot <- merge(epidemio_trimestre, eessSatipo, by="eess_DESC", all.x = FALSE)
count2 <- plot %>% group_by(eess_DESC) %>% summarise(n = n())

eeselecionados <- count2$eess_DESC[count2$n > 30] 

unique(plot$eess_DESC)
unique(eessSatipo$eess_DESC)
unique(epidemio$eess_DESC)

PosNegRandom <- function(n){
  p <- floor(runif(n,-1,1))
  p[p == 0] <- 1
  t <- runif(n,0.001, 0.01)
  return(p*t)
}

str(PosNegRandom(nrow(plot)))

plot$V5 <- as.numeric(plot$V5) + PosNegRandom(nrow(plot))
plot$V6 <- as.numeric(plot$V6) + PosNegRandom(nrow(plot))



chart <- ggmap(ggmap=mapa)

chart <- chart+geom_density2d(data=plot, 
                              aes(x = as.numeric(V6), 
                                  y = as.numeric(V5)), size = .2)

chart <- chart+stat_density2d(data=plot, 
                              aes(x=as.numeric(V6),
                                  y=as.numeric(V5), fill=..level.., 
                                  alpha=..level..),geom="polygon", size=2, bins=22)

chart <- chart+scale_fill_gradient(low = "yellow", high = "red")

chart


### vector adulto ###


### criaderos ###-11.975685, -74.057912

satipomap <- qmap(c(lat = -11.975685, lon =-74.057912), zoom =10)

satipomap +
  geom_point(aes(x =as.numeric(V6), y = as.numeric(V5), colour = UBIGEO_DESC), data = plot, size = 1)



################## Anexo ###################

ggmap(mapa, extent = "device") + 
  stat_contour( data = eess, geom="polygon", 
                aes( x = x_gis, y = y_gis, z = est_codi_1, 
                     fill = ..level.. ) ) +
  scale_fill_continuous( name = "Casos", low = "yellow", high = "red" )


ggmap(mapa, extent = "device") +
  stat_summary_2d(data = eess, aes(x = x_gis, y = y_gis, 
                                         z = est_codi_2), fun = mean, alpha = 0.6, bins = 30) +
  scale_fill_gradient(name = "Riesgo", low = "green", high = "red") 




########## Vectores ################

vector <- read.csv("BaseDatos/VectorAdultoSetiembre.csv")
vector <- read.csv("Vectores//VectorAdultoOctubreF.csv")
vector <- read.csv("Vectores/VectorAdultoNoviembre.csv")
eessSatipo <- read.csv("BaseDatos/EESSsatipo2.csv", header = FALSE)

eessSatipo <- eessSatipo %>% filter(V4 %in% c("VIZCATAN DEL ENE",
                                              "RIO TAMBO",
                                              "PANGOA",
                                              "MAZAMARI",
                                              "CORIVIALI",
                                              "RIO NEGRO"))
colnames(eessSatipo) <- c("V1","V2","EESS","V4","V5","V6")

satipomap <- qmap(c(lat = -11.975685, lon =-74.057912), zoom =10)
satipomap <- mapa <- get_map(c(lat = -11.975685, lon =-74.057912), 
                             source = "google", maptype = "roadmap" , 
                             zoom = 9,language = "spanish" )
satipomap <- ggmap(ggmap=mapa)

IPH <- merge(vector, eessSatipo, by = "EESS",  all.x = FALSE)

PosNegRandom <- function(n){
  p <- floor(runif(n,-1,1))
  p[p == 0] <- 1
  t <- runif(n,0.001, 0.002)
  return(p*t)
}

IPH$V5 <- as.numeric(IPH$V5) + PosNegRandom(nrow(IPH))
IPH$V6 <- as.numeric(IPH$V6) + PosNegRandom(nrow(IPH))


## #6A0097
## #154360 
satipomap + geom_density2d(data=plot, 
                            aes(x = as.numeric(V6), 
                                y = as.numeric(V5)), size = .2)+
            stat_density2d(data=plot, 
                              aes(x=as.numeric(V6),
                                  y=as.numeric(V5), fill=..level.., 
                                  alpha=..level..),geom="polygon", size=1, bins=22) +
  scale_fill_gradient(low = "yellow", high = "red") +
  geom_point(aes(x =as.numeric(V6), 
                 y = as.numeric(V5),
                 size = IPHH), 
             data = IPH, colour = "#6A0097", alpha = .5) + # scale_color_gradient(low = "#FDFEFE",high = "#7D3C98") + 
  theme_base()
 


count <- plot %>% group_by(eess_DESC) %>% summarise(n = n())

unique(IPH$EESS)


##### metaxenicas IPA ##########


IPA <- read.csv("BaseDatos/IPA.csv")
IPA <- IPA %>% filter(DISTRITO == "PANGOA")
epidemio <- read.csv("BaseDatos/epidemio2022.csv")
epidemio <- epidemio %>% filter(UBIGEO_DESC == "RIO TAMBO")

eessSatipo <- read.csv("BaseDatos/EESSsatipo2.csv", header = FALSE)

eessSatipo <- eessSatipo %>% filter(V4 %in% c("VIZCATAN DEL ENE",
                                              "RIO TAMBO",
                                              "PANGOA",
                                              "MAZAMARI",
                                              "CORIVIALI",
                                              "RIO NEGRO"))
colnames(eessSatipo) <- c("V1","V2","eess_DESC","V4","V5","V6")


plot <- merge(epidemio, eessSatipo, by="eess_DESC", all.x = FALSE)

PosNegRandom <- function(n){
  p <- floor(runif(n,-1,1))
  p[p == 0] <- 1
  t <- runif(n,0.001, 0.03)
  return(p*t)
}

plot$V5 <- as.numeric(plot$V5) + PosNegRandom(nrow(plot))
plot$V6 <- as.numeric(plot$V6) + PosNegRandom(nrow(plot))

## -12.003347, -74.012125
## -11.521393, -74.466632
## -11.721813, -74.227871

satipomap <- mapa <- get_map(c(lat = -12.003347, lon =-74.012125), 
                             source = "google", maptype = "roadmap" , 
                             zoom = 11,language = "spanish" )

satipomap <- ggmap(ggmap=mapa)

satipomap <- qmap(c(lat = -12.00003, lon = -74.007871), zoom =11)

satipomap + geom_density2d(data=plot, 
                           aes(x = as.numeric(V6), 
                               y = as.numeric(V5)), size = .2)+
  stat_density2d(data=plot, 
                 aes(x=as.numeric(V6),
                     y=as.numeric(V5), fill=..level.., 
                     alpha=..level..),geom="polygon", size=1, bins=22) +
  scale_fill_gradient(low = "yellow", high = "red") +
  
  geom_label(aes(x=as.numeric(ESTE), y = as.numeric(NORTE),
                 label = LOCALIDAD), data = IPA, label.size = 0.05)+
  geom_point(aes(x =as.numeric(ESTE), 
                 y = as.numeric(NORTE),
                 size = IPA.3), 
             data = IPA, colour = "#6A0097", alpha = .5) +
  theme_base()


count <- plot %>% group_by(eess_DESC) %>% summarise(n = n())



################ larvas ##############


IPA <- read.csv("Vectores/larva.csv")
IPA <- IPA %>% filter(MES == "OCTUBRE")
epidemio <- read.csv("BaseDatos/epidemio2022.csv")
epidemio <- epidemio %>% filter(SEMANA >= 38, SEMANA <= 44)

eessSatipo <- read.csv("BaseDatos/EESSsatipo2.csv", header = FALSE)

eessSatipo <- eessSatipo %>% filter(V4 %in% c("VIZCATAN DEL ENE",
                                              "RIO TAMBO",
                                              "PANGOA",
                                              "MAZAMARI",
                                              "CORIVIALI",
                                              "RIO NEGRO"))
colnames(eessSatipo) <- c("V1","V2","eess_DESC","V4","V5","V6")


plot <- merge(epidemio, eessSatipo, by="eess_DESC", all.x = FALSE)

PosNegRandom <- function(n){
  p <- floor(runif(n,-1,1))
  p[p == 0] <- 1
  t <- runif(n,0.001, 0.03)
  return(p*t)
}

plot$V5 <- as.numeric(plot$V5) + PosNegRandom(nrow(plot))
plot$V6 <- as.numeric(plot$V6) + PosNegRandom(nrow(plot))

## -12.003347, -74.012125
## -11.521393, -74.466632
## -11.721813, -74.227871

satipomap <- mapa <- get_map(c(lat = -12.003347, lon =-74.012125), 
                             source = "google", maptype = "roadmap" , 
                             zoom = 11,language = "spanish" )

satipomap <- ggmap(ggmap=mapa)

satipomap <- qmap(c(lat = -12.00003, lon = -74.007871), zoom =11)

satipomap + geom_density2d(data=plot, 
                           aes(x = as.numeric(V6), 
                               y = as.numeric(V5)), size = .2)+
  stat_density2d(data=plot, 
                 aes(x=as.numeric(V6),
                     y=as.numeric(V5), fill=..level.., 
                     alpha=..level..),geom="polygon", size=1, bins=22) +
  scale_fill_gradient(low = "yellow", high = "red") +
  
  geom_label(aes(x=as.numeric(ESTE), y = as.numeric(NORTE),
                 label = LOCALIDAD), data = IPA, label.size = 0.05)+
  geom_point(aes(x =as.numeric(ESTE), 
                 y = as.numeric(NORTE),
                 size = INDICE_LARVARIO, colour = TIPO), 
             data = IPA,alpha = .5) +
  scale_color_discrete() +
  theme_base()


count <- plot %>% group_by(eess_DESC) %>% summarise(n = n())





