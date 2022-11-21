################################################################################
## ubuntu 20.04 / 21.10
## debian 10
# Install dependencies 
# . libprotobuf-dev
# . libjq-dev
# . protobuf-compiler
# . libudunits2-dev
# . libgdal-dev

################################################################################
# Install missing packages

missingPackages <- function(pkg){
    if( !is.element(pkg,rownames(installed.packages() ) ) ){
      message(pkg, "-----> Package is not installed ")
      if(pkg == "htmlwidgets"){
        install.packages(pkg, version = "1.5.4")
      }else{
        install.packages(pkg)
      }
    }
}
################################################################################


dependencies <- c("shiny","shinyauthr","shinycssloaders","shinythemes","plotly","fossil", "remotes","dplyr",
                  "rgdal","sp","sf","geojsonsf","DT","htmlwidgets", "leaflet","leaflet.minicharts",
                  "viridisLite","viridis","leaflet.extras", "leaflet.extras2", "RColorBrewer","stringr","tidyverse","ggplot2","splitstackshape")

################################################################################
# Package R dependencies
################################################################################
for(i in dependencies){
  missingPackages(i)
  library(i, character.only = TRUE)
}

library(rlang)
library(splines)
library(writexl)
library(stringr)
library(readr)
library(shinyjs)
library(epical)
library(readxl)
################################################################################

