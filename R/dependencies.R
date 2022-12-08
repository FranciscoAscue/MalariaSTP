################################################################################
## ubuntu 20.04 / 21.10
## debian 10
# Install dependencies 
# . libprotobuf-dev
# . libjq-dev
# . protobuf-compiler
# . libudunits2-dev
# . libgdal-dev
# . libsodium-dev

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

if( !is.element("devtools",rownames(installed.packages() ) ) ){
  install.packages("devtools")
}

library(devtools)

if( !is.element("rlang",rownames(installed.packages() ) )  ){
  install_version("rlang", version = "1.0.6", repos = "http://cran.us.r-project.org")
}
library(rlang)

################################################################################


dependencies <- c("shiny","shinyauthr","shinycssloaders","shinythemes","plotly","fossil",
                  "remotes","dplyr","rgdal","sp","sf","geojsonsf","DT","htmlwidgets", 
                  "leaflet","leaflet.minicharts","shinyjs","viridisLite","viridis",
                  "leaflet.extras", "leaflet.extras2", "RColorBrewer","stringr",
                  "tidyverse","ggplot2","splitstackshape","RMySQL")

################################################################################
# Package R dependencies
################################################################################
for(i in dependencies){
  missingPackages(i)
  library(i, character.only = TRUE)
}

if( !is.element("epical",rownames(installed.packages() ) ) ){
  remotes::install_github("chrismerkord/epical")
}


library(splines)
library(epical)
library(utils)

################################################################################

