library(shiny)
library(ggplot2)
# library(devtools)
# devtools::install_github("rstudio/shinythemes",force=T)
#source('r_packages.R')
# devtools::install_github("gaborcsardi/pkgconfig")
# devtools::install_github("igraph/rigraph")
#install.packages('igraph')
#library(shinythemes)
library(igraph)

library(sp)  
library(rgdal)
library(leaflet)

ui <- fluidPage(
  #theme = shinytheme('spacelab'),
  titlePanel('CO Water Right Explorer'),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel('Introduction', id='tabIntro',
                 'TEST'
        ),
        tabPanel('Instructions', id='tabInstr',
                 'write here too!')
      )
    ),
    mainPanel(
      textOutput('txt'),  # Output text (only if no data)
      h4('Full Dataset, Master'),
      #      textOutput('txtNoData'),  # Output text before data selection
      leafletOutput('map1')   # Output plot full dataset (only if data))
    )
  )
)
