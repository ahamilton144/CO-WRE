# shiny app for CO water rights explorer

rm(list=ls())

install.packages('shinythemes')
library(shiny)
library(ggplot2)
library(shinythemes)
library(sp)  
library(rgdal)
library(leaflet)
library(igraph)

# get ditches, fields, downloaded from CDSS
div <- readOGR('.','Div4_2010_Ditches_1402_sub')
stm <- readOGR('.','NHDFlowline_sub')

# project to lat/long
div <- spTransform(div, CRS("+init=epsg:4326"))  # spTransform makes the projection
stm <- spTransform(stm, CRS("+init=epsg:4326"))  # spTransform makes the projection

# get water rights info
wr <- read.csv('DWR_Water_Right_Net_Amounts_small.csv', stringsAsFactors = F)

# get subset of water rights present in our WS
wrsub <- wr[wr$WDID %in% div@data$wdid, ]
wrsub$WDID <- as.character(wrsub$WDID)
wrsub$Appropriation.Date <- as.Date(wrsub$Appropriation.Date, '%m/%d/%Y')

# get rights details for ditches
div@data$numRights <- sapply(div@data$wdid, function(x) sum(wrsub$WDID==x))
div@data$pri <- lapply(div@data$wdid, function(x) wrsub$Appropriation.Date[wrsub$WDID==x])
div@data$approp <- lapply(div@data$wdid, function(x) wrsub$Net.Abs[wrsub$WDID==x])
div@data$inset <- sapply(1:nrow(div@data), function(x) 
  paste('wdid =',div@data$wdid[x],'<br>Priorities =',div@data$pri[x],'<br>Appropriations =', div@data$approp[x]))

ui <- fluidPage(
  theme = shinytheme('spacelab'),
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

# Processing for server
server <- function(input, output){
  # Plot map with Leaflet
  output$map1 <- renderLeaflet({
    m <- leaflet(stm) %>%
    addTiles() %>%
    addPolylines(color='black') %>%
    # addCircles(data=div, color='red')
    addMarkers(data=div, popup = ~inset)
      
  })
  
}

shinyApp(ui=ui, server=server)

