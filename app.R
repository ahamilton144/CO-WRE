# shiny app for CO water rights explorer
rm(list=ls())
library(shiny)
library(ggplot2)
# library(devtools)
# devtools::install_github("rstudio/shinythemes",force=T)
#source('r_packages.R')
# devtools::install_github("gaborcsardi/pkgconfig")
# devtools::install_github("igraph/rigraph")
#install.packages('igraph')
library(shinythemes)
library(igraph)

library(sp)  
library(rgdal)
library(leaflet)

# Load graphs from parent folder
#load('gDiv.rda')
#load('gStm.rda')
load('div.rda')
load('div_geo.rda')
load('stm_geo.rda')
#load('m1.rda')
# load('gDivSub.rda')
# load('gPlot_vert.rda')
# load('gPlot_edge.rda')
#load('m2.rda')
load('vertPaths.rda')
load('gpv1.rda')
load('gpv2.rda')
load('gpv3.rda')
load('gpe1.rda')
load('gpe2.rda')
load('gpe3.rda')

# get list of wdids
wdidList <- div@data$wdid
# remove wdids with no allocation
nodeList <- match(wdidList, gpv1$wdid1)
nodeList[is.na(nodeList)] <- match(wdidList[is.na(nodeList)], gpv1$wdid2)
nodeList[is.na(nodeList)] <- match(wdidList[is.na(nodeList)], gpv1$wdid3)
nodeList <- nodeList[gpv1$atot[nodeList] > 0]
wdidList <- wdidList[wdidList %in% c(gpv1$wdid1[nodeList], gpv1$wdid2[nodeList], gpv1$wdid3[nodeList])]



# gpv$inset <- as.character(gpv$inset)

ui <- fluidPage(
  theme = shinytheme('spacelab'),
  titlePanel('CO Water Right Explorer'),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel('Data Selection', id='tabIntro',
                 selectInput('WDID','Diversion WDID',wdidList),
                 selectInput('buysell','Buy or sell right',c('Buy','Sell')),
                 numericInput('numRights','Number of potential rights',value=5,min=0,step=1),
                 numericInput('amt','Size of diversion (cfs)',value=1.0,min=0, step=0.1),
                 selectInput('sec','Security of right',c('High','Medium','Low')),
                 textOutput('text1'),
                 textOutput('text2')
        ),
        tabPanel('Instructions', id='tabInstr',
                 'write here too!')
      )
    ),
    mainPanel(
      h4('Full stream network'),
      leafletOutput('map1')   # Output plot full dataset (only if data))
    )
  )
)

# Processing for server
server <- function(input, output){
   
  # print results
  output$text1 <- renderText({
    paste('Found', length(outputVerts()), 'potential trades.')
  })
  output$text2 <- renderText({
    if(length(outputVerts()) > 0){
      paste(outputVerts()$name)
    }
  })
  
  # Plot map
  output$map1 <- renderLeaflet({
    leaflet(stm_geo) %>%
      addTiles() %>%
      addPolylines(color=~color,popup=~inset, group='Original') %>%
      addCircles(data=div_geo, color='black',popup = ~inset, group='Original') %>%
      addPolylines(data=gpe1, color=~color, popup=~inset, group='Average Flow') %>%
      addCircles(data=gpv1, color=~color, popup=~inset, group='Average Flow') %>%
      addPolylines(data=gpe2, color=~color, popup=~inset, group='Low Flow') %>%
      addCircles(data=gpv2, color=~color, popup=~inset, group='Low Flow') %>%
      addPolylines(data=gpe3, color=~color, popup=~inset, group='High Flow') %>%
      addCircles(data=gpv3, color=~color, popup=~inset, group='High Flow') #%>%
      # hideGroup('Average Flow') %>%
      # hideGroup('Low Flow') %>%
      # hideGroup('High Flow') #%>%
      # addLayersControl(
      #   baseGroups = c('simple','Original'),
      #   options = layersControlOptions(collapsed=F)
      # )
  })
  
  #dataset changes based on security selection
  getGpv <- function(sec){
    if(sec == 'Medium'){
      dum <- gpv1
    }else if (sec == 'Low'){
      dum <- gpv3
    }else{
      dum <- gpv2
    }
    return(dum)
  }
  getGpe <- function(sec){
    if(sec == 'Medium'){
      dum <- gpe1
    }else if (sec == 'Low'){
      dum <- gpe3
    }else{
      dum <- gpe2
    }
    return(dum)
  }
  gpv <- reactive({
    getGpv(input$sec)
  })
  gpe <- reactive({
    getGpe(input$sec)
  })
  gShow <- reactive({
    ifelse(input$sec == 'Medium', 'Average Flow', ifelse(input$sec == 'Low', 'High Flow', 'Low Flow'))
  })
  observe({
    # Switch stm/div network when chosen security level changes
    leafletProxy('map1') %>%
      hideGroup('Average Flow') %>%
      hideGroup('Low Flow') %>%
      hideGroup('High Flow') %>%
      showGroup(gShow()) %>%
      addLayersControl(
        baseGroups = c(gShow(),'Original'),
        options = layersControlOptions(collapsed=F)
      )
      

  })

  # Monitor selected WDID, highlight matching node
  chosenVert <- reactive({
    gpv()[which((gpv()$wdid1 == input$WDID) | (gpv()$wdid2 == input$WDID) | (gpv()$wdid3 == input$WDID)),]
  })
  observe({
    # Add marker at chosen node
    leafletProxy('map1') %>%
      clearGroup('chosenVert') %>%
      addMarkers(data=chosenVert(), popup=~inset, group='chosenVert')
  })
  
  # Moniter selected all inputs & highlight output nodes
  getOutputVerts <- function(buysell, numRights, amt, chosenVert, gpv){
    req(numRights)
    req(amt)
    if (buysell == 'Buy'){
      # get list of upstream nodes
      dum <- vertPaths[chosenVert$name, colnames(vertPaths)!=chosenVert$name]
      dum <- dum[order(dum)]
      dum <- dum[!is.na(dum)]
      neiVert <- names(dum)
      neiVert <- match(neiVert, gpv$name)   
      if (sum(gpv$atot[neiVert] > 0) > 0){
        neiVert <- neiVert[gpv$atot[neiVert] > 0]      
        # # limit to nodes with full diversions
        # if (sum(gpv$atot[neiVert] == gpv$wtot[neiVert]) > 0){
        #   neiVert <- neiVert[gpv$atot[neiVert] == gpv$wtot[neiVert]]
          # limit to nodes with total diversions > amt 
          if (sum(gpv$wtot[neiVert]) > amt - 1e-13){
            neiVert <- neiVert[gpv$wtot[neiVert] > amt - 1e-13]
            # get closest numRights
            if(length(neiVert) > numRights){
              neiVert <- neiVert[1:numRights]
            }
            neiVert <- gpv[neiVert,]
          }else{
            neiVert <- numeric()
          }
        # }else{
        #   neiVert <- numeric()
        # }
      }
    }else{
      # get list of downstream nodes
      dum <- vertPaths[rownames(vertPaths)!=chosenVert$name, chosenVert$name]
      dum <- dum[order(dum)]
      dum <- dum[!is.na(dum)]
      neiVert <- names(dum)
      neiVert <- match(neiVert, gpv$name)   
      if (sum(gpv$atot[neiVert] > 0) > 0){
        neiVert <- neiVert[gpv$atot[neiVert] > 0]      
        # # limit to nodes with full diversions
        # if (sum(gpv$atot[neiVert] == gpv$wtot[neiVert]) > 0){
        #   neiVert <- neiVert[gpv$atot[neiVert] == gpv$wtot[neiVert]]
          # limit to nodes with total diversions > amt 
          if (sum(gpv$wtot[neiVert]) > amt - 1e-13){
            neiVert <- neiVert[gpv$wtot[neiVert] > amt - 1e-13]
            # get closest numRights
            if(length(neiVert) > numRights){
              neiVert <- neiVert[1:numRights]
            }
            neiVert <- gpv[neiVert,]
          }else{
            neiVert <- numeric()
          }
        # }else{
        #   neiVert <- numeric()
        # }
      }
    }
    return (neiVert)
  }
  outputVerts <- reactive({
    getOutputVerts(input$buysell, input$numRights, as.numeric(input$amt), chosenVert(), gpv())
  })
  observe({
    # Add output nodes to map
    leafletProxy('map1') %>%
      clearGroup('outputVerts') 
    if(length(outputVerts()) > 0){
      leafletProxy('map1') %>%
        addMarkers(data=outputVerts(), popup=~inset, group='outputVerts')
    }
  })
  


}

shinyApp(ui=ui, server=server)




