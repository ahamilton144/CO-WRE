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
load('vertPathsFull.rda')
load('gpv1.rda')
load('gpv2.rda')
load('gpv3.rda')
load('gpe1.rda')
load('gpe2.rda')
load('gpe3.rda')
load('g1.rda')
load('g2.rda')
load('g3.rda')

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
                 selectInput('buysell','Upstream or Downstream',c('Upstream'='Buy','Downstream'='Sell')),
                 numericInput('numRights','Number of potential rights',value=5,min=0,step=1),
                 numericInput('amt','Size of diversion (cfs)',value=1.0,min=0, step=0.1),
                 selectInput('sec','Security of right',c('High','Medium','Low')),
                 textOutput('text1'),
                 textOutput('text2')
        )#,
        # tabPanel('Instructions', id='tabInstr',
        #          'write here too!')
      )
    ),
    mainPanel(
      # h4('Full stream network'),
      leafletOutput('map1')   # Output plot full dataset (only if data))
    )
  )
)

# Processing for server
server <- function(input, output){
   
  # print results
  output$text1 <- renderText({
    as.character(V(g())$q[1])
    # paste('Found', length(outputVerts()), 'potential trades.')
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
      addPolylines(color=~color,popup=~inset, group='Original stream network') %>%
      addCircles(data=div_geo, color='black',popup = ~inset, group='Original stream network') %>%
      addPolylines(data=gpe1, color='grey', popup=~inset, group='Allocation network - Mean flow') %>%
      addCircles(data=gpv1, color=~color, popup=~inset, group='Allocation network - Mean flow') %>%
      addPolylines(data=gpe2, color='grey', popup=~inset, group='Allocation network - Low flow') %>%
      addCircles(data=gpv2, color=~color, popup=~inset, group='Allocation network - Low flow') %>%
      addPolylines(data=gpe3, color='grey', popup=~inset, group='Allocation network - High flow') %>%
      addCircles(data=gpv3, color=~color, popup=~inset, group='Allocation network - High flow') #%>%
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
  getG <- function(sec){
    if(sec == 'Medium'){
      dum <- g1
    }else if (sec == 'Low'){
      dum <- g3
    }else{
      dum <- g2
    }
    return(dum)
  }
  gpv <- reactive({
    getGpv(input$sec)
  })
  gpe <- reactive({
    getGpe(input$sec)
  })
  g <- reactive({
    getG(input$sec)
  })
  gShow <- reactive({
    ifelse(input$sec == 'Medium', 'Allocation network - Mean flow', ifelse(input$sec == 'Low', 'Allocation network - High flow', 'Allocation network - Low flow'))
  })
  observe({
    # Switch stm/div network when chosen security level changes
    leafletProxy('map1') %>%
      hideGroup('Allocation network - Mean flow') %>%
      hideGroup('Allocation network - Low flow') %>%
      hideGroup('Allocation network - High flow') %>%
      showGroup(gShow()) %>%
      addLayersControl(
        baseGroups = c(gShow(),'Original stream network'),
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
  getOutputVerts <- function(buysell, numRights, amt, chosenVert, gpv, g){
    req(numRights)
    req(amt)
    if (buysell == 'Buy'){
      # get list of upstream nodes
      dum <- vertPaths[chosenVert$name, colnames(vertPaths)!=chosenVert$name]
      dum <- dum[order(dum)]
      dum <- dum[!is.na(dum)]
      neiVert <- names(dum)
      neiVert <- match(neiVert, gpv$name)
    }else{
      # get list of downstream nodes
      dum <- vertPaths[rownames(vertPaths)!=chosenVert$name, chosenVert$name]
      dum <- dum[order(dum)]
      dum <- dum[!is.na(dum)]
      neiVert <- names(dum)
      neiVert <- match(neiVert, gpv$name)
    }
    # decide whether each alloc meets reqs 
    gpv$meetCrit <- NA
    for (i in 1:length(gpv)){
      # is alloc filled
      t1 <- gpv[i,]$w[[1]] == gpv[i,]$a_full[[1]]
      # is sum of filled allocs >= amt
      t2 <- sum(t1 * gpv[i,]$w[[1]]) > amt - 1e-13
      # does node have at least one alloc meeting reqs
      gpv$meetCrit[i] <- t2 
      
    }
    # limit to nodes meeting crits
    if (sum(gpv[neiVert, ]$meetCrit) > 0){
      neiVert <- neiVert[gpv[neiVert, ]$meetCrit]   
      # neiVert <- gpv[neiVert,]$name
    }else{
      neiVert <- numeric()
    }
    
    # check whether each trade allowed, and limit to numRights
    if (length(neiVert) > 0){
      feas <- rep(F, length(neiVert))
      j <- 1
      while ((j <= length(neiVert)) & (sum(feas) < numRights)){
        feas[j] <- tryTrade_simple(chosenVert$name, gpv[neiVert[j],]$name, amt, g)
        j <- j + 1
      }
      neiVert <- neiVert[feas]
    }
    if (length(neiVert) > 0){
      neiVert <- gpv[neiVert,]
    }else{
      neiVert <- numeric()
    }
    
    return (neiVert)
  }
  
  tryTrade_simple <- function(nb, ns, amt, g){
    # choose allocs for trade, based on meeting crits
    full <- V(g)[ns]$w[[1]] == V(g)[ns]$a_full[[1]]
    i <- length(full)
    trd <- rep(0, length(full))
    pri <- as.integer(V(g)[ns]$pri[[1]])
    while((i > 0) & (sum(trd) < amt)){
      if (full[order(pri)][i]){
        dum <- pri[order(pri)][i]
        trd[order(pri)[i]] <- min(V(g)[ns]$w[[1]][order(pri)[i]], amt - sum(trd))
      }
      i <- i - 1
    }

    # make test graph
    gt <- g
    
    # make trade
    for (i in 1:length(trd)){
      if (trd[i] > 0){
        V(gt)[nb]$a_full[[1]] <- c(V(gt)[nb]$a_full[[1]], trd[i])      
        V(gt)[nb]$pri[[1]] <- c(V(gt)[nb]$pri[[1]], V(gt)[ns]$pri[[1]][i])
        V(gt)[ns]$a_full[[1]][i] <- V(gt)[ns]$a_full[[1]][i] - trd[i]
        # set withdrawals too, assuming trade ok
        V(gt)[nb]$w[[1]] <- c(V(gt)[nb]$w[[1]], trd[i])
        V(gt)[ns]$w[[1]][i] <- V(gt)[ns]$w[[1]][i] - trd[i]
      }
    }
    V(gt)$atot <- sapply(V(gt)$a_full, sum)
    V(gt)$wtot <- sapply(V(gt)$w, sum)
    
    # adjust flow q based on trades
    for (i in 1:length(ns)){
      # if buyer is US, flows between will be smaller by dw*con
      if (!is.na(vertPathsFull[ns[i], nb[i]])){
        bw <- get.shortest.paths(gt, from=nb[i], to=ns[i])$vpath[[1]]$name
        # dont need to adjust flow at DS node
        V(gt)[bw[1:(length(bw)-1)]]$q <- V(gt)[bw[1:(length(bw)-1)]]$q - V(gt)[bw[1]]$con * amt[i]
      }else{  # if buyer is DS, flows b/w will be larger by dw*con
        bw <- get.shortest.paths(gt, from=ns[i], to=nb[i])$vpath[[1]]$name
        # dont need to adjust flow at DS node
        V(gt)[bw[1:(length(bw)-1)]]$q <- V(gt)[bw[1:(length(bw)-1)]]$q + V(gt)[bw[1]]$con * amt[i]
      }
      # round
      V(gt)$q[abs(V(gt)$q) < 1e-13] <- 0
    }
    
    # get US-most (us), and DS-most (ds) buyer and seller, and all in-between nodes (bw)
    us <- c(ns,nb)[which(colSums(!is.na(vertPathsFull[c(ns,nb),c(ns,nb)])) == length(c(ns,nb)))]
    ds <- c(ns,nb)[which(rowSums(!is.na(vertPathsFull[c(ns,nb),c(ns,nb)])) == length(c(ns,nb)))]
    bw <- get.shortest.paths(gt, from=us, to=ds)$vpath[[1]]$name
    
    # check whether trade ok
    # check that all outflows positive
    t1 <- sum(V(gt)$q < 0) == 0 
    # check that all inflows sufficient for withdrawals
    t2 <- sum(V(gt)$q - V(gt)$wtot * (1 - V(gt)$con) < -1e-13) == 0
    # V(gt)$goodTrade <- (t1 & t2)
    
    return(t1 & t2)
  }
  outputVerts <- reactive({
    getOutputVerts(input$buysell, input$numRights, as.numeric(input$amt), chosenVert(), gpv(), g())
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




