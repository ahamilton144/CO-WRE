# shiny app for CO water rights explorer
rm(list=ls())
library(shiny)
library(ggplot2)
library(shinythemes)
library(igraph)

library(sp)  
library(rgdal)
library(leaflet)

load('div.rda')
load('div_geo.rda')
load('stm_geo.rda')
load('vertPaths.rda')
load('vertPathsFull.rda')
load('irr_geo.rda')
# load('gpv1.rda')
load('gpv2.rda')
load('gpv3.rda')
load('gpv4.rda')
# load('gpe1.rda')
load('gpe2.rda')
load('gpe3.rda')
load('gpe4.rda')
# load('g1.rda')
load('g2.rda')
load('g3.rda')
load('g4.rda')

# g1$color <- as.character(g1$color_PercUsed)
g2$color <- as.character(g2$color_PercUsed)
g3$color <- as.character(g3$color_PercUsed)
g4$color <- as.character(g4$color_PercUsed)
# gpe1$color <- as.character(gpe1$color_PercUsed)
gpe2$color <- as.character(gpe2$color_PercUsed)
gpe3$color <- as.character(gpe3$color_PercUsed)
gpe4$color <- as.character(gpe4$color_PercUsed)
# gpv1$color <- as.character(gpv1$color)
gpv2$color <- as.character(gpv2$color)
gpv3$color <- as.character(gpv3$color)
gpv4$color <- as.character(gpv4$color)


# get list of wdids
wdidList <- div@data$wdid
# remove wdids with no allocation
nodeList <- match(wdidList, gpv2$wdid1)
nodeList[is.na(nodeList)] <- match(wdidList[is.na(nodeList)], gpv2$wdid2)
nodeList[is.na(nodeList)] <- match(wdidList[is.na(nodeList)], gpv2$wdid3)
nodeList <- nodeList[gpv2$atot[nodeList] > 0]
wdidList <- wdidList[wdidList %in% c(gpv2$wdid1[nodeList], gpv2$wdid2[nodeList], gpv2$wdid3[nodeList])]

mapDone <- T

ui <- navbarPage('CO Water Right Explorer', 
                 tabPanel('Instructions', value='tab1',
                          'Instructions coming soon...'),
                 tabPanel('Explore allocations', value='tab2',
                          sidebarLayout(
                            sidebarPanel(id='tab1Controls',
                                         selectInput('bWDID','Buyer WDID',wdidList),
                                         selectInput('sec','Security of right',c('High (drought of record, 29 years)'='sec3', 'Medium (5 year return low flow)'='sec2',
                                                                                 'Low (median flow)'='sec1')),
                                         numericInput('amt','Size of diversion (cfs)',value=1.0,min=0, step=0.1), 
                                         selectInput('usds','Loop upstream or downstream',c('Upstream'='us','Downstream'='ds')),
                                         numericInput('numRights','Number of potential rights',value=5,min=0,step=1),
                                         textOutput('text1.1'),
                                         textOutput('text1.2')
                            ),
                            mainPanel(
                              # textOutput('text1.3'),
                              leafletOutput('map1', height='800px')   # Output plot full dataset (only if data))
                            )
                          )
                 ),
                 tabPanel('Test trades', value='tab3',
                          sidebarLayout(
                            sidebarPanel(id='tabIntro',
                                         # textOutput('text2.1'),
                                         # textOutput('text2.2'),
                                         selectInput('t2.numSales', 'Number of sales', 1:3),
                                         selectInput('t2.bWDID.1','Buyer 1 WDID',wdidList),
                                         uiOutput('t2.sWDID.list.1'),
                                         uiOutput('t2.sWDID.allocs.1'),
                                         uiOutput('t2.bWDID.2'),
                                         uiOutput('t2.sWDID.list.2'),
                                         uiOutput('t2.sWDID.allocs.2'),
                                         uiOutput('t2.bWDID.3'),
                                         uiOutput('t2.sWDID.list.3'),
                                         uiOutput('t2.sWDID.allocs.3'),
                                         actionButton('button2', 'Submit')

                            ),
                            mainPanel(
                              # textOutput('text2.1'),
                              # textOutput('text2.2'),
                              leafletOutput('map2', height='800px')   # Output plot full dataset (only if data))
                            )
                          )
                 ),

                selected='tab2'
)
                 


# Processing for server
server <- function(input, output){
  ###### tab 1 ############### 
  # print results
  output$text1.1 <- renderText({
    # as.character(V(g())$q[1])
    paste('Found', length(outputVerts()), 'potential trades.')
  })
  output$text1.2 <- renderText({
    if(length(outputVerts()) > 0){
      paste(outputVerts()$name)
    }
  })
  output$text1.3 <- renderText({
    req(mapDone)
    'Loading...'
  })
  
  # Plot map
  output$map1 <- renderLeaflet({
    leaflet(stm_geo) %>%
      addTiles() %>%
      addPolygons(data=irr_geo, color='grey', popup=~inset, group='Original stream network') %>%
      addPolylines(color=~color,popup=~inset, group='Original stream network') %>%
      addCircles(data=div_geo, color='black',popup = ~inset, group='Original stream network') %>%
      addPolylines(data=gpe2, color=~color, popup=~inset, group='Allocation network - Low security') %>%
      addCircles(data=gpv2, color=~color, popup=~inset, group='Allocation network - Low security') %>%
      addPolylines(data=gpe3, color=~color, popup=~inset, group='Allocation network - Medium security') %>%
      addCircles(data=gpv3, color=~color, popup=~inset, group='Allocation network - Medium security') %>%
      addPolylines(data=gpe4, color=~color, popup=~inset, group='Allocation network - High security') %>%
      addCircles(data=gpv4, color=~color, popup=~inset, group='Allocation network - High security')

  })
  
  #dataset changes based on security selection
  getGpv <- function(sec){
    if(sec == 'sec1'){
      dum <- gpv2
    }else if (sec == 'sec2'){
      dum <- gpv3
    # }else if (sec == 'sec3'){
    #   dum <- gpv3
    }else{
      dum <- gpv4
    }
    return(dum)
  }
  getGpe <- function(sec){
    if(sec == 'sec1'){
      dum <- gpe2
    }else if (sec == 'sec2'){
      dum <- gpe3
    # }else if (sec == 'sec3'){
    #   dum <- gpe3
    }else{
      dum <- gpe4
    }
    return(dum)
  }
  getG <- function(sec){
    if(sec == 'sec1'){
      dum <- g2
    }else if (sec == 'sec2'){
      dum <- g3
    # }else if (sec == 'sec3'){
    #   dum <- g3
    }else{
      dum <- g4
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
    ifelse(input$sec == 'sec1', 'Allocation network - Low security', ifelse(input$sec == 'sec2', 'Allocation network - Medium security', ifelse(input$sec == 'sec3', 'Allocation network - High security')))
  })
  observe({
    # Switch stm/div network when chosen security level changes
    leafletProxy('map1') %>%
      hideGroup('Allocation network - Low security') %>%
      hideGroup('Allocation network - Medium security') %>%
      hideGroup('Allocation network - High security') %>%
      showGroup(gShow()) %>%
      addLayersControl(
        baseGroups = c(gShow(),'Original stream network'),
        options = layersControlOptions(collapsed=F)
      )
      

  })

  # Monitor selected bWDID, highlight matching node
  nb <- reactive({
    gpv()[which((gpv()$wdid1 == input$bWDID) | (gpv()$wdid2 == input$bWDID) | (gpv()$wdid3 == input$bWDID)),]
    
  })
  observe({
    # Add marker at chosen node
    leafletProxy('map1') %>%
      clearGroup('nb') %>%
      addMarkers(data=nb(), popup=~inset, group='nb')
  })
  
  # Moniter selected all inputs & highlight output nodes
  getOutputVerts <- function(usds, numRights, amt, nb, gpv, g){
    req(numRights)
    req(amt)
    if (usds == 'us'){
      # get list of upstream nodes
      dum <- vertPaths[nb$name, colnames(vertPaths)!=nb$name]
      dum <- dum[order(dum)]
      dum <- dum[!is.na(dum)]
      neiVert <- names(dum)
      neiVert <- match(neiVert, gpv$name)
    }else{
      # get list of downstream nodes
      dum <- vertPaths[rownames(vertPaths)!=nb$name, nb$name]
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
        feas[j] <- tryTrade_simple(nb$name, gpv[neiVert[j],]$name, amt, g)
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
    getOutputVerts(input$usds, input$numRights, as.numeric(input$amt), nb(), gpv(), g())
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
  
  
  
  
  
  
  ########################################################  
  #### tab 2 ####################
  # #########################################################
  # print controls from tab1
  output$text2.1 <- renderText({
    as.character(b2count())
  })
  output$text2.2 <- renderText({
    as.character(tradeDone())
  })
  b2count <- eventReactive(input$button2, {'hello'})
  tradeDone <- reactive({length(unlist(V(gt())$w))})
  
  ### controls dependent on input
  # seller list of WDID does not contain selected usdser
  output$t2.sWDID.list.1 <- renderUI({
    nbdum <- gpv()[which((gpv()$wdid1 == input$t2.bWDID.1) | (gpv()$wdid2 == input$t2.bWDID.1) | (gpv()$wdid3 == input$t2.bWDID.1)),]
    nbdum1 <- getOutputVerts_noCheck('us', 1000, 0.01, nbdum, gpv(), g())
    nbdum2 <- getOutputVerts_noCheck('ds', 1000, 0.01, nbdum, gpv(), g())
    nbdum <- character()
    if (!is.null(nrow(nbdum1))){
      nbdum <- c(nbdum, nbdum1$name)
    }
    if (!is.null(nrow(nbdum2))){
      nbdum <- c(nbdum, nbdum2$name)
    }
   if (length(nbdum) > 0){
      wdidDum <- unlist(gpv()$wdid[gpv()$name %in% nbdum])
      wdidDum <- wdidDum[wdidDum != input$t2.bWDID.1]
     if (length(wdidDum) > 0){
        selectInput('t2.sWDID.1', 'Seller 1 WDID', wdidDum)
      }else{
        selectInput('t2.sWDID.1', 'Seller 1 WDID', 'None')
      }
    }else{
      selectInput('t2.sWDID.1', 'Seller 1 WDID', 'None')
    }

  })
  # slider for amt to buy, between 0 and sum of filled allocs at buyer
  output$t2.sWDID.allocs.1 <- renderUI({
    sliderInput('t2.amt.1', 'Amount to buy from seller 1', min=0,
                max=sum(t2.ns()[1,]$a_full[[1]][t2.ns()[1,]$a_full[[1]] == t2.ns()[1,]$w[[1]]]),
                value=sum(t2.ns()[1,]$a_full[[1]][t2.ns()[1,]$a_full[[1]] == t2.ns()[1,]$w[[1]]]))
  })
  # number of selectable buyers/sellers/amts depends on selection of numSales
  output$t2.bWDID.2 <- renderUI({
    if (as.numeric(input$t2.numSales) > 1){
      selectInput('t2.bWDID.2','Buyer 2 WDID',wdidList[!wdidList %in% c(input$t2.bWDID.1, input$t2.sWDID.1)])
    }
  })
  output$t2.sWDID.list.2 <- renderUI({
    if (as.numeric(input$t2.numSales) > 1){
      nbdum <- gpv()[which((gpv()$wdid1 == input$t2.bWDID.2) | (gpv()$wdid2 == input$t2.bWDID.2) | (gpv()$wdid3 == input$t2.bWDID.2)),]
      nbdum <- c(getOutputVerts('us', 1000, 0, nbdum, gpv(), g())$name, getOutputVerts('ds', 1000, 0, nbdum, gpv(), g())$name)
      wdidDum <- unlist(gpv()$wdid[gpv()$name %in% nbdum])
      # dum <- unlist(gpv()$wdid[gpv()$wtot > 1e-13])
      # wdidDum <- wdidList[wdidList %in% dum]
      selectInput('t2.sWDID.2', 'Seller 2 WDID', wdidDum[!wdidDum %in% c(input$t2.bWDID.2, input$t2.bWDID.1, input$t2.sWDID.1)])
    }
  })
  output$t2.sWDID.allocs.2 <- renderUI({
    if (as.numeric(input$t2.numSales) > 1){
      sliderInput('t2.amt.2', 'Amount to buy from seller 2', min=0,
                  max=sum(t2.ns()[2,]$a_full[[1]][t2.ns()[2,]$a_full[[1]] == t2.ns()[2,]$w[[1]]]),
                  value=sum(t2.ns()[2,]$a_full[[1]][t2.ns()[2,]$a_full[[1]] == t2.ns()[2,]$w[[1]]]))
    }
  })
  output$t2.bWDID.3 <- renderUI({
    if (as.numeric(input$t2.numSales) > 2){
      selectInput('t2.bWDID.3','Buyer 3 WDID',wdidList[!wdidList %in% c(input$t2.bWDID.1, input$t2.sWDID.1, input$t2.bWDID.2, input$t2.sWDID.2)])
    }
  })
  output$t2.sWDID.list.3 <- renderUI({
    if (as.numeric(input$t2.numSales) > 2){
      nbdum <- gpv()[which((gpv()$wdid1 == input$t2.bWDID.3) | (gpv()$wdid2 == input$t2.bWDID.3) | (gpv()$wdid3 == input$t2.bWDID.3)),]
      nbdum <- c(getOutputVerts('us', 1000, 0, nbdum, gpv(), g())$name, getOutputVerts('ds', 1000, 0, nbdum, gpv(), g())$name)
      wdidDum <- unlist(gpv()$wdid[gpv()$name %in% nbdum])
      # dum <- unlist(gpv()$wdid[gpv()$wtot > 1e-13])
      # wdidDum <- wdidList[wdidList %in% dum]
      selectInput('t2.sWDID.3', 'Seller 2 WDID', wdidDum[!wdidDum %in% c(input$t2.bWDID.3, input$t2.bWDID.1, input$t2.sWDID.1, input$t2.bWDID.2, input$t2.sWDID.2)])
    }
  })
  output$t2.sWDID.allocs.3 <- renderUI({
    if (as.numeric(input$t2.numSales) > 2){
      sliderInput('t2.amt.3', 'Amount to buy from seller 3', min=0,
                  max=sum(t2.ns()[2,]$a_full[[1]][t2.ns()[2,]$a_full[[1]] == t2.ns()[2,]$w[[1]]]),
                  value=sum(t2.ns()[2,]$a_full[[1]][t2.ns()[2,]$a_full[[1]] == t2.ns()[2,]$w[[1]]]))
    }
  })
  
  # Monitor selected bWDID, highlight matching node
  getNb <- function(gpv, numSales, bWDID.1, bWDID.2, bWDID.3){
    req(bWDID.1)
    t2.nb <- gpv[which((gpv$wdid1 == bWDID.1) | (gpv()$wdid2 == bWDID.1) | (gpv()$wdid3 == bWDID.1)),]
    if (numSales > 1){
      req(bWDID.2)
      t2.nb <- rbind(t2.nb, gpv[which((gpv$wdid1 == bWDID.2) | (gpv$wdid2 == bWDID.2) | (gpv$wdid3 == bWDID.2)),])
    }
    if (numSales > 2){
      req(bWDID.3)
      t2.nb <- rbind(t2.nb, gpv[which((gpv$wdid1 == bWDID.3) | (gpv$wdid2 == bWDID.3) | (gpv$wdid3 == bWDID.3)),])
    }
    return(t2.nb)
  }
  t2.nb <- reactive({
    getNb(gpv(), input$t2.numSales, input$t2.bWDID.1, input$t2.bWDID.2, input$t2.bWDID.3)
  })
  # Monitor selected sWDID, get matching node
  getNs <- function(gpv, numSales, sWDID.1, sWDID.2, sWDID.3){
    req(sWDID.1)
    t2.ns <- gpv[which((gpv$wdid1 == sWDID.1) | (gpv()$wdid2 == sWDID.1) | (gpv()$wdid3 == sWDID.1)),]
    if (numSales > 1){
      req(sWDID.2)
      t2.ns <- rbind(t2.ns, gpv[which((gpv$wdid1 == sWDID.2) | (gpv()$wdid2 == sWDID.2) | (gpv()$wdid3 == sWDID.2)),])
    }
    if (numSales > 2){
      req(sWDID.3)
      t2.ns <- rbind(t2.ns, gpv[which((gpv$wdid1 == sWDID.3) | (gpv()$wdid2 == sWDID.3) | (gpv()$wdid3 == sWDID.3)),])
    }
    return(t2.ns)
  }
  t2.ns <- reactive({
    getNs(gpv(), input$t2.numSales, input$t2.sWDID.1, input$t2.sWDID.2, input$t2.sWDID.3)
  })
  # get amounts for trade
  getAmt <- function(gpv, numSales, amt.1, amt.2, amt.3){
    req(amt.1)
    t2.amt <- amt.1
    if (numSales > 1){
      req(amt.2)
      t2.amt <- c(t2.amt, amt.2)
    }
    if (numSales > 2){
      req(amt.3)
      t2.amt <- c(t2.amt, amt.3)
    }
    return(t2.amt)
  }
  t2.amt <- reactive({
    getAmt(gpv(), input$t2.numSales, as.numeric(input$t2.amt.1), as.numeric(input$t2.amt.2), as.numeric(input$t2.amt.3))
  })


  getOutputVerts_noCheck <- function(usds, numRights, amt, nb, gpv, g){
    req(numRights)
    req(amt)
    if (usds == 'us'){
      # get list of upstream nodes
      dum <- vertPaths[nb$name, colnames(vertPaths)!=nb$name]
      dum <- dum[order(dum)]
      dum <- dum[!is.na(dum)]
      neiVert <- names(dum)
      neiVert <- match(neiVert, gpv$name)
    }else{
      # get list of downstream nodes
      dum <- vertPaths[rownames(vertPaths)!=nb$name, nb$name]
      dum <- dum[order(dum)]
      dum <- dum[!is.na(dum)]
      neiVert <- names(dum)
      neiVert <- match(neiVert, gpv$name)
    }
    neiVert <- gpv[neiVert,]

    return (neiVert)
  }
  


  # # map2 originally taken  from map1
  output$map2 <- renderLeaflet({
    req(input$button2)
    leaflet(stm_geo) %>%
      addTiles() %>%
      addPolylines(data=gpe2, color='grey', popup=~inset, group='Allocation network - Mean flow') %>%
      addCircles(data=gpv2, color='white', popup=~inset, group='Allocation network - Mean flow') 
  })
    
  observeEvent(input$button2, {
    leafletProxy('map2') %>%
      addMarkers(data=t2.nb(), popup=~inset, group='t2.nb') %>%
      addMarkers(data=t2.ns(), popup=~inset, group='t2.ns')
  })


  observeEvent(input$button2, {
    # Add marker at chosen buyer node
    leafletProxy('map2') %>%
      clearGroup('t2.nb') %>%
      addMarkers(data=t2.nb(), popup=~inset, group='t2.nb')
  })
  observeEvent(input$button2, {
    # Add marker at chosen seller node
    leafletProxy('map2') %>%
      clearGroup('t2.ns') %>%
      addMarkers(data=t2.ns(), popup=~inset, group='t2.ns')
  })

  
  
  
  
  # try trade, based on input
  doTrade <- function(t2.nb, t2.ns, amt, g){
    req(t2.nb())
    req(t2.ns())
    req(amt)

    # make test graph
    gt <- g

    # choose allocs for trade, based on meeting crits
    for (t in 1:length(t2.nb)){
      full <- V(g)[t2.ns[t]]$w[[1]] == V(g)[t2.ns[t]]$a_full[[1]]
      i <- length(full)
      trd <- rep(0, length(full))
      pri <- as.integer(V(g)[t2.ns[t]]$pri[[1]])
      while((i > 0) & (sum(trd) < amt[t])){
        if (full[order(pri)][i]){
          dum <- pri[order(pri)][i]
          trd[order(pri)[i]] <- min(V(g)[t2.ns[t]]$w[[1]][order(pri)[i]], amt[t] - sum(trd))
        }
        i <- i - 1
      }

      # make trade
      for (i in 1:length(trd)){
        if (trd[i] > 0){
          V(gt)[t2.nb[t]]$a_full[[1]] <- c(V(gt)[t2.nb[t]]$a_full[[1]], trd[i])
          V(gt)[t2.nb[t]]$pri[[1]] <- c(V(gt)[t2.nb[t]]$pri[[1]], V(gt)[t2.ns[t]]$pri[[1]][i])
          V(gt)[t2.ns[t]]$a_full[[1]][i] <- V(gt)[t2.ns[t]]$a_full[[1]][i] - trd[i]
          # set withdrawals too, assuming trade ok
          V(gt)[t2.nb[t]]$w[[1]] <- c(V(gt)[t2.nb[t]]$w[[1]], trd[i])
          V(gt)[t2.ns[t]]$w[[1]][i] <- V(gt)[t2.ns[t]]$w[[1]][i] - trd[i]
        }
      }
    }

    V(gt)$atot <- sapply(V(gt)$a_full, sum)
    V(gt)$wtot <- sapply(V(gt)$w, sum)

    # adjust flow q based on trades
    for (i in 1:length(t2.ns)){
      # if buyer is US, flows between will be smaller by dw*con
      if (!is.na(vertPathsFull[t2.ns[i], t2.nb[i]])){
        bw <- get.shortest.paths(gt, from=t2.nb[i], to=t2.ns[i])$vpath[[1]]$name
        # dont need to adjust flow at DS node
        V(gt)[bw[1:(length(bw)-1)]]$q <- V(gt)[bw[1:(length(bw)-1)]]$q - V(gt)[bw[1]]$con * amt[i]
      }else{  # if buyer is DS, flows b/w will be larger by dw*con
        bw <- get.shortest.paths(gt, from=t2.ns[i], to=t2.nb[i])$vpath[[1]]$name
        # dont need to adjust flow at DS node
        V(gt)[bw[1:(length(bw)-1)]]$q <- V(gt)[bw[1:(length(bw)-1)]]$q + V(gt)[bw[1]]$con * amt[i]
      }
      # round
      V(gt)$q[abs(V(gt)$q) < 1e-13] <- 0
    }

    # get US-most (us), and DS-most (ds) buyer and seller, and all in-between nodes (bw)
    us <- unique(c(t2.ns,t2.nb))[which(colSums(!is.na(vertPathsFull[unique(c(t2.ns,t2.nb)),unique(c(t2.ns,t2.nb))])) == length(unique(c(t2.ns,t2.nb))))]
    ds <- unique(c(t2.ns,t2.nb))[which(rowSums(!is.na(vertPathsFull[unique(c(t2.ns,t2.nb)),unique(c(t2.ns,t2.nb))])) == length(unique(c(t2.ns,t2.nb))))]
    bw <- get.shortest.paths(gt, from=us, to=ds)$vpath[[1]]$name

    # check whether trade ok
    # check that all outflows positive
    t1 <- sum(V(gt)$q < 0) == 0
    # check that all inflows sufficient for withdrawals
    t2 <- sum(V(gt)$q - V(gt)$wtot * (1 - V(gt)$con) < -1e-13) == 0

    V(gt)$goodTrade <- (t1 & t2)

    #### vis effects of trade on flow and allocations

    # get differences due to trade
    V(gt)$dq <- V(gt)$q - V(g)$q
    V(gt)$dq[abs(V(gt)$dq) < 1e-10] <- 0
    V(gt)$dwtot <- V(gt)$wtot - V(g)$wtot
    V(gt)$dwtot[abs(V(gt)$dwtot) < 1e-10] <- 0
    E(gt)$dq <- NA
    for (e in 1:length(E(gt))){
      E(gt)$dq[e] <- V(gt)[ends(gt, e)[1,1]]$dq
    }
    # set inset
    # V(gt)$inset <- sapply(1:length(V(gt)), function(x)
    #   paste('node =',V(gt)$name[x],'<br>wdid =',V(gt)$wdid[x],'<br>pri',V(gt)$pri[x],'<br>a_full =',V(gt)$a_full[x],
    #         '<br>w =',V(gt)$w[x],'<br>sl =',V(gt)$sl[x],'<br>qout =', V(gt)$q[x]))
    V(gt)$inset <- paste('dw =', as.character(V(gt)$dwtot))
    E(gt)$inset <- paste('dq =', as.character(E(gt)$dq))

    # color vertices
    V(gt)$color <- ifelse(V(gt)$dwtot == 0, 'white',
                            ifelse(V(gt)$dwtot > 0, 'green', 'red'))
    E(gt)$color <- ifelse(E(gt)$dq == 0, 'grey',
                            ifelse(E(gt)$dq > 0, 'blue', 'yellow'))
    return(gt)
  }

  # Calculate new graph from trade
  gt <- eventReactive(input$button2, {
    doTrade(t2.nb()$name, t2.ns()$name, t2.amt(), g())
  })

  # get subset of affected nodes/edges from trade for vis
  updateTradeVert <- function(gt, gpv){
    req(gpv)
    # gdum <- V(gt)$name[V(gt)$color != 'white']
    # gpvt <- gpv[gpv$name %in% gdum, ]
    gpvt <- gpv
    # gpvt$color <- sapply(gpvt$name, function(n) V(gt)$color[V(gt)$name == n])
    # gpvt$inset <- sapply(gpvt$name, function(n) V(gt)$inset[V(gt)$name == n])
    gpvt$color <- V(gt)$color
    gpvt$inset <- V(gt)$inset
    
    return(gpvt)
  }
  gpvt <- eventReactive(input$button2, {
    updateTradeVert(gt(), gpv())
  })
  
  updateTradeEdge <- function(gt, gpe){
    req(gpe)
    # gdum <- E(gt)$usname[E(gt)$color != 'grey']
    # gpet <- gpe[gpe$usname %in% gdum, ]
    gpet <- gpe
    # gpet$color <- sapply(gpet$usname, function(n) E(gt)$color[E(gt)$usname == n])
    # gpet$inset <- sapply(gpet$usname, function(n) E(gt)$inset[E(gt)$usname == n])
    gpet$color <- E(gt)$color
    gpet$inset <- E(gt)$inset

    return(gpet)
  }
  gpet <- eventReactive(input$button2, {
    updateTradeEdge(gt(), gpe())
  })

  #visualize and zoom on results of trade
  observeEvent(gpet(), {
    req(gpet())
    req(gpvt())
    leafletProxy('map2') %>%
      clearGroup('trade') %>%
      addPolylines(data=gpet(), color=~color, popup=~inset, group='trade') %>%
      addCircles(data=gpvt(), color=~color, popup=~inset, group='trade') %>%
      fitBounds(gpet()@bbox['x','min'], gpet()@bbox['y','min'], gpet()@bbox['x','max'], gpet()@bbox['y','max'])
  })

}

shinyApp(ui=ui, server=server)




