# shiny app for CO water rights explorer
rm(list=ls())
library(shiny)
library(ggplot2)
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
                 # tabPanel('Instructions', value='tab1',
                 #          'Wecome to the Colorado Water Right Explorer. This tool will allow you to explore water rights and water ', 
                 #          'availability within the Upper Gunnison River Basin.'),
                 #          # img(src='trd-10pt-named.jpg')),
                 tabPanel('Explore water rights', value='tab2',
                          sidebarLayout(
                            sidebarPanel(id='tab1Controls',
                                         'Welcome to the Colorado Water Right Explorer. This tool will allow you to explore water availability, water rights, ', 
                                         'and potential transfers within the Upper Gunnison River Basin. ',br(),br(), 
                                         'Choose a buyer and parameters for the transfer in order to find potential sellers. ',
                                         'Buyer marked with up-arrow and potential sellers with down-arrows. ',
                                         'Click on markers or circles for information on water rights, or on lines for information on stream flow', br(),br(),
                                         selectInput('bWDID','Buyer WDID',wdidList),
                                         selectInput('sec','Security of right',c('High (drought of record, 29 years)'='sec3', 'Medium (5 year return low flow)'='sec2',
                                                                                 'Low (median flow)'='sec1')),
                                         numericInput('amt','Size of diversion (cfs)',value=1.0,min=0, step=0.1), 
                                         selectInput('usds','Loop upstream or downstream',c('Upstream'='us','Downstream'='ds')),
                                         numericInput('numRights','Number of potential rights',value=5,min=0,step=1),
                                         textOutput('text1.1'),
                                         textOutput('text1.2'), br(),
                                         'Please note that these results have not been validated and are not intended to replace a full modeling effort. ',
                                         'Tool is simply an educational tool for understanding prior appropriation and the third-party effects of water right transfers.'
                                         # textOutput('text1.3')
                                         
                            ),
                            mainPanel(
                              # textOutput('text1.3'),
                              leafletOutput('map1', height='800px')   # Output plot full dataset (only if data))
                            )
                          )
                 ),
                 tabPanel('Test right transfers', value='tab3',
                          sidebarLayout(
                            sidebarPanel(id='tabIntro',
                                         'This part of the tool allows you to test potential multi-party water right transfers. ',
                                         'After finding a set of potential transfers (buyers, sellers, allocations, and amounts) ',
                                         'using the previous tab, enter them here and hit submit to calculate the effects of the trade ',
                                         'on buyers, sellers, third-parties, and instream flows.', br(), br(),
                                         'Be sure to choose the desired security level on the last tab, as this will carry over. ',
                                         'Choose the selections on this page in the order that they appear. ',
                                         'Hit submit when ready and please be patient, as this calculation will take a few minutes.', br(), br(),
                                         textOutput('text2.1'), br(), 
                                         # textOutput('text2.2'),
                                         selectInput('t2.numSales', 'Number of sales', 1:5),
                                         selectInput('t2.bWDID.1','Buyer 1 WDID',wdidList),
                                         uiOutput('t2.sWDID.list.1'),
                                         uiOutput('t2.bWDID.2'),
                                         uiOutput('t2.sWDID.list.2'),
                                         uiOutput('t2.bWDID.3'),
                                         uiOutput('t2.sWDID.list.3'),
                                         uiOutput('t2.bWDID.4'),
                                         uiOutput('t2.sWDID.list.4'),
                                         uiOutput('t2.bWDID.5'),
                                         uiOutput('t2.sWDID.list.5'),
                                         
                                         uiOutput('t2.sWDID.nsalloc.1'),
                                         uiOutput('t2.sWDID.amt.1'),
                                         uiOutput('t2.sWDID.nsalloc.2'),
                                         uiOutput('t2.sWDID.amt.2'),
                                         uiOutput('t2.sWDID.nsalloc.3'),
                                         uiOutput('t2.sWDID.amt.3'),
                                         uiOutput('t2.sWDID.nsalloc.4'),
                                         uiOutput('t2.sWDID.amt.4'),
                                         uiOutput('t2.sWDID.nsalloc.5'),
                                         uiOutput('t2.sWDID.amt.5'),
                                         actionButton('button2', 'Submit'), br(),br(),
                                         'Please note that these results have not been validated and are not intended to replace a full modeling effort. ',
                                         'Tool is simply an educational tool for understanding prior appropriation and the third-party effects of water right transfers.'

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
  # output$text1.3 <- renderText({
  #   input$map1_groups[1]
  # })
  
  
  # Plot map
  output$map1 <- renderLeaflet({
    leaflet(stm_geo) %>%
      addProviderTiles(providers$Esri.WorldShadedRelief) %>%
      addTiles(options=providerTileOptions(opacity = 0.7)) %>%
      addPolygons(data=irr_geo, color='grey', popup=~inset, group='Original stream network') %>%
      addPolylines(data=stm_geo[stm_geo$color == 'orange', ], color='orange',popup=~inset, opacity=1, weight=3, group='Original stream network') %>%
      addPolylines(data=stm_geo[stm_geo$color == 'green', ], color='green',popup=~inset, opacity=1, weight=3, group='Original stream network') %>%
      addPolylines(data=stm_geo[stm_geo$color == 'blue', ], color='blue',popup=~inset, opacity=1, weight=3, group='Original stream network') %>%
      addCircles(data=div_geo, color='black',popup = ~inset, group='Original stream network') %>%
      addPolylines(data=gpe2, color='grey', popup=~inset, weight=3, opacity=1, group='Allocation network - Low security') %>%
      addCircles(data=gpv2[gpv2$atot > 0,], color=~color, popup=~inset, group='Allocation network - Low security') %>%
      addPolylines(data=gpe3, color='grey', popup=~inset, weight=3, opacity=1, group='Allocation network - Medium security') %>%
      addCircles(data=gpv3[gpv3$atot > 0,], color=~color, popup=~inset, group='Allocation network - Medium security') %>%
      addPolylines(data=gpe4, color='grey', popup=~inset, weight=3, opacity=1, group='Allocation network - High security') %>%
      addCircles(data=gpv4[gpv4$atot > 0,], color=~color, popup=~inset, group='Allocation network - High security') 
  })
  
  # add legend dependent on group
  observeEvent(input$map1_groups[1],{
    map1 <- leafletProxy('map1') %>% clearControls()
    if (input$map1_groups[1] == 'Original stream network'){
      map1 <- map1 %>% addLegend("topright", colors=c('blue','green','orange'), labels=c('> 200 cfs','> 10 cfs','< 10 cfs'), title = 'Mean June flow rate')
    }else{
      map1 <- map1 %>% addLegend("topright", colors=c('blue','orange','red'), labels=c('Full withdrawal','Partial withdrawal','No withdrawal'), title = 'Right fulfillment')
    }
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
  icons_nb <- awesomeIcons(
    icon = 'arrow-up',
    iconColor = 'black',
    # library = 'ion',
    markerColor = 'lightgray'
  )
  observe({
    # Add marker at chosen node
    leafletProxy('map1') %>%
      clearGroup('nb') %>%
      # addMarkers(data=nb(), popup=~inset, group='nb')
      addAwesomeMarkers(nb()$x, nb()$y, icon=icons_nb, popup=nb()$inset, group='nb')
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
  
  icons_outputVerts <- awesomeIcons(
    icon = 'arrow-down',
    iconColor = 'black',
    # library = 'ion',
    markerColor = 'lightgray'
  )
  observe({
    # Add output nodes to map
    leafletProxy('map1') %>%
      clearGroup('outputVerts') 
    if(length(outputVerts()) > 0){
      leafletProxy('map1') %>%
        # addMarkers(data=outputVerts(), popup=~inset, group='outputVerts')
        addAwesomeMarkers(outputVerts()$x, outputVerts()$y, icon=icons_outputVerts, popup=outputVerts()$inset, group='outputVerts')
    }
  })
  
  
  
  
  
  
  ########################################################  
  #### tab 2 ####################
  # #########################################################
  # print controls from tab1
  output$text2.1 <- renderText({
    if (input$sec == 'sec1'){
      'Security of rights: Low'
    }else if (input$sec == 'sec2'){
      'Security of rights: Medium'
    }else {
      'Security of rights: High'
    }
    
  })
  # output$text2.2 <- renderText({
  #   t2.alloc()
  # })
  
  
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
    gpvdum <- gpv()[gpv()$name %in% nbdum, ]
    gpvdum <- gpvdum[gpvdum$wtot > 0, ]
    gpvdum <- gpvdum[sapply(1:length(gpvdum$a_full), function(x) sum(gpvdum$a_full[[x]]==gpvdum$w[[x]]) > 0), ]
    wdidDum <- unlist(gpvdum$wdid)
    wdidDum <- wdidDum[order(as.numeric(wdidDum))]
    # dum <- unlist(gpv()$wdid[gpv()$wtot > 1e-13])
    # wdidDum <- wdidList[wdidList %in% dum]
    selectInput('t2.sWDID.1', 'Seller 1 WDID', wdidDum)
    
  })
  # slider for which alloc to buy from for seller
  output$t2.sWDID.nsalloc.1 <- renderUI({
    selectInput('t2.nsalloc.1', 'Which allocation to buy from, seller 1', which(t2.ns()[1,]$a_full[[1]] == t2.ns()[1,]$w[[1]]))
  })
  # slider for amt to buy, between 0 and sum of filled allocs at buyer
  output$t2.sWDID.amt.1 <- renderUI({
    sliderInput('t2.amt.1', 'Amount to buy from seller 1', min=0,
                max=t2.ns()[1,]$a_full[[1]][as.integer(input$t2.nsalloc.1)],
                value=t2.ns()[1,]$a_full[[1]][as.integer(input$t2.nsalloc.1)] ,step = 0.1)
  })
  # number of selectable buyers/sellers/amts depends on selection of numSales
  output$t2.bWDID.2 <- renderUI({
    if (as.numeric(input$t2.numSales) > 1){
      selectInput('t2.bWDID.2','Buyer 2 WDID',wdidList)#[!wdidList %in% c(input$t2.bWDID.1, input$t2.sWDID.1)])
    }
  })
  output$t2.sWDID.list.2 <- renderUI({
    if (as.numeric(input$t2.numSales) > 1){
      nbdum <- gpv()[which((gpv()$wdid1 == input$t2.bWDID.2) | (gpv()$wdid2 == input$t2.bWDID.2) | (gpv()$wdid3 == input$t2.bWDID.2)),]
      nbdum1 <- getOutputVerts_noCheck('us', 1000, 0.01, nbdum, gpv(), g())
      nbdum2 <- getOutputVerts_noCheck('ds', 1000, 0.01, nbdum, gpv(), g())
      nbdum <- character()
      if (!is.null(nrow(nbdum1))){
        nbdum <- c(nbdum, nbdum1$name)
      }
      if (!is.null(nrow(nbdum2))){
        nbdum <- c(nbdum, nbdum2$name)
      }
      gpvdum <- gpv()[gpv()$name %in% nbdum, ]
      gpvdum <- gpvdum[gpvdum$wtot > 0, ]
      gpvdum <- gpvdum[sapply(1:length(gpvdum$a_full), function(x) sum(gpvdum$a_full[[x]]==gpvdum$w[[x]]) > 0), ]
      ns1dum <- gpv()[which((gpv()$wdid1 == input$t2.sWDID.1) | (gpv()$wdid2 == input$t2.sWDID.1) | (gpv()$wdid3 == input$t2.sWDID.1)),]
      if (sum(ns1dum$a_full[[1]] == ns1dum$w[[1]]) < 2){
        gpvdum <-gpvdum[gpvdum$name != ns1dum$name, ]
      }
      wdidDum <- unlist(gpvdum$wdid)
      wdidDum <- wdidDum[order(as.numeric(wdidDum))]
      
      
      selectInput('t2.sWDID.2', 'Seller 2 WDID', wdidDum[!wdidDum %in% c(input$t2.bWDID.2)])#, input$t2.bWDID.1, input$t2.sWDID.1)])
    }
  })
  output$t2.sWDID.nsalloc.2 <- renderUI({
    if (as.numeric(input$t2.numSales) > 1){
      adum <- which(t2.ns()[2,]$a_full[[1]] == t2.ns()[2,]$w[[1]])
      ns1dum <- gpv()[which((gpv()$wdid1 == input$t2.sWDID.1) | (gpv()$wdid2 == input$t2.sWDID.1) | (gpv()$wdid3 == input$t2.sWDID.1)),]
      ns2dum <- gpv()[which((gpv()$wdid1 == input$t2.sWDID.2) | (gpv()$wdid2 == input$t2.sWDID.2) | (gpv()$wdid3 == input$t2.sWDID.2)),]
      if (ns1dum$name == ns2dum$name){
        adum <- adum[-which(adum == as.integer(input$t2.nsalloc.1))]
      }
      selectInput('t2.nsalloc.2', 'Which allocation to buy from, seller 2', adum)
    }
  })  
  output$t2.sWDID.amt.2 <- renderUI({
    if (as.numeric(input$t2.numSales) > 1){
      sliderInput('t2.amt.2', 'Amount to buy from seller 2', min=0,
                max=t2.ns()[2,]$a_full[[1]][as.integer(input$t2.nsalloc.2)],
                value=t2.ns()[2,]$a_full[[1]][as.integer(input$t2.nsalloc.2)] ,step = 0.1)
    }
  })
  output$t2.bWDID.3 <- renderUI({
    if (as.numeric(input$t2.numSales) > 2){
      selectInput('t2.bWDID.3','Buyer 3 WDID',wdidList)#[!wdidList %in% c(input$t2.bWDID.1, input$t2.sWDID.1, input$t2.bWDID.2, input$t2.sWDID.2)])
    }
  })
  output$t2.sWDID.list.3 <- renderUI({
    if (as.numeric(input$t2.numSales) > 2){
      nbdum <- gpv()[which((gpv()$wdid1 == input$t2.bWDID.3) | (gpv()$wdid2 == input$t2.bWDID.3) | (gpv()$wdid3 == input$t2.bWDID.3)),]
      nbdum1 <- getOutputVerts_noCheck('us', 1000, 0.01, nbdum, gpv(), g())
      nbdum2 <- getOutputVerts_noCheck('ds', 1000, 0.01, nbdum, gpv(), g())
      nbdum <- character()
      if (!is.null(nrow(nbdum1))){
        nbdum <- c(nbdum, nbdum1$name)
      }
      if (!is.null(nrow(nbdum2))){
        nbdum <- c(nbdum, nbdum2$name)
      }
      gpvdum <- gpv()[gpv()$name %in% nbdum, ]
      gpvdum <- gpvdum[gpvdum$wtot > 0, ]
      gpvdum <- gpvdum[sapply(1:length(gpvdum$a_full), function(x) sum(gpvdum$a_full[[x]]==gpvdum$w[[x]]) > 0), ]
      ns1dum <- gpv()[which((gpv()$wdid1 == input$t2.sWDID.1) | (gpv()$wdid2 == input$t2.sWDID.1) | (gpv()$wdid3 == input$t2.sWDID.1)),]
      ns2dum <- gpv()[which((gpv()$wdid1 == input$t2.sWDID.2) | (gpv()$wdid2 == input$t2.sWDID.2) | (gpv()$wdid3 == input$t2.sWDID.2)),]
      if (ns1dum$name == ns2dum$name){
        if (sum(ns1dum$a_full[[1]] == ns1dum$w[[1]]) < 3){
          gpvdum <-gpvdum[gpvdum$name != ns1dum$name, ]
        }
      }else{
        if (sum(ns1dum$a_full[[1]] == ns1dum$w[[1]]) < 2){
          gpvdum <-gpvdum[gpvdum$name != ns1dum$name, ]
        }
        if (sum(ns2dum$a_full[[1]] == ns2dum$w[[1]]) < 2){
          gpvdum <-gpvdum[gpvdum$name != ns2dum$name, ]
        }
      }
      
      wdidDum <- unlist(gpvdum$wdid)
      wdidDum <- wdidDum[order(as.numeric(wdidDum))]
      
      selectInput('t2.sWDID.3', 'Seller 3 WDID', wdidDum[!wdidDum %in% c(input$t2.bWDID.3)])#, input$t2.bWDID.1, input$t2.sWDID.1, input$t2.bWDID.2, input$t2.sWDID.2)])
    }
  })
  output$t2.sWDID.nsalloc.3 <- renderUI({
    if (as.numeric(input$t2.numSales) > 2){
      adum <- which(t2.ns()[3,]$a_full[[1]] == t2.ns()[3,]$w[[1]])
      ns1dum <- gpv()[which((gpv()$wdid1 == input$t2.sWDID.1) | (gpv()$wdid2 == input$t2.sWDID.1) | (gpv()$wdid3 == input$t2.sWDID.1)),]
      ns2dum <- gpv()[which((gpv()$wdid1 == input$t2.sWDID.2) | (gpv()$wdid2 == input$t2.sWDID.2) | (gpv()$wdid3 == input$t2.sWDID.2)),]
      ns3dum <- gpv()[which((gpv()$wdid1 == input$t2.sWDID.3) | (gpv()$wdid2 == input$t2.sWDID.3) | (gpv()$wdid3 == input$t2.sWDID.3)),]
      if (ns1dum$name == ns3dum$name){
        adum <- adum[-which(adum == as.integer(input$t2.nsalloc.1))]
      }
      if (ns2dum$name == ns3dum$name){
        adum <- adum[-which(adum == as.integer(input$t2.nsalloc.2))]
      }
      selectInput('t2.nsalloc.3', 'Which allocation to buy from, seller 3', adum)
    }
  })
  output$t2.sWDID.amt.3 <- renderUI({
    if (as.numeric(input$t2.numSales) > 2){
      sliderInput('t2.amt.3', 'Amount to buy from seller 3', min=0,
                  max=t2.ns()[3,]$a_full[[1]][as.integer(input$t2.nsalloc.3)],
                  value=t2.ns()[3,]$a_full[[1]][as.integer(input$t2.nsalloc.3)] ,step = 0.1)
    }
  })
  
  output$t2.bWDID.4 <- renderUI({
    if (as.numeric(input$t2.numSales) > 3){
      selectInput('t2.bWDID.4','Buyer 4 WDID',wdidList)#[!wdidList %in% c(input$t2.bWDID.1, input$t2.sWDID.1, input$t2.bWDID.2, input$t2.sWDID.2)])
    }
  })
  output$t2.sWDID.list.4 <- renderUI({
    if (as.numeric(input$t2.numSales) > 3){
      nbdum <- gpv()[which((gpv()$wdid1 == input$t2.bWDID.4) | (gpv()$wdid2 == input$t2.bWDID.4) | (gpv()$wdid3 == input$t2.bWDID.4)),]
      nbdum1 <- getOutputVerts_noCheck('us', 1000, 0.01, nbdum, gpv(), g())
      nbdum2 <- getOutputVerts_noCheck('ds', 1000, 0.01, nbdum, gpv(), g())
      nbdum <- character()
      if (!is.null(nrow(nbdum1))){
        nbdum <- c(nbdum, nbdum1$name)
      }
      if (!is.null(nrow(nbdum2))){
        nbdum <- c(nbdum, nbdum2$name)
      }
      gpvdum <- gpv()[gpv()$name %in% nbdum, ]
      gpvdum <- gpvdum[gpvdum$wtot > 0, ]
      gpvdum <- gpvdum[sapply(1:length(gpvdum$a_full), function(x) sum(gpvdum$a_full[[x]]==gpvdum$w[[x]]) > 0), ]
      ns1dum <- gpv()[which((gpv()$wdid1 == input$t2.sWDID.1) | (gpv()$wdid2 == input$t2.sWDID.1) | (gpv()$wdid3 == input$t2.sWDID.1)),]
      ns2dum <- gpv()[which((gpv()$wdid1 == input$t2.sWDID.2) | (gpv()$wdid2 == input$t2.sWDID.2) | (gpv()$wdid3 == input$t2.sWDID.2)),]
      ns3dum <- gpv()[which((gpv()$wdid1 == input$t2.sWDID.3) | (gpv()$wdid2 == input$t2.sWDID.3) | (gpv()$wdid3 == input$t2.sWDID.3)),]
      if ((ns1dum$name == ns2dum$name) & (ns2dum$name == ns3dum$name)){
        if (sum(ns1dum$a_full[[1]] == ns1dum$w[[1]]) < 4){
          gpvdum <-gpvdum[gpvdum$name != ns1dum$name, ]
        }
      }else if (ns1dum$name == ns2dum$name){
          if (sum(ns1dum$a_full[[1]] == ns1dum$w[[1]]) < 3){
            gpvdum <-gpvdum[gpvdum$name != ns1dum$name, ]
          }
      }else if (ns2dum$name == ns3dum$name){
        if (sum(ns2dum$a_full[[1]] == ns2dum$w[[1]]) < 3){
          gpvdum <-gpvdum[gpvdum$name != ns2dum$name, ]
        }
      }else{
        if (sum(ns1dum$a_full[[1]] == ns1dum$w[[1]]) < 2){
          gpvdum <-gpvdum[gpvdum$name != ns1dum$name, ]
        }
        if (sum(ns2dum$a_full[[1]] == ns2dum$w[[1]]) < 2){
          gpvdum <-gpvdum[gpvdum$name != ns2dum$name, ]
        }
        if (sum(ns3dum$a_full[[1]] == ns3dum$w[[1]]) < 2){
          gpvdum <-gpvdum[gpvdum$name != ns3dum$name, ]
        }
      }

      
      wdidDum <- unlist(gpvdum$wdid)
      wdidDum <- wdidDum[order(as.numeric(wdidDum))]
      
      selectInput('t2.sWDID.4', 'Seller 4 WDID', wdidDum[!wdidDum %in% c(input$t2.bWDID.4)])#, input$t2.bWDID.1, input$t2.sWDID.1, input$t2.bWDID.2, input$t2.sWDID.2)])
    }
  })
  output$t2.sWDID.nsalloc.4 <- renderUI({
    if (as.numeric(input$t2.numSales) > 3){
      adum <- which(t2.ns()[4,]$a_full[[1]] == t2.ns()[4,]$w[[1]])
      ns1dum <- gpv()[which((gpv()$wdid1 == input$t2.sWDID.1) | (gpv()$wdid2 == input$t2.sWDID.1) | (gpv()$wdid3 == input$t2.sWDID.1)),]
      ns2dum <- gpv()[which((gpv()$wdid1 == input$t2.sWDID.2) | (gpv()$wdid2 == input$t2.sWDID.2) | (gpv()$wdid3 == input$t2.sWDID.2)),]
      ns3dum <- gpv()[which((gpv()$wdid1 == input$t2.sWDID.3) | (gpv()$wdid2 == input$t2.sWDID.3) | (gpv()$wdid3 == input$t2.sWDID.3)),]
      ns4dum <- gpv()[which((gpv()$wdid1 == input$t2.sWDID.4) | (gpv()$wdid2 == input$t2.sWDID.4) | (gpv()$wdid3 == input$t2.sWDID.4)),]
      if (ns1dum$name == ns4dum$name){
        adum <- adum[-which(adum == as.integer(input$t2.nsalloc.1))]
      }
      if (ns2dum$name == ns4dum$name){
        adum <- adum[-which(adum == as.integer(input$t2.nsalloc.2))]
      }
      if (ns3dum$name == ns4dum$name){
        adum <- adum[-which(adum == as.integer(input$t2.nsalloc.3))]
      }
      selectInput('t2.nsalloc.4', 'Which allocation to buy from, seller 4', adum)
    }
  })
  output$t2.sWDID.amt.4 <- renderUI({
    if (as.numeric(input$t2.numSales) > 3){
      sliderInput('t2.amt.4', 'Amount to buy from seller 4', min=0,
                  max=t2.ns()[4,]$a_full[[1]][as.integer(input$t2.nsalloc.4)],
                  value=t2.ns()[4,]$a_full[[1]][as.integer(input$t2.nsalloc.4)] ,step = 0.1)
    }
  })
  
  output$t2.bWDID.5 <- renderUI({
    if (as.numeric(input$t2.numSales) > 3){
      selectInput('t2.bWDID.5','Buyer 5 WDID',wdidList)#[!wdidList %in% c(input$t2.bWDID.1, input$t2.sWDID.1, input$t2.bWDID.2, input$t2.sWDID.2)])
    }
  })
  output$t2.sWDID.list.5 <- renderUI({
    if (as.numeric(input$t2.numSales) > 4){
      nbdum <- gpv()[which((gpv()$wdid1 == input$t2.bWDID.5) | (gpv()$wdid2 == input$t2.bWDID.5) | (gpv()$wdid3 == input$t2.bWDID.5)),]
      nbdum1 <- getOutputVerts_noCheck('us', 1000, 0.01, nbdum, gpv(), g())
      nbdum2 <- getOutputVerts_noCheck('ds', 1000, 0.01, nbdum, gpv(), g())
      nbdum <- character()
      if (!is.null(nrow(nbdum1))){
        nbdum <- c(nbdum, nbdum1$name)
      }
      if (!is.null(nrow(nbdum2))){
        nbdum <- c(nbdum, nbdum2$name)
      }
      gpvdum <- gpv()[gpv()$name %in% nbdum, ]
      gpvdum <- gpvdum[gpvdum$wtot > 0, ]
      gpvdum <- gpvdum[sapply(1:length(gpvdum$a_full), function(x) sum(gpvdum$a_full[[x]]==gpvdum$w[[x]]) > 0), ]
      ns1dum <- gpv()[which((gpv()$wdid1 == input$t2.sWDID.1) | (gpv()$wdid2 == input$t2.sWDID.1) | (gpv()$wdid3 == input$t2.sWDID.1)),]
      ns2dum <- gpv()[which((gpv()$wdid1 == input$t2.sWDID.2) | (gpv()$wdid2 == input$t2.sWDID.2) | (gpv()$wdid3 == input$t2.sWDID.2)),]
      ns3dum <- gpv()[which((gpv()$wdid1 == input$t2.sWDID.3) | (gpv()$wdid2 == input$t2.sWDID.3) | (gpv()$wdid3 == input$t2.sWDID.3)),]
      ns4dum <- gpv()[which((gpv()$wdid1 == input$t2.sWDID.4) | (gpv()$wdid2 == input$t2.sWDID.4) | (gpv()$wdid3 == input$t2.sWDID.4)),]
      if ((ns1dum$name == ns2dum$name) & (ns2dum$name == ns3dum$name) & (ns3dum$name == ns4dum$name)){
        if (sum(ns1dum$a_full[[1]] == ns1dum$w[[1]]) < 5){
          gpvdum <-gpvdum[gpvdum$name != ns1dum$name, ]
        }
      }else if ((ns1dum$name == ns2dum$name) & (ns2dum$name == ns3dum$name)){
        if (sum(ns1dum$a_full[[1]] == ns1dum$w[[1]]) < 4){
          gpvdum <-gpvdum[gpvdum$name != ns1dum$name, ]
        }
      }else if ((ns1dum$name == ns3dum$name) & (ns3dum$name == ns4dum$name)){
        if (sum(ns1dum$a_full[[1]] == ns1dum$w[[1]]) < 4){
          gpvdum <-gpvdum[gpvdum$name != ns1dum$name, ]
        }
      }else if ((ns1dum$name == ns2dum$name) & (ns2dum$name == ns4dum$name)){
        if (sum(ns1dum$a_full[[1]] == ns1dum$w[[1]]) < 4){
          gpvdum <-gpvdum[gpvdum$name != ns1dum$name, ]
        }
      }else if ((ns2dum$name == ns3dum$name) & (ns3dum$name == ns4dum$name)){
        if (sum(ns2dum$a_full[[1]] == ns2dum$w[[1]]) < 4){
          gpvdum <-gpvdum[gpvdum$name != ns1dum$name, ]
        }
      }else{
        if (ns1dum$name == ns2dum$name){
          if (sum(ns1dum$a_full[[1]] == ns1dum$w[[1]]) < 3){
            gpvdum <-gpvdum[gpvdum$name != ns1dum$name, ]
          }
        }
        if (ns1dum$name == ns3dum$name){
          if (sum(ns1dum$a_full[[1]] == ns1dum$w[[1]]) < 3){
            gpvdum <-gpvdum[gpvdum$name != ns1dum$name, ]
          }
        }
        if (ns1dum$name == ns4dum$name){
          if (sum(ns1dum$a_full[[1]] == ns1dum$w[[1]]) < 3){
            gpvdum <-gpvdum[gpvdum$name != ns1dum$name, ]
          }
        }
        if (ns2dum$name == ns3dum$name){
          if (sum(ns2dum$a_full[[1]] == ns2dum$w[[1]]) < 3){
            gpvdum <-gpvdum[gpvdum$name != ns2dum$name, ]
          }
        }
        if (ns2dum$name == ns4dum$name){
          if (sum(ns2dum$a_full[[1]] == ns2dum$w[[1]]) < 3){
            gpvdum <-gpvdum[gpvdum$name != ns2dum$name, ]
          }
        }
        if (ns3dum$name == ns4dum$name){
          if (sum(ns3dum$a_full[[1]] == ns3dum$w[[1]]) < 3){
            gpvdum <-gpvdum[gpvdum$name != ns3dum$name, ]
          }
        }
     
        if (sum(ns1dum$a_full[[1]] == ns1dum$w[[1]]) < 2){
          gpvdum <-gpvdum[gpvdum$name != ns1dum$name, ]
        }
        if (sum(ns2dum$a_full[[1]] == ns2dum$w[[1]]) < 2){
          gpvdum <-gpvdum[gpvdum$name != ns2dum$name, ]
        }
        if (sum(ns3dum$a_full[[1]] == ns3dum$w[[1]]) < 2){
          gpvdum <-gpvdum[gpvdum$name != ns3dum$name, ]
        }
        if (sum(ns4dum$a_full[[1]] == ns4dum$w[[1]]) < 2){
          gpvdum <-gpvdum[gpvdum$name != ns4dum$name, ]
        }
      }
      
      
      wdidDum <- unlist(gpvdum$wdid)
      wdidDum <- wdidDum[order(as.numeric(wdidDum))]
      
      selectInput('t2.sWDID.5', 'Seller 5 WDID', wdidDum[!wdidDum %in% c(input$t2.bWDID.5)])#, input$t2.bWDID.1, input$t2.sWDID.1, input$t2.bWDID.2, input$t2.sWDID.2)])
    }
  })
  output$t2.sWDID.nsalloc.5 <- renderUI({
    if (as.numeric(input$t2.numSales) > 4){
      adum <- which(t2.ns()[5,]$a_full[[1]] == t2.ns()[5,]$w[[1]])
      ns1dum <- gpv()[which((gpv()$wdid1 == input$t2.sWDID.1) | (gpv()$wdid2 == input$t2.sWDID.1) | (gpv()$wdid3 == input$t2.sWDID.1)),]
      ns2dum <- gpv()[which((gpv()$wdid1 == input$t2.sWDID.2) | (gpv()$wdid2 == input$t2.sWDID.2) | (gpv()$wdid3 == input$t2.sWDID.2)),]
      ns3dum <- gpv()[which((gpv()$wdid1 == input$t2.sWDID.3) | (gpv()$wdid2 == input$t2.sWDID.3) | (gpv()$wdid3 == input$t2.sWDID.3)),]
      ns4dum <- gpv()[which((gpv()$wdid1 == input$t2.sWDID.4) | (gpv()$wdid2 == input$t2.sWDID.4) | (gpv()$wdid3 == input$t2.sWDID.4)),]
      ns5dum <- gpv()[which((gpv()$wdid1 == input$t2.sWDID.5) | (gpv()$wdid2 == input$t2.sWDID.5) | (gpv()$wdid3 == input$t2.sWDID.5)),]
      if (ns1dum$name == ns5dum$name){
        adum <- adum[-which(adum == as.integer(input$t2.nsalloc.1))]
      }
      if (ns2dum$name == ns5dum$name){
        adum <- adum[-which(adum == as.integer(input$t2.nsalloc.2))]
      }
      if (ns3dum$name == ns5dum$name){
        adum <- adum[-which(adum == as.integer(input$t2.nsalloc.3))]
      }
      if (ns4dum$name == ns5dum$name){
        adum <- adum[-which(adum == as.integer(input$t2.nsalloc.4))]
      }
      selectInput('t2.nsalloc.5', 'Which allocation to buy from, seller 5', adum)
    }
  })
  output$t2.sWDID.amt.5 <- renderUI({
    if (as.numeric(input$t2.numSales) > 4){
      sliderInput('t2.amt.5', 'Amount to buy from seller 5', min=0,
                  max=t2.ns()[5,]$a_full[[1]][as.integer(input$t2.nsalloc.5)],
                  value=t2.ns()[5,]$a_full[[1]][as.integer(input$t2.nsalloc.5)] ,step = 0.1)
    }
  })
  
  # Monitor selected bWDID, highlight matching node
  getNb <- function(gpv, numSales, bWDID.1, bWDID.2, bWDID.3, bWDID.4, bWDID.5){
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
    if (numSales > 3){
      req(bWDID.4)
      t2.nb <- rbind(t2.nb, gpv[which((gpv$wdid1 == bWDID.4) | (gpv$wdid2 == bWDID.4) | (gpv$wdid3 == bWDID.4)),])
    }
    if (numSales > 4){
      req(bWDID.5)
      t2.nb <- rbind(t2.nb, gpv[which((gpv$wdid1 == bWDID.5) | (gpv$wdid2 == bWDID.5) | (gpv$wdid3 == bWDID.5)),])
    }
    return(t2.nb)
  }
  t2.nb <- reactive({
    getNb(gpv(), input$t2.numSales, input$t2.bWDID.1, input$t2.bWDID.2, input$t2.bWDID.3, input$t2.bWDID.4, input$t2.bWDID.5)
  })
  # Monitor selected sWDID, get matching node
  getNs <- function(gpv, numSales, sWDID.1, sWDID.2, sWDID.3, sWDID.4, sWDID.5){
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
    if (numSales > 3){
      req(sWDID.4)
      t2.ns <- rbind(t2.ns, gpv[which((gpv$wdid1 == sWDID.4) | (gpv()$wdid2 == sWDID.4) | (gpv()$wdid3 == sWDID.4)),])
    }
    if (numSales > 4){
      req(sWDID.5)
      t2.ns <- rbind(t2.ns, gpv[which((gpv$wdid1 == sWDID.5) | (gpv()$wdid2 == sWDID.5) | (gpv()$wdid3 == sWDID.5)),])
    }
    return(t2.ns)
  }
  t2.ns <- reactive({
    getNs(gpv(), input$t2.numSales, input$t2.sWDID.1, input$t2.sWDID.2, input$t2.sWDID.3, input$t2.sWDID.4, input$t2.sWDID.5)
  })
  # get allocs for trade
  getAlloc <- function(gpv, numSales, alloc.1, alloc.2, alloc.3, alloc.4, alloc.5){
    req(alloc.1)
    t2.alloc <- alloc.1
    if (numSales > 1){
      req(alloc.2)
      t2.alloc <- c(t2.alloc, alloc.2)
    }
    if (numSales > 2){
      req(alloc.3)
      t2.alloc <- c(t2.alloc, alloc.3)
    }
    if (numSales > 3){
      req(alloc.4)
      t2.alloc <- c(t2.alloc, alloc.4)
    }
    if (numSales > 4){
      req(alloc.5)
      t2.alloc <- c(t2.alloc, alloc.5)
    }
    return(t2.alloc)
  }
  t2.alloc <- reactive({
    getAlloc(gpv(), input$t2.numSales, as.numeric(input$t2.nsalloc.1), as.numeric(input$t2.nsalloc.2), 
             as.numeric(input$t2.nsalloc.3), as.numeric(input$t2.nsalloc.4), as.numeric(input$t2.nsalloc.5))
  })
  # get amounts for trade
  getAmt <- function(gpv, numSales, amt.1, amt.2, amt.3, amt.4, amt.5){
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
    if (numSales > 3){
      req(amt.4)
      t2.amt <- c(t2.amt, amt.4)
    }
    if (numSales > 4){
      req(amt.5)
      t2.amt <- c(t2.amt, amt.5)
    }
    return(t2.amt)
  }
  t2.amt <- reactive({
    getAmt(gpv(), input$t2.numSales, as.numeric(input$t2.amt.1), as.numeric(input$t2.amt.2), 
           as.numeric(input$t2.amt.3), as.numeric(input$t2.amt.4), as.numeric(input$t2.amt.5))
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
  


  # # map2 and trade
  
  output$map2 <- renderLeaflet({
    # req(input$button2)
    leaflet(stm_geo) %>%
      addProviderTiles(providers$Esri.WorldShadedRelief) %>%
      addTiles(options=providerTileOptions(opacity = 0.7)) %>%
      addPolylines(data=gpe(), color='grey', weight=3, opacity=1, popup=~inset, group='Allocation network - Mean flow') #%>%
      # addCircles(data=gpv()[gpv()$atot > 0,], color='grey', popup=~inset, group='Allocation network - Mean flow')
  })

  observeEvent(input$button2, {
    
    withProgress(message = 'Calculation in progress',
                 detail = 'This will take a few minutes...', value = 0, {
                   
                   progCount <- sum(unlist(V(g())$a_full) > 0)
                   
                   req(t2.nb())
                   req(t2.ns())
                   req(t2.amt())
                   req(t2.alloc())
                   
                   # make test graph
                   gt <- g()
                   t2.nb.t <- t2.nb()$name
                   t2.ns.t <- t2.ns()$name
                   amt.t <- t2.amt()
                   alloc.t <- t2.alloc()
                   
                   # make trade
                   for (i in 1:length(t2.ns.t)){
                     # for (j in 1:length(alloc.t)){
                     # amt.t[i] <- V(gt)[t2.ns.t[i]]$a_full[[1]][alloc.t[i]]
                     V(gt)[t2.nb.t[i]]$a_full[[1]] <- c(V(gt)[t2.nb.t[i]]$a_full[[1]], amt.t[i])
                     V(gt)[t2.nb.t[i]]$pri[[1]] <- c(V(gt)[t2.nb.t[i]]$pri[[1]], V(gt)[t2.ns.t[i]]$pri[[1]][alloc.t[i]])
                     V(gt)[t2.nb.t[i]]$rnk[[1]] <- c(V(gt)[t2.nb.t[i]]$rnk[[1]], V(gt)[t2.ns.t[i]]$rnk[[1]][alloc.t[i]])
                     V(gt)[t2.ns.t[i]]$a_full[[1]][alloc.t[i]] <- V(gt)[t2.ns.t[i]]$a_full[[1]][alloc.t[i]] - amt.t[i]
                     # }
                   }
                   
                   #############################################

                   # allocations
                   a <- unlist(V(gt)$a_full)
                   # priorities
                   rnk <- unlist(V(gt)$rnk)
                   # instream flows
                   isf_a <- unlist(E(gt)$ISF_Amt)
                   isf_rnk <- unlist(E(gt)$ISF_rnk)
                   isf_edge <- unlist(lapply(1:length(E(gt)), function(e) rep(e, length(E(gt)$ISF_rnk[[e]]))))
                   isf_us <- sapply(1:length(isf_edge), function(x) ends(gt, E(gt)[isf_edge[x]])[1,1])
                   isf_node <- rep(0, length(V(gt)))
                   # name of node for each alloc
                   node <- unlist(lapply(1:length(V(gt)), function(v) rep(V(gt)$name[v], length(V(gt)$a_full[[v]]))))
                   node_node <- V(gt)$name
                   # consumption ratio
                   con_node <- V(gt)$con
                   con_alloc<- unlist(lapply(1:length(V(gt)), function(v) rep(V(gt)$con[v], length(V(gt)$a_full[[v]]))))
                   # supply - loss
                   sl_node <- V(gt)$sl
                   sl_alloc <- unlist(lapply(1:length(V(gt)), function(v) rep(V(gt)$sl[v], length(V(gt)$a_full[[v]]))))
                   # get list of unique rnks (for w) and order
                   rnkOrder <- unique(rnk[a>0])
                   rnkOrder <- rnkOrder[order(rnkOrder)]
                   # set up vector to hold withdrawals. All zero initially.
                   w <- rep(0, length(a))
                   # set up vector to hold instream flows. All zero initially.
                   I <- rep(0, length(isf_a))
                   # num allocations (can be more than one per node)
                   na <- length(a)
                   napn <- sapply(V(gt)$a_full, function(x) length(x))     # num allocs per node
                   nv <- length(V(gt))                        # number of nodes
                   ne <- length(E(gt))                        # number of edges (should be nv-1)
                   
                   # # get matrix to hold indices of input neighbors. Each row has 1's for input q.
                   load('inp.rda')

                   # Loop through allocations, starting with most senior, and find w for each
                   iter <- 0
                   obj <- rep(NA, length(a))
                   allocOrder <- rep(NA, length(a))
                   dvFound <- rep(NA, length(a))
                   b2gt0 <- rep(F, length(a))
                   wrnk <- matrix(NA, nrow=length(rnkOrder), ncol=length(w))
                   for (p in rnkOrder){
                     # allocs with this priority
                     palloc <- which((rnk == p) & (a > 0))
                     for (i in palloc){
                       iter <- iter + 1
                       allocOrder[iter] <- i
                       # if (iter < 21){
                       bTrans <- character()
                       b2 <- numeric()

                       # First try setting w_i==a_i, solving just for q
                       w[i] <- a[i]
                       feasSoln <- F
                       # min resolution of withdrawals
                       delw <- 0.01
                       # set up equality constraints (nv)
                       b3 <- sl_node - con_node * sapply(node_node, function(x) sum(w[node == x]))        # remember that only senior allocs (and i) have non-zero withdrawals as we loop through allocs.
                       # b3[abs(b3) < 1e-10] <- 0
                       dum <- which(node_node == node[i])
                       A3 <- diag(nv) - inp        # Mass balance:   qout - sum( qin) (+ cr*w)_if_i = sl (- cr*w)_if_not_i
                       # solve
                       qtest <- solve(A3, b3)
                       # calc total ISF out of each node, to compare to q

                       ### test if constraints met
                       # all flows non-negative, withdrawals not more than available
                       if ((sum(qtest - isf_node < -1e-13) == 0) & (sum(sapply(node_node, function(x) sum(w[node == x])) - inp %*% qtest - sl_node > 1e-13) == 0)){
                         feasSoln <- T
                       }else{
                         # If full alloc didnt work, try with min non-zero alloc
                         w[i] <- delw
                         # set up equality constraints (nv)
                         b3 <- sl_node - con_node * sapply(node_node, function(x) sum(w[node == x]))        # remember that only senior allocs (and i) have non-zero withdrawals as we loop through allocs.
                         # b3[abs(b3) < 1e-10] <- 0
                         dum <- which(node_node == node[i])
                         A3 <- diag(nv) - inp        # Mass balance:   qout - sum( qin) (+ cr*w)_if_i = sl (- cr*w)_if_not_i
                         # solve
                         qtest <- solve(A3, b3)
                         ### test if constraints met
                         # all flows non-negative, withdrawals not more than available
                         if ((sum(qtest - isf_node < -1e-13) == 0) & (sum(sapply(node_node, function(x) sum(w[node == x])) - inp %*% qtest - sl_node > 1e-13) == 0)){
                           # withdrawal of min amount worked, so we need to search for max allowable w[i] in (delw, a[i])
                           # iteratively decrease w until feasible soln
                           w[i] <- ifelse((a[i] > 2 * delw), a[i] / 2, 0)
                           wceil <- a[i]
                           wfloor <- delw
                           while ((feasSoln == F) & (w[i] > delw)){
                             # set up equality constraints (nv)
                             b3 <- sl_node - con_node * sapply(node_node, function(x) sum(w[node == x]))        # remember that only senior allocs (and i) have non-zero withdrawals as we loop through allocs.
                             # b3[abs(b3) < 1e-10] <- 0
                             dum <- which(node_node == node[i])
                             A3 <- diag(nv) - inp        # Mass balance:   qout - sum( qin) (+ cr*w)_if_i = sl (- cr*w)_if_not_i
                             # solve
                             qtest <- solve(A3, b3)
                             ### test if constraints met
                             # all flows non-negative & withdrawals not more than available
                             if ((sum(qtest - isf_node < -1e-13) == 0) & (sum(sapply(node_node, function(x) sum(w[node == x])) - inp %*% qtest - sl_node > 1e-13) == 0)){
                               # if constraints met, this is feasible, increase w[i]
                               wfloor <- w[i]
                               w[i] <- (wceil + wfloor) / 2
                               # round up to nearest delw
                               w[i] <- ceiling(w[i] / delw) * delw
                               # test if we have reached resolution delw
                               if (w[i] == wceil){
                                 w[i] <- wfloor
                                 feasSoln <- T
                               }
                             }else{
                               # if constraints not met, this is infeasible, continue to lower w[i]
                               wceil <- w[i]
                               w[i] <- (wceil + wfloor) / 2
                               # round down to nearest delw
                               w[i] <- floor(w[i] / delw) * delw
                               if (w[i] == wfloor){
                                 feasSoln <- T
                               }
                             }
                           }
                           w[i] <- ifelse(w[i] < delw, 0, w[i])

                         }else{
                           # if min w was infeasible, then no withdrawal allowed
                           w[i] <- 0
                         }

                       }

                       incProgress(1/progCount)

                     }

                     # get flows after withdrawals
                     b3 <- sl_node - con_node * sapply(node_node, function(x) sum(w[node == x]))        # remember that only senior allocs (and i) have non-zero withdrawals as we loop through allocs.
                     A3 <- diag(nv) - inp        # Mass balance:   qout - sum( qin) (+ cr*w)_if_i = sl (- cr*w)_if_not_i
                     q <- solve(A3, b3)

                     # Fill instream flows between this and next w rank
                     k <- which(rnkOrder == p)
                     if (k == length(rnkOrder)){
                       isfdum <- which(isf_rnk >= rnkOrder[k])
                     }else{
                       isfdum <- which(isf_rnk >= rnkOrder[k] & isf_rnk < rnkOrder[k+1])
                     }
                     for (k in isfdum){
                       e <- isf_edge[k]
                       # allocate full alloc if q high enough, or else q minus sum of other filled instream allocs
                       I[k] <- min(isf_a[k], q[which(node_node == isf_us[k])] - sum(I[isf_us == isf_us[k]]) )
                     }
                     isf_node <- sapply(node_node, function(x) sum(I[isf_us ==x]))


                     # # save w at end of this rnk
                     # wrnk[which(rnkOrder == p), ] <- w
                     # # print progress
                     # obj[iter] <- w[i]

                   }

                   # store withdrawals
                   V(gt)$w <- relist(w, V(gt)$a_full)
                   V(gt)$q <- relist(q, V(gt)$sl)
                   E(gt)$I <- relist(I, E(gt)$ISF_App)
                   V(gt)$atot <- sapply(V(gt)$a_full, sum)
                   V(gt)$wtot <- sapply(V(gt)$w, sum)
                   V(gt)$Itot <- isf_node
                   E(gt)$Itot <- sapply(E(gt)$I, sum)
                   E(gt)$ISF_Amt_tot <- sapply(E(gt)$ISF_Amt, sum)

                   ##################################################

                   # set graph params
                   gt <- setGraphPlotParms_dTrade(gt, g(), amt.t, t2.nb.t, t2.ns.t)
                   
                   # get verts and edges for leaflet
                   gpvt <- gpv()
                   gpvt$color <- V(gt)$color
                   gpvt$inset <- V(gt)$inset
                   gpvt$dwtot <- V(gt)$dwtot
                   
                   gpet <- gpe()
                   gpet$color <- E(gt)$color
                   gpet$inset <- E(gt)$inset
                 })
    gpeZoom <- gpet[gpet$color != 'grey', ]
    gpvZoom <- gpvt[gpvt$color != 'white', ]
    leafletProxy('map2') %>%
      clearGroup('trade') %>%
      addCircles(data=gpvt[gpvt$color == 'grey',], color=~color, popup=~inset, group='trade') %>%
      addCircles(data=gpvt[gpvt$color != 'grey',], color=~color, popup=~inset, radius=~sqrt(abs(dwtot) * 500)*10, group='trade') %>%
      addPolylines(data=gpet[gpet$color != 'grey',], color=~color, popup=~inset, opacity=1, weight=3, group='trade') %>%
      addAwesomeMarkers(t2.nb()$x, t2.nb()$y, icon=icons_nb, popup=t2.nb()$inset,group='nb') %>%
      addAwesomeMarkers(t2.ns()$x, t2.ns()$y, icon=icons_outputVerts, popup=t2.ns()$inset,group='ns') %>%
      # addMarkers(data=t2.nb(), popup=~inset, group='trade') %>%
      # addMarkers(data=t2.ns(), popup=~inset, group='trade') %>%
      fitBounds(min(gpvZoom@bbox[1,1], gpeZoom@bbox[1,1]), min(gpvZoom@bbox[2,1], gpeZoom@bbox[2,1]),
                max(gpvZoom@bbox[1,2], gpeZoom@bbox[1,2]), max(gpvZoom@bbox[2,2], gpeZoom@bbox[2,2])) %>%
      addLegend("topright", colors=c('green','red','grey'), 
                labels=c('Increase','Decrease','No change'), 
                title = 'Change in withdrawals') %>%
      addLegend("topright", colors=c('green','red','blue','orange','grey'), 
                labels=c('Increase - ISF','Decrease - ISF','Increase - no ISF','Decrease - no ISF','No change'), 
                title = 'Change in stream flow')
  })
  
  setGraphPlotParms_dTrade <- function(gNew, gOld, amt.t, t2.nb.t, t2.ns.t){
    #### vis effects of trade on flow and allocations
    ### check whether good trade
    V(gNew)$t1 <- NA
    V(gNew)$t2 <- NA
    V(gNew)$t3 <- NA
    V(gNew)$t4 <- NA
    E(gNew)$t5 <- NA
    E(gNew)$t6 <- NA
    # did nb get full purchased alloc, without taking away from other nodes
    V(gNew)[t2.nb.t]$t1 <- abs(V(gNew)[t2.nb.t]$wtot - (V(gOld)[t2.nb.t]$wtot + amt.t)) < 1e-13
    # did ns lose full purchased alloc, without taking away from other nodes
    V(gNew)[t2.ns.t]$t2 <- abs(V(gNew)[t2.ns.t]$wtot - (V(gOld)[t2.ns.t]$wtot - amt.t)) < 1e-13
    # # were any other nodes impacted negatively
    # V(gNew)[!V(gNew)$name %in% c(nb, ns)]$t3 <- V(gNew)[!V(gNew)$name %in% c(nb, ns)]$wtot - V(gOld)[!V(gOld)$name %in% c(nb, ns)]$wtot < -1e-13
    # # were any other nodes impacted positively
    # V(gNew)[!V(gNew)$name %in% c(nb, ns)]$t4 <- V(gNew)[!V(gNew)$name %in% c(nb, ns)]$wtot - V(gOld)[!V(gOld)$name %in% c(nb, ns)]$wtot > 1e-13
    # # were any ISF impacted negatively
    # E(gNew)$t5 <- (E(gNew)$ISF_Amt_tot - E(gNew)$Itot) - (E(gOld)$ISF_Amt_tot - E(gOld)$Itot) > 1e-13
    # # were any ISF impacted positively
    # E(gNew)$t6 <- (E(gNew)$ISF_Amt_tot - E(gNew)$Itot) - (E(gOld)$ISF_Amt_tot - E(gOld)$Itot) < -1e-13
    # 
    
    # get differences due to trade
    V(gNew)$dq <- V(gNew)$q - V(gOld)$q
    V(gNew)$dI <- V(gNew)$Itot - V(gOld)$Itot
    V(gNew)$dq[abs(V(gNew)$dq) < 1e-10] <- 0
    V(gNew)$dI[abs(V(gNew)$dI) < 1e-10] <- 0
    V(gNew)$dwtot <- V(gNew)$wtot - V(gOld)$wtot
    V(gNew)$dwtot[abs(V(gNew)$dwtot) < 1e-10] <- 0
    E(gNew)$dq <- V(gNew)[ends(gNew, E(gNew))[,1]]$dq
    E(gNew)$dI <- V(gNew)[ends(gNew, E(gNew))[,1]]$dI
    # set inset
    V(gNew)$inset <- paste('dw =', as.character(round(V(gNew)$dwtot, 2)))
    E(gNew)$inset <- paste('dq =', as.character(round(E(gNew)$dq, 2)), '<br>dI =', as.character(round(E(gNew)$dI,2)))
    
    # color vertices
    V(gNew)$color <- ifelse(V(gNew)$dwtot == 0, 'grey', 
                            ifelse(V(gNew)$dwtot > 0, 'green', 'red'))
    # V(gNew)$color[!is.na(V(gNew)$t1) & V(gNew)$dwtot > 0] <- 'darkblue'
    # V(gNew)$color[!is.na(V(gNew)$t2) & V(gNew)$dwtot < 0] <- 'darkorange'
    # V(gNew)$color[!is.na(V(gNew)$t1) & V(gNew)$dwtot == 0] <- 'black'
    # V(gNew)$color[!is.na(V(gNew)$t2) & V(gNew)$dwtot == 0] <- 'black'
    E(gNew)$color <- ifelse(E(gNew)$dq == 0, 'grey', 
                            ifelse((is.na(E(gNew)$dI) & (E(gNew)$dq > 0)), 'blue',
                                   ifelse((is.na(E(gNew)$dI) & (E(gNew)$dq < 0)), 'orange',   
                                          ifelse(((E(gNew)$dI == 0) & (E(gNew)$dq > 0)), 'blue',
                                                 ifelse(((E(gNew)$dI == 0) & (E(gNew)$dq < 0)), 'orange',
                                                        ifelse(E(gNew)$dI > 0, 'green', 'red'))))))
    
    return(gNew)
  }
  # observeEvent(input$button2, {
  #   
  #   withProgress(message = 'Calculation in progress',
  #                detail = 'This will take a few minutes...', value = 0, {
  #                  req(t2.nb())
  #                  req(t2.ns())
  #                  req(t2.amt())
  #                  
  #                  # make test graph
  #                  gt <- g()
  #                  t2.nb.t <- t2.nb()$name
  #                  t2.ns.t <- t2.ns()$name
  #                  amt.t <- t2.amt()
  #                  
  #                  # choose allocs for trade, based on meeting crits
  #                  for (t in 1:length(t2.nb.t)){
  #                    full <- V(g())[t2.ns.t[t]]$w[[1]] == V(g())[t2.ns.t[t]]$a_full[[1]]
  #                    i <- length(full)
  #                    trd <- rep(0, length(full))
  #                    pri <- as.integer(V(g())[t2.ns.t[t]]$pri[[1]])
  #                    while((i > 0) & (sum(trd) < amt.t[t])){
  #                      if (full[order(pri)][i]){
  #                        dum <- pri[order(pri)][i]
  #                        trd[order(pri)[i]] <- min(V(g())[t2.ns.t[t]]$w[[1]][order(pri)[i]], amt.t[t] - sum(trd))
  #                      }
  #                      i <- i - 1
  #                    }
  #                    
  #                    # make trade
  #                    for (i in 1:length(trd)){
  #                      if (trd[i] > 0){
  #                        V(gt)[t2.nb.t[t]]$a_full[[1]] <- c(V(gt)[t2.nb.t[t]]$a_full[[1]], trd[i])
  #                        V(gt)[t2.nb.t[t]]$pri[[1]] <- c(V(gt)[t2.nb.t[t]]$pri[[1]], V(gt)[t2.ns.t[t]]$pri[[1]][i])
  #                        V(gt)[t2.ns.t[t]]$a_full[[1]][i] <- V(gt)[t2.ns.t[t]]$a_full[[1]][i] - trd[i]
  #                        # set withdrawals too, assuming trade ok
  #                        V(gt)[t2.nb.t[t]]$w[[1]] <- c(V(gt)[t2.nb.t[t]]$w[[1]], trd[i])
  #                        V(gt)[t2.ns.t[t]]$w[[1]][i] <- V(gt)[t2.ns.t[t]]$w[[1]][i] - trd[i]
  #                      }
  #                    }
  #                  }
  #                  
  #                  V(gt)$atot <- sapply(V(gt)$a_full, sum)
  #                  V(gt)$wtot <- sapply(V(gt)$w, sum)
  #                  
  #                  # adjust flow q based on trades
  #                  for (i in 1:length(t2.ns.t)){
  #                    # if buyer is US, flows between will be smaller by dw*con
  #                    if (!is.na(vertPathsFull[t2.ns.t[i], t2.nb.t[i]])){
  #                      bw <- get.shortest.paths(gt, from=t2.nb.t[i], to=t2.ns.t[i])$vpath[[1]]$name
  #                      # dont need to adjust flow at DS node
  #                      V(gt)[bw[1:(length(bw)-1)]]$q <- V(gt)[bw[1:(length(bw)-1)]]$q - V(gt)[bw[1]]$con * amt.t[i]
  #                    }else{  # if buyer is DS, flows b/w will be larger by dw*con
  #                      bw <- get.shortest.paths(gt, from=t2.ns.t[i], to=t2.nb.t[i])$vpath[[1]]$name
  #                      # dont need to adjust flow at DS node
  #                      V(gt)[bw[1:(length(bw)-1)]]$q <- V(gt)[bw[1:(length(bw)-1)]]$q + V(gt)[bw[1]]$con * amt.t[i]
  #                    }
  #                    # round
  #                    V(gt)$q[abs(V(gt)$q) < 1e-13] <- 0
  #                  }
  #                  
  #                  # get US-most (us), and DS-most (ds) buyer and seller, and all in-between nodes (bw)
  #                  us <- unique(c(t2.ns.t,t2.nb.t))[which(colSums(!is.na(vertPathsFull[unique(c(t2.ns.t,t2.nb.t)),unique(c(t2.ns.t,t2.nb.t))])) == length(unique(c(t2.ns.t,t2.nb.t))))]
  #                  ds <- unique(c(t2.ns.t,t2.nb.t))[which(rowSums(!is.na(vertPathsFull[unique(c(t2.ns.t,t2.nb.t)),unique(c(t2.ns.t,t2.nb.t))])) == length(unique(c(t2.ns.t,t2.nb.t))))]
  #                  bw <- get.shortest.paths(gt, from=us, to=ds)$vpath[[1]]$name
  #                  
  #                  # check whether trade ok
  #                  # check that all outflows positive
  #                  t1 <- sum(V(gt)$q < 0) == 0
  #                  # check that all inflows sufficient for withdrawals
  #                  t2 <- sum(V(gt)$q - V(gt)$wtot * (1 - V(gt)$con) < -1e-13) == 0
  #                  
  #                  V(gt)$goodTrade <- (t1 & t2)
  #                  
  #                  #### vis effects of trade on flow and allocations
  #                  
  #                  # get differences due to trade
  #                  V(gt)$dq <- V(gt)$q - V(g())$q
  #                  V(gt)$dq[abs(V(gt)$dq) < 1e-10] <- 0
  #                  V(gt)$dwtot <- V(gt)$wtot - V(g())$wtot
  #                  V(gt)$dwtot[abs(V(gt)$dwtot) < 1e-10] <- 0
  #                  E(gt)$dq <- NA
  #                  for (e in 1:length(E(gt))){
  #                    E(gt)$dq[e] <- V(gt)[ends(gt, e)[1,1]]$dq
  #                  }
  #                  # set inset
  #                  # V(gt)$inset <- sapply(1:length(V(gt)), function(x)
  #                  #   paste('node =',V(gt)$name[x],'<br>wdid =',V(gt)$wdid[x],'<br>pri',V(gt)$pri[x],'<br>a_full =',V(gt)$a_full[x],
  #                  #         '<br>w =',V(gt)$w[x],'<br>sl =',V(gt)$sl[x],'<br>qout =', V(gt)$q[x]))
  #                  V(gt)$inset <- paste('dw =', as.character(V(gt)$dwtot))
  #                  E(gt)$inset <- paste('dq =', as.character(E(gt)$dq))
  #                  
  #                  # color vertices
  #                  V(gt)$color <- ifelse(V(gt)$dwtot == 0, 'white',
  #                                        ifelse(V(gt)$dwtot > 0, 'green', 'red'))
  #                  E(gt)$color <- ifelse(E(gt)$dq == 0, 'grey',
  #                                        ifelse(E(gt)$dq > 0, 'blue', 'yellow'))
  #                  
  #                  # get verts and edges for leaflet
  #                  gpvt <- gpv()
  #                  gpvt$color <- V(gt)$color
  #                  gpvt$inset <- V(gt)$inset
  #                  gpvt$dwtot <- V(gt)$dwtot
  #                  
  #                  gpet <- gpe()
  #                  gpet$color <- E(gt)$color
  #                  gpet$inset <- E(gt)$inset
  #                })
  #   gpeZoom <- gpet[gpet$color != 'grey', ]
  #   gpvZoom <- gpvt[gpvt$color != 'white', ]
  #   leafletProxy('map2') %>%
  #     clearGroup('trade') %>%
  #     # addPolylines(data=gpet, color=~color, popup=~inset, opacity=1, group='trade') %>%
  #     addCircles(data=gpvt[gpvt$color != 'white',], color=~color, popup=~inset, radius=~sqrt(abs(dwtot) * 500)*10, group='trade') %>%
  #     addPolylines(data=gpet[gpet$color != 'grey',], color=~color, popup=~inset, opacity=1, group='trade') %>%
  #     addMarkers(data=t2.nb(), popup=~inset, group='trade') %>%
  #     addMarkers(data=t2.ns(), popup=~inset, group='trade') %>%
  #     fitBounds(min(gpvZoom@bbox[1,1], gpeZoom@bbox[1,1]), min(gpvZoom@bbox[2,1], gpeZoom@bbox[2,1]),
  #               max(gpvZoom@bbox[1,2], gpeZoom@bbox[1,2]), max(gpvZoom@bbox[2,2], gpeZoom@bbox[2,2]))
  #   
  # })


}

shinyApp(ui=ui, server=server)




