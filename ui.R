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
