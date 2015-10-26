##########################################
##                  UI                  ##
##########################################
source("chooser.R")
# A dashboard has three parts: a header, a sidebar, and a body.
shinydashboard::dashboardPage(
  ## The header content
  shinydashboard::dashboardHeader(
    title = "Open Data Portal Dashboard",
    titleWidth = 300,
    disable = FALSE # If TRUE, don't display the header bar.
  ),
  ## The sidebar content
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem( "Site performance", tabName = "siteperf", icon = icon("dashboard") ),
      shinydashboard::menuItem( "Dimension preferences", tabName = "dimpref", icon = icon("th") ),
      shinydashboard::menuItem( "User guide", tabName = "userg", icon = icon("file") ),
      sliderInput("mood", "What's your confidence with AHP", min = 1,max = 100, value = 50, step = 1, ticks = TRUE)
    )
  ),
  ## The body content
  shinydashboard::dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "tree.css")
    ),
    shinydashboard::tabItems(
      shinydashboard::tabItem( tabName = "siteperf",
                               shinydashboard::box( title = NULL,
                                                   collapsible = TRUE, collapsed = FALSE, width = 12, 
                                                   status = NULL, background = "black", solidHeader = TRUE,
                                                   widgetHierarchy::widgetHierarchyOutput("ahpTree", height = '600px') 
                              ),
                               fluidRow(
                                 shinydashboard::box( title = "AHP TREE file structure",
                                                      collapsible = TRUE, collapsed = FALSE, width = 6,
                                                      status = "info", background = NULL, solidHeader = TRUE,
                                                      rhandsontable::rHandsontableOutput("exampleSheetTree", height = '500px')
                                 ),
                                 shinydashboard::box( title = "AHP TREE initialisation",
                                                      collapsible = TRUE, collapsed = FALSE, width = 6, height = '500px',
                                                      status = "primary", background = NULL, solidHeader = TRUE,
                                                      HTML("<p>You should <span style='color:red;'>upload</span> a file containing the required data to build the AHP tree.</p>
                                                      <p>Please choose required option for your csv file</p>"),
                                                      makeCSVFileInput('fileTree', 12)
                                                      #, footer = "There is a footer now"
                                 )
                              ),
                              uiOutput("ahpConfigurationBox"),
                              fluidRow(
                                shinydashboard::box()
                              )
      ),
      shinydashboard::tabItem( tabName = "dimpref",
                               fluidRow(
                               )
      ),
      shinydashboard::tabItem( tabName = "userg",
                               fluidRow(
                               )
      )
    )
  ) # End of body
) # End of dashboard