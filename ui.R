makeCSVFileInput <- function(varName, nCol) {
  column(nCol,
         column(4, checkboxInput('header', 'Header', TRUE) ),
         column(4, radioButtons( 'sep', 'Separator', c(Comma = ',', Semicolon = ';',Tab = '\t'), ',', inline = FALSE) ),
         column(4, radioButtons( 'quote', 'Quote', c(None = '', 'Double Quote' = '"', 'Single Quote' = "'"), selected = '', inline = FALSE) ),
         fileInput(varName, label = HTML("<p>Choose <span style='color:red;'>file</span> to upload</p>"), 
          accept = c( 'text/csv', 'text/comma-separated-values', 'text/tab-separated-values', 'text/plain', '.csv', '.tsv' ))
  )
}

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
      shinydashboard::menuItem( "User guide", tabName = "userg", icon = icon("file") )
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
                               shinydashboard::box(width = 12, 
                                                   widgetHierarchy::widgetHierarchyOutput("ahpTree", height = '600px') 
                                ),
                               fluidRow(
                                 shinydashboard::box( title = "AHP TREE file structure",
                                                      collapsible = TRUE, collapsed = FALSE, width = 6,
                                                      status = "success", solidHeader = TRUE, # background = 'green',
                                                      rhandsontable::rHandsontableOutput("exampleSheetTree", height = '500px')
                                 ),
                                 shinydashboard::box( title = "AHP TREE initialisation",
                                                      collapsible = TRUE, collapsed = FALSE, width = 6,
                                                      status = "primary", solidHeader = TRUE, # background = 'green',
                                                      HTML("<p>You should <span style='color:red;'>upload</span> a file containing the required data to build the AHP tree.</p>
                                                      <p>Please choose required option for your csv file</p>"),
                                                      makeCSVFileInput('fileTree', 12)
                                                      #, footer = "here is the footer "
                                 )
                              ),
                              uiOutput("treeDataBox")
                               #                                
                               #                                fluidRow(
                               #                                  shinydashboard::box(
                               #                                    checkboxInput("workingTree", "Check the box to start AHP process", value =  FALSE),
                               #                                    sliderInput("mood", "What is your moood", min = 1,max = 100, value = 50, step = 1, ticks = TRUE)
                               #                                  ),
                               #                                  uiOutput("listMenuBoxMatrix")
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