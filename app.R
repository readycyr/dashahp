library(shiny)
##########################################
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*(1024 ^ 2) )
##########################################
##                  UI                  ##
##########################################
ui <- source("ui.R")
##########################################
##                Server                ##
##########################################
server <- source("server.R")
##########################################
##                  APP                 ##
##########################################
shinyApp(ui = ui, server = server)