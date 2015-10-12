library(shiny)
library(htmlwidgets)
library(widgetHierarchy)
##########################################
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*(1024 ^ 2) )
##########################################
##          Tierce Function             ##
##########################################
statsCKAN <- NULL
source("chooser.R")
makeSaatyListMenu <- function(id, alabel) {
    sliderInput( id, alabel, min = 1,max = 9, value = 5, step = 2, ticks = FALSE, post = "x more important")
}
makeCriteriaListMenu <- function(id, alabel, crit1, crit2) {
    selectInput(inputId = id, label = alabel, choices = c(crit1, crit2), multiple = FALSE, selected = crit1)
}
makeCSVFileInput <- function(var, nCol) {
    column(nCol,
           column(4, 
                  fileInput(var, 'Choose file to upload', accept = c( 'text/csv', 'text/comma-separated-values', 'text/tab-separated-values', 'text/plain', '.csv', '.tsv' )),
                  checkboxInput('header', 'Header', TRUE)
           ),
           column(4, radioButtons( 'sep', 'Separator', c(Comma = ',', Semicolon = ';',Tab = '\t'), ',', inline = FALSE) ),
           column(4, radioButtons( 'quote', 'Quote', c(None = '', 'Double Quote' = '"', 'Single Quote' = "'"), '"', inline = FALSE) )
    )
}
myEigenValue <- function(matx) {
    sqOne <- matx %*% matx
    sqTwo <- sqOne %*% sqOne
    sqMatx <- sqTwo %*% sqTwo
    #
    dimRow <- nrow(matx)
    vMat <- vector(mode = "numeric", length = dimRow)
    for (i in 1:dimRow ) {
        vMat[i] = sum( sqMatx[i,] )
    }
    vMat
}
specify_digits <- function(x, k) {
    format(round(x, k), nsmall = k)
}
EigenVector_basedMeasurement <- function(col_A_Cj) {
    m <- length(col_A_Cj)
    PcAcj <- matrix(c(0), ncol = m, nrow = m);
    # Compute the upper half Pc matrix based on measurement
    for (i in seq_len(m)) {
        for (j in seq_len(m)) {
            #            if (i == j) { PcAcj[i, j] <- 1 } else 
            if (i > j) {
                PcAcj[i,j] <- 1 / PcAcj[j,i]
            } else if (i <= j) {
                PcAcj[i,j] <- col_A_Cj[i] / col_A_Cj[j]
            }
        }
    }
    PcAcj[ is.na(PcAcj) ] <- 0
    PcAcj[ is.infinite(PcAcj) ] <- 0
    eigenVector <- pmr::ahp(dset = PcAcj, sim_size = 500)
    eigenVector
    #    mThree <- EigenVector_basedMeasurement(col_qu)
    #    tt <- as.matrix(read.table("./datasets/qu_extras_EVbasedMeasurement.txt", sep = ","))
    #    for ( k in 1:126) { print( all.equal(as.numeric(tt[k,]), mThree[k,]) ) }
}
##########################################

###################################
#               UI
###################################
# A dashboard has three parts: a header, a sidebar, and a body.
ui <- shinydashboard::dashboardPage(
    title = "Open Data",
    ## The header content
    shinydashboard::dashboardHeader(
        title = "Open Data Portal Dashboard",
        titleWidth = 300,
        disable = FALSE
    ),
    ## The sidebar content
    shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
            shinydashboard::menuItem( "Site performance", tabName = "dashboard", icon = icon("dashboard") ),
            shinydashboard::menuItem( "Dimension preferences", tabName = "rval", icon = icon("th") )
        )
    ),
    ## The body content
    shinydashboard::dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "tree.css")
        ),
        shinydashboard::tabItems(
            shinydashboard::tabItem( tabName = "dashboard",
                                     fluidRow(
                                         shinydashboard::box( title = "Uploading file to build tree", width = 12, makeCSVFileInput('fileTree', 12)
                                         )
                                     ),
                                     uiOutput("treeDataBox"),
                                     fluidRow(
                                         shinydashboard::box(
                                         checkboxInput("workingTree", "Check the box to start AHP process", value =  FALSE),
                                         sliderInput("mood", "What is your moood", min = 1,max = 100, value = 50, step = 1, ticks = TRUE)
                                         ),
                                         uiOutput("listMenuBoxMatrix")
                                     )
            ),
            shinydashboard::tabItem( tabName = "rval",
                                     fluidRow(
                                     )
            )
        )
    ) # End of body
) # End of dashboard

##########################################
##                Server                ##
##########################################
server <- function(input, output) {
    theTree <- NULL
    treeMenuScript <- ""
    ##########################################
    output$listMenuBoxMatrix <- renderUI({
        if (!input$workingTree || length(input$treeLevelChoice) == 0) { 
            return()
        }
        if ( input$treeLevelChoice == 0 ) { return(NULL) }
        invisible( source(treeMenuScript, local = TRUE) )
        #fconn <- file(filename)
        #writeLines(succ$value, con = filename)
        #close(fconn)
    })
    ##########################################
    valuesTree <- list()
    output$hotSheetTree <- rhandsontable::renderRHandsontable({
        require(rhandsontable)
        if (!is.null(input$hotSheetTree)) {
            dataT <- hot_to_r( input$hotSheetTree )
        } else {
            dataT <- theTree
        }
        valuesTree[["hoT"]] <<- dataT
        rhandsontable(dataT, useTypes = TRUE, readOnly = FALSE) %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = FALSE, search = TRUE) %>%
            hot_cols(renderer = "
                function (instance, td, row, col, prop, value, cellProperties) {
                Handsontable.renderers.TextRenderer.apply(this, arguments);
                if (row == col) {
                td.style.background = 'lightgrey';
                } else if (col > row) {
                td.style.background = 'grey';
                td.style.color = 'grey';
                } else if (value < 3) {
                td.style.background = 'pink';
                } else if (value > 2) {
              td.style.background = 'lightgreen';
             }
           }")
    })
    #saveSheet <- 
    eventReactive(input$saveBtn, {
        if (!is.null(valuesTree[["hoT"]])) {
            write.csv(valuesTree[["hoT"]], file = file.path("tmpdatasaved") )
        }
    })
    ##########################################
    output$treeDataBox <- renderUI({
        inFile <- input$fileTree
        # input$fileMeasure will be NULL initially.
        if ( is.null(inFile) ) {
            return(NULL)
        } else if ( is.null( theTree ) ) {
            portals <- read.csv( inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
            nbAlternative <- nrow(portals)
            theTree <<- data.frame(
                name = c( 'Portal Assessment', 'Usage', 'Completeness', 'Openness','Addressability',
                          "qu_core","qu_res","qu_extras",
                          "qc_core","qc_res","qc_extra",
                          "qo_f","qo_l",
                          "qa_url","qa_email",
                          paste0( as.character( portals[1:nbAlternative, c("portal_id")]) )
                ),
                depth = c( 1, 
                           rep(2, each = 4),
                           rep(3, each = 10),
                           rep(4, each = nbAlternative)
                ),
                parent = c( NA, 'Portal Assessment', 'Portal Assessment', 'Portal Assessment', 'Portal Assessment',
                            rep('Usage', each = 3),
                            rep('Completeness', each = 3),
                            rep('Openness', each = 2),
                            rep('Addressability', each = 2),
                            rep("qc_extra", each = nbAlternative)
                ),
                score = runif( (nbAlternative + 10 + 5), 1, 100),
                stringsAsFactors = FALSE)
        }
        ## Script
        treeMenuScript <<-  paste0("generated_TreeManagement",".R")
        if (!file.exists(treeMenuScript)) {
            numberLevel <- names(table(theTree$depth))
            write(paste0('
if (input$treeLevelChoice == ', 1,') {
    namesLv1 <- as.character( theTree[theTree$depth == 1, c("name")] )
    namesLv2 <- as.character( theTree[theTree$depth == 2, c("name")] )
    namesLv3 <- as.character( theTree[theTree$depth == 3, c("name")] )
    filename <- paste0("generated_",input$treeLevelChoice,".dynR")
    output$thenames1 <- renderText({ as.character( namesLv1 ) })
    output$thenames2 <- renderText({ as.character( namesLv2 ) })
    output$thenames3 <- renderText({ as.character( namesLv3 ) })
if (!file.exists(filename)) {
    write(\'paste0( textOutput("thenames1"),\', file = filename)
    write(\'textOutput("thenames2"), \', file = filename, append = TRUE)
    write(\'textOutput("thenames3") )\', file = filename, append = TRUE)
    succ <- source(filename, local = TRUE)
    fconn <- file(filename)
    writeLines(succ$value, con = filename)
    close(fconn)
}
}'), file = treeMenuScript)
            for (lv in 2:length(numberLevel) ) {
                write( paste0('if (input$treeLevelChoice == ', lv,') {
                filename <- paste0("generated_",input$treeLevelChoice,".dynR")
                ln <- theTree[theTree$depth == ',lv,', c("name")]'), file = treeMenuScript, append = TRUE)
                if ( lv > 2) {
                    write('if ( length(input$subsetTreeChoice) > 0 && input$subsetTreeChoice != 0) {
                            ln <- na.omit( theTree[theTree$parent == input$subsetTreeChoice, c("name")] )
                        }', file = treeMenuScript, append = TRUE)
                }
                write( paste0('if ( input$typeOfMeasurement == "Based preferences" ) {
                    #if (!file.exists(filename)) {
                            len <- length(ln)
                            write( \'paste0( column(2,\', file = filename )
                            for (ko in 1:(len - 1) ) { 
                                for (kt in (ko + 1):len ) {
                                    var <- paste0(ln[ko],"and",ln[kt])
                                    if ( ko == len - 1 && kt == len) {
                                        write( paste0(\'makeCriteriaListMenu("\',var,\'","Criteria importance", "\',ln[ko],\' over \',ln[kt],\'","\',ln[kt],\' over \', ln[ko],\'") ), #Close column\'), file = filename, append = TRUE)
                                    } else {
                                        write( paste0(\'makeCriteriaListMenu("\',var,\'","Criteria importance", "\',ln[ko],\' over \',ln[kt],\'","\',ln[kt],\' over \', ln[ko],\'"),\'), file = filename, append = TRUE)
                                    }
                                }
                            }
                        write(\'column(4,\', file = filename, append = TRUE )
                        for (ko in 1:(len - 1) ) { 
                            for (kt in (ko + 1):len ) {
                                var <- paste0(ln[ko],"over",ln[kt])
                                if ( ko == len - 1 && kt == len) {
                                    write( paste0(\'makeSaatyListMenu("\',var,\'"," ") ) #Close column
                                    ) # Close paste0\'), file = filename, append = TRUE)
                                } else {
                                    write( paste0(\'makeSaatyListMenu("\',var,\'"," "),\'), file = filename, append = TRUE)
                                }
                            }
                        }
                    succ <- source(filename, local = TRUE)
                    fconn <- file(filename)
                    writeLines(succ$value, con = filename)
                    close(fconn)
                    #}
                } else if ( input$typeOfMeasurement == "Based measurements" ) {
                    if (file.exists(filename)) {
                         unlink(filename, recursive = FALSE)
                    }
                }
        }'), file = treeMenuScript, append = TRUE)
            }
            write( paste0('
if ( input$typeOfMeasurement == "Based preferences" ) {
            ## INPUT BOX
            shinydashboard::box(width = 12,
                checkboxInput("visibleMTX", "Check the box to see the comparison matrix", value =  FALSE),
                includeHTML(filename),
                column(6, uiOutput("matx"))
            )
 } else if ( input$typeOfMeasurement == "Based measurements" ) {
            ## INPUT BOX
            shinydashboard::box(width = 12,
                checkboxInput("visibleMTX", "Check the box to see the comparison matrix", value =  FALSE),
                makeCSVFileInput("fileMeasurement", 12),
                column(6, uiOutput("matx"))
            )
}
            '), file = treeMenuScript, append = TRUE)
        }
        ## INPUT BOX
        fluidRow(
            shinydashboard::box(width = 12, 
                                widgetHierarchyOutput("hier", height = '600px')
            ),
            shinydashboard::box(width = 12, title = "Edit Data File", 
                                column(6,
                                       # uncomment line below to use action button to commit changes
                                       actionButton("saveBtn", "Save"),
                                       rhandsontable::rHandsontableOutput("hotSheetTree", height = '500px')
                                ),
                                column(6,
                                       column(12,
                                           column(6, selectInput( inputId = "treeLevelChoice", label = "Choose level to compute pairwise comparison", choices = c('Select a level' = 0, names(table(theTree$depth))), multiple = FALSE, selected = 0)),
                                           column(6, uiOutput("uiSubsetTreeChoice") )
                                       ),
                                       radioButtons( "typeOfMeasurement", 'Choose how to compute pairwise comparison', c("Based preferences", "Based measurements"), selected = NULL, inline = TRUE),
                                       uiOutput("uiMeasurementSheet")
                                )
            )
        )
    })
    output$matx <- renderUI({
        ## Script
        matrixScript <- "script_matrices.R"
        if (!file.exists(matrixScript)) {
            numberLevel <- names(table(theTree$depth))
            write(paste0('
            intMatx <- NULL
            matrixRepresentation <- c()
            '), file = matrixScript)
            
            for (lv in 2:length(numberLevel) ) {
                write(paste0('
                if ( input$treeLevelChoice == ',lv,' ) {
                    #filename <- paste0("script_matrix_lv"',lv,'".R")
                    if ( input$typeOfMeasurement == "Based preferences" ) {
                        #if (!file.exists(filename)) {
                            ln <- as.character( theTree[theTree$depth == ',lv,', c("name")] )'), file = matrixScript, append = TRUE)
                if ( lv > 2) {
                    write('if ( length(input$subsetTreeChoice) > 0 && input$subsetTreeChoice != 0) {
                            ln <- na.omit( theTree[theTree$parent == input$subsetTreeChoice, c("name")] )
                        }', file = matrixScript, append = TRUE)
                }
                write( paste0('len <- length(ln)
                            intMatx <- matrix( c(1), nrow = len, ncol = len)
                            for (ko in 1:(len - 1)) { 
                                for (kt in (ko + 1):len) {
                                    evalCr <- eval(parse(text = paste0("input$",ln[ko],"and",ln[kt], collapse = "")))
                                    evalSaaty <- eval(parse(text = paste0("input$",ln[ko],"over",ln[kt], collapse = "")))
                                    if ( evalCr == paste0(ln[ko]," over ",ln[kt]) ) {
                                        intMatx[ko, kt] <- as.numeric(evalSaaty) 
                                        intMatx[kt, ko] <- 1/as.numeric(evalSaaty)
                                    } else if ( evalCr == paste0(ln[kt]," over ",ln[ko]) ) {
                                        intMatx[ko, kt] <- 1/as.numeric(evalSaaty)
                                        intMatx[kt, ko] <- as.numeric(evalSaaty)
                                    }
                                }
                            }
                            matrixRepresentation <- c(matrixRepresentation, " $$ \\\\begin{matrix} ")
                            matrixRepresentation <- c(matrixRepresentation, paste0(" & \\\\text{",ln[1:(len-1)],"}") )
                            matrixRepresentation <- c(matrixRepresentation, paste0(" & \\\\text{",ln[len],"} \\\\\\\\ ") )
                            for (ko in 1:len ) { 
                                matrixRepresentation <- c(matrixRepresentation, paste0("\\\\text{",ln[ko],"}") )
                                for (kt in 1:len) {
                                    if (ko == kt) { matrixRepresentation <- c(matrixRepresentation, paste0(" & ", as.numeric(1)) ) }
                                    else{matrixRepresentation <- c(matrixRepresentation, paste0(" & ", MASS::fractions(intMatx[ko, kt])) ) } 
                                }
                                matrixRepresentation <- c( matrixRepresentation, paste0(" \\\\\\\\ ") )
                            }
                            matrixRepresentation <- c(matrixRepresentation, " \\\\end{matrix} $$ ")
                        #}
                    } else if ( input$typeOfMeasurement == "Based measurements" ) {
                        values <- valuesTree[[paste0("level",lv)]] # careful subscript out of bounds
                    }
                }
                '), file = matrixScript, append = TRUE) 
            }
            
            write(paste0('
                vMat <- myEigenValue(intMatx)
                vMatSum <- sum(vMat)
                #ahppmr <- pmr::ahp(dset = intMatx, sim_size = 500)   
                ## INPUT BOX
                withMathJax( helpText("The pairwise comparison matrix:"), matrixRepresentation) 
             '), file = matrixScript, append = TRUE)
        }
        ## Script use
        if (!input$visibleMTX) { 
            return()
        }
        source(matrixScript, local = TRUE, verbose = FALSE)
    })
    ##########################################
    output$hier <- renderWidgetHierarchy({
        if ( is.null(theTree) ) { return(NULL) }
        widgetHierarchy(theTree, boxHeight = 60, boxWidth = 130, tx = 700, ty = 0, angle = 0, scale = 0.75)
    })
    ##########################################
    output$uiSubsetTreeChoice <- renderUI({
        if (!input$workingTree ) { 
            return()
        }
        numberLevel <- names(table(theTree$depth))
        if ( input$treeLevelChoice %in% 3:length(numberLevel) ) {
            namesLv <- theTree[theTree$depth == (as.numeric(input$treeLevelChoice) - 1), c("name")]
            if ( input$treeLevelChoice == length(numberLevel) ) {
                ## INPUT
                selectInput(inputId = "subsetTreeChoice", label = "Choose parent to compute pairwise comparison", choices = c("Select multiple parent" = 0, namesLv), multiple = TRUE, selected = 0)
            } else {
                ## INPUT
                selectInput(inputId = "subsetTreeChoice", label = "Choose parent to compute pairwise comparison", choices = c("Select a parent" = 0, namesLv), multiple = FALSE, selected = 0)
            }
        }
    })
    ##########################################
    output$uiMeasurementSheet <- renderUI({
        inFile <- input$fileMeasurement
        # input$fileMeasure will be NULL initially.
        if ( is.null(inFile) ) {
            return(NULL)
        } else if ( is.null(statsCKAN) ) {
            # After the user selects and uploads a file, it will be a data frame with \'name\', \'size\', \'type\', and \'datapath\' columns.
            # The \'datapath\' column will contain the local filenames where the data can be found.
            statsCKAN <<- read.csv( inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
        } else {
        }
        ## INPUT 
        rhandsontable::rHandsontableOutput("hotMeasurement", height = "400px")
    })
    output$hotMeasurement <- rhandsontable::renderRHandsontable({
        require(rhandsontable)
        if (!is.null(input$hotMeasurement)) {
            dataT <- hot_to_r( input$hotMeasurement )
        } else {
            if ( length(input$subsetTreeChoice) == 1 && input$subsetTreeChoice != 0) { 
                namesLv <- theTree[theTree$parent == input$subsetTreeChoice, c("name")]
                dataT <- statsCKAN[, as.character( na.omit( namesLv ))]
            } else if ( length(input$subsetTreeChoice) > 1) { 
                dataT <- statsCKAN[, input$subsetTreeChoice]
            } else {
                return(NULL)
            }
        }
        valuesTree[[paste0("level",input$treeLevelChoice)]] <<- dataT
        rhandsontable(dataT, useTypes = TRUE, readOnly = FALSE) # %>% hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        #hot_col("factor_allow", allowInvalid = TRUE)
    })
    ##########################################


    
    
    ##########################################    
    ##########################################
}

shinyApp(ui = ui, server = server)