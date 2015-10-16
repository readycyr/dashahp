##########################################
##          Tierce Function             ##
##########################################
makeCriteriaListMenu <- function(id, alabel, crit1, crit2) {
  selectInput(inputId = id, label = alabel, choices = c(crit1, crit2), multiple = FALSE, selected = crit1)
}
makeSaatyListMenu <- function(id, alabel) {
  sliderInput( id, alabel, min = 1,max = 9, value = 5, step = 2, ticks = FALSE, post = "x more important")
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
##                SERVER                ##
##########################################
function(input, output) {
  # Global server variables
  reactValues <- reactiveValues()
  treeMenuScript <- ""
  # Data to provide end-user an example
  reactValues$treeData <- data.frame(
    name = c( 'Root', 
              paste0('Level-2-',letters[1:3]),
              paste0('Level-n-',letters[1:6]),
              paste0('alternative ', 1:5)
    ),
    parent = c( NA,
                rep('Root', each = 3),
                rep('Level-2-a',each = 2),
                rep('Level-2-b',each = 2),
                rep('Level-2-c',each = 2),
                rep('Level-n-c',each = 5)
    ),
    depth = c( 1, 
               rep(2,each = 3),
               rep(3,each = 6),
               rep(4,each = 5)
    ),
    # score = runif(14,1, 100)
    stringsAsFactors = FALSE)
  #####
  # output BOX for the TREE
  #####
  output$ahpTree <- widgetHierarchy::renderWidgetHierarchy({
    require(widgetHierarchy)
    if ( is.null(reactValues$treeData) ) { 
      return(NULL) 
    }
    widgetHierarchy(reactValues$treeData, boxHeight = 60, boxWidth = 130, tx = 700, ty = 0, angle = 0, scale = 0.75)
  })
  #####
  # output BOX for data sheet used to build the TREE
  #####
  output$exampleSheetTree <- rhandsontable::renderRHandsontable({
    require(rhandsontable)
    if ( is.null(reactValues$treeData) ) { 
      return(NULL) 
    }
    rhandsontable(reactValues$treeData, useTypes = TRUE, readOnly = TRUE, width = '100%') %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = FALSE, search = TRUE)
  })
  #####
  # output BOX
  #####
  output$ahpConfigurationBox <- renderUI({
    inFile <- input$fileTree
    # input$fileTree will be NULL initially.
    if ( is.null(inFile) ) {
      return(NULL)
    } else { #if ( is.null( treeData ) ) {
      reactValues$treeData <- read.csv( inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
      ## Script
      treeMenuScript <<-  paste0("generated_TreeManagement",".R")
      if (!file.exists(treeMenuScript)) {
        numberLevel <- names(table(reactValues$treeData$depth))
        write(paste0('
if (input$treeLevelChoice == ', 1,') {
    namesLv1 <- as.character( reactValues$treeData[reactValues$treeData$depth == 1, c("name")] )
    namesLv2 <- as.character( reactValues$treeData[reactValues$treeData$depth == 2, c("name")] )
    namesLv3 <- as.character( reactValues$treeData[reactValues$treeData$depth == 3, c("name")] )
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
                ln <- reactValues$treeData[reactValues$treeData$depth == ',lv,', c("name")]'), file = treeMenuScript, append = TRUE)
          if ( lv > 2) {
            write('if ( length(input$subsetTreeChoice) > 0 && input$subsetTreeChoice != 0) {
                            ln <- na.omit( reactValues$treeData[reactValues$treeData$parent == input$subsetTreeChoice, c("name")] )
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
if (input$treeLevelChoice >= 1) {
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
}
            '), file = treeMenuScript, append = TRUE)
        
      }
    }
    ## INPUT BOX
    shinydashboard::box(title = "Edit Data File", 
                        collapsible = FALSE, collapsed = FALSE, width = 12, 
                        status = NULL, background = NULL, solidHeader = FALSE,
                        column(6,
                              column(6, selectInput( inputId = "treeLevelChoice", label = "Choose level to compute pairwise comparison", choices = c('Select a level' = 0, names(table(reactValues$treeData$depth))), multiple = FALSE, selected = 0)),
                              column(6, uiOutput("uiSubsetTreeChoice"))
                        ),
                        column(6,
                               radioButtons( "typeOfMeasurement", 'Choose how to compute pairwise comparison', c("Based preferences", "Based measurements"), selected = NULL, inline = TRUE)
                        )
    )
  })
  #####
  # output
  #####
  output$uiValueBasedWhatever <- renderUI({
    # Use the fact that the tree is loaded or not
    if ( length(input$treeLevelChoice) == 0 ) {
      return(NULL)
    } else {
      if ( input$typeOfMeasurement == "Based preferences" ) {
        source(treeMenuScript, local = TRUE)
      } else if ( input$typeOfMeasurement == "Based measurements" ) {
        uiOutput("uiMeasurementSheet")
      }
    }
  })
  #####
  # output
  #####
  output$uiSubsetTreeChoice <- renderUI({
    if ( length(input$treeLevelChoice) == 0 ) { return(NULL) }

    numberLevel <- names(table(reactValues$treeData$depth))
    if ( input$treeLevelChoice %in% 3:length(numberLevel) ) {
      namesLv <- as.character(reactValues$treeData[reactValues$treeData$depth == (as.numeric(input$treeLevelChoice) - 1), c("name")])
      if ( input$treeLevelChoice == length(numberLevel) ) {
        ## INPUT
        selectInput(inputId = "subsetTreeChoice", label = "Choose parent to compute pairwise comparison", choices = c("Select multiple parent" = 0, namesLv), multiple = TRUE, selected = 0)
      } else {
        ## INPUT
        selectInput(inputId = "subsetTreeChoice", label = "Choose parent to compute pairwise comparison", choices = c("Select a parent" = 0, namesLv), multiple = FALSE, selected = 0)
      }
    }
  })
  #####
  # output MATRIX
  #####
  output$matx <- renderUI({
    ## Script
    matrixScript <- "script_matrices.R"
    if (!file.exists(matrixScript)) {
      numberLevel <- names(table(reactValues$treeData$depth))
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
                             ln <- as.character( reactValues$treeData[reactValues$treeData$depth == ',lv,', c("name")] )'), file = matrixScript, append = TRUE)
                if ( lv > 2) {
                    write('if ( length(input$subsetTreeChoice) > 0 && input$subsetTreeChoice != 0) {
                          ln <- na.omit( reactValues$treeData[reactValues$treeData$parent == input$subsetTreeChoice, c("name")] )
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
        if (input$visibleMTX && input$treeLevelChoice > 1) {
          source(matrixScript, local = TRUE, verbose = FALSE)
        } else{
          return(NULL)
        }
    })
  #####
  # output SHEET
  #####
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
}