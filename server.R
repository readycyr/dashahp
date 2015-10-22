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
  preferenceSliderScriptName <- ""
  valuesTree <- list()
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
    rhandsontable(reactValues$treeData, useTypes = TRUE, readOnly = TRUE) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = FALSE, search = TRUE)
  })
  #####
  # output BOX for the AHP configuration
  #####
  output$ahpConfigurationBox <- renderUI({
    inFile <- input$fileTree # input$fileTree will be NULL initially.
    if ( is.null(inFile) ) {
      return(NULL)
    } else { 
      succ <- source("scriptGeneratorAHP.R", local = TRUE)
      return(## INPUT BOX
        shinydashboard::box(title = "Edit Data File", 
                            collapsible = FALSE, collapsed = FALSE, width = 12, 
                            status = NULL, background = NULL, solidHeader = FALSE,
                            column(6,
                                   column(6, selectInput( inputId = "treeLevelChoice", label = "Choose level to compute pairwise comparison", choices = c('Select a level' = 0, names(table(reactValues$treeData$depth))), multiple = FALSE, selected = 0)),
                                   column(6, uiOutput("uiSubsetTreeChoice"))
                            ),
                            column(6,
                                   radioButtons( "typeOfMeasurement", 'Choose how to compute pairwise comparison', c("Based preferences", "Based measurements"), selected = NULL, inline = TRUE)
                            ),
                            uiOutput("uiValueBasedWhatever")
        ) 
      )
    }
  })
  #####
  # output for sliders or measurement sheet
  #####
  output$uiValueBasedWhatever <- renderUI({
    # Use the fact that the tree is loaded or not
    if ( length(input$treeLevelChoice) == 0 ) {
      return(NULL)
    } else {
      if ( input$typeOfMeasurement == "Based preferences" ) {
        succ <- source(preferenceSliderScriptName, local = TRUE)
        if (input$treeLevelChoice == 0) { return(NULL) }
        else if ( (input$treeLevelChoice == 1) ||
                  (input$treeLevelChoice == 2) ||
                  ( (input$treeLevelChoice > 2) && 
                    (length(input$subsetTreeChoice)!=0) && 
                    (input$subsetTreeChoice != 0))
                ) {
          return(## INPUT BOX
            shinydashboard::box(width = 12,
                                checkboxInput("visibleMTX", "Check the box to see the comparison matrix", value = FALSE),
                                includeHTML(filename),
                                column(6, uiOutput("matx"))
            ))
        } else {  return(NULL) }
      } else if ( input$typeOfMeasurement == "Based measurements" ) {
        return(## INPUT BOX
          shinydashboard::box(width = 12,
                              checkboxInput("visibleMTX", "Check the box to see the comparison matrix", value = FALSE),
                              makeCSVFileInput("fileMeasurement", 12),
                              column(6, uiOutput("uiMeasurementSheet")),
                              column(6, uiOutput("matx"))
          ))
      }
    }
  })
  #####
  # output sub menu to choose parent of the desired criteria 
  #####
  output$uiSubsetTreeChoice <- renderUI({
    numberLevel <- names( table(reactValues$treeData$depth) )
    if ( (length(input$treeLevelChoice) != 0) && input$treeLevelChoice %in% 3:length(numberLevel) ) {
      namesLv <- as.character( reactValues$treeData[reactValues$treeData$depth == (as.numeric(input$treeLevelChoice) - 1), c("name")] )
      if ( input$treeLevelChoice == length(numberLevel) ) {
        return(## INPUT
          selectInput(inputId = "subsetTreeChoice", label = "Choose parent to compute pairwise comparison", choices = c("Select multiple parent" = 0, namesLv), multiple = TRUE, selected = 0)
        )
      } else {
        return(## INPUT
          selectInput(inputId = "subsetTreeChoice", label = "Choose parent to compute pairwise comparison", choices = c("Select a parent" = 0, namesLv), multiple = FALSE, selected = 0)
        )
      }
    } else { return(NULL) }
  })
  #####
  # output MATRIX
  #####
  output$matx <- renderUI({
    ## Script
    matrixScript <- "generated_matrics.R"
    if (!file.exists(matrixScript)) {
      numberLevel <- names(table(reactValues$treeData$depth))
      write(paste0('
                  intMatx <- NULL
                  matrixRepresentation <- c()
                  vectorRepresentation <- c()'), file = matrixScript)
            
            for (lv in 2:(length(numberLevel)-1) ) {
                write(paste0('
                             if ( input$treeLevelChoice == ',lv,' ) {
                             if ( input$typeOfMeasurement == "Based preferences" ) {'), file = matrixScript, append = TRUE)
                if ( lv > 2) {
                  write(paste0('ln <- na.omit( reactValues$treeData[reactValues$treeData$parent %in% input$subsetTreeChoice, c("name")] )'),
                        file = matrixScript, append = TRUE)
                } else {
                  write(paste0('ln <- as.character( reactValues$treeData[reactValues$treeData$depth == ',lv,', c("name")] )'),
                        file = matrixScript, append = TRUE)
                }
                write( paste0('len <- length(ln)
                              intMatx <- matrix( c(1), nrow = len, ncol = len)
                              for (ko in 1:(len - 1)) { 
                                for (kt in (ko + 1):len) {
                                  evalCr <- eval(parse(text = paste0("input$",ln[ko],"and",ln[kt], collapse = "")))
                                  if (length(evalCr) == 0 ) { return(NULL) }
                                  evalSaaty <- eval(parse(text = paste0("input$",ln[ko],"over",ln[kt], collapse = "")))
                                  if (evalCr == paste0(ln[ko]," over ",ln[kt])) {
                                    intMatx[ko, kt] <- as.numeric(evalSaaty) 
                                    intMatx[kt, ko] <- 1/as.numeric(evalSaaty)
                                  } else if ( evalCr == paste0(ln[kt]," over ",ln[ko])) {
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
                             } else if ( input$typeOfMeasurement == "Based measurements" ) {
                              values <- valuesTree[[paste0("saveDataLevel",lv)]] # careful subscript out of bounds
                             }
                             }
                            '), file = matrixScript, append = TRUE) 
            }
          write(paste0('if ( input$treeLevelChoice == ',length(numberLevel),' ) {
                          if ( input$typeOfMeasurement == "Based preferences" ) {
                            ln <- as.character( reactValues$treeData[reactValues$treeData$depth == ',length(numberLevel),', c("name")] )
                            #selectedParent <- na.omit( reactValues$treeData[reactValues$treeData$parent %in% input$subsetTreeChoice, c("name")] )
                            len <- length(ln)
                            intMatx <- matrix( c(1), nrow = len, ncol = len)
                            for (ko in 1:(len - 1)) { 
                              for (kt in (ko + 1):len) {
                                evalCr <- eval(parse(text = paste0("input$",ln[ko],"and",ln[kt], collapse = "")))
                                evalSaaty <- eval(parse(text = paste0("input$",ln[ko],"over",ln[kt], collapse = "")))
                                if (paste0(ln[ko]," over ",ln[kt]) == evalCr) {
                                  intMatx[ko, kt] <- as.numeric(evalSaaty) 
                                  intMatx[kt, ko] <- 1/as.numeric(evalSaaty)
                                } else if (paste0(ln[kt]," over ",ln[ko]) == evalCr) {
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
                          } else if ( input$typeOfMeasurement == "Based measurements" ) {
                            values <- valuesTree[[paste0("saveDataLevel",lv)]] # careful subscript out of bounds
                          }
                        }'), file = matrixScript, append = TRUE)
            write(paste0('
                        vMat <- myEigenValue(intMatx)
                        vMatSum <- sum(vMat)
                        ahppmr <- pmr::ahp(dset = intMatx, sim_size = 500)
                        vectorRepresentation <- c(vectorRepresentation, " $$ \\\\Longrightarrow ")
                        vectorRepresentation <- c(vectorRepresentation, "  \\\\begin{bmatrix} ")
                        vectorRepresentation <- c(vectorRepresentation, paste0( specify_digits((vMat/vMatSum), 3)," \\\\\\\\ "))
                        vectorRepresentation <- c(vectorRepresentation, paste0(" \\\\hline", sum(vMat/vMatSum)," \\\\\\\\ ")) 
                        vectorRepresentation <- c(vectorRepresentation, " \\\\end{bmatrix} or \\\\begin{bmatrix} ")
                        vectorRepresentation <- c(vectorRepresentation, paste0( specify_digits((ahppmr$weighting), 3)," \\\\\\\\ "))
                        vectorRepresentation <- c(vectorRepresentation, paste0(" \\\\hline", sum(ahppmr$weighting)," \\\\\\\\ ")) 
                        vectorRepresentation <- c(vectorRepresentation, " \\\\end{bmatrix} \\\\\\\\ $$ ")
                        vectorRepresentation <- c(vectorRepresentation, paste0(" \\\\begin{array}{c} 
                                \\\\text{Saaty\'s inconsistency} = ", specify_digits((ahppmr$Saaty), 3)," \\\\\\\\ ",
                                "\\\\text{Koczkodaj\'s inconsistency} = ", specify_digits((ahppmr$Koczkodaj), 3),
                                " \\\\end{array}")
                        )'), file = matrixScript, append = TRUE)
        }
        ## Script use
        if (input$visibleMTX && (length(input$treeLevelChoice) != 0)) {
          if ( (input$treeLevelChoice == 2) ||
               ( (input$treeLevelChoice > 2) && 
                 (length(input$subsetTreeChoice)!=0) && 
                 (input$subsetTreeChoice != 0))
          ) {
            if ( !exists("matrixRepresentation") ) {
              succ <- source(matrixScript, local = TRUE, verbose = FALSE)
            }
            return(## INPUT BOX
                  withMathJax( helpText("The pairwise comparison matrix:"), 
                               matrixRepresentation, vectorRepresentation)
            )
          } else{  return(NULL) }
        } else{  return(NULL) }
    })
  #####
  # output measurement SHEET
  #####
  output$uiMeasurementSheet <- renderUI({
    inFile <- input$fileMeasurement
    # input$fileMeasure will be NULL initially.
    if ( is.null(inFile) ) {
      if ( input$treeLevelChoice > 2) {
        if ( length(input$subsetTreeChoice) != 0 && input$subsetTreeChoice != 0) {
          colOfNames <- na.omit( reactValues$treeData[reactValues$treeData$parent %in% input$subsetTreeChoice, c("name")] )
          gen <- paste0(input$subsetTreeChoice," = rep(NA, length(colOfNames))", collapse = " , ")        
          valuesTree[[paste0("file_level",input$treeLevelChoice)]] <<- eval(parse(text=paste0("data.frame( Au_vu_de = colOfNames,", gen, ", stringsAsFactors = FALSE)")))
        } else {
          return(NULL)
        }
      } else if ( input$treeLevelChoice == 2) {
        colOfNames <- as.character( reactValues$treeData[reactValues$treeData$depth == input$treeLevelChoice, c("name")] )
        valuesTree[[paste0("file_level",input$treeLevelChoice)]] <<- data.frame( 
          Au_vu_de = colOfNames,
          root = rep(NA, length(colOfNames)),
          stringsAsFactors = FALSE)
      } else {
        return(NULL)
      }
    } else if ( is.null( valuesTree[[paste0("file_level",input$treeLevelChoice)]] ) ) {
      # After the user selects and uploads a file, it will be a data frame with \'name\', \'size\', \'type\', and \'datapath\' columns.
      # The \'datapath\' column will contain the local filenames where the data can be found.
      valuesTree[[paste0("file_level",input$treeLevelChoice)]] <<- read.csv( inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    }
    ## INPUT 
    rhandsontable::rHandsontableOutput("measurementSheet", height = "400px")
  })
  output$measurementSheet <- rhandsontable::renderRHandsontable({
    require(rhandsontable)
    if (!is.null(input$measurementSheet)) {
      dataT <- hot_to_r( input$measurementSheet )
    } else {
      if ( input$treeLevelChoice > 2) {
        if ( length(input$subsetTreeChoice) != 0 && input$subsetTreeChoice != 0) {
          fileForMeasure <- valuesTree[[paste0("file_level",input$treeLevelChoice)]]
          if ( length(input$subsetTreeChoice) == 1)  {
            dataT <- fileForMeasure#[, as.character(input$subsetTreeChoice)]
          } else {
            dataT <- fileForMeasure#[, as.character(input$subsetTreeChoice)]
          }
        } else {
          return(NULL)
        }
      } else if ( input$treeLevelChoice == 2) {
        fileForMeasure <- valuesTree[[paste0("file_level", input$treeLevelChoice)]]
        dataT <- fileForMeasure
      }
    }
    valuesTree[[paste0("saveDataLevel",input$treeLevelChoice)]] <<- dataT
    rhandsontable(dataT, rowHeaders = NULL, useTypes = TRUE, readOnly = FALSE) # %>% hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
    #hot_col("factor_allow", allowInvalid = TRUE)
  })
}