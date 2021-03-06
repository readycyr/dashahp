##########################################
##          Tierce Function             ##
##########################################
makeCriteriaListMenu <- function(id, alabel, crit1, crit2) {
  selectInput(inputId = id, label = alabel, choices = c(crit1, crit2), multiple = FALSE, selected = crit1)
}
makeSaatyListMenu <- function(id, alabel) {
  sliderInput( id, alabel, min = 1,max = 9, value = 5, step = 2, ticks = FALSE, post = "x more important")
}
sylEigenValue <- function(matx) {
  dd <- dim(matx)
  fmat <- c()
  sum_call <- colSums(matx)
  for( j in seq_len(dd[2]) ) {
    if(sum_call[j] == 0) {
      fmat <- c(fmat, rep(0, each=dd[1]) )
    } else {
      fmat <- c(fmat, matx[,j]/sum_call[j])
    }
  }
  fmat<- matrix(fmat, byrow = FALSE, ncol = dd[2])
  normalized_fmat <- c()
  for( i in seq_len(dd[1])) {
    normalized_fmat <- c(normalized_fmat, sum(fmat[i,])/dd[1])
  }
  normalized_fmat  
}
myEigenValue <- function(matx) {
  sqOne <- matx %*% matx
  sqTwo <- sqOne %*% sqOne
  sqMatx <- sqTwo %*% sqTwo
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
matrix_basedMeasurement <- function(col_A_Cj) {
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
  PcAcj
  #    mThree <- eigenVector_basedMeasurement(col_qu)
  #    tt <- as.matrix(read.table("./datasets/qu_extras_EVbasedMeasurement.txt", sep = ","))
  #    for ( k in 1:126) { print( all.equal(as.numeric(tt[k,]), mThree[k,]) ) }
}

topsis_ranking <- function(matx) {
  colMax <- apply(matx,2,max)
  colMin <- apply(matx,2,min)
  dd <- dim(matx)
  d_al <- c()
  for( i in seq_len(dd[1])) {
    sum_res_p <- 0
    sum_res_n <- 0
    for( j in seq_len(dd[2])) {
      sum_res_p <- sum_res_p + ( matx[i,j] - colMax[j] )^2
      sum_res_n <- sum_res_n + ( matx[i,j] - colMin[j] )^2
    }
    d_al <- c(d_al, sqrt(sum_res_n), sqrt(sum_res_p))
  }
  md_al <- matrix(d_al, byrow = TRUE, ncol = 2)
  r_al <- c()
  for( i in seq_len(dd[1])) {
    r_al_i <- md_al[i,1]/(md_al[i,2]+md_al[i,1])
    r_al <- c(r_al, r_al_i)
  }
  r_al
}

#colMin <- function(data) sapply(data, min, na.rm = TRUE)
##########################################
##                SERVER                ##
##########################################
function(input, output) {
  # Global server variables
  reactValues <- reactiveValues()
  preferenceSliderScriptName <- ""
  hasDataFile <- NULL
  lastUploadedFile <- c()
  valuesTree <- list()
  eigenFrame <- NULL
  ## Function
  weights_aggregation <- function(parent, wg, level) {
    if(level == 2) {
      valuesTree[[paste0("weight_aggr_",parent)]] <<- wg*valuesTree[[paste0("weight_level_",parent)]]
    } else {
      k <- 1
      lv <- level-1      
      for(n in na.omit( reactValues$treeData[reactValues$treeData$parent %in% parent, c("name")] )) {
        nwg <- valuesTree[[paste0("weight_level_",parent)]]
        weights_aggregation(n, wg*nwg[k], lv)
        k <- k+1
      }
    }
  }
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
                  dataByMeasure <- NULL
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
                                    intMatx[kt, ko] <- (1/as.numeric(evalSaaty))
                                  } else if ( evalCr == paste0(ln[kt]," over ",ln[ko])) {
                                    intMatx[ko, kt] <- (1/as.numeric(evalSaaty))
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
                                dataByMeasure <- valuesTree[[paste0("file_level",input$treeLevelChoice)]]
                                intMatx <- list()
                                if ( input$treeLevelChoice > 2) {
                                  if ( length(input$subsetTreeChoice) != 0 && input$subsetTreeChoice != 0) {
                                    for(nm in as.character(input$subsetTreeChoice)) {
                                      intMatx[[paste0(nm)]] <- matrix_basedMeasurement( as.numeric(dataByMeasure[,c(nm)]) )
                                    }
                                  } else {
                                    return(NULL)
                                  }                                  
                                } else {
                                  intMatx[["root"]] <- matrix_basedMeasurement( as.numeric(dataByMeasure[, c("root")]) )
                                }
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
                                  intMatx[kt, ko] <- (1/as.numeric(evalSaaty))
                                } else if (paste0(ln[kt]," over ",ln[ko]) == evalCr) {
                                  intMatx[ko, kt] <- (1/as.numeric(evalSaaty))
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
                                dataByMeasure <- valuesTree[[paste0("file_level",input$treeLevelChoice)]]
                                intMatx <- list()
                                if ( length(input$subsetTreeChoice) != 0 && input$subsetTreeChoice != 0) {
                                  for(nm in as.character(input$subsetTreeChoice)) {
                                    intMatx[[paste0(nm)]] <- matrix_basedMeasurement( as.numeric(dataByMeasure[,c(nm)]) )
                                  }
                                } else {
                                  return(NULL)
                                } 
                          }
                        }'), file = matrixScript, append = TRUE)
            write(paste0('
                    if ( input$typeOfMeasurement == "Based preferences" ) {
                        vMat <- myEigenValue(intMatx)
                        vMatSum <- sum(vMat)
                        vectorRepresentation <- c(vectorRepresentation, " $$ \\\\Longrightarrow ")
                        vectorRepresentation <- c(vectorRepresentation, "  \\\\begin{bmatrix} ")
                        vectorRepresentation <- c(vectorRepresentation, paste0( specify_digits((vMat/vMatSum), 3)," \\\\\\\\ "))
                        vectorRepresentation <- c(vectorRepresentation, paste0(" \\\\hline", sum(vMat/vMatSum)," \\\\\\\\ ")) 
                      if( dim(intMatx)[1] > 2 ) {
                        ahppmr <- pmr::ahp(dset = intMatx, sim_size = 500)

## For level 4 only one matrices is created
if ( input$treeLevelChoice == 2) {
  valuesTree[[paste0("weight_level_","root")]] <<- ahppmr$weighting
} else {
  valuesTree[[paste0("weight_level_",input$subsetTreeChoice)]] <<- ahppmr$weighting
}
                        vectorRepresentation <- c(vectorRepresentation, " \\\\end{bmatrix} or \\\\begin{bmatrix} ")
                        vectorRepresentation <- c(vectorRepresentation, paste0( specify_digits((ahppmr$weighting), 3)," \\\\\\\\ "))
                        vectorRepresentation <- c(vectorRepresentation, paste0(" \\\\hline", sum(ahppmr$weighting)," \\\\\\\\ ")) 
                        vectorRepresentation <- c(vectorRepresentation, " \\\\end{bmatrix} \\\\\\\\ $$ ")
                        vectorRepresentation <- c(vectorRepresentation, paste0(" \\\\begin{array}{c} 
                                \\\\text{Saaty\'s inconsistency} = ", specify_digits((ahppmr$Saaty), 3)," \\\\\\\\ ",
                                "\\\\text{Koczkodaj\'s inconsistency} = ", specify_digits((ahppmr$Koczkodaj), 3),
                                " \\\\end{array}"))
                      } else {

## For level 4 only one matrices is created
if ( input$treeLevelChoice == 2) {
  valuesTree[[paste0("weight_level_","root")]] <<- (vMat/vMatSum)
} else {
  valuesTree[[paste0("weight_level_",input$subsetTreeChoice)]] <<- (vMat/vMatSum)
}

                        vectorRepresentation <- c(vectorRepresentation, " \\\\end{bmatrix} \\\\\\\\ $$ ")
                      }
                    } else if ( input$typeOfMeasurement == "Based measurements" ) {

                      eigenFrame <<- data.frame("Au_vu_de"= as.character(dataByMeasure[, c("Au_vu_de")]))
                      if ( input$treeLevelChoice > 2) {
                        if ( length(input$subsetTreeChoice) != 0 && input$subsetTreeChoice != 0) {
                                for(nm in as.character(input$subsetTreeChoice)) {
                                  res <- NULL
                                  if( nrow(intMatx[[paste0(nm)]])[1] > 2 ) {
                                    # res <- pmr::ahp(dset = intMatx[[paste0(nm)]] , sim_size = 500)
                                    # eigenFrame <<- eval(parse(text= paste0("cbind(eigenFrame,",nm," = ",res$weighting,")") ))
                                     res <- sylEigenValue( intMatx[[paste0(nm)]] )
                                     eigenFrame <<- eval(parse(text= paste0("cbind(eigenFrame,",nm," = ",res,")") ))
valuesTree[[paste0("weight_level_",nm)]] <<- res #$weighting
                                  } else {
                                     res <- myEigenValue(intMatx[[paste0(nm)]])
                                     eigenFrame <<- eval(parse(text= paste0("cbind(eigenFrame,",nm," = ",(res/sum(res)),")") ))
valuesTree[[paste0("weight_level_",nm)]] <<- (res/sum(res))
                                   }
                                }
                        } else {
                            return(NULL)
                        }                                  
                      } else if ( input$treeLevelChoice == 2) {
                            # res <- pmr::ahp(dset = intMatx[["root"]], sim_size = 500)
                            # eigenFrame <- cbind(eigenFrame, root = res$weighting)
                            res <- sylEigenValue(intMatx[["root"]])
                            eigenFrame <- cbind(eigenFrame, root = res)
valuesTree[[paste0("weight_level_", "root")]] <<- res #$weighting
                      } else {
                          return(NULL)
                      }
                    }

## To update TREE
nodeName <- NULL
nodeValues <- NULL
if ( input$treeLevelChoice <4) {
   if ( input$treeLevelChoice > 2) {
     nodeValues <- valuesTree[[paste0("weight_level_",input$subsetTreeChoice)]]
     nodeName <- as.character(na.omit( reactValues$treeData[reactValues$treeData$parent %in% input$subsetTreeChoice, c("name")] ))
   } else {
     nodeValues <- valuesTree[[paste0("weight_level_","root")]]
     nodeName <- as.character( reactValues$treeData[reactValues$treeData$depth == 2, c("name")] )
   }
}
print("==============================")
print(nodeName)
print(nodeValues)
print("==============================")
#   for(index in seq_len(length(nodeName))) {
#     #reactValues$treeData[ reactValues$treeData$name == as.character(nodeName[index]), c("score")] <- as.numeric(nodeValues[index])  
#   }

  '), file = matrixScript, append = TRUE)
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
    # input$fileMeasure will be NULL initially.
    inFile <- input$fileMeasurement
    if ( identical(TRUE,hasDataFile[input$treeLevelChoice]) ) {
      # After the user selects and uploads a file, it will be a data frame with \'name\', \'size\', \'type\', and \'datapath\' columns.
      # The \'datapath\' column will contain the local filenames where the data can be found.
      valuesTree[[paste0("file_level",input$treeLevelChoice)]] <<- read.csv( inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    } else {
      if ( is.null(inFile) || (inFile$datapath %in% lastUploadedFile) ) { 
        if ( input$treeLevelChoice > 2) { # XXX TODO XXX one or several files for an intermediate level
          if ( length(input$subsetTreeChoice) != 0 && input$subsetTreeChoice != 0) {
            nametab <- table(reactValues$treeData$depth)
            numberLevel <- names( nametab )
            colOfNames <- na.omit( reactValues$treeData[reactValues$treeData$parent %in% input$subsetTreeChoice, c("name")] )
            gen <- paste0(input$subsetTreeChoice," = rep(1, length(colOfNames))", collapse = " , ")
            if(input$treeLevelChoice == length(numberLevel)) {
              colOfNames <- na.omit( reactValues$treeData[reactValues$treeData$depth == length(numberLevel), c("name")] )
              nb <- nametab[[numberLevel[length(numberLevel)]]]
              gen <- paste0(input$subsetTreeChoice," = rep(1, as.numeric(nb))", collapse = " , ")
            }
            valuesTree[[paste0("file_level",input$treeLevelChoice)]] <<- eval(parse(text=paste0("data.frame( Au_vu_de = colOfNames,", gen, ", stringsAsFactors = FALSE)")))
          } else {
            return(NULL)
          }
        } else if ( input$treeLevelChoice == 2) {
          colOfNames <- as.character( reactValues$treeData[reactValues$treeData$depth == input$treeLevelChoice, c("name")] )
          valuesTree[[paste0("file_level",input$treeLevelChoice)]] <<- data.frame( 
            Au_vu_de = colOfNames,
            root = rep(1, length(colOfNames)),
            stringsAsFactors = FALSE)
        } else {
          return(NULL)
        }
      } else {
        hasDataFile[input$treeLevelChoice] <- TRUE
        lastUploadedFile <- c(lastUploadedFile, inFile$datapath)
        valuesTree[[paste0("file_level",input$treeLevelChoice)]] <<- read.csv( inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
      }
    }
    ## INPUT 
    rhandsontable::rHandsontableOutput("measurementSheet", height = "400px")
  })
  output$measurementSheet <- rhandsontable::renderRHandsontable({
    require(rhandsontable)
#    if (!is.null(input$measurementSheet)) {
#      valuesTree[[paste0("file_level",input$treeLevelChoice)]] <<-
#      dataT <- hot_to_r( input$measurementSheet )
#    } else {
      if ( input$treeLevelChoice > 2) {
        if ( length(input$subsetTreeChoice) != 0 && input$subsetTreeChoice != 0) {
          fileForMeasure <- valuesTree[[paste0("file_level",input$treeLevelChoice)]]
          dataT <- fileForMeasure[, c("Au_vu_de",as.character(input$subsetTreeChoice))]
        } else {
          return(NULL)
        }
      } else if ( input$treeLevelChoice == 2) {
        fileForMeasure <- valuesTree[[paste0("file_level", input$treeLevelChoice)]]
        dataT <- fileForMeasure
      }
#    }
    rhandsontable(dataT, rowHeaders = NULL, useTypes = TRUE, readOnly = FALSE) # %>% hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
    #hot_col("factor_allow", allowInvalid = TRUE)
  })
  
  #####
  # output
  #####
  output$plothist <- renderPlot({
    if( !is.null(eigenFrame) )  {
      # weight aggregation
      if ( input$treeLevelChoice == 4) {
        numberLevel <- names( table(reactValues$treeData$depth) )
# Work just for this tree
#       for(lv in 2:(length(numberLevel)-2) ) {
#         for(p in  names(table( reactValues$treeData[reactValues$treeData$depth == lv, c("parent")] )) ) {
#           pwg <- valuesTree[[paste0("weight_level_","root")]]
#           index <- 1
#           for(n in na.omit( reactValues$treeData[reactValues$treeData$parent %in% p, c("name")] )) {
#             nwg <- valuesTree[[paste0("weight_level_",n)]]
#             k <- 1
#             for( subn in na.omit( reactValues$treeData[reactValues$treeData$parent %in% n, c("name")] ) ) {
#               valuesTree[[paste0("weight_aggr_",subn)]] <- pwg[index]*nwg[k]*valuesTree[[paste0("weight_level_",subn)]]
#               k <- k+1
#             }
#             index <- index+1
#           }
#         }
#       }
        # Weight aggragation
        pwg <- valuesTree[[paste0("weight_level_","root")]]
        root <- as.character( reactValues$treeData[reactValues$treeData$depth == 1, c("name")] )
        index <- 1
        for(node in na.omit( reactValues$treeData[reactValues$treeData$parent %in% root, c("name")] )) {
          weights_aggregation(node, pwg[index], length(numberLevel)-1)
          index <- index+1
        }
        ultim <- c()
        for(nm in as.character(input$subsetTreeChoice)) {
          ultim <- c(ultim, valuesTree[[paste0("weight_aggr_",nm)]] )
        }
        theMat <- matrix(ultim, byrow = FALSE, ncol = length(input$subsetTreeChoice))
        #
        topMat <- topsis_ranking(theMat)
        indTopMat <- cbind( 1:length(topMat), topMat )
        indTopMat <- indTopMat[ order(indTopMat[,2]),]
        print(indTopMat)
#         altRanked <- topsis::topsis(decision = theMat, 
#                                     weights = rep(1, each = dim(theMat)[2]),  
#                                     impacts = rep("+", each = dim(theMat)[2]))        
#         print(altRanked$score)
#         print(altRanked$rank)
        barplot(topMat, main = "Site Distribution", horiz = FALSE, 
                       names.arg = as.character(eigenFrame[, c("Au_vu_de")]), cex.names = 1, axis.lty = 1, las = 2)
      }
      #lines(thesiteX, c(0, thesiteY), col = "red",lwd = 5)      
    }
  })
  
  
}