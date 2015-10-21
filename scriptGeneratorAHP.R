reactValues$treeData <- read.csv( inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
## Script
preferenceSliderScriptName <<-  paste0("generated_basedPreferenceSlider",".R")
if ( !file.exists(preferenceSliderScriptName) ) {
  numberLevel <- names( table(reactValues$treeData$depth) )
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
               }'), file = preferenceSliderScriptName)
        for (lv in 2:(length(numberLevel)-1)) {
          write( paste0('if (input$treeLevelChoice == ', lv,') {
                        filename <- paste0("generated_",input$treeLevelChoice,".dynR")
                        '), file = preferenceSliderScriptName, append = TRUE)
          if ( lv > 2) {
            write(paste0('if ( (length(input$subsetTreeChoice) != 0) && (input$subsetTreeChoice != 0) ) {
                    ln <- na.omit( reactValues$treeData[reactValues$treeData$parent %in% input$subsetTreeChoice, c("name")] )'),
                  file = preferenceSliderScriptName, append = TRUE)
          } else {
            write(paste0('ln <- reactValues$treeData[reactValues$treeData$depth == ',lv,', c("name")]'),
                  file = preferenceSliderScriptName, append = TRUE)
          }
          write( paste0('if ( input$typeOfMeasurement == "Based preferences" ) {
                        if (!file.exists(filename)) {
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
                    }
                } else if ( input$typeOfMeasurement == "Based measurements" ) {
                    if (file.exists(filename)) {
                         unlink(filename, recursive = FALSE)
                    }
                }
        }'), file = preferenceSliderScriptName, append = TRUE)
        if ( lv > 2) {# Add the missing curl brace to close the open if statement to select desired parents
            write(paste0('}'), file = preferenceSliderScriptName, append = TRUE)
        }
        }
        write(paste0('if ( input$treeLevelChoice == ',length(numberLevel),' ) {
                      filename <- paste0("generated_",input$treeLevelChoice,".dynR")
                      if ( input$typeOfMeasurement == "Based preferences" ) {
                      if (!file.exists(filename)) {
                        ln <- as.character( reactValues$treeData[reactValues$treeData$depth == ',length(numberLevel),', c("name")] )
                        #selectedParent <- na.omit( reactValues$treeData[reactValues$treeData$parent %in% input$subsetTreeChoice, c("name")] )
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
                        }
                      } else if ( input$typeOfMeasurement == "Based measurements" ) {
                              if (file.exists(filename)) {
                                 unlink(filename, recursive = FALSE)
                              }
                          }
                        }
                       '), file = preferenceSliderScriptName, append = TRUE)
}