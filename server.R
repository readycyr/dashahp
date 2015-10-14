function(input, output) {
  # Fake data for example
  
  reactValues <- reactiveValues()
  
  reactValues$treeData <- data.frame(
    name = c( 'Root', 
              paste0('Level-2-',letters[1:3]),
              paste0('Level-n-',letters[1:6]),
              paste0('alternative ', 1:5)
    ),
    parent = c( NA,
                rep('Root', each = 3),
                rep('Level-2-a', each = 2),
                rep('Level-2-b', each = 2),
                rep('Level-2-c', each = 2),
                rep('Level-n-c', each = 5)
    ),
    depth = c( 1, 
               rep(2, each = 3),
               rep(3, each = 6),
               rep(4, each = 5)
    ), 
    # score = runif(14,1, 100)
    stringsAsFactors = FALSE)
  ##########################################
  
  output$ahpTree <- widgetHierarchy::renderWidgetHierarchy({
    require(widgetHierarchy)
    if ( is.null(reactValues$treeData) ) { 
      return(NULL) 
    }
    widgetHierarchy(reactValues$treeData, boxHeight = 60, boxWidth = 130, tx = 700, ty = 0, angle = 0, scale = 0.75)
  })

  output$exampleSheetTree <- rhandsontable::renderRHandsontable({
    require(rhandsontable)
    if ( is.null(reactValues$treeData) ) { 
      return(NULL) 
    }
    rhandsontable(reactValues$treeData, useTypes = TRUE, readOnly = TRUE) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = FALSE, search = TRUE)

  })
  
  output$treeDataBox <- renderUI({
    inFile <- input$fileTree
    
    # input$fileTree will be NULL initially.
    if ( is.null(inFile) ) {
      return(NULL)
    } else { #if ( is.null( treeData ) ) {
      reactValues$treeData <<- read.csv( inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
      ## Script
      treeMenuScript <<-  paste0("generated_TreeManagement",".R")
      if (!file.exists(treeMenuScript)) {
        
      }
    }
  })
  
  
}