
#[INSTRUCTIONS]
# Before using this module, please make sure to execute the
# buildSrcContent.R or the readingPathway.R file to initialize the necessary variables:
# allNodeNames and pathwaysList

#  Make sure to include the shinyjs package and enable it. 
#  Refer line 8 and 96 in ui.R


#------[NavBar Tab: Analysis by pathway (UI code)]-----------------------------

prefix <- "xsq"
analysisByPathwayInput <- function(id,dataSourceChoices) {
  ns <- NS(id)
  dataSourcesToRemove <- c("almanac","mdaMills","ctrp")
  dataSourceChoices <- dataSourceChoices[!(dataSourceChoices %in% dataSourcesToRemove)]
  tabPanel("Analysis by Pathway",
           fluidPage(
             tags$head(tags$style("#cyjShiny{height:95vh !important;}")),
             titlePanel(title = "Analysis by Pathway"),
             sidebarLayout(
               sidebarPanel(
                 selectInput(ns("cellLineSet"), "Cell line Set",
                             choices = dataSourceChoices),
                
                 radioButtons(ns("selectPathwaySource"),"Select Pathway Source",
                              c("Use Built-in Pathway List","Upload Pathway")),
                 conditionalPanel(
                   condition = paste0("input['", ns("selectPathwaySource"),
                                      "'] =='Use Built-in Pathway List'"),
                   selectInput(ns("selectGene"), "Select Gene: ",
                               choices = NULL)
                 ),
                 uiOutput(ns("fileInputUI")),
                 shinyjs::hidden(selectInput(
                   ns("selectPathway"), "Select Pathway", choices = NULL
                 )),
                 shinyjs::hidden(selectInput(
                   ns("options"),
                   "Select Cell Line or Tissue",
                   c("Tissue","Cell line")
                 )),

                 conditionalPanel(
                   condition = paste0("input['", ns("options"),
                                      "'] =='Cell line'"),
                   selectizeInput(ns("selectCellLine"), "Select Cell Line",
                                  choices = NULL),
                 ),
                 conditionalPanel(
                   condition = paste0("input['", ns("options"), "']=='Tissue'"),
                   selectizeInput(ns("selectTissue"), "Select Tissue",
                                  choices = NULL),
                 ),
                 shinyjs::hidden(uiOutput(ns("rangeSliderUI"))),
                 shinyjs::hidden(plotOutput(
                   ns("colorPlot"), height = "50px", width = "auto"
                 )),

                 selectInput(ns("selectNode"), "Select Node by ID:"
                             , choices = NULL),
                 actionButton(ns("fit"), "Fit Graph"),
                 actionButton(ns("fitSelected"), "Fit Selected"),
                 DT::dataTableOutput(ns("nodeDatatable")),
                 width = 3,
               ),
               mainPanel(
                 textOutput(ns("gseaText")),  
                 cyjShinyOutput(ns("cyjShiny"), height = 800), 
                 imageOutput(ns("notationLegend"))
                 )
             ) # sidebarLayout
           )) #end tabPane
}

#-----[NavBar Tab:Analysis by pathway (Server code)]---------------------------


pathwayAnalysis <- function(id, srcContentReactive) {
    moduleServer(id,function(input, output, session){
    reactiveData <- reactiveVal()
    reactiveAverage <- reactiveVal()
    reactiveDfList <- reactiveVal(pathwaysList)
    reactiveTissueToSampleMap <- reactiveVal()
    #reactiveMean <- reactiveVal()

    displayGraph <- function(averageValues, minVal, maxVal) {
      #cat("Display Graph Entered\n")
      pathwaysList <- reactiveDfList()
      
      forNodes <- pathwaysList[[input$selectPathway]][[1]]
      forEdges <- pathwaysList[[input$selectPathway]][[2]]
      
      if (minVal == 0)
        minVal <- -0.0001
      if (maxVal == 0)
        maxVal <- 0.0001

      rescaledAverage <- ifelse(
        is.na(averageValues),
        NA,
        ifelse(
          averageValues >= 0,
          averageValues * 10 / maxVal,
          averageValues * -10 / minVal
        )
      )

      tblNodes <- data.frame(
        id = forNodes[["NodeID"]],
        name = forNodes[["NodeName"]],
        x = forNodes[["PosX"]],
        y = forNodes[["PosY"]],
        avgValues = rescaledAverage,
        parent = forNodes[["ParentId"]],
        nodeType = forNodes[["NodeType"]],
        stringsAsFactors = FALSE
      )
      tblEdges <- data.frame(
        source = forEdges[["Source"]],
        target = forEdges[["Target"]],
        interaction = forEdges[["EdgeType"]],
        stringsAsFactors = FALSE
      )

      graphJson <- dataFramesToJSON(tblEdges, tblNodes)
      #write(graphJson,file="GraphData.json")
      output$cyjShiny <- renderCyjShiny({
        cyjShiny(graph = graphJson,
                 layoutName = "preset",
                 styleFile = "basicStyle.js")
      })

      displayColorPlot(minVal, maxVal)
      
      output$notationLegend <- renderImage({
        list(src="www/files/pathway_network2.png",
             alt="Network Notation")
      },deleteFile=FALSE)
      
      #-----------------------[FGSEA Start]---------------------------
      #namedMean <- reactiveMean()
      
      # examplePathways <- list()
      # examplePathways[[input$selectPathway]] <- namesOfNodes
      # fgseaRes <- fgsea(pathways = examplePathways[1],
      #                   stats    = averageValues,
      #                   minSize  = 3,
      #                   maxSize  = 500,
      #                   nperm = 1000)
      # print(fgseaRes)
  
      # output$gseaText <-renderText({
      #   paste0("p-Value: ",round(fgseaRes$padj,5))
      # })
      
      #-----------------[FGSEA End]---------------------------------------
      
      
      #cat("Display Graph Out\n")
    }

    displayTable <- function(selectedCells) {
      
      data <- reactiveData()
      cellLineData <- data[, selectedCells, drop = FALSE]

      pathwaysList <- reactiveDfList()
      namesOfNodes <-
        pathwaysList[[input$selectPathway]][[1]][["NodeName"]]
      names <- as.list(row.names(data))
      tableValuesAverages <- NULL
      tableValuesMedians <- NULL

      for (nodeName in namesOfNodes) {
        # Construct the name of the gene in the "xsq" data frame
        nameInData <- paste0(prefix, nodeName)
        if (nameInData %in% names) {
          # Calculate the average value for the gene
          cellVal <-
            mean(as.numeric(cellLineData[nameInData, ]), na.rm = TRUE)
          cellMed <-
            median(as.numeric(cellLineData[nameInData, ], na.rm = TRUE))

          cellVal <- round(cellVal, 3)
          cellMed <- round(cellMed, 3)

          tableValuesAverages <- c(tableValuesAverages, cellVal)
          tableValuesMedians <- c(tableValuesMedians, cellMed)
        } else {
          tableValuesAverages <- c(tableValuesAverages, NA)
          tableValuesMedians <- c(tableValuesMedians, NA)
        }
      }
      
      reactiveAverage(tableValuesAverages)
      maxVal <- max(c(tableValuesAverages, 0.0001), na.rm = TRUE)
      minVal <- min(c(tableValuesAverages, -0.0001), na.rm = TRUE)

      if (length(selectedCells) > 1) {
        tableValuesDataFrame <-
          data.frame(Name = namesOfNodes,
                     Average = tableValuesAverages,
                     Median = tableValuesMedians)
      } else {
        tableValuesDataFrame <-
          data.frame(Name = namesOfNodes, Value = tableValuesAverages)
      }

      tableValuesDataFrame <-
        tableValuesDataFrame[!is.na(tableValuesDataFrame$Name) &
                               tableValuesDataFrame$Name != "", ]
      # Display the data table using DT package
      output$nodeDatatable <- renderDataTable({
        datatable(tableValuesDataFrame)
      })
      #cat("Display table Out\n")
      return(tableValuesAverages)
    }

    displayColorPlot <- function(minVal, maxVal) {
      shinyjs::showElement("colorPlot")
      output$colorPlot <- renderPlot({
        par(mar = c(0, 0, 0, 0)) # Set all margins to 0
        plot(
          NULL,
          xaxt = "n",
          yaxt = "n",
          bty = "n",
          ylab = "",
          xlab = "",
          xlim = 0:1,
          ylim = 0:1
        )
        legend(
          "center",
          legend = c(minVal, "0.0", maxVal),
          pt.cex = 3,
          cex = 1.5,
          bty = "n",
          fill = c("blue", "white", "red"),
          horiz = TRUE
        )
      })
    }
    
    displayRangeSlider <- function(averages, minVal, maxVal) {
      shinyjs::showElement("rangeSliderUI")
      output$rangeSliderUI <- renderUI({
        sliderInput(
          session$ns("rangeSlider"),
          "Value Range",
          min = minVal,
          max = maxVal,
          value = c(minVal, maxVal)
        )
      })
    }
    
    observeEvent(input$cellLineSet, {
     # cat("Entering Select Cell line set\n")
      ns <- session$ns
      srcContent <- srcContentReactive()
      selectedCellLineSet <- input$cellLineSet
      data <-
        srcContent[[selectedCellLineSet]][["molPharmData"]][[prefix]]
      
      # eachRowMeans <- rowMeans(data)
      # rowNames <- sub(paste0("^",prefix),"",row.names(data))
      # namedMean <- setNames(eachRowMeans,rowNames)
      # reactiveMean(namedMean)
      
      reactiveData(data)
      tissueToSampleMap <-
        srcContent[[selectedCellLineSet]][["tissueToSamplesMap"]]
      reactiveTissueToSampleMap(tissueToSampleMap)
      
      tissueNames <- names(tissueToSampleMap)
      cellLines <- as.list(colnames(data))
      
      updateSelectizeInput(session, "selectCellLine",
                           choices =cellLines)
      updateSelectizeInput(session, "selectTissue",
                           choices = tissueNames)
      #cat("Out Select Cell line set\n")
      })
    
    observeEvent(input$selectPathwaySource,{
      srcContent <- srcContentReactive()
      selectedCellLineSet <- input$cellLineSet
      if(input$selectPathwaySource == "Use Built-in Pathway List"){
        data <-
          srcContent[[selectedCellLineSet]][["molPharmData"]][[prefix]]
        genes <- rownames(data)
        nodeNames <- intersect(sub(paste0("^",prefix), "", genes), allNodeNames)
        updateSelectizeInput(session,"selectGene",choices = nodeNames)
      }
    })

    observeEvent(input$selectGene, {
      #cat("Entering Select Gene set\n")
      ns <- session$ns
      if (input$selectGene != "") {
        shinyjs::showElement("selectPathway")
        gene <- input$selectGene
        pathwaysList <- reactiveDfList()
        extrapathwayNames <-
          names(pathwaysList)[grep(input$selectGene, names(pathwaysList))]
        pathwayChoices <- extrapathwayNames

        for (pathway_name in names(pathwaysList)) {
          pathway <- pathwaysList[[pathway_name]][[1]]
          genes <- pathway[["NodeName"]]
          if (gene %in% genes) {
            pathwayChoices <- c(pathwayChoices, pathway_name)
          }
        }
        pathwayChoices <- unique(pathwayChoices)
        updateSelectInput(session, "selectPathway", choices = pathwayChoices)
        updateSelectInput(session, "selectGene", selected = input$selectGene)
        #cat("Out Select Gene set\n")
      }
    })

    observeEvent(input$selectPathway, {
      #cat("Inside Select Pathway\n")
      if (input$selectPathway != "") {
        shinyjs::showElement("options")
        pathwaysList <- reactiveDfList()
        namesOfNodes <- pathwaysList[[input$selectPathway]][[1]][["NodeName"]]
        updateSelectInput(session, "selectPathway",
                          selected = input$selectPathway)
        updateSelectInput(session, "selectNode",
                          choices = c("", namesOfNodes))
        
        if(!is.null(input$selectCellLine) && input$selectCellLine!="")
        {
          cellLine <- input$selectCellLine
          updateSelectizeInput(session,"selectCellLine",selected="")
          updateSelectizeInput(session,"selectCellLine",selected=cellLine)
        }
        if(!is.null(input$selectTissue) && input$selectTissue!="")
        {
          tissue <- input$selectTissue
          updateSelectizeInput(session,"selectTissue",selected="")
          updateSelectizeInput(session,"selectTissue",selected=tissue)
        }
        
      }
      #cat("Going out of Select Pathway\n")
    })
    
    observeEvent(input$options,{
      if(input$options == "Cell line"){
        if(!is.null(input$selectCellLine) && input$selectCellLine!="")
        {
          cellLine <- input$selectCellLine
          updateSelectizeInput(session,"selectCellLine",selected="")
          updateSelectizeInput(session,"selectCellLine",selected=cellLine)
        }
      }
      else{
        if(!is.null(input$selectTissue) && input$selectTissue!="")
        {
          tissue <- input$selectTissue
          updateSelectizeInput(session,"selectTissue",selected="")
          updateSelectizeInput(session,"selectTissue",selected=tissue)
        }
      }
    })
    
    observeEvent(input$selectCellLine,ignoreInit = TRUE,ignoreNULL = TRUE, {
      #cat("Inside of Select Cell line\n")
      if (input$selectPathway != "" && !is.null(input$selectPathway) &&
          input$selectCellLine != "") {
        selectedCellLine <- input$selectCellLine
        averages <- displayTable(selectedCellLine)
        maxVal <- max(c(averages, 0.0001), na.rm = TRUE)
        minVal <- min(c(averages, -0.0001), na.rm = TRUE)
        # this will indirectly also call the display graph 
        # when the range slider is set
        displayRangeSlider(averages, minVal, maxVal)
      }
      #cat("Going out of Select Cell line\n")
    })

    observeEvent(input$selectTissue,ignoreInit = TRUE,ignoreNULL = TRUE, {
      #cat("Inside of Select Tissue\n")
      if (input$selectTissue != "") {
        selectedTissue <- input$selectTissue
        tissueToSampleMap <- reactiveTissueToSampleMap()
        selectedCellLines <- tissueToSampleMap[selectedTissue]

        selectedCellLines <-
          unlist(selectedCellLines, use.names = FALSE)

        averages <- displayTable(selectedCellLines)
        maxVal <- max(c(averages, 0.0001), na.rm = TRUE)
        minVal <- min(c(averages, -0.0001), na.rm = TRUE)
        # this will indirectly also call the display graph 
        # when the range slider is set
        displayRangeSlider(averages, minVal, maxVal) 
      }
      #cat("Going out of Select Tissue\n")
    })

    observeEvent(input$rangeSlider,ignoreInit = TRUE,{
      #cat("Inside range Slider\n")
      sliderValues <- input$rangeSlider
      #setting condition that first slider cannot go beyond zero
      #and second cannot come below zero
      if (sliderValues[1] > 0) {
        updateSliderInput(session, "rangeSlider",
                          value = c(-0.001, sliderValues[2]))
      }
      if (sliderValues[2] < 0) {
        updateSliderInput(session, "rangeSlider",
                          value = c(sliderValues[1], 0.001))
      }
      averageValues <- reactiveAverage()
      #cat("range slider calling display graph \n")
      displayGraph(averageValues, sliderValues[1], sliderValues[2])
      #cat("going out of range slider\n")
    })

    output$fileInputUI <- renderUI({
      ns <- session$ns
      if (input$selectPathwaySource == "Upload Pathway") {
        fileInput(ns("file"), "Upload PathwayMapper.com File")
      } else {
        NULL
      }
    })
    # Event handler for file upload
    observeEvent(input$file, {
      ns <- session$ns
      # Check if a file is uploaded
      if (!is.null(input$file)) {
        # Read the file

        data <- readLines(input$file$datapath)
        # Initialize empty vectors for nodes and edges
        nodeLines <- NULL
        edgeLines <- NULL
        nameOfPathway <- NULL
        # Find the lines containing node and edge information
        if (length(data) > 0) {
          nodeLineStart <- NA
          edgeLineStart <- NA
          nameOfPathway <- data[1]
          for (index in seq_along(data)) {
            line <- data[index]
            if (grepl("--NODE_NAME", line)) {
              nodeLineStart <- index
            } else if (grepl("--EDGE_ID", line)) {
              edgeLineStart <- index
              break
            }
          }
          if (!is.na(nodeLineStart) && !is.na(edgeLineStart)) {
            nodeLines <- data[(nodeLineStart + 1):(edgeLineStart - 1)]
            edgeLines <- data[(edgeLineStart + 1):length(data)]

            nodeData <- paste(nodeLines, collapse = "\t")
            edgeData <- paste(edgeLines, collapse = "\t")
            nodefields <- strsplit(nodeData, "\t")
            edgesfields <- strsplit(edgeData, "\t")
            nodeDf <-
              data.frame(matrix(
                unlist(nodefields),
                ncol = 8,
                byrow = TRUE
              ))
            edgeDf <-
              data.frame(matrix(
                unlist(edgesfields),
                ncol = 8,
                byrow = TRUE
              ))

            colnames(nodeDf) <-
              c(
                "NodeName",
                "NodeID",
                "NodeType",
                "ParentId",
                "PosX",
                "PosY",
                "Width",
                "Height"
              )
            # Convert PosX and PosY columns to numeric data type
            nodeDf$PosX <- as.double(nodeDf$PosX)
            nodeDf$PosY <- as.double(nodeDf$PosY)
            colnames(edgeDf) <-
              c(
                "EdgeID",
                "Source",
                "Target",
                "EdgeType",
                "Interaction",
                "EdgeName",
                "Bend",
                "Curve"
              )
            pathwaysList <- reactiveDfList()
            pathwaysList[[nameOfPathway]] <-
              list(nodeDf, edgeDf)
            reactiveDfList(pathwaysList)
            shinyjs::showElement("selectPathway")
            updateSelectInput(
              session,
              "selectPathway",
              choices = nameOfPathway,
              selected = nameOfPathway
            )

            shinyjs::showElement("options")

          }
        }
      }
    })
      
    observeEvent(input$selectNode,  ignoreInit = TRUE, {
      forNodes <- pathwaysList[[input$selectPathway]][[1]]
      selectedNodeID <-
        forNodes$NodeID[forNodes$NodeName == input$selectNode]
      selectNodes(session, selectedNodeID)
    })
    
    observeEvent(input$fitSelected,  ignoreInit = TRUE, {
      fitSelected(session, 100)
    })
    
    observeEvent(input$fit, ignoreInit = TRUE, {
      fit(session, 80)
    })
    })
}
