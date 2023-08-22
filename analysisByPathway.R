
#[INSTRUCTIONS]
# Before using this module, please make sure to execute the
# buildSrcContent.R or the readingPathway.R file to initialize the necessary variables:
# allNodeNames and pathwaysList
  
#------[NavBar Tab: Analysis by pathway (UI code)]-----------------------------

prefix <- "xsq"
analysisByPathwayInput <- function(id,dataSourceChoices) {
  ns <- NS(id)
  tabPanel("Analysis by Pathway",
           fluidPage(
             tags$head(tags$style("#cyjShiny{height:95vh !important;}")),
             titlePanel(title = "Analysis by Pathway"),
             sidebarLayout(
               sidebarPanel(
                 selectInput(ns("cellLineSet"), "Cell line Set",
                             choices = dataSourceChoices),

                 selectInput(
                   ns("selectPathwayType"),
                   "Select Pathway",
                   c("", "Upload Pathway", "Select using Gene")
                 ),
                 conditionalPanel(
                   condition = paste0("input['", ns("selectPathwayType"),
                                      "'] =='Select using Gene'"),
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
                   c("", "Cell line", "Tissue")
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
                 #tags$head(tags$style(HTML(".mainPanelCyjShiny {border: 1px solid #000000;}"))),
                 cyjShinyOutput(ns("cyjShiny"), height = 800), 
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

    displayGraph <- function(averageValues, minVal, maxVal) {
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

      output$cyjShiny <- renderCyjShiny({
        cyjShiny(graph = graphJson,
                 layoutName = "preset",
                 styleFile = "basicStyle.js")
      })

      displayColorPlot(minVal, maxVal)
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

      if (length(tableValuesAverages) > 1) {
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

    observeEvent(input$fit, ignoreInit = TRUE, {
      fit(session, 80)
    })
    observeEvent(input$selectPathwayType,ignoreInit = TRUE,{
      srcContent <- srcContentReactive()
      selectedCellLineSet <- input$cellLineSet
      if(input$selectPathwayType == "Select using Gene"){
        data <-
          srcContent[[selectedCellLineSet]][["molPharmData"]][[prefix]]
        genes <- rownames(data)
        nodeNames <- intersect(sub(paste0("^",prefix), "", genes), allNodeNames)
        updateSelectizeInput(session,"selectGene",choices = nodeNames)
      }
    })

    output$fileInputUI <- renderUI({
      ns <- session$ns
      if (input$selectPathwayType == "Upload Pathway") {
        fileInput(ns("file"), "Choose a file")
      } else {
        NULL
      }
    })

    observeEvent(input$cellLineSet, {
      ns <- session$ns
      srcContent <- srcContentReactive()
      selectedCellLineSet <- input$cellLineSet
      data <-
        srcContent[[selectedCellLineSet]][["molPharmData"]][[prefix]]

      reactiveData(data)
      tissueToSampleMap <-
        srcContent[[selectedCellLineSet]][["tissueToSamplesMap"]]
      reactiveTissueToSampleMap(tissueToSampleMap)

      tissueNames <- names(tissueToSampleMap)
      cellLines <- as.list(colnames(data))

      updateSelectizeInput(session, "selectCellLine",
                           choices = c("", cellLines))
      updateSelectizeInput(session, "selectTissue",
                           choices = c("", tissueNames))
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

    observeEvent(input$selectGene,  ignoreInit = TRUE, {
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
        if (length(pathwayChoices) == 0) {
          updateSelectInput(session, "selectPathway",
                            choices = "No Pathway exists")
        }
        updateSelectInput(session, "selectGene", selected = input$selectGene)
      }
    })

    observeEvent(input$selectPathway, ignoreInit = TRUE, {
      if (input$selectPathway != "" &
          input$selectPathway != "No Pathway exists") {
        shinyjs::showElement("options")
        pathwaysList <- reactiveDfList()
        namesOfNodes <- pathwaysList[[input$selectPathway]][[1]][["NodeName"]]
        updateSelectInput(session, "selectPathway",
                          selected = input$selectPathway)
        updateSelectInput(session, "selectNode",
                          choices = c("", namesOfNodes))
      }
    })

    observeEvent(input$selectCellLine, ignoreInit = TRUE, {
      if (input$selectCellLine != "") {
        selectedCellLine <- input$selectCellLine
        if (input$selectPathway != "No Pathway exists") {
          averages <- displayTable(selectedCellLine)
          maxVal <- max(c(averages, 0.0), na.rm = TRUE)
          minVal <- min(c(averages, 0.0), na.rm = TRUE)
          displayGraph(averages, maxVal, minVal)
        }
      }
    })

    observeEvent(input$selectTissue, ignoreInit = TRUE, {
      if (input$selectTissue != "") {
        selectedTissue <- input$selectTissue
        tissueToSampleMap <- reactiveTissueToSampleMap()
        selectedCellLines <- tissueToSampleMap[selectedTissue]

        selectedCellLines <-
          unlist(selectedCellLines, use.names = FALSE)

        averages <- displayTable(selectedCellLines)
        reactiveAverage(averages)
        maxVal <- max(c(averages, 0.0), na.rm = TRUE)
        minVal <- min(c(averages, 0.0), na.rm = TRUE)
        displayGraph(averages, minVal, maxVal)
      }
    })

    observeEvent(input$rangeSlider, {
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
      displayGraph(averageValues, sliderValues[1], sliderValues[2])
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
        # Find the lines containing node and edge information
        if (length(data) > 0) {
          nodeLineStart <- NA
          edgeLineStart <- NA

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

            nodeDfPathway <-
              data.frame(matrix(
                unlist(nodefields),
                ncol = 8,
                byrow = TRUE
              ))
            edgeDfPathway <-
              data.frame(matrix(
                unlist(edgesfields),
                ncol = 8,
                byrow = TRUE
              ))

            colnames(nodeDfPathway) <-
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
            nodeDfPathway$PosX <- as.double(nodeDfPathway$PosX)
            nodeDfPathway$PosY <- as.double(nodeDfPathway$PosY)
            colnames(edgeDfPathway) <-
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
            fileName <- input$file$name
            pathwaysList[[fileName]] <-
              list(nodeDfPathway, edgeDfPathway)
            reactiveDfList(pathwaysList)
            shinyjs::showElement("selectPathway")
            updateSelectInput(
              session,
              "selectPathway",
              choices = c(fileName),
              selected = fileName
            )
            shinyjs::showElement("options")

          }
        }
      }
    })
    })
}
