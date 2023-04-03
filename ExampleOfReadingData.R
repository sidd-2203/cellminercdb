library(cyjShiny)
library(later)
library(DT)

allData <- srcContent[["nci60"]][["molPharmData"]][["expA"]][["Gene name"]]


#tp53_row <- data["expTP53", ]
#avg <- mean(as.numeric(tp53_row))



ui = shinyUI(fluidPage(

  tags$head(tags$style("#cyjShiny{height:95vh !important;}")),
  titlePanel(title="Example"),
  sidebarLayout(
    sidebarPanel(
      
      selectInput("doGene","Select Gene: ",choices=allData),
      selectInput("doPathway","Select Pathway: ",choices =c("")),
      
      
      selectInput("doLayout", "Select Layout:",
                  choices=c("","cose","cola","circle","concentric","breadthfirst","grid","random","fcose","springy")),

      selectInput("selectName", "Select Node by ID:", choices = c("", sort(tbl.nodes$id))),
      actionButton("sfn", "Select First Neighbor"),
      actionButton("fit", "Fit Graph"),
      actionButton("fitSelected", "Fit Selected"),
      
      actionButton("getSelectedNodes", "Get Selected Nodes"), HTML("<br><br>"),
      htmlOutput("selectedNodesDisplay"),
      
      dataTableOutput("table",),
      
      width=3
    ),
    mainPanel(cyjShinyOutput('cyjShiny', height=400),width=9)
  ) # sidebarLayout
))
#----------------------------------------------------------------------------------------------------
server = function(input, output, session)
{
  
  
  observeEvent(input$fit, ignoreInit=TRUE, {
    fit(session, 80)
  })

  observeEvent(input$showCondition, ignoreInit=TRUE, {
    condition.name <- isolate(input$showCondition)
 
    values <- as.numeric(tbl.lfc[condition.name,])
    node.names <- colnames(tbl.lfc)
   
    setNodeAttributes(session, attributeName="lfc", nodes=node.names, values)
    values <- as.numeric(tbl.count[condition.name,])
    node.names <- colnames(tbl.count)

    setNodeAttributes(session, attributeName="count", nodes=colnames(tbl.count), values)
  })

  observeEvent(input$loadStyleFile,  ignoreInit=FALSE, {
    if(input$loadStyleFile != ""){
      tryCatch({
        loadStyleFile(input$loadStyleFile)
      }, error=function(e) {
        msg <- sprintf("ERROR in stylesheet file '%s': %s", input$loadStyleFile, e$message)
        showNotification(msg, duration=NULL, type="error")
      })
      #later(function() {updateSelectInput(session, "loadStyleFile", selected=character(0))}, 0.5)
    }
  })

  observeEvent(input$doLayout,  ignoreInit=TRUE,{
    if(input$doLayout != ""){
      strategy <- input$doLayout
      doLayout(session, strategy)
      later(function() {updateSelectInput(session, "doLayout", selected=character(0))}, 1)
    }
  })
  observeEvent(input$doGene,  ignoreInit=TRUE,{
    if(input$doGene != ""){
      gene<- input$doGene
      pathwayChoices <- names(df_list)[grep(input$doGene, names(df_list))]
      updateSelectInput(session, "doPathway",choices = pathwayChoices)
      if(length(pathwayChoices)==0){
        updateSelectInput(session,"doPathway",choices="No Pathway exists")
      }
      updateSelectInput(session,"doGene",selected = input$doGene)
    }
  })

  observeEvent(input$doPathway,ignoreInit = TRUE,{
    if(input$doPathway!=""){
      forNodes<- df_list[[input$doPathway]][[1]]
      forEdges<- df_list[[input$doPathway]][[2]]
      namesOfNodes <- forNodes[["NodeName"]]
      tbl.nodes <- data.frame(id=forNodes[["NodeName"]],
                              x=forNodes[["PosX"]],
                              y=forNodes[["PosY"]],
                              stringsAsFactors=FALSE)
      
      tbl.edges <- data.frame(source=forEdges[["Source"]],
                              target=forEdges[["Target"]],
                              interaction=forEdges[["EdgeType"]],
                              stringsAsFactors=FALSE)
      
      cyjShiny::removeGraph(session)
      cyjShiny::addGraphFromDataFrame(session,tbl.edges,tbl.nodes)
      updateSelectInput(session,"doPathway",selected = input$doPathway)
      
      
      data<-srcContent[["nci60"]][["molPharmData"]][["exp"]]
      
      # updating the table also
      averages <- c()
      # Loop through each gene name in namesOfNodes
      for(nodeName in namesOfNodes){
        # Construct the name of the gene in the "exp" data frame
        nameIndata <- paste("exp", nodeName, sep = "")
        if(nameIndata %in% rownames(data)){
          # Calculate the average value for the gene
          avg <- mean(as.numeric(data[nameIndata,]))
          averages <- c(averages, avg)
        } else {
          averages <- c(averages, "NA")   
        }
      }
      # Create a data frame with two columns, one for gene name and the other for average value
      dataFrame <- data.frame(Name = namesOfNodes, Average_Value = averages)
      # Display the data table using DT package
      output$table <- renderDataTable({
        datatable(dataFrame)
      })
      
    }
  })
  
  
  observeEvent(input$selectName,  ignoreInit=TRUE,{
    selectNodes(session, input$selectName)
  })

  observeEvent(input$sfn,  ignoreInit=TRUE,{
    selectFirstNeighbors(session)
  })

  observeEvent(input$fitSelected,  ignoreInit=TRUE,{
    fitSelected(session, 100)
  })

  observeEvent(input$getSelectedNodes, ignoreInit=TRUE, {
    output$selectedNodesDisplay <- renderText({" "})
    getSelectedNodes(session)
  })

  #observeEvent(input$showAll,  ignoreInit=TRUE, {
   # output$showAll <- renderText({" "})
   # showAll(session)
  #})

  observeEvent(input$loopConditions, ignoreInit=TRUE, {
    condition.names <- rownames(tbl.lfc)
    for(condition.name in condition.names[-1]){
      #browser()
      lfc.vector <- as.numeric(tbl.lfc[condition.name,])
      node.names <- rownames(tbl.lfc)
      setNodeAttributes(session, attributeName="lfc", nodes=node.names, values=lfc.vector)
      #updateSelectInput(session, "setNodeAttributes", selected=condition.name)
      Sys.sleep(1)
    } # for condition.name
    updateSelectInput(session, "setNodeAttributes", selected="baseline")
  })


  observeEvent(input$selectedNodes, {
    #  communicated here via assignement in cyjShiny.js
    #     Shiny.setInputValue("selectedNodes", value, {priority: "event"});
    newNodes <- input$selectedNodes;
    output$selectedNodesDisplay <- renderText({
      paste(newNodes)
    })
  })

  output$value <- renderPrint({ input$action })
  output$cyjShiny <- renderCyjShiny({
    cyjShiny(graph=graph.json, layoutName="preset")
  })

} # server
#----------------------------------------------------------------------------------------------------
app <- shinyApp(ui = ui, server = server)
