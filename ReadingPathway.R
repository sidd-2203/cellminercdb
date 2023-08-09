

library(jsonlite)
# Read the JSON file
pathways <- jsonlite::fromJSON("pathways.json")

# Initialize data frame for node and edges fields
#nodeDataFrame <- data.frame()
#edgeDataFrame <- data.frame()
pathwaysList <- list()


# Initialize a vector to store all node names
allNodeNames <- c()

#for demo pathways
#demoPathways <-c("Cell Cycle","HIPPO","MYC","NOTCH","WNT","TP53","NRF2","PI3K","RTK-RAS","TGF-Beta") 
# Loop through each pathway

for (pathway in pathways) {
  # Get the nodes and edges
  
  node1 <- pathway[grep("--NODE_NAME", pathway):grep("--EDGE_ID", pathway)-1]
  edges <- pathway[grep("--EDGE_ID", pathway):length(pathway)]
  
  # Split nodes and edges on "\t" delimiter
  nodefields <- strsplit(node1, "\t")
  edgesfields <- strsplit(edges ,"\t")
  
  nodeDataFramePathway <- data.frame(matrix(unlist(nodefields), ncol = 6, byrow=TRUE))
  edgeDataFramePathway <- data.frame(matrix(unlist(edgesfields), ncol = 4, byrow=TRUE))
  
  colnames(nodeDataFramePathway) <- c("NodeName", "NodeID", "NodeType", "ParentId", "PosX", "PosY")
  # Convert PosX and PosY columns to numeric data type:: this was needed in later part( In Cytoscape.js) 
  nodeDataFramePathway$PosX <- as.double(nodeDataFramePathway$PosX)
  nodeDataFramePathway$PosY <- as.double(nodeDataFramePathway$PosY)
  
  
  colnames(edgeDataFramePathway) <- c("EdgeID", "Source", "Target", "EdgeType")
  
  # Remove empty rows
  nodeDataFramePathway <- nodeDataFramePathway[rowSums(nodeDataFramePathway == "") != ncol(nodeDataFramePathway),]
  edgeDataFramePathway <- edgeDataFramePathway[rowSums(edgeDataFramePathway == "") != ncol(edgeDataFramePathway),]
  
  nodeDataFramePathway<- nodeDataFramePathway[-1,]
  edgeDataFramePathway<- edgeDataFramePathway[-1,]
  
  pathwaysList[[pathway[1]]] <- list(nodeDataFramePathway, edgeDataFramePathway)

  allNodeNames <- c(allNodeNames, nodeDataFramePathway$NodeName)
}


# Remove duplicates from the vector
allNodeNames <- unique(allNodeNames)





