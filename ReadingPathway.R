

library(jsonlite)
# Read the JSON file
pathways <- jsonlite::fromJSON("pathways.json")
pathwaysList <- list()

# Initialize a vector to store all node names
allNodeNames <- NULL

#for demo pathways
demoPathways <- c("Cell Cycle", "HIPPO", "MYC", "NOTCH", "WNT", "TP53",
                  "NRF2", "PI3K", "RTK-RAS", "TGF-Beta")
# Loop through each pathway

for (pathway in pathways) {
  # Get the nodes and edges
  if (!(pathway[1] %in% demoPathways)) next

  node1 <- pathway[grep("--NODE_NAME", pathway):grep("--EDGE_ID", pathway) - 1]
  edges <- pathway[grep("--EDGE_ID", pathway):length(pathway)]

  # Split nodes and edges on "\t" delimiter
  nodefields <- strsplit(node1, "\t")
  edgesfields <- strsplit(edges, "\t")
  nodeDFPathway <- data.frame(matrix(unlist(nodefields),
                                            ncol = 6, byrow = TRUE))
  edgeDFPathway <- data.frame(matrix(unlist(edgesfields),
                                            ncol = 4, byrow = TRUE))

  colnames(nodeDFPathway) <- c("NodeName", "NodeID", "NodeType",
                               "ParentId", "PosX", "PosY")

  # Convert PosX and PosY columns to numeric data type
  # this was needed in later part( In Cytoscape.js)
  nodeDFPathway$PosX <- as.double(nodeDFPathway$PosX)
  nodeDFPathway$PosY <- as.double(nodeDFPathway$PosY)

  colnames(edgeDFPathway) <- c("EdgeID", "Source", "Target", "EdgeType")
  # Remove empty rows
  nodeDFPathway <- nodeDFPathway[rowSums(nodeDFPathway == "")
                                 != ncol(nodeDFPathway), ]
  edgeDFPathway <- edgeDFPathway[rowSums(edgeDFPathway == "")
                                 != ncol(edgeDFPathway), ]

  nodeDFPathway <- nodeDFPathway[-1, ]
  edgeDFPathway <- edgeDFPathway[-1, ]

  pathwaysList[[pathway[1]]] <- list(nodeDFPathway, edgeDFPathway)
  allNodeNames <- c(allNodeNames, nodeDFPathway$NodeName)
}
# Remove duplicates from the vector
allNodeNames <- unique(allNodeNames)
