# Used this File to create a pathway list from the json file
# Read this file in global.R

library(jsonlite)
# Read the JSON file
pathways <- jsonlite::fromJSON("pathway_as_json.json")
pathwaysList <- list()

# Initialize a vector to store all node names
allNodeNames <- NULL

pathwayGenesList <- list()

#for demo pathways
#demoPathways <- c("Cell Cycle", "HIPPO", "MYC", "NOTCH", "WNT", "TP53",
 #                 "NRF2", "PI3K", "RTK-RAS", "TGF-Beta")
# Loop through each pathway

for (pathway in pathways) {
  # Get the nodes and edges
  #if (!(pathway[1] %in% demoPathways)) next

  nodes <- pathway[grep("--NODE_NAME", pathway):grep("--EDGE_ID", pathway) - 1]
  edges <- pathway[grep("--EDGE_ID", pathway):length(pathway)]

  # Split nodes and edges on "\t" delimiter
  nodefields <- strsplit(nodes, "\t")
  edgesfields <- strsplit(edges, "\t")
  nodeDFPathway <- data.frame(matrix(unlist(nodefields),
                                            ncol = 8, byrow = TRUE))
  edgeDFPathway <- data.frame(matrix(unlist(edgesfields),
                                            ncol = 8, byrow = TRUE))

  colnames(nodeDFPathway) <- c("NodeName", "NodeID", "NodeType",
                               "ParentId", "PosX", "PosY","Width","Height")

  colnames(edgeDFPathway) <- c("EdgeID", "Source", "Target", "EdgeType",
                               "Interaction", "EdgeName",
                               "EdgeBends", "EdgeCurveStyle")
  # Remove empty rows
  nodeDFPathway <- nodeDFPathway[rowSums(nodeDFPathway == "")
                                 != ncol(nodeDFPathway), ]
  edgeDFPathway <- edgeDFPathway[rowSums(edgeDFPathway == "")
                                 != ncol(edgeDFPathway), ]

  nodeDFPathway <- nodeDFPathway[-1, ]
  edgeDFPathway <- edgeDFPathway[-1, ]

  # Convert PosX and PosY columns to numeric data type
  # this was needed in later part( In Cytoscape.js)
  nodeDFPathway$PosX <- as.double(nodeDFPathway$PosX)
  nodeDFPathway$PosY <- as.double(nodeDFPathway$PosY)

  pathwaysList[[pathway[1]]] <- list(nodeDFPathway, edgeDFPathway)
  
  # Filter nodes by NodeType "GENE" and GENE name has no spaces 
  # And all Uppercase 
  geneNodes <- nodeDFPathway[
    nodeDFPathway$NodeType == "GENE" &
      !grepl(" ", nodeDFPathway$NodeName) &
      toupper(nodeDFPathway$NodeName) == nodeDFPathway$NodeName,
    "NodeName"
  ]
    
  allNodeNames <- c(allNodeNames, geneNodes)
  # Store genes associated with the pathway in the list
  pathwayGenesList[[pathway[1]]] <- geneNodes
  }
# Remove duplicates from the vector
allNodeNames <- unique(allNodeNames)
