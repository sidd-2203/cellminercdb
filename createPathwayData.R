library(jsonlite)
outputFile <- "pathway_as_json.json"
allFiles <- list.files("pathway_mapper_networks", pattern = ".txt", full.names = TRUE)

# Initialize an empty character vector to store data from all files
allData <- character()

for (inputFile in allFiles) {
  t1 <- readLines(inputFile)
  # Concatenate the lines from each file
  t2 <- list(t1)
  names(t2) <- t1[1]
  allData <- c(allData, t2)
}
allData <- toJSON(allData)

# Write the concatenated data to the output file
writeLines(allData, outputFile)




