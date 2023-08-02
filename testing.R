myList<-  readRDS("df_list.rds")
#data<-srcContent[["nci60"]][["molPharmData"]][["exp"]] 


#tissueDataColNames<- molData@sampleData@samples[["Name"]]
#tissuesMapping<- molData@sampleData@samples[["OncoTree1"]]

#nameTissueMap<- split(tissueDataColNames,tissuesMapping)
#names(nameTissueMap)



names <- as.list(row.names(data))

averages <- c()
medians <- c()
genesCalculated <- c()
allDataDataFrame <-data.frame()

result_list <- list()

for(pathways in myList){
  genes<- pathways[[1]][["NodeName"]]
    
  for(nodeName in genes){
    #nameInData <- paste0("exp", nodeName)
    nameInData<-nodeName
    if (nameInData %in% names & !(nodeName %in% genesCalculated)) {
      geneData <- data[nameInData,]
      #print(geneData)
      
     
      result_list[[nodeName]] <- geneData
      
      # Calculate the average value for the gene  
      
      #avg <- mean(as.double(data[nameInData,]),na.rm=TRUE)
      #med <-median(as.double(data[nameInData,]),na.rm=TRUE)
      
      #avg_rounded <- round(avg, 5)  # Round the average value to 3 decimal places
      #med_rounded <-round(med,5)
      #averages <- c(averages, avg_rounded)
      #medians <- c(medians,med_rounded)
      
      genesCalculated <- c(genesCalculated,nodeName)
    }
  }
}

# Combine all dataframes in the list into a single dataframe
allDataDataFrame <- do.call(rbind, result_list)

#allDataDataFrame <- data.frame(Name=genesCalculated,Averages=averages,Medians=medians)
write.csv(allDataDataFrame, "CCLE-AllGenesData.csv", row.names = TRUE)

