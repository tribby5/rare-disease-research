#Matthew Tribby
#July 2017

#The goal of this script is to utilize the visNetwork R package to create a network visualization
#of a SNOMED group. Currently the SNOMED concept ID is hard-coded, but could create it into
#a function to visualize a grouping. 

#load libraries
library(dplyr)
library(visNetwork)
library(igraph)


#### load data ####, assumes you are in scripts folder

load("../data/unique_snomed_relationships.RData")
load("../data/unique_snomed_concepts.RData")
source("./loadMappingFiles.R")
load("../data/icd10SnomedMapping.RData")
#creates a simplistic map between icd10 and SNOMED CT
icd10Snomed <- inner_join(snomed, icd10, by = "id")


#Function which finds the number of occurrences in the patient dataset of a certain ICD10
#code by taking in a snomed Code. the translation from SNOMED to ICD10 is approximate,
#often offering a more general, approximate SNOMED code rather than a specific one,
#Look at the mappingToSnomed.R file for more info on this mapping or the data.frame
#icd10SnomedMapping.RData to see the finished mapping
findNumOccur <- function(snomed){
  #Need the icd10SnomedMapping and a patients dataset
  if(snomed %in% icd10Snomed$snomedCode){
    return(nrow(patients %>% 
                  filter(Code == (icd10Snomed %>% 
                                    filter(snomedCode == snomed) %>% 
                                    pull(icd10Code)))))
  }
  return(0)
}

#test file for patients
load("rarePatientsSubset2k.RData")
patients <- rarePatientsSubset
rm(rarePatientsSubset)

# color rare diseases red
conceptData$color <- "blue"
conceptData$color[conceptData$id %in% icd10Snomed$snomedCode] <- "red"

# edges columns taken: source, destination, type, color
possibleEdges <- dplyr::select(relationDataShort, c(5,6,11,12))
names(possibleEdges) <- c("from", "to", "title", "color")

# vertices columns taken: id, title, color
vertices <- dplyr::select(conceptData, c(1,7,8))

#Utilizes findSnomedChildren method (code for that in findSnomedChildren.R)
verticesGraph <- c("17901006", findSnomedChildren("17901006", 2))
edgesGraph <- relations %>% filter(sourceId %in% verticesGraph, destinationId %in% verticesGraph)
verticesGraph <- data.frame(id = as.numeric(verticesGraph))%>%
                    left_join(vertices, by = "id") %>%
                    mutate(size = unlist(lapply(id, function(x) {return(10 + findNumOccur(x))})))
                       #mutate(size = lapply(id, function(x){return(10 + nrow(edgesGraph[edgesGraph$destinationId == x,]))}))
#verticesGraph <- verticesGraph[veritcesGraph$id %in% icd10Snomed$snomedCode,]
#  mutate(title = paste(title, as.character(size - 10)))


  
#new names needed in order to input into visNetwork
edgesGraph <- edgesGraph %>% rename(from = sourceId, to = destinationId)

    
# build the network visualization
visNetwork(verticesGraph, edgesGraph) %>%
  visEdges(arrows = list(to = TRUE)) %>%
  # improve performance (speed it up)
  visPhysics(stabilization = FALSE) %>%
  visEdges(smooth = FALSE) %>%
  visIgraphLayout()
    