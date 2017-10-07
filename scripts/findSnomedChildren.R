#Matthew Tribby
#July 2017

#The goal of this script is to create a function that can find the hierarchical children
#of a SNOMED CT code given the parent code and the number of levels to traverse downwards.
#The original use of this script was in order to visualize SNOMED CT hierarchies, specifically
#for groupings of rare diseases. An example use is in the visualizeSnomedGroups.R code

#load libraries
library(dplyr)

#Assumes you are working in the scripts and that data is available in data folder
setwd("../data")

load("unique_snomed_relationships.RData")
#Gets out only the hierarchial info and only the source and destination columns 
relations <- distinct(relationDataShort %>% filter(type == "Is a") %>% select(5,6))
rm(relationDataShort)
gc()

#expects a character string which tells a SNOMED CT concept ID and the number of levels to 
#get the children from. Uses recursion to achieve its goals
findSnomedChildren <- function(snomed,numLevels){
  children <- relations %>% filter(destinationId == snomed) %>% pull(sourceId)
  if(length(children) > 0 & numLevels != 0){
    children <- c(children, unlist(sapply(children, function(child){findSnomedChildren(child,numLevels - 1)})))
  }
  return(unique(children))
}



