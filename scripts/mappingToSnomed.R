#Matthew Tribby
#July 2017

#The goal of this script is to attempt to map icd codes to snomed. The reason for this is that
#we believe SNOMED CT is superior in its specificty which is important for rare diseases
#A takeaway from the results of this script is that it is difficult to map to SNOMED from ICD
#because it is the problem of trying to map from something general to something more specific
#without a tremendous amount of background info

#loads in necessary libraries
library(dplyr)
library(stringr)
library(ggplot2)

#Loads in mapping files, runs script to do that, assumes you are in script directory
source("./loadMappingFiles.R")

#Joins the mapping files based on the ORDR id which gives a uniqe identifier for diseases
#By joinin these objects, we essentially create an imperfect map from the terminologies
joinBy <- "id"
icd9combine <- inner_join(icd9, snomed, by = joinBy)
icd10combine <- inner_join(icd10, snomed, by = joinBy)

#Pseudo 1 to 1 Mappings ICD9, pseudo meaning that they are 1 to 1 in the objects but the 
#dataframe only contains rare disease codes. It is very possibly that non-rare diseases
#map to this same icd code, making this not truly 1 to 1. However, this is the best approximation
#of 1 to 1 with the data we have
icd9combineGroupSummary <- icd9combine %>% 
  group_by(icd9Code) %>% 
  summarize("uniquePaths" = length(levels(as.factor(snomedCode))))
icd9WithUniqueEnd <- icd9combineGroupSummary %>% filter(uniquePaths == 1) %>% pull(icd9Code)
icd9PathCounts <- table(icd9combineGroupSummary$uniquePaths)

uniqueICD9 <- distinct(icd9combine %>% filter(icd9Code %in% icd9WithUniqueEnd) %>% select(4,8))
uniqueICD9 <- cbind(uniqueICD9, data.frame("uniquePaths" = rep(1, length(icd9WithUniqueEnd)), "parentLevel" = rep(0, length(icd9WithUniqueEnd))))

rm(icd9WithUniqueEnd, icd9PathCounts)

#Pseudo 1 to 1 Mappings ICD10, see above for explanation of pseudo 1 to 1
icd10combineGroupSummary <- icd10combine %>% 
  group_by(icd10Code) %>% 
  summarize("uniquePaths" = length(levels(as.factor(snomedCode))))
icd10WithUniqueEnd <- icd10combineGroupSummary %>% filter(uniquePaths == 1) %>% pull(icd10Code)
icd10PathCounts <- table(icd10combineGroupSummary$uniquePaths)
uniqueICD10 <- distinct(icd10combine %>% filter(icd10Code %in% icd10WithUniqueEnd) %>% select(4,8))
uniqueICD10 <- cbind(uniqueICD10, data.frame("uniquePaths" = rep(1, length(icd10WithUniqueEnd)), "parentLevel" = rep(0, length(icd10WithUniqueEnd))))

rm(icd10WithUniqueEnd, icd10PathCounts)

#Next Attempt: traverse up tree structure for ones with more than one and find a more generalized
#SNOMED code that is the parent

#Method for doing so:
#Load up the relationData and subset all the "Is a" relationships
#Get all SNOMED codes that map from an ICD code to SNOMED 
#For all those SNOMED codes, look for a common parent node
  #Find all parents for the SNOMED code and look for intersection between all children
  #If intersection exists, then you're done
  #If not, repeat for the parents of the node, with some given numbre of iterations
load("unique_snomed_relationships.RData")

isICD9 <- function(code){
  return(!grepl("[[:alpha:]]", code) | substr(code,1,1) == "V")
}

#Create subset of relation Data that are the Is a relations
relationHierarchy <- relationDataShort %>%  select(c(5,6,11)) %>% filter(type == "Is a")

#Goal of this function is that given snomed codes it will find the snomed code that is the
#first common parent. (If there are multiple parents, it just takes the first one. This may
#not be the best method)
findCommonParent <- function(snomedCodes){
  commonParentNotFound <- T
  levelsDeep <- 1
  parents <- lapply(snomedCodes, as.list)
  
  while(commonParentNotFound & levelsDeep != 10){
    for(i in 1:length(parents)){
      parents[[i]][[levelsDeep + 1]] <- relationHierarchy %>% filter(sourceId %in% parents[[i]][[levelsDeep]]) %>% pull(destinationId)
    }
    levelsDeep <- levelsDeep + 1 
    intersection <- Reduce(intersect, lapply(parents, unlist))
    if(length(intersection) > 0){
      return(paste(intersection, "_", levelsDeep, sep = ""))
    }
  }
  return(c("NA"))
}

#Given an icd9 code, it will return the first common SNOMED CT parent
findICD9SnomedParent <- function(icd9){
  snomedCodes <- unique(icd9combine %>% 
    filter(icd9Code == icd9) %>%
    pull(snomedCode))
  return(findCommonParent(snomedCodes))
}

#Given an icd10 code, it will return the first common SNOMED CT parent
findICD10SnomedParent <- function(icd10){
  snomedCodes <- unique(icd10combine %>% 
    filter(icd10Code == icd10) %>%
    pull(snomedCode))
  return(findCommonParent(snomedCodes))
}

#Codes that utilizes the above functions to get the common snomed parents for nonunique icd9 codes,
#by non-unique, meaning that it doesn't have a pseudo 1 to 1 mapping
nonUniqueICD9 <- icd9combineGroupSummary %>% 
  filter(uniquePaths != 1)
generalSnomedCodeICD9 <- lapply(nonUniqueICD9$icd9Code, function(x) return(findICD9SnomedParent(x)[1]))
nonUniqueICD9 <- cbind(nonUniqueICD9, str_split_fixed(generalSnomedCodeICD9, "_",2))
names(nonUniqueICD9)[3:4] <- c("snomedCode", "parentLevel")
nonUniqueICD9 <- nonUniqueICD9 %>% select(1,3,2,4)

#Combines pseduo 1 to 1 and this new parent technique
icd9SnomedMapping <- rbind(uniqueICD9, nonUniqueICD9)

#Codes that utilizes the above functions to get the common snomed parents for nonunique icd10 codes,
#by non-unique, meaning that it doesn't have a pseudo 1 to 1 mapping
nonUniqueICD10 <- icd10combineGroupSummary %>% filter(uniquePaths != 1)
generalSnomedCodeICD10 <- lapply(nonUniqueICD10$icd10Code, function(x) return(findICD10SnomedParent(x)[1]))
nonUniqueICD10 <- cbind(nonUniqueICD10, str_split_fixed(generalSnomedCodeICD10, "_",2))
names(nonUniqueICD10)[3:4] <- c("snomedCode", "parentLevel")
nonUniqueICD10 <- nonUniqueICD10 %>% select(1,3,2,4)

#Combines pseudo 1 to 1 and this new parent technique
icd10SnomedMapping <- rbind(uniqueICD10, nonUniqueICD10)

#### Test of matching on a subset of the data ###
#load("rarePatientsSubset2k.RData")
#patients <- rarePatientsSubset
###PatientsMatch represents if there is a match in this snomed mapping technique done above
#patientsMatch <-patients %>% 
#  filter(Code %in% icd9$icd9Code | Code %in% icd10$icd10Code) %>%
#  mutate("matchFound" = Code %in% icd9SnomedMapping$icd9Code | Code %in% icd10SnomedMapping$icd10Code) 
###Summarizes the match based on a row instance (row instance just a way of not double counting)
###rowID given to original dataset meaning that all icd codes in a row have the same rowID
#summaryMatch <- patientsMatch %>% group_by(rowID) %>% 
#  summarize("match" = max(matchFound))

#print(sum(summaryMatch$match))
#print(sum(summaryMatch$match)/nrow(summaryMatch))
###NOTE: THE MATCHING DOESNT MEAN IT'S A GOOD MATCH. the high percentage is deceptive as will
###be shown if you look at the actual match

###Looks at the actual mapping for ICD9
#patientsICD9 <- data.frame("icd9Code" = as.character(patientsMatch %>% filter(Code %in% icd9$icd9Code) %>% pull(Code)))
#patientsICD9 <- left_join(patientsICD9, icd9SnomedMapping, by = "icd9Code") 
#patientsICD9Counts <- distinct(as.data.frame(table(patientsICD9$snomedCode)) %>% 
#  select(snomedCode = 1, count = 2) %>%
#  left_join(icd9SnomedMapping, by = "snomedCode") %>% 
#  select(1,2,5) %>% 
#  arrange(desc(count), desc(parentLevel)))
#load("unique_snomed_concepts.RData")
#names(patientsICD9Counts)[1] <- "id"
#patientsICD9Counts$id <- as.numeric(patientsICD9Counts$id)
#patientsICD9Counts <- left_join(patientsICD9Counts, conceptData %>% select(1,7), by = "id")

###Looks at the actual mapping for ICD10: note the unspecific mappings that happen with SNOMED parent
#patientsICD10 <- data.frame("icd10Code" = as.character(patientsMatch %>% filter(Code %in% icd10$icd10Code) %>% pull(Code)))
#patientsICD10 <- left_join(patientsICD10, icd10SnomedMapping, by = "icd10Code") 
#patientsICD10Counts <- distinct(as.data.frame(table(patientsICD10$snomedCode)) %>% 
#                                 select(snomedCode = 1, count = 2) %>%
#                                 left_join(icd10SnomedMapping, by = "snomedCode") %>% 
#                                 select(1,2,5) %>% 
#                                 arrange(desc(count), desc(parentLevel)))
#names(patientsICD10Counts)[1] <- "id"
#patientsICD10Counts$id <- as.numeric(patientsICD10Counts$id)
#patientsICD10Counts <- left_join(patientsICD10Counts, conceptData %>% select(1,7), by = "id")

