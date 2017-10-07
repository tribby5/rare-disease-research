#Matthew Tribby
#June 2017

#The goal of this script is to examine on a patient specific basis, the breakdown of rare codes
#This is done separately for both ICD9 and ICD10 in the dataset

#load necessary libraries
library(dplyr)
library(stringdist)

#Assumes you are working in the scripts folder
#Loads in mapping files
source("./loadMappingFiles.R")
names(diseases)[1] <- "id"

#To save/load data
setwd("../data")
###NOTE: UNCOMMENT THE NEXT 2 CODE CHUNKS IF YOU DO NOT HAVE rareICD9Patients.RData or rareICD10Patients.RData

###Load in patient file of interest and marks if the code is rare or not
#patientDataToLoad <- "rarePatients.RData"
#load(patientDataToLoad)
#rarecodes <- c(as.character(icd9$icd9Code), as.character(icd10$icd10Code))
#rareData <- rareData %>% mutate(rare_code = Code %in% rarecodes)

###Breaks the patient data into rare ICD9 and rare ICD10 subsets and save them out. 
#rareICD9 <- rareData %>% filter(rare_code == T, Terminology == "ICD9")
#rareICD10 <- rareData %>% filter(rare_code == T, Terminology == "ICD10")
#save("rareICD9", file = "rareICD9Patients.RData")
#save("rareICD10", file = "rareICD10Patients.RData")
#rm(rareData)
#gc() 

#Loads in pre-saved files (for speed sake)
load("rareICD9Patients.RData")
load("rareICD10Patients.RData")

#Total rare codes counts per patient, summarizes on a patient basis and sums up their total
#number of rare codes and total number of codes 
rareICD9Patients <- rareICD9 %>% 
  group_by(PAT_ID) %>%
  summarize(rare_code_count = sum(rare_code), total_count = length(rare_code))

rareICD10Patients <- rareICD10 %>% 
  group_by(PAT_ID) %>%
  summarize(rare_code_count = sum(rare_code), total_count = length(rare_code))

#Make a table of the frequency of patients who have a certain number of rare codes:
#For example, x number of patients have only 1 rare disease code ever in the dataset
numberOfLevels <- 10
library(gdata)
icd9Counts <- as.data.frame(table(rareICD9Patients$rare_code_count))
icd9Counts$Var1 <- as.character(icd9Counts$Var1)
icd9Counts <- rbind(slice(icd9Counts, 1:numberOfLevels), 
      data.frame(Var1 = c(paste(numberOfLevels+1, "+", sep ="")), Freq =c(sum(icd9Counts$Freq[(numberOfLevels+1):nrow(icd9Counts)]))))
icd9Counts <- icd9Counts[rep(seq(nrow(icd9Counts)), icd9Counts$Freq),]

#reordering of the counts, just for the plotting
icd9Counts$Var1 <- reorder(as.factor(icd9Counts$Var1), new.order = c(1,4:11,2,3))

ggplot(icd9Counts, aes(Var1)) + xlab("Number of Rare ICD9 Codes Assigned to Patient") + 
  ylab("Count of Patients") + ggtitle("Breakdown of Number of Rare ICD9 Codes for Rare Patients") +
  geom_bar()

icd10Counts <- as.data.frame(table(rareICD10Patients$rare_code_count))
icd10Counts$Var1 <- as.character(icd10Counts$Var1)
icd10Counts <- rbind(slice(icd10Counts, 1:numberOfLevels), 
      data.frame(Var1 = c(paste(numberOfLevels+1,"+", sep ="")), Freq =c(sum(icd10Counts$Freq[(numberOfLevels+1):nrow(icd10Counts)]))))
icd10Counts <- icd10Counts[rep(seq(nrow(icd10Counts)), icd10Counts$Freq),]
icd10Counts$Var1 <- reorder(as.factor(icd10Counts$Var1), new.order = c(1,4:11,2,3))

ggplot(icd10Counts, aes(Var1)) + xlab("Number of Rare ICD10 Codes Assigned to Patient") + 
  ylab("Count of Patients") + ggtitle("Breakdown of Number of Rare ICD10 Codes for Rare Patients") +
  geom_bar()

#Counts of pairs of patients and their codes and their occurences 
rareICD9PatientPairs <- rareICD9 %>%
  group_by(PAT_ID, Code) %>%
  summarize(rare_code_count = sum(rare_code))

rareICD10PatientPairs <- rareICD10 %>%
  group_by(PAT_ID, Code) %>%
  summarize(rare_code_count = sum(rare_code))
