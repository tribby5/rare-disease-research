#Matthew Tribby
#June 2017

#The goal of this script is to take in a text file of ICD codes and then readjust the 
#rare patients subset. Reason for this script: ICD codes are not specific enough often meaning
#that common diseases can get combined with rare ones in the same code. So we might want to 
#remove a certain code from the "rare patients subset" in order to get a more approximate, more
#lower bound data set of rare patients

#load libraries
library(stringr)
library(dplyr)

#Assumes that the text files are in the data folder and that current folder is script folder
icd9CodesToRemove <- str_trim(scan("../data/ICD9CodesToRemove.txt", what = character(), sep = ",", quote ="", quiet = T))
icd10CodesToRemove <- str_trim(scan("../data/ICD10CodesToRemove.txt", what = character(), sep = ",", quote = "", quiet = T))

#Loading the subset from which to remove these codes from (default is a small subset)
rarePatientsFile <- "../data/rarePatientsSubset2k.RData"
load(rarePatientsFile)

#Splitting into different terminologies
rarecodes <- c(as.character(icd9$icd9Code), as.character(icd10$icd10Code))
patients <- rarePatientsSubset %>% mutate(rare_code = Code %in% rarecodes)

patientsICD9 <- patients %>% filter(Terminology == "ICD9", rare_code == T, !Code %in% icd9CodesToRemove)
patientsICD10 <- patients %>% filter(Terminology == "ICD10", rare_code == T, !Code %in% icd10CodesToRemove)
#Can combine these two to get back to the original dataset if desired