#Matthew Tribby
#June 2017

#The goal of this script is to automate the reading in and formatting of the mapping files
#that we received from the NLM 
#There are 4 main files that fall under this
#1. diseases which is list of rare diseases from the ORDR
#2. icd9 which is the mapping file from the ORDR which maps rare diseases to ICD9
#3. icd10 which is the mapping file from the ORDR which maps rare diseases to ICD10
#4. snomed which is the mapping file from the ORDR which maps rare diseases to SNOMED CT
#These files can be joined based on the ORDR_ID which is the unique identifier for a disease

#load libraries
library(dplyr)

#This current file directory assumes you are in script folder and that the mapping files
#are in the data folder 
diseases <- read.table("../data/ORDR_FULL_LIST.txt", sep = "\t", quote = "", header = T)
snomed <- read.table("../data/ORDR_SNOMED_FULL.txt", sep = "\t", quote = "", header = T)
icd9 <- read.table("../data/ORDR_ICD9_FULL.txt", sep = "\t", quote = "", header = T)
icd9 <- select(icd9, c(1:3,7,8))
icd10 <- read.table("../data/ORDR_ICD10_FULL.txt", sep = "\t", quote = "", header = T)
icd10 <- select(icd10, c(1:3,7,8))

#Renaming columns for more precision and better understanding
commonColNames <- c("id", "termType", "disease")
#SNOMED names
names(snomed)[1:3] <- commonColNames
names(snomed)[4:5] <- c("snomedCode", "snomedDesc")
snomed$id <- as.character(snomed$id)
#ICD9 names
names(icd9)[1:3] <- commonColNames
names(icd9)[4:5] <- c("icd9Code", "icd9Desc")
icd9$id <- as.character(icd9$id)
icd9$icd9Code <- as.character(icd9$icd9Code)
icd9$id[1] <- "1"
#ICD10 names
names(icd10)[1:3] <- commonColNames
names(icd10)[4:5] <- c("icd10Code", "icd10Desc")
icd10$id <- as.character(icd10$id)
icd10$icd10Code <- as.character(icd10$icd10Code)
icd10$id[1] <- "1"
