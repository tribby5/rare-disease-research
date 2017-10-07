#Matthew Tribby
#June 2017

#The goal of this script is to determine the uniqueness of ICD codes, not data dependent

#load libraries
library(data.table)
library(dplyr)

#load in the mapping files, assumes that you are in the scripts folder
source("./loadMappingFiles.R")

#DETERMINING THE UNIQUENESS OF CODES
snomedCodeFrequency <- data.frame(table(snomed$code))
names(snomedCodeFrequency)[1] <- "code"

icd9CodeFrequency <- data.frame(table(icd9$code))
names(icd9CodeFrequency)[1] <- "code"

icd10CodeFrequency <- data.frame(table(icd10$code))
names(icd10CodeFrequency)[1] <- "code"

#oneMap means that the code maps to only one RARE disease. However, an insight gained after
#writing this code is that a common disease could also map to this code meaning tha this
#does not necessarily suggest a perfect mapping
oneMapICD9 <- icd9CodeFrequency %>% filter(icd9CodeFrequency == 1) %>% pull(code)
oneMapICD10 <- icd10CodeFrequency %>% filter(icd9CodeFrequency == 1) %>% pull(code)

#Comparing the frequency of a code in the patient dataset to its uniqueness

#First creating dataframe for frequency of code in patient dataset:
load("rarePatients.RData")
patients <- rarePatients

#ICD9 Patient Frequency
patientICD9Freq <- data.frame(table(patients$Code[patients$Terminology == "ICD9"]))
names(patientICD9Freq)[1] <- "code"

patientICD9Freq <- left_join(patientICD9Freq, icd9CodeFrequency, by = "code")
names(patientICD9Freq)[2:3] <- c("freqPatient","freqUnique")
#removes all cases in which the disease isn't rare
patientICD9Freq <- patientICD9Freq[complete.cases(patientICD9Freq[,3]),]

#ICD10 Patient Frequency
patientICD10Freq <- data.frame(table(patients$Code[patients$Terminology == "ICD10"]))
names(patientICD10Freq)[1] <- "code"

patientICD10Freq <- left_join(patientICD10Freq, icd10CodeFrequency, by = "code")
names(patientICD10Freq)[2:3] <- c("freqPatient","freqUnique")
patientICD10Freq <- patientICD10Freq[complete.cases(patientICD10Freq[,3]),]

#Creating dataframes which summarizes the count of codes based on uniqueness 
#uniqueness meaning how many diseases map to that code (i.e. 2 - 2 rare diseases map to single code)
patientCountsICD9 <- data.frame(uniqueFreq = unique(patientICD9Freq$freqUnique))
patientCountsICD9$count <- lapply(unique(patientICD9Freq$freqUnique), function(x){
  sum(patientICD9Freq$freqPatient[patientICD9Freq$freqUnique == x])
})
patientCountsICD9$count <- as.numeric(patientCountsICD9$count)

patientCountsICD10 <- data.frame(uniqueFreq = unique(patientICD10Freq$freqUnique))
patientCountsICD10$count <- lapply(unique(patientICD10Freq$freqUnique), function(x){
  sum(patientICD10Freq$freqPatient[patientICD10Freq$freqUnique == x])
})
patientCountsICD10$count <- as.numeric(patientCountsICD10$count)



