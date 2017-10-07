#Matthew Tribby, Gary Koplik
#June 2017

#The goal of this script is to take the given patient data (called diagnoses.csv) and to
#format it in a usable form for analysis
#The largest issue with the data we were given is that it had icd9 and icd10 codes comma 
#separated in one column if there were multiple. This script will fix that issue and then
#also separate the data into either a rare or non-rare subset (can only do one at a time
#in the current version because of memory constraints)

#NOTE
#Needs ~16GB of RAM
#if not enough space, periodically save out parts and rm() it and gc()

#YOU NEED TO CHOOSE WHETHER YOU WANT RARE OR NON RARE PATIENT SUBSET, default is RARE
rare <- T
#If you want both the rare and non rare subsets, run again with rare = F
#NOTE: If making multiple runs, you can skip the step of saving out subsets and go straight
#to the main for loop after the first time

#Basic steps of this script
#1. Load in original diagnosis file, and save out subsets so easier to handle in memory
#2. For each subset, transform from wide to long form (changing the ICD9 and ICD10 columns)
#3. For each subset, divide between those affected by rare diseases and those not
#4. Combine the subsets and save it out at the end

#load libraries
library(data.table)
library(tidyr)
library(dplyr)
library(stringr)

#Set working directory, assumes you are working in the scripts folder
setwd("Y:/shared/data")
#setwd("../data")

#Set up of files
# read in icd 9 rare diseases
icd9 <- read.table("./ORDR_ICD9_FULL.txt", sep = "\t", quote = "", header = T)
names(icd9)[1] <- "id"

# read in icd 10 rare diseases
icd10 <- read.table("./ORDR_ICD10_FULL.txt", sep = "\t", quote = "", header = T)
names(icd10)[1] <- "id"

# save the unique icd codes as vectors
disease_codes_icd9 <- dplyr::select(icd9, 1, "Code" = 7) 
disease_codes_icd10 <- dplyr::select(icd10, 1, "Code" = 7) 

# stack these codes
disease_codes <- base::rbind(disease_codes_icd9, disease_codes_icd10)

#The file you want to change
load("diagnoses.RData")
#diagnoses <- fread("diagnoses2.csv")
patientData <- data
rm(data)
#add in unique row identifier
patientData$rowID <- 1:nrow(patientData)

#Gets unique patient ids (needed to correctly subset the original dataset by patients)
patientId <- unique(patientData$PAT_ID)
#Number of subsets to make of the original dataset, 25 is a recommended number, make the 
#number too small and it will take up more memory 
num_subsets <- 25

#destination to save out the subset files
setwd("./allPatientSubsets")
#for loop which divides the original patient data into subsets and saves them out with systematic
#and unique names
for(i in 1:num_subsets){
  patients <- patientData[patientData$PAT_ID %in% patientId[(floor((i-1)*length(patientId)/num_subsets + 1)):(floor(i*length(patientId)/num_subsets))],]
  save("patients", file = paste("patients", i, ".RData", sep = ""))
}

#Removes files and then garbarage collects (to try to optimize memory usage)
rm(patientData)
rm(patients)
gc()

#For loop which takes the actual transformation on the data. Each iteration will load in a 
#new subset, trasform it to wide form (separating those comma separated values) and then 
#putting it back into long form and then combine into a finished dataframe with previous subsets
for(j in 1:num_subsets){
  #loads subset
  load(paste("patients", j, ".RData", sep = ""))

#Counts the max number of ICD codes that appear in the columns (i.e. code1,code2,code3 = 3)
#The reason for this is that we need to know how many columns to create when going to wide form
ICD9cols <- max(str_count(patients$ICD9, ",") + 1)
ICD10cols <- max(str_count(patients$ICD10, ",") +1)
  
#IC9 into wide format
patients <- cbind(patients, str_split_fixed(patients$ICD9, ", ", str_count(patients$ICD9, ",") + 1))
#making ICD9 col names, names need to be unique in order to work for column selection
for(i in 1:ICD9cols){
  names(patients)[9+i] <- paste("ICD9", i, sep = "_")
}
  
#ICD10 into wide format
patients <- cbind(patients, str_split_fixed(patients$ICD10, ", ", str_count(patients$ICD10, ",") + 1))
#making ICD10 col names, names need to be unique in order to work for next step
for(i in 1:ICD10cols){
  names(patients)[9+ICD9cols+i] <- paste("ICD10", i, sep = "_")
}
  
#removing original ICD9 and ICD10 cols (before they were wide)
patients <- select(patients, -c(7,8))
  
#Makes empty spaces into NAs
patients <- patients %>% mutate_all(function(x){replace(x, x == "", NA)})
  
#Turns wide format into long format, removing unnecessary rows
patients <- gather(patients, "Terminology", "Code", -c(1:7), na.rm = T)
  
#Fixes the names so that ICD9 and ICD10 are all coded as such, before different columns needed
#original names (i.e. ICD9_1, ICD9_2, etc.) but now we wanted all ICD9 codes labeled similarly in long
patients <- patients %>% mutate(Terminology = (str_split_fixed(Terminology, fixed("_"), 2)[,1]))
  
# left join the disease code with health data
rare_dat <- dplyr::left_join(disease_codes, patients, by = "Code")
  
# keep the unique patient ID's of people identified among rare diseases
rare_ids <- unique(na.omit(rare_dat$PAT_ID))
rm(rare_dat)
gc()
  
rareRows <- patients$PAT_ID %in% rare_ids

if(rare){
  subset <- patients[rareRows, ]
}
else{
  subset <- patients[!rareRows, ]
}
rm(patients)

  if(j == 1){
    data <- subset
  }
  else{
    data <- rbind(data, subset)
  }
rm(subset)
gc()
}

#Goes back to data folder to save out the finished data
setwd("..")

#You can attempt to rename the file from data, but would be better to save it out first in case
#memory constraints from this script. 
if(rare){
  rarePatients <- data
  save("rarePatients", file = "rarePatients.RData")
}else{
  save("data", file = "nonRarePatients.RData")
}
