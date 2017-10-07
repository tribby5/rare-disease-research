#Matthew Tribby
#June 2017

#The goal of this script is to try to filter out some "rare disease" codes that may lump
#common diseases and rare diseases into the same code bucket. For instance, having diabetes
#and MODY (a rare disease) in the same ICD10 code. This is an important problem when trying
#to establish the burden of rare diseases. The efforts of this script are to create an adjustable
#higher bound of frequency for the codes (eliminate anything over it as more frequent codes
#are more likely to have common diseases, not a perfect metric but one that often holds) and to
#eliminate descriptions with keywords like "Other" or "unspecified" as those often signal of 
#unspecific and therefore general codes. 

#After further discussion with experts, the efforts of this script may offer an approximation
#but true validity of the actual specficity of the codes can only come through expert checking
#of these codes. This script was not used in our final analysis but is here for exploratory purposes
#and to document our efforts

#load libraries
library(dplyr)


#Default file currently is just the subset, assumes you are in the scripts folder and data is
#in the data folder
patientFileToLoad <- "../data/rarePatientsSubset2k.RData"
load(patientFileToLoad)
patients <- rarePatientsSubset
rm(rarePatientsSubset)

#Splitting into different terminologies to check both
rarecodes <- c(as.character(icd9$icd9Code), as.character(icd10$icd10Code))
patients <- patients %>% mutate(rare_code = Code %in% rarecodes)

patientsICD9 <- patients %>% filter(Terminology == "ICD9", rare_code == T)
patientsICD10 <- patients %>% filter(Terminology == "ICD10", rare_code == T)
rm(patients)

#Finding the frequency of ICD9
ICD9Freq <- distinct(as.data.frame(table(patientsICD9$Code)) %>% 
  arrange(desc(Freq)) %>%
  rename(icd9Code = Var1) %>%
  left_join(icd9 %>% select(c(4,5)), by = "icd9Code"))

#Finding the frequency of ICD10
ICD10Freq <- distinct(as.data.frame(table(patientsICD10$Code)) %>% 
  arrange(desc(Freq)) %>%
  rename(icd10Code = Var1) %>%
  left_join(icd10 %>% select(c(4,5)), by = "icd10Code"))

#Name matched in disease. This is important because it means that the description from the 
#terminologies matches the rare disease name, signaling that with all likelihood, that is 
#a specific code
ICD9FreqDiseaseMatch <- ICD9Freq %>% 
  filter(icd9Desc %in% diseases$ORDR_NAME)

ICD10FreqDiseaseMatch <- ICD10Freq %>% 
  filter(icd10Desc %in% diseases$ORDR_NAME)

#keywords to be taken out
keywordsToFilter <- c("unspecified", "other", "Other", "Unspecified")
ICD9FreqKeyword <- ICD9Freq %>%
  filter(!grepl(paste(keywordsToFilter, collapse = "|"), icd9Desc))

ICD10FreqKeyword <- ICD10Freq %>%
  filter(!grepl(paste(keywordsToFilter, collapse = "|"), icd10Desc))

#Arbitrary cut-off bound. Not a perfect metric, more for exploratory reasons. We did confer
#with a geneticist and she said that the first 100 or so codes ordered by frequency were 
#basically completely useless in identifying rare diseases because of their lack of specificity
upperBound <- 150
ICD9Filtered <- ICD9Freq %>%
  filter(!grepl(paste(keywordsToFilter, collapse = "|"), icd9Desc), Freq < upperBound)

