# Gary Koplik
# Summer, 2017
# make_1_to_1.R

# reads in the full icd 9 and 10 mapping files
#   creates subset files of only the codes
#     that map to exactly 1 rare disease

# load libraries
library(dplyr)

# read in tables
icd9 <- read.table("../data/ORDR_ICD9_FULL.txt", sep = "\t", quote = "",
                   stringsAsFactors = F, header = T)
icd10 <- read.table("../data/ORDR_ICD10_FULL.txt", sep = "\t", quote = "",
                    stringsAsFactors = F, header = T)

# restructure for later code to work
icd9 <- icd9 %>%
  # keep primary term only
  filter(ORDR_TTY == "PT") %>%
  dplyr::select("code" = ICD9CM_CODE,
                "disease_num" = ï..ORDR_ID,
                "disease_name" = ORDR_NAME,
                "code_description" = ICD9NAME)

icd10 <- icd10 %>%
  # keep primary term only
  filter(ORDR_TTY == "PT") %>%
  dplyr::select("code" = ICD10CM_CODE,
                "disease_num" = ï..ORDR_ID,
                "disease_name" = ORDR_NAME,
                "code_description" = ICD10NAME)

icd9_1to1 <- icd9 %>%
  # look within icd codes
  group_by(code) %>%
  # figure out how many unique diseases that code maps to
  summarize(rare = length(unique(disease_num))) %>%
  # keep icd codes that only map to one rare disease
  filter(rare == 1) %>%
  # join in orignial icd information
  left_join(icd9) %>%
  # only need to keep icd code and its description
  select(code, code_description, disease_name) %>%
  # lots of repeats so only keep unique values
  distinct()

icd10_1to1 <- icd10 %>%
  # look within icd codes
  group_by(code) %>%
  # figure out how many unique diseases that code maps to
  summarize(rare = length(unique(disease_num))) %>%
  # keep icd codes that only map to one rare disease
  filter(rare == 1) %>%
  # join in orignial icd information
  left_join(icd10) %>%
  # only need to keep icd code and its description
  select(code, code_description, disease_name) %>%
  # lots of repeats so only keep unique values
  distinct()  

# save the codes
save(icd9_1to1, file = "../data/icd9_1to1.Rdata")
save(icd10_1to1, file = "../data/icd10_1to1.Rdata")

