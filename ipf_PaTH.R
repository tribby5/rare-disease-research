# Gary Koplik
# Summer, 2017
# ipf_PaTH.R

# looking into replicating prevalence numbers from paper Rachel sent to us
# Dimmock et al (2015) - Evaluation of a Computable Phenotype for
#   Identification of Patients with Idiopathic Pulmonary Fibrosis
# ../PaTH_Evaluation of a Computable Phenotype for Identification of Patients with IPF.pdf

library(dplyr)
library(lubridate)

load("../data/rarePatients.Rdata")

# rename for consistency later
rare <- rareData
rm(rareData)

# initial restrictions on patients
pat_id_ipf_code <- rare %>%
  filter(Code %in% c("516.31", "516.3"))

# patients satisfying intial restrictions
pat_id_include <- pat_id_ipf_code %>%
  group_by(PAT_ID) %>%
  summarize(unique(PAT_ID))

# patients to exclude after initial restrictions
pat_id_exclude <- rare %>%
  # grab from rare set to get ICD10 codes for those pats
  filter(PAT_ID %in% pat_id_include[[1]]) %>%
  mutate("letters" = substr(Code, 1, 3)) %>%
  filter(letters %in% c("M30", "M31", "M32", "M33",
                        "M34", "M35", "M36")) %>%
  group_by(PAT_ID) %>%
  summarize(unique(PAT_ID))

# get the count of the number who satisfy both constraints
nrow(pat_id_include) - nrow(pat_id_exclude)
  