# Gary Koplik
# Summer, 2017
# num_visits_rare_v_nonrare.R

# document for comparing rare and non-rare cohorts with respect to number of visits
#   note currently, this document is using an unreliable visits variable
#     but reliable variable coming...


#### set up ####

# clear old code for replicability
rm(list = ls() )

# load libraries
library(dplyr)
library(ggplot2)

# load in data sets
load("../data/rarePatients2.Rdata")
rare <- rarePatients
# remove original and garbage collect to free up memory  
rm(rarePatients)
gc()

load("../data/nonRarePatients2.Rdata")
nonrare <- nonRarePatients
# remove original and garbage collect to free up memory  
rm(nonRarePatients)
gc()

#### build data frames ####

# create subset of rare diseases
#   that includes a count of # of unique visit code
#   and only one row per pat_id
#   NOTE: using EVENT_MONTH will change since not actually unique identifier
rare_unique <- rare %>%
  group_by(PAT_ID) %>%
  # get counts and age
  summarize(count_visits = length(unique(VISIT_ID)),
            AGE = mean(AGE) ) %>%
  # add in factor of Rare name for when combining data frames
  mutate("is_rare" = factor("Rare")) %>%
  # add in age factor
  mutate("age_factor" = cut(AGE, seq(0, 100, by = 5)))

# do the same for non-rare diseases
nonrare_unique <- nonrare %>%
  group_by(PAT_ID) %>%
  # get counts and Age
  summarize(count_visits = length(unique(VISIT_ID)),
            AGE = mean(AGE) ) %>%
  # add in factor of Non-Rare name for when combining data frames
  mutate("is_rare" = factor("Non-Rare")) %>%
  # add in age factor
  mutate("age_factor" = cut(AGE, seq(0, 100, by = 5)))
  


unique_pats <- rbind(rare_unique, nonrare_unique)

# boxplot of rare vs non-rare
#   trimming y axis because highest y outlier is 1500 (can't see anything on graph)
png("../figures/visits_rare_v_nonrare2.png",
    width = 600, height = 600)
ggplot(unique_pats) +
  geom_boxplot(aes(x = is_rare, y = count_visits, fill = is_rare),
               outlier.fill = "white",
               outlier.alpha = 0.8, show.legend = F) +
  ggtitle("Number of Visits for Rare and Non-Rare Cohorts") +
  ylab("Number of Visits") +
  xlab("Cohort") +
  ylim(0, 100) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

# boxplot of rare over age (ignore ages not in [0,100])
# putting limit of 50 in since not showing outliers that go to 1500
png("../figures/visits_vs_age_rare_v_nonrare2.png",
    width = 10, height = 5, units = 'in', res = 300)
ggplot(filter(unique_pats, !is.na(age_factor))) +
  geom_boxplot(aes(x = age_factor, y = count_visits, fill = is_rare),
               outlier.color = "white") +
  ggtitle("Number of Visits for Rare and Non-Rare Cohorts Over Age") +
  ylab("Number of Visits") +
  xlab("Age") +
  ylim(0, 50) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(title = "Cohort"))
dev.off()

print("Setting an error here to stop execution of later slow / incomplete code")
stop()

# also make df with the chapters for each patient -- memory constraints (16gb)...
  # NOTE: patients can obviously be in multiple chapters
  # looking for heterogeneity btwn patients who have at least one disease
  #   within icd-10 chapter
### did not complete this, but leaving this code for future generations ###

non_rare_chapter <- nonrare %>%
  select(PAT_ID,
         Code,
         Terminology) %>%
  # left_join(icd10_blocks_chapters, by = c('Code' = 'code') ) %>%
  
  # 1 rare disease instance per patient
  distinct(PAT_ID, Code, .keep_all = T) %>%
  filter(Terminology == "ICD10") %>%
  mutate(is_rare = factor("Non-Rare")) %>%
  # add in column of the letter in ICD10 code
  mutate("letter" = substr(Code, 1, 1)) %>%
  # add in column of first two numbers
  mutate("numbers" = as.numeric(substr(Code, 2, 3))) %>%
  # note this causes NA's which will check by hand
  # and group accordingly
  # add empty column to fill with chapter names
  mutate("chapter" = NA) %>%
  # make modifications to chapter column
  #   beyond hardcoded referencing website codebook
  mutate("chapter" = 
           ifelse(letter %in% c("A", "B"),
                  "Certain Infectious and Parasitic Diseases",
                  ifelse(letter == "C" | (letter == "D" & numbers <= 49) | substr(Code, 1, 3) == "D3A",
                         "Neoplasms",
                         ifelse(letter == "D" & numbers > 49,
                                "Diseases of the Blood and Blood-Forming Organs and Certain Disorders Involving the Immune Mechanism",
                                ifelse(letter == "E",
                                       "Endocrine, Nutritional, and Metabolic Diseases",
                                       ifelse(letter == "F",
                                              "Mental, Behavioral, and Neurodevelopmental Disorders",
                                              ifelse(letter == "G",
                                                     "Diseases of the Nervous System",
                                                     ifelse(letter == "H" & numbers <= 59,
                                                            "Diseases of the Eye and Adnexa",
                                                            ifelse(letter == "H" & numbers > 59,
                                                                   "Diseases of the Ear and Mastoid Process",
                                                                   ifelse(letter == "I",
                                                                          "Diseases of the Circulatory System",
                                                                          ifelse(letter == "J",
                                                                                 "Diseases of the Respiratory System",
                                                                                 ifelse(letter == "K",
                                                                                        "Diseases of the Digestive System",
                                                                                        ifelse(letter == "L",
                                                                                               "Diseases of the Skin and Subcutaneous Tissue",
                                                                                               ifelse(letter == "M",
                                                                                                      "Diseases of the Musculoskeletal System and Connective Tissue",
                                                                                                      ifelse(letter == "N",
                                                                                                             "Diseases of the Genitourinary System",
                                                                                                             ifelse(letter == "O",
                                                                                                                    "Pregrancy, Childbirth, and the Puerperium",
                                                                                                                    ifelse(letter == "P",
                                                                                                                           "Certain Conditions Originating in the Perinatal Period",
                                                                                                                           ifelse(letter == "Q",
                                                                                                                                  "Congenital Malformations, Deformations, and Chromosomal Abnormlaities",
                                                                                                                                  ifelse(letter == "R",
                                                                                                                                         "Symptoms, Signs, and Abnormal Clinical and Laboratory Findings, not Elsewhere Classified",
                                                                                                                                         ifelse(letter %in% c("S", "T"),
                                                                                                                                                "Injury, Poisoning, and Certain Other Consequences of External Causes",
                                                                                                                                                ifelse(letter %in% c("V", "W", "X", "Y"),
                                                                                                                                                       "External Causes of Morbidity",
                                                                                                                                                       ifelse(letter == "Z",
                                                                                                                                                              "Factors Influencing Health Status and Contact with Health Services",
                                                                                                                                                              NA
                                                                                                                                                       )))))))))))))))))))))) %>%
  select(-letter, -numbers)


rare_chapter <- rare %>%
  select(PAT_ID,
         Code,
         Terminology) %>%
  # left_join(icd10_blocks_chapters, by = c('Code' = 'code') ) %>%
  
  # 1 rare disease instance per patient
  distinct(PAT_ID, Code, .keep_all = T) %>%
  filter(Terminology == "ICD10") %>%
  mutate(is_rare = factor("Rare")) %>%
  # add in column of the letter in ICD10 code
  mutate("letter" = substr(Code, 1, 1)) %>%
  # add in column of first two numbers
  mutate("numbers" = as.numeric(substr(Code, 2, 3))) %>%
  # note this causes NA's which will check by hand
  # and group accordingly
  # add empty column to fill with chapter names
  mutate("chapter" = NA) %>%
  # make modifications to chapter column
  #   beyond hardcoded referencing website codebook
  mutate("chapter" = 
           ifelse(letter %in% c("A", "B"),
                  "Certain Infectious and Parasitic Diseases",
                  ifelse(letter == "C" | (letter == "D" & numbers <= 49) | substr(Code, 1, 3) == "D3A",
                         "Neoplasms",
                         ifelse(letter == "D" & numbers > 49,
                                "Diseases of the Blood and Blood-Forming Organs and Certain Disorders Involving the Immune Mechanism",
                                ifelse(letter == "E",
                                       "Endocrine, Nutritional, and Metabolic Diseases",
                                       ifelse(letter == "F",
                                              "Mental, Behavioral, and Neurodevelopmental Disorders",
                                              ifelse(letter == "G",
                                                     "Diseases of the Nervous System",
                                                     ifelse(letter == "H" & numbers <= 59,
                                                            "Diseases of the Eye and Adnexa",
                                                            ifelse(letter == "H" & numbers > 59,
                                                                   "Diseases of the Ear and Mastoid Process",
                                                                   ifelse(letter == "I",
                                                                          "Diseases of the Circulatory System",
                                                                          ifelse(letter == "J",
                                                                                 "Diseases of the Respiratory System",
                                                                                 ifelse(letter == "K",
                                                                                        "Diseases of the Digestive System",
                                                                                        ifelse(letter == "L",
                                                                                               "Diseases of the Skin and Subcutaneous Tissue",
                                                                                               ifelse(letter == "M",
                                                                                                      "Diseases of the Musculoskeletal System and Connective Tissue",
                                                                                                      ifelse(letter == "N",
                                                                                                             "Diseases of the Genitourinary System",
                                                                                                             ifelse(letter == "O",
                                                                                                                    "Pregrancy, Childbirth, and the Puerperium",
                                                                                                                    ifelse(letter == "P",
                                                                                                                           "Certain Conditions Originating in the Perinatal Period",
                                                                                                                           ifelse(letter == "Q",
                                                                                                                                  "Congenital Malformations, Deformations, and Chromosomal Abnormlaities",
                                                                                                                                  ifelse(letter == "R",
                                                                                                                                         "Symptoms, Signs, and Abnormal Clinical and Laboratory Findings, not Elsewhere Classified",
                                                                                                                                         ifelse(letter %in% c("S", "T"),
                                                                                                                                                "Injury, Poisoning, and Certain Other Consequences of External Causes",
                                                                                                                                                ifelse(letter %in% c("V", "W", "X", "Y"),
                                                                                                                                                       "External Causes of Morbidity",
                                                                                                                                                       ifelse(letter == "Z",
                                                                                                                                                              "Factors Influencing Health Status and Contact with Health Services",
                                                                                                                                                              NA
                                                                                                                                                       )))))))))))))))))))))) %>%
  select(-letter, -numbers)


chapter_pats <- rbind(rare_chapter, non_rare_chapter)
