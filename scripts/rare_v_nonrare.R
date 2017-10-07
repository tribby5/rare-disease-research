# Gary Koplik
# Summer, 2017
# rare_v_nonrare.R

#### set up ####

# clear old code for replicability
rm(list = ls() )

# load libraries
library(dplyr)
library(ggplot2)
library(stringr)

# load in data sets
load("../data/rarePatients.Rdata")
rare <- rarePatients
# remove original and garbage collect to free up memory  
rm(rarePatients)
gc()

load("../data/nonRarePatients.Rdata")
nonrare <- nonRareData
# remove original and garbage collect to free up memory  
rm(nonRareData)
gc()

#### build data frames ####

# create subset of rare diseases
#   that includes a count of # of icd codes
#   and only one row per pat_id
rare_unique <- rare %>%
  group_by(PAT_ID) %>%
  # get counts and age (maybe should redo this only looking at just 9 or 10 codes?)
  summarize(count_unique = length(unique(Code)),
            AGE = mean(AGE),
            count = n() )

# do the same for non-rare diseases
nonrare_unique <- nonrare %>%
  group_by(PAT_ID) %>%
  # get counts and Age
  summarize(count_unique = length(unique(Code)),
            AGE = mean(AGE),
            count = n()
            # year_vis = str_sub(EVENT_MONTH, -4) doesn't run right
  )

# create data frame to compare chapter frequencies between rare and non-rare

non_rare_chapter <- nonrare %>%
  select(PAT_ID,
         Code,
         Terminology) %>%
  # left_join(icd10_blocks_chapters, by = c('Code' = 'code') ) %>% # wasn't sufficient here...
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
  # left_join(icd10_blocks_chapters, by = c('Code' = 'code') ) %>% # wasn't sufficient here...
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

sum(is.na(rare_chapter))

disease_chapter <- rbind(rare_chapter, non_rare_chapter)




#### plots ####
png("../figures/chapter_rare_v_nonrare.png",
    width = 8, height = 7, units = 'in', res = 300)
  ggplot(disease_chapter, aes(x = is_rare)) +
    geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(chapter)),
             color = "black", position = "fill", size = 0.05) +
    xlab("Cohort") +
    ylab("Fraction of Cohort") +
    ggtitle("ICD-10 Chapter Breakdown of Rare and Non-Rare Cohorts") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(fill = guide_legend(title = "ICD-10 Chapter",
                               ncol = 1))
dev.off()
  
# breakdown of prevalence of chapters of ICD-10 codes
#   for rare vs. non-rare patients



# kernel density age plot rare vs. non-rare
# set legend colors
cols <- c("Rare" = "#FF9999", "Non-Rare" = "lightblue")
# plot
png("../figures/age_rare_v_nonrare.png",
    width = 8, height = 7, units = 'in', res = 300)
  ggplot() +
    stat_density(data = rare_unique,
                 aes(x = as.numeric(as.character(rare_unique$AGE)),
                     color = "Rare"),
                 size = 1.25, geom = "line") +
    stat_density(data = nonrare_unique,
                 aes(x = as.numeric(as.character(nonrare_unique$AGE)),
                     color = "Non-Rare"),
                 size = 1.25, geom = "line") +
    scale_color_manual(values = cols) +
    xlim(0, 105) +
    xlab("Age") +
    ylab("Density") +
    ggtitle("Distribution of Ages for Rare and Non-Rare Disease Patients") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
dev.off()


# number of icd codes by age

cols <- c("Rare" = "#FF9999", "Non-Rare" = "lightblue",
          "Smoothed Fit Rare" = "#990000",
          "Smoothed Fit Non-Rare" = "#6666FF")
png("../figures/numcode_vs_age_rare_v_nonrare.png",
    width = 800, height = 600)
ggplot() +
  geom_point(data = rare_unique,
             aes(x = AGE, y = count_unique, color = "Rare"),
             alpha = 0.3) +
  geom_point(data = nonrare_unique,
             aes(x = AGE, y = count_unique, color = "Non-Rare"),
             alpha = 0.3) +
  geom_smooth(data = rare_unique,
              aes(x = AGE, y = count_unique, color = "Smoothed Fit Rare"),
              se = F, size = 1.25) +
  geom_smooth(data = nonrare_unique,
            aes(x = AGE, y = count_unique, color = "Smoothed Fit Non-Rare"),
            se = F, size = 1.25) +
  scale_color_manual(values = cols) +
  ggtitle("Rare Disease Patients Have a Higher Average Number of Assigned ICD Codes") +
  ylab("Number of ICD Codes for Each Patient") +
  xlab("Age") +
  xlim(0, 105) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

