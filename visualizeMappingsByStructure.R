#Matthew Tribby
#July 2017

#The goal of this script is to analyze and visualize the results of the breakdown of 
#ICD10 codes based on the hierarchy (chapters and blocks)

#Assumes you are in scripts folder
#NOTE: if you don't have the icd10SnomedMapping.RData file, run the script mappingToSnomed.R
load("../data/icd10SnomedMapping.RData")
load("../data/icd10_blocks_chapters.RData")
load("../data/rareICD10Patients.RData")

#load libraries
library(dplyr)
library(ggplot2)
library(ggthemes)

#Joins the mapping data with chapters and blocks of ICD for future analysis
icd10MappingStruct <- icd10SnomedMapping %>% 
  left_join(icd10_blocks_chapters, by= c("icd10Code" = "code"))

#Counts the occurences of the ICD code in the patient dataset
icd10MappingStruct <- icd10MappingStruct %>% 
  mutate(count = unlist(lapply(icd10MappingStruct$icd10Code, function(x){sum(x == rareICD10$Code)})))    

#This creates statistics on a chapter-level granularity
mapByChapter <- icd10MappingStruct %>%
  group_by(chapter) %>%
  summarize(avgUniquePaths = mean(as.numeric(uniquePaths)),
            avgParent = mean(as.numeric(parentLevel)),
            oneToOne = sum(uniquePaths == 1),
            rareCodeOccur = sum(count), 
            rareCodes = length(parentLevel),
            propOne = (oneToOne/rareCodes),
            propNonOne = (rareCodes-oneToOne)/rareCodes)
mapByChapter <- mapByChapter[complete.cases(mapByChapter),]

#titles that are shorter for visualization purposes
chapterTitles <- c("Conditions in the Perinatal Period", "Infectious and Parasitic Diseases", 
              "Congenital Abnormalities", "Blood and Immune Diseases", "Circulatory Diseases",
              "Digestive Diseases", "Ear and Mastoid Diseases", "Eye and Adnexa Diseases", 
              "Genitourinary Diseases", "Muscoskeletal Diseases", "Nervous System Diseases", 
              "Repsiratory Diseases", "Skin Diseases", "Metabolic Diseases", "Health Status and Services",
              "Mental Disorders", "Neoplasms", "Pregnancy and Childbirth", "Abnormal Findings, NEC")
mapByChapter$chapterTitle <- chapterTitles

#Subsetting to look at comparison of "one to one" codes versus non "one to one" on per chapter basis
#One to one meaning that the disease maps to only one code and not multiple
mapByChapterOne <- mapByChapter[rep(seq_len(nrow(mapByChapter)), mapByChapter$oneToOne),] %>%
  select(chapter, chapterTitle, rareCodes, propNonOne) %>% mutate(One_to_One = "1 SNOMED CT Code")
mapByChapterNonOne <- mapByChapter[rep(seq_len(nrow(mapByChapter)), mapByChapter$rareCodes - mapByChapter$oneToOne),] %>%
  select(chapter, chapterTitle, rareCodes, propNonOne) %>% mutate(One_to_One = ">1 SNOMED CT Code")
uniqueByChapter <- rbind(mapByChapterOne, mapByChapterNonOne) 

#Custom theme for the plots
theme_hc_slant <- theme_hc() + theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 10))

#plot which shows raw count of ICD10 codes per chapter
ggplot(uniqueByChapter, aes(x = reorder(chapter, -rareCodes))) +geom_bar(aes(fill = One_to_One)) +
  theme_minimal() +  theme(plot.title = element_text(size = 12, margin = margin(20,0,20,0))) + 
  labs(title  = expression(paste('Number of "Rare" ICD10 Codes in Each Chapter, \nColored by how many SNOMED CT codes the ICD10 code maps to')), 
       x = "Chapter", y = "Count of ICD10 Codes", fill = '') + coord_flip()

png("../figures/prop_unique_chapters.png",
    width = 14, height = 8, units = "in", res = 300)
#plot which shows porportion of Uniquness by Chapters
ggplot(uniqueByChapter, aes(x = reorder(chapter, -propNonOne))) +geom_bar(aes(fill = One_to_One), position = "fill") +
  theme_minimal() + 
  labs(title  = 'Proportion of ICD10 "Rare" Codes that Map Uniquely to SNOMED CT by ICD10 Chapter', 
       x = "Chapter", y= "Proportion", fill = '') + coord_flip()
dev.off()

#Bar plot for the number of occurrences of "rare" codes in each chapter
ggplot(mapByChapter, aes(x = reorder(chapter, rareCodeOccur), y = (rareCodeOccur/100000))) + 
  geom_bar(stat = "identity", aes(fill = avgParent)) + scale_fill_gradient(low = "#00C4FF", high = "#040050") + 
  theme_minimal() + 
  labs(title = 'Counts of ICD10 "Rare" Codes in Rare Patients by Chapter',
       x = "Chapter", y = "Occurences of Rare ICD10 Codes (By 100,000)", fill = expression(paste("Average Common Parent Level \n Above (SNOMED CT)"))) + 
  coord_flip()

#Bar plot for the number of occurrences of "rare" codes in each chapter WITHOUT NEOPLASMS
ggplot(mapByChapter %>% filter(chapterTitle != "Neoplasms"), 
  aes(x = reorder(chapter, rareCodeOccur), y = (rareCodeOccur/100000))) + 
  geom_bar(stat = "identity", aes(fill = avgParent)) + scale_fill_gradient(low = "#00C4FF", high = "#040050") + 
  theme_minimal() + 
  labs(title = expression(paste('ICD10 "Rare" Codes in Patients by Chapter, Excluding Neoplasms')),
       x = "Chapter", y = "Occurences of Rare ICD10 Codes (By 100,000)", fill = expression(paste("Average Common Parent Level \n Above (SNOMED CT)"))) + 
  coord_flip()

#Closer Look at Metabolic Chapter
chapterName <- "Endocrine, Nutritional, and Metabolic Diseases"

specificChap <- icd10MappingStruct %>%
  filter(chapter == chapterName) %>%
  group_by(block_name) %>% 
  summarize(avgUniquePaths = mean(as.numeric(uniquePaths)),
            avgParent = mean(as.numeric(parentLevel)),
            oneToOne = sum(uniquePaths == 1),
            rareCodeOccur = sum(count), 
            rareCodes = length(parentLevel),
            propOne = (oneToOne/rareCodes),
            propNonOne = (rareCodes-oneToOne)/rareCodes)
metabolic <- metabolic[complete.cases(mapByChapter),]


#Bar plot for the number of occurrences of "rare" codes in each chapter
ggplot(block, aes(x = reorder(specificChap, rareCodeOccur), y = (rareCodeOccur/1))) + 
  geom_bar(stat = "identity", aes(fill = avgParent)) + scale_fill_gradient(low = "#00C4FF", high = "#040050") + 
  theme_hc_slant + theme(axis.text.x = element_text(size = 1)) + 
  labs(title = 'Counts of ICD10 "Rare" Codes in Rare Patients by Chapter',
       x = "Chapter", y = "Occurences of Rare ICD10 Codes (By 100,000)")
