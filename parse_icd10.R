# Gary Koplik
# Summer, 2017
# parse_icd10.R

# file to create a .Rdata file that has a column of ICD-10 codes
#   and the corresponding ICD-10 chapter and block

# clear old code
rm(list = ls())

# load libraries
library(xml2)
library(XML)
library(dplyr)
library(rvest)

icd10 <- read_xml("../data/icd10cm_tabular_2017.xml")
  # xmlParse() %>%

nodes <- icd10 %>% 
  xml_nodes('section')

codes <- nodes %>% 
  xml_nodes('name') %>%
  xml_text()



# method that aggregates block title with icd10 code
  # doing this very hardcoded way
make_code_blocks <- function(){
  # using the nodes variable created above
  
  # start by creating 2 empty vectors
  #   one for storing the codes
  codes <- c()
  #   and one for storing block title names
  block_names <- c()
  
  # take the ith node
   #  get its nested icd codes
  for (i in 1:length(nodes)){
    temp_code <- nodes[i] %>%
      xml_children() %>%
      xml_nodes('name') %>%
      xml_text()
    # append them to our list of icd codes
    codes <- c(codes, temp_code)
    # make copy of block title name for each icd code
    temp_name <- rep((nodes[i] %>%
                        xml_nodes('desc') %>%
                        xml_text())[1],
                     times = length(temp_code))
    # append them to list of block title names
    block_names <- c(block_names, temp_name)
  }
  # return icd 10 codes and block names as dataframe
  return(data.frame("code" = codes, "block_name" = block_names))
}

# get icd-10 blocks
icd10_blocknames <- make_code_blocks()

# add in chapters
icd10_blocks_chapters <- icd10_blocknames %>%
  # add in column of the letter in ICD10 code
  mutate("letter" = substr(code, 1, 1)) %>%
  # add in column of first two numbers
  mutate("numbers" = as.numeric(substr(code, 2, 3))) %>%
    # note this causes NA's which will check by hand
      # and group accordingly
  # add empty column to fill with chapter names
  mutate("chapter" = NA) %>%
  # make modifications to chapter column
  #   beyond hardcoded referencing website codebook
  mutate("chapter" = 
           ifelse(letter %in% c("A", "B"),
                  "Certain Infectious and Parasitic Diseases",
            ifelse(letter == "C" | (letter == "D" & numbers <= 49) | substr(code, 1, 3) == "D3A",
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
  

  
save(icd10_blocks_chapters, file = "icd10_blocks_chapters.Rdata")  
  
  
  
  
  

