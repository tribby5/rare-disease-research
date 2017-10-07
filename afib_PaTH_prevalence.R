# Gary Koplik
# Summer, 2017
# PaTH_prevalence.R

# Atrial fibrillation subsetting according to criteria in
#   ../path_afib_inclusion_criteria.docx

library(dplyr)
library(lubridate)

load("../data/rarePatients.Rdata")

# rename for consistency later
rare <- rareData
rm(rareData)

# initial restrictions on patients
pat_id_first_cutoff <- rare %>%
  # age cutoff
  filter(AGE >= 18) %>%
  # at least 3 outpatient visits
  # NOTE: this is wrong with original EVENT_MONTH variable
  #   need unique visit ID still
  group_by(PAT_ID, EVENT_MONTH) %>%
  summarize(n = n()) %>%
  filter(n >= 3) %>%
  # just keep unique pat_id
  summarize(unique(PAT_ID))

# incorporate first restrictions on whole data set
pat_id_second_cutoff <- rare %>%
  # only keep patients to satisfy criteria so far
  filter(PAT_ID %in% pat_id_first_cutoff[[1]]) %>%
  # only keep people with ICD9 codes 427.3 or 427.31
  filter(Terminology == "ICD9" & Code %in% c("427.3", "427.31")) %>%
  # keep unique patient ID's
  group_by(PAT_ID) %>%
  summarize(unique(PAT_ID))


# pause to put the EVENT_TIME variable in useful sortable format
#   before the next criteria
rare$date <- parse_date_time(paste0(as.character(
                                      as.numeric(
                                        factor(substr(rare$EVENT_MONTH, 1, 3),
                                               levels = c("JAN", "FEB", "MAR", "APR",
                                                         "MAY", "JUN", "JUL", "AUG",
                                                         "SEP", "OCT", "NOV", "DEC")
                                        )
                                      )
                                     ),
                             substr(rare$EVENT_MONTH, 4, 8)
                             ), "my"
              )

# incorporate second restrictions on whole data set
rare_subset <- rare %>%
  # only keep patients to satisfy criteria so far
  filter(PAT_ID %in% pat_id_second_cutoff[[1]]) %>%
  # sort on PAT_ID then date
  arrange(PAT_ID, date)

rm(rare)
gc()

# need to look at the one month before and one month after window
  #  requires some set up

# grab the first date
first_date <- rare_subset$date[1]

# loop will start in 2nd available month
second_date <- first_date
month(second_date) <- month(second_date) + 1

# grab the last date
last_date <- rare_subset$date[nrow(rare_subset)]

# loop will end in 2nd to last available month
second_to_last_date <- last_date
month(second_to_last_date) <- month(second_to_last_date) - 1

# create list of dates inside range(2nd date - 2nd to last date)
to_loop <- list(seq(second_date, second_to_last_date, by = "months"))
# vectors don't seem to preserve POSIXct format so need lists


# exclusion criteria
#   take out ICD9 codes with 242.X (any digit after decimal)
#     or 245.X
#     WITHIN 1 month of the diagnosis of interest

# OR
# 35.x the month BEFORE afib

# need a column with first 3 digits of code to check for these
rare_subset$code_first3 <- substr(rare_subset$Code, 1, 3)

# create empty list to put exclusion people into
pats_to_exclude <- c()

# loop over the list, storing the names that satisfy threshold
for(i in 1:length(to_loop[[1]])){
  print(to_loop[[1]][i])
  # create the 3 month window data frame
  # previous month date
  date_prev <- to_loop[[1]][i]
  month(date_prev) <- month(date_prev) - 1
  # next month date
  date_post <- to_loop[[1]][i]
  month(date_post) <- month(date_post) + 1
  
  # subset data frame to patients with afib codes
  #   in center month of 3 month window
  pats_with_afib <- rare_subset %>%
    filter(date == to_loop[[1]][i] & Code %in% c("427.3", "427.31") ) %>%
    select(PAT_ID)
  
  # subset the data frame to 2 outer months of window
  #   of patients with possible exclusion criterion
  #     242.x or 245.x
  pats_with_excl1 <- rare_subset %>%
    filter(date %in% c(date_prev, date_post) &
           code_first3 %in% c("242", "245") ) %>%
    select(PAT_ID)
  
  # subset the data frame to first month of window
  #   of patients with possible exclusion criterion
  #     35.x
  pats_with_excl2 <- rare_subset %>%
    filter(date == date_prev &
           code_first3 == "35." ) %>%
    select(PAT_ID)
  
  # the intersection of pats_with_afib
  #   and pats with at least one exclusion criteria
  #     should be removed from the prevalence measure
  pats_to_exclude <- c(pats_to_exclude,
                       intersect(pats_with_afib[ , 1], pats_with_excl1[ , 1]),
                       intersect(pats_with_afib[ , 1], pats_with_excl2[ , 1]) )
  
  print(paste0("Excluding: ", intersect(pats_with_afib[ , 1], pats_with_excl1[ , 1]),
               intersect(pats_with_afib[ , 1], pats_with_excl2[ , 1])
               )
  )
  
} 

# pull out the pats_to_exclude
final_cohort <- rare_subset %>%
  filter(!(PAT_ID %in% pats_to_exclude))

length(unique(final_cohort$PAT_ID))
# 27596 for our data set Summer, 2017 
  
  
  
  