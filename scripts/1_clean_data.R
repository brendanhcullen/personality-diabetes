# This script imports raw SAPA data and performs the following cleaning operations:
# 1) score SPI data
# 2) convert variables to correct type (e.g. factors)
# 3) create composite demographic variables (SES)

# load libraries
library(here)
library(tidyverse)
library(psych)
library(janitor)
library(dataverse)
library(data.table)

# Import data -------------------------------------------------------------

# function to retrieve SAPA data from dataverse
retrieve_data <- function(doi, dataset_name){
  dataset <- get_dataset(doi)
  writeBin(get_file(dataset_name, doi), dataset_name)
  dataset <- fread(dataset_name, na.strings=getOption("<NA>","NA")) %>%
    as.data.frame() %>% 
    subset(select = -c(1))
  file.remove(dataset_name)
  return(dataset)
}

# example for retrieving real SAPA data
# data <- retrieve_data(doi = "doi:10.7910/DVN/TZJGAT", 
#                       dataset_name = "sapaTempData696items22dec2015thru07feb2017.tab")

# Import toy data ---------------------------------------------------------

######################### REMOVE THIS FOR REAL ANALYSIS ######################### 

# import toydataset to use for writing analysis code
source(here("src/build_toy_data.R"))

# add RID variable to be consistent with real SAPA data
data = toydata %>% 
  rownames_to_column() %>% 
  rename(RID = rowname)

rm(toydata)

# Filter data -------------------------------------------------------------

data = data %>% 
  filter(!is.na(diagnosis), # only people who responsed to diabetes question
         country == "USA") # only USA data

# Wrangle demographic vars ------------------------------------------------

## SES

# make sure occupational variables are numeric
data = data %>%
  mutate_at(vars(matches("^(p)\\d(occ)")), as.numeric)

# recode all education variables (for self, parent 1, parent 2)
recode_edu_vars = function(x){
  x = case_when(
    x == "less12yrs" ~ "6", 
    x == "HSgrad" ~ "12", 
    x == "SomeCollege" ~ "14", 
    x == "CurrentInUniv" ~ "14", 
    x == "AssociateDegree" ~ "14", 
    x == "CollegeDegree" ~ "16", 
    x == "InGradOrProSchool" ~ "18", 
    x == "GradOrProDegree" ~ "20")
  
  x = as.numeric(x)
}

data = data %>% 
  mutate_at(vars(matches("edu")), recode_edu_vars)

rm(recode_edu_vars)

# create composite SES vars for self (referring to actual respondent) and parent (average of p1 and p2 vars)
data = data %>%
  mutate_at(vars(matches("edu|occ")), scale) %>% # standardize all SES variables of interest
  mutate(self_ses = rowMeans(.[,c("education", "occPrestige", "occIncomeEst")], na.rm = TRUE),
         parent_ses = rowMeans(.[,grepl("p1|p2", names(.))], na.rm = TRUE))

# use parent_ses for respondents under age 18 or current students over 18; otherwise use self_ses
data = data %>% 
  mutate(which_ses = ifelse(age <= 18 | (age > 18 & jobstatus == "student"), 
                            "parent", 
                            "self"),
         ses = ifelse(which_ses == "self", 
                      self_ses, 
                      parent_ses))

# select only relevant demographic vars and filter out people with missing demographic data
data = data %>% 
  select(RID, # ID numnber
         diagnosis, # diabetes diagnosis
         age, ses, ethnic, # relevant demographic vars
         starts_with("q_")) %>%  # all personality vars
  filter(!is.na(age), 
         !is.na(ses),
         !is.nan(ses), # filter out NaN's as well (NaN's may have been created in addition to NA's, as this is a derived composite variable)
         !is.na(ethnic))

# Fix variable types
data = data %>% 
  mutate(diagnosis = as.factor(diagnosis),
         ethnic = as.factor(ethnic))


# Prep for SPI scoring ----------------------------------------------------

# import SPI scale names
source(here("src/personality_scale_names.R"))

# Read in superKey data
keys = read.csv("src/superKey.csv", header = TRUE, row.names = 1) %>% 
  clean_names()

# only retain SPI items that are part of the SPI-135
data = data %>% 
  select(spi_135_names) %>% 
  cbind(select(data, -starts_with("q_")), .)

# filter for rows that correspond to variables in the current data and select only SPI columns
keys = keys[names(data), ] %>% 
  select(contains("spi_135"))


# Score SPI-5 (i.e. Big 5) ------------------------------------------------

# get keys for Big 5
spi_5_keys = keys %>% 
  select(1:5)

# score the Big 5 scales
scored = scoreItems(spi_5_keys, data)
spi_5_scores = as.data.frame(scored$scores)
names(spi_5_scores) = spi_5_names

# add SPI-5 scores to data
data = cbind(select(data, -starts_with("q_")),
        spi_5_scores, 
        select(data, starts_with("q_")))



# Save cleaned data -------------------------------------------------------

save(data_scored, file = here("output/data_cleaned.Rdata"))
