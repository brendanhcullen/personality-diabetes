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

# import helper functions
source(here("src/helper_functions.R"))

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

# recode all education variables to numeric values -- TURN THIS INTO A FUNCTION
data = data %>%
  mutate(education = case_when(
    education == "less12yrs" ~ "6", 
    education == "HSgrad" ~ "12", 
    education == "SomeCollege" ~ "14", 
    education == "CurrentInUniv" ~ "14", 
    education == "AssociateDegree" ~ "14", 
    education == "CollegeDegree" ~ "16", 
    education == "InGradOrProSchool" ~ "18", 
    education == "GradOrProDegree" ~ "20")) %>% 
  mutate(education = as.numeric(education))

data = data %>%
  mutate(p1edu = case_when(
    p1edu == "less12yrs" ~ "6", 
    p1edu == "HSgrad" ~ "12", 
    p1edu == "SomeCollege" ~ "14", 
    p1edu == "CurrentInUniv" ~ "14", 
    p1edu == "AssociateDegree" ~ "14", 
    p1edu == "CollegeDegree" ~ "16", 
    p1edu == "InGradOrProSchool" ~ "18", 
    p1edu == "GradOrProDegree" ~ "20")) %>% 
  mutate(p1edu = as.numeric(p1edu))

data = data %>%
  mutate(p2edu = case_when(
    p2edu == "less12yrs" ~ "6", 
    p2edu == "HSgrad" ~ "12", 
    p2edu == "SomeCollege" ~ "14", 
    p2edu == "CurrentInUniv" ~ "14",   
    p2edu == "AssociateDegree" ~ "14", 
    p2edu == "CollegeDegree" ~ "16", 
    p2edu == "InGradOrProSchool" ~ "18", 
    p2edu == "GradOrProDegree" ~ "20")) %>% 
  mutate(p2edu = as.numeric(p2edu))

# Remove missing data for SES???
# data = data %>%
#   filter(!is.na(p1edu) | !is.na(p2edu) |
#         !is.na(p1occIncomeEst) | !is.na(p2occIncomeEst) |
#         !is.na(p1occPrestige) | !is.na(p2occPrestige))

# estimate SES composite
data = data %>%
  mutate(z.education = scale(education),
         z.occIncomeEst = scale(occIncomeEst),
         z.occPrestige = scale(occPrestige),
         z.p1edu = scale(p1edu),
         z.p2edu = scale(p2edu),
         z.p1occIncomeEst = scale(p1occIncomeEst),
         z.p2occIncomeEst = scale(p2occIncomeEst),
         z.p1occPrestige = scale(p1occPrestige),
         z.p2occPrestige = scale(p2occPrestige)) %>% 
  mutate(ses = rowMeans(.[,grepl("^z\\.", names(.))], na.rm=TRUE)) %>% 
  select(-starts_with("z"))

## ETHNICITY

# RE-CODE ETHNICITY VARIABLE HERE

# select only relevant demographic vars
data = data %>% 
  select(RID, # ID numnber
         diagnosis, # diabetes diagnosis
         age, ses, ethnic, # relevant demographic vars
         starts_with("q_")) # all personality vars

# Score SPI-135 data ----------------------------------------------------------

#data_scored = score_spi(data)

keys = read.table(url("https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/TZJGAT/YGMHBT"), 
                  header = TRUE, 
                  row.names = 1)

# select just the rows that correspond to variables in the current SAPA dataset
keys = keys[names(data), ]

# select just the scales that are scored using the SPI_135 form
keys = keys %>%
  select(contains("SPI_135"))

spi_135_indices = grepl("q_", levels(spi.dictionary$item_id))
spi_135_names = subset(levels(spi.dictionary$item_id), spi_135_indices)

tmp1 <- data %>% 
  select(-starts_with("q_")) 
tmp2 <- data %>% 
  select(spi_135_names) #
data <- cbind(tmp1, tmp2)


# score the items (this contains item and scale statistics too!)
scored = scoreItems(keys, data)
spi_scores = as.data.frame(scored$scores)
names(spi_scores) = gsub("SPI_135_27_5_", "", names(spi_scores))

# add scores to data
data_scored = cbind(data, spi_scores) %>% 
  clean_names()

spi_names = gsub("SPI_135_27_5_", "", names(keys))
spi_5_names = spi_names[1:5]
spi_27_names = spi_names[6:32]

# Fix variable types ------------------------------------------------------

# change char vars to factors 
data_scored = fix_var_types(data_scored)


# Save cleaned data -------------------------------------------------------

save(data_scored, file = here("output/data_cleaned.Rdata"))
