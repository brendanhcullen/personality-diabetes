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

######################### IMPORT ACTUAL DATASET HERE ######################### 

# Import toy data ---------------------------------------------------------

######################### REMOVE THIS FOR REAL ANALYSIS ######################### 

# load in toy dataset
load(here("data/toydata.Rdata"))

# add RID variable to be consistent with real SAPA data
data = toydata %>% 
  rownames_to_column() %>% 
  rename(RID = rowname)

rm(toydata)

# Filter data -------------------------------------------------------------

data = data %>% 
  filter(!is.na(diabetes), # only people who responsed to diabetes question
         country == "USA") # only USA data

# Wrangle demographic vars ------------------------------------------------

## SES

# make sure occupational variables are numeric
data = data %>%
  mutate_at(vars(matches("^(p)\\d(occ)")), as.numeric)

# recode all education variables (for self, parent 1, parent 2)
recode_edu_vars = function(x){
  x = case_when(
    x == "less12yrs" ~ "1", 
    x == "HSgrad" ~ "2", 
    x == "SomeCollege" ~ "3", 
    x == "CurrentInUniv" ~ "4", 
    x == "AssociateDegree" ~ "5", 
    x == "CollegeDegree" ~ "6", 
    x == "InGradOrProSchool" ~ "7", 
    x == "GradOrProDegree" ~ "8")
  
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
  mutate(which_ses = ifelse(age <= 18 | (dplyr::between(age, 19, 26) & jobstatus == "student"), 
                            "parent", 
                            "self"),
         ses = ifelse(which_ses == "self", 
                      self_ses, 
                      parent_ses))

# select only relevant demographic vars and filter out people with missing demographic data
data = data %>% 
  select(RID, # ID numnber
         diabetes, # diabetes diagnosis
         age, ses, ethnic, # relevant demographic vars
         starts_with("q_")) %>%  # all personality vars
  filter(!is.na(age), 
         !is.na(ses),
         !is.nan(ses), # filter out NaN's as well (NaN's may have been created in addition to NA's, as this is a derived composite variable)
         !is.na(ethnic))

# Fix variable types
data = data %>% 
  mutate(diabetes = as.factor(diabetes),
         ethnic = as.factor(ethnic))


# Prep for SPI scoring ----------------------------------------------------

# import SPI scale names
source(here("src/personality_scale_names.R"))

# Read in superKey data
keys = read.csv("data/superKey.csv", header = TRUE, row.names = 1) %>% 
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


# Score SPI-27 (using IRT) ------------------------------------------------

# load info on IRT calibrations
load(here("data/IRTinfoSPI27.rdata")) # this file is in .gitignore for now

# Read in superKey data again (with uppercase names in order to be compatible with IRT code below)
keys = read.csv("data/superKey.csv", header = TRUE, row.names = 1)

######## ALL IRT CODE WAS COPIED FROM SARA'S SAPA BMI PROJECT ######## 
# more info here: https://github.com/sjweston/SAPA_BMI/tree/master/irt_troubleshoot

# IRT score
dataSet <- subset(data, select = c(orderForItems))

SPIirtScores <- matrix(nrow=dim(dataSet)[1], ncol=27)

scaleNames = gsub("SPI27_", "", names(IRToutputSPI27))
spi_keys = keys %>%
  select(matches("SPI_135")) %>%
  select(-c(1:5)) %>%
  mutate(item = rownames(.)) %>%
  gather("scale", "key", -item) %>%
  filter(key != 0)

for (i in 1:length(IRToutputSPI27)) {
  data1 <- subset(dataSet, select = c(rownames(IRToutputSPI27[[i]]$irt$difficulty[[1]])))
  calibrations <- IRToutputSPI27[[i]]
  #check calibration direction
  loadings = calibrations$fa$loadings[,1]
  loadings = ifelse(loadings < 0, -1, 1)
  loadings = data.frame(item = names(loadings), loadings = loadings)
  keys_direction = spi_keys %>%
    filter(grepl(scaleNames[i], scale)) %>%
    full_join(loadings)
  same = sum(keys_direction$key == keys_direction$loadings)
  if(same == 0) data1[,1:ncol(data1)] = apply(data1[,1:ncol(data1)], 2, function(x) max(x, na.rm=TRUE) + 1 - x)
  if (same > 0 & same < 5) print("Error in loadings")
  scored <- scoreIrt(calibrations, data1, keys = NULL, cut = 0)
  SPIirtScores[,i] <- scored$theta1
}

SPIirtScores <- as.data.frame(SPIirtScores)
colnames(SPIirtScores) = spi_27_names

# add IRT scores to data
data = cbind(select(data, -starts_with("q_")),
             SPIirtScores,
             select(data, starts_with("q_")))

# Save cleaned data -------------------------------------------------------

save(data, file = here("output/data_cleaned.Rdata"))
