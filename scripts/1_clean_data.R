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

# import SPI scale names
source(here("src/personality_scale_names.R"))

# min number of responses to SPI-135 items required to be included in analysis
min_responses_allowed = 135
#min_responses_allowed = 27

data = data %>% 
  mutate(n_missing_135 = apply(.[,spi_135_names], 1, function(x) sum(is.na(x)))) %>%  
  filter(!is.na(diabetes), # only people who responsed to diabetes question
         country == "USA", # only USA data
         n_missing_135 <= 135 - min_responses_allowed) %>%  # only people with at least 27 responses on SPI-135 items
  select(-n_missing_135)


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
