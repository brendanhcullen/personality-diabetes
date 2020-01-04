# This script imports raw SAPA data and performs the following cleaning operations:
# 1) score SPI data
# 2) convert variables to correct type (e.g. factors)
# 3) create composite demographic variables (SES)

# load libraries
library(here)
library(tidyverse)
library(psych)
library(janitor)
library(missMDA)
library(caret)

# Source pre-processing functions and import SPI keys ---------------------

source(here("scripts/preprocessing/preprocess.R"))

# read in keys for SPI scoring
keys = read.csv(here("data/superKey.csv"), header = TRUE, row.names = 1)

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

# get SPI names
spi_names = get_spi_names(keys)
spi_5_names = spi_names$spi_5
spi_27_names = spi_names$spi_27
spi_135_names = spi_names$spi_135
all_spi_names = unlist(spi_names, use.names = FALSE)

# min number of responses to SPI-135 items required to be included in analysis
#min_n_valid = 27
min_n_valid = 10 # just for toy data

data = data %>% 
  mutate(n_valid_135 = apply(.[,spi_135_names], 1, function(x) sum(!is.na(x)))) %>%  
  filter(!is.na(diabetes), # only people who responsed to diabetes question
         country == "USA", # only USA data
         n_valid_135 >= min_n_valid) %>%  # only people with at least 27 responses on SPI-135 items
  select(-n_valid_135)

# only retain SPI items that are part of the SPI-135
data = data %>% 
  select(spi_135_names) %>% 
  cbind(select(data, -starts_with("q_")), .)


# Partition data into training and testing --------------------------------

partition = createDataPartition(data$diabetes,
                                times = 1,
                                p = .75,
                                list = FALSE)

train_data = data[partition, ] # training data 
test_data = data[-partition, ] # holdout test data


# Preprocess data ---------------------------------------------------------

# specify demographic variables to use as covariates when residualizing
demographic_vars = c(
  "age", # age
  "ethnic",  # ethnicity
  "jobstatus", # current job status
  "education", "occPrestige", "occIncomeEst", # self SES
  "p1edu", "p1occPrestige", "p1occIncomeEst", # parent 1 SES
  "p2edu", "p2occPrestige", "p2occIncomeEst") # parent 2 SES

# pre-process train data
train_data_pp = preprocess_sapa(data = train_data, 
                                keys = keys, 
                                id = "RID", 
                                VOI = all_spi_names, 
                                covariates = demographic_vars, 
                                IRT_path = here("data/IRTinfoSPI27.rdata"), 
                                order = c("score", "impute", "residualize"))

# pre-process test data
test_data_pp = preprocess_sapa(data = test_data, 
                               keys = keys, 
                               id = "RID", 
                               VOI = all_spi_names, 
                               covariates = demographic_vars, 
                               IRT_path = here("data/IRTinfoSPI27.rdata"), 
                               order = c("score", "impute", "residualize"))

# Save cleaned data -------------------------------------------------------
saveRDS(spi_names, file = here("output/spi_names.RDS"))
saveRDS(data, file = here("output/data_filtered.RDS"))
saveRDS(train_data_pp, file = here("output/train_data_pp.RDS"))
saveRDS(test_data_pp, file = here("output/test_data_pp.RDS"))
