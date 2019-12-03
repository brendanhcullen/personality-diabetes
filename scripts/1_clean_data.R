# This script imports raw SAPA data and performs the following cleaning operations:
# 1) score SPI data
# 2) convert variables to correct type (e.g. factors)
# 3) create composite demographic variables (SES)

# load libraries
library(here)
library(tidyverse)
library(psych)
library(janitor)


# Source pre-processing functions and import SPI keys ---------------------

source(here("scripts/0.3_score_spi.R"))
source(here("scripts/0.2_residualize.R"))

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

# get names of SPI-135 items
spi_names = get_spi_names(keys)
spi_135_names = spi_names$spi_135

# min number of responses to SPI-135 items required to be included in analysis
min_n_valid = 27
min_n_valid = 10

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

# Score SPI-5 (i.e. Big 5) ------------------------------------------------

spi_5_scores = score_spi_5(data = data, keys = keys)

# add SPI-5 scores to data
data = cbind(select(data, -starts_with("q_")),
        spi_5_scores, 
        select(data, starts_with("q_")))


# Score SPI-27 (using IRT) ------------------------------------------------

path_to_IRT_calibrations = here("data/IRTinfoSPI27.rdata") # specify where IRT calibrations file is saved
spi_27_scores = score_spi_27(data = data, 
                             keys = keys, 
                             path_to_IRT_calibrations = path_to_IRT_calibrations)

# add IRT scores to data
data = cbind(select(data, -starts_with("q_")),
             spi_27_scores,
             select(data, starts_with("q_")))



# Impute missing data -----------------------------------------------------



# Residualize -------------------------------------------------------------

# Try to break it!

VTC = c("age", "gender", "relstatus") # variables to control
VOI = c("agree", "neuro") # variables of interest
id = "RID" # name of ID variable

newdata = residualize(VOI = VOI, VTC = VTC, data = data, id = id)

# Save cleaned data -------------------------------------------------------

save(data, file = here("output/data_cleaned.Rdata"))
