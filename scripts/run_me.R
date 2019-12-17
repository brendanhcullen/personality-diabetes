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


# Source pre-processing functions and import SPI keys ---------------------

source(here("scripts/0.3_score_spi.R"))
source(here("scripts/0.2_residualize.R"))
source(here("scripts/0.4_impute.R"))

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

# Residualize -------------------------------------------------------------

demographic_vars = c(
  "age", # age
  "ethnic",  # ethnicity
  "jobstatus", # current job status
  "education", "occPrestige", "occIncomeEst", # self SES
  "p1edu", "p1occPrestige", "p1occIncomeEst", # parent 1 SES
  "p2edu", "p2occPrestige", "p2occIncomeEst") # parent 2 SES

# convert relevant variables to factors
data = data %>% 
  mutate_at(c("ethnic", "jobstatus", "education", "p1edu", "p2edu"), as.factor)

VOI = all_spi_names
VTC = demographic_vars
id = "RID"

# extract residuals 
residualized_data = residualize(VOI = all_spi_names, VTC = demographic_vars, data = data, id = "RID")

rbind(describe(data[,VOI], fast = T),
                     describe(residualized_data[,VOI], fast = T)) %>%
  mutate(g1 = rep(c("raw scores", "residualized scores"), each = length(VOI)),
         category = rep(c(rep("Big 5", 5), rep("SPI 27", 27), rep("Items", 135)), 2)) %>%
  ggplot(aes(x = -vars, y = sd, fill = category)) +
  geom_bar(stat = "identity") +
  coord_flip()+
  facet_wrap(~g1)
