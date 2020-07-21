# This script takes in raw SAPA data, filters it according to inclusion criteria, and 
# selects only the relevant variables for pre-processing and modelling.

# Load libraries
library(tidyverse)
library(tidymodels)
library(here)
library(janitor)

# Import data -------------------------------------------------------------

# load raw data
load(here("../SAPAdata07feb2017thru18nov2019.rdata"))

# rename to 'data' and convert to tibble
data <- as_tibble(SAPAdata07feb2017thru18nov2019)

# convert data types
data <- data %>% 
  mutate(RID = as.character(RID)) %>% 
  mutate_at(c("p1occPrestige", "p1occIncomeEst", "p2occPrestige", "p2occIncomeEst"),
            as.numeric)

# remove all objects except data 
rm(list=setdiff(ls(), "data"))

# Sample fraction of data ----------------------------------

# # ***TEMPORARY FOR MORE SPEED***
# set.seed(123)
# data <- data %>% 
#   sample_frac(.2)

# Basic data cleaning -----------------------------------------------------

# source function for extracting spi names
source(here("scripts/tidymodels/get_spi_names.R"))

# read in keys
keys <- read.csv(here("data/superKey.csv"), header = TRUE, row.names = 1)

# get spi names
spi_names <- get_spi_names(keys)
spi_5_names <- spi_names$spi_5
spi_27_names <- spi_names$spi_27
spi_135_names <- spi_names$spi_135
all_spi_names <- unlist(spi_names, use.names = FALSE)

# min number of responses to spi-135 items required to be included in analysis
min_n_valid <- 27

# filter observations according to inclusion criteria
data <- data %>% 
  mutate(n_valid_135 = apply(.[,spi_135_names], 1, function(x) sum(!is.na(x)))) %>%  
  filter(!is.na(diabetes), # only people who responsed to diabetes question
         country == "USA", # only USA data
         n_valid_135 >= min_n_valid) %>%  # only people with at least 27 responses on SPI-135 items
  select(-n_valid_135)

# names of relevant demographic variables
demographic_vars <- c(
  "age", # age
  "ethnic",  # ethnicity
  "jobstatus", # current job status
  "education", "occPrestige", "occIncomeEst", # self SES
  "p1edu", "p1occPrestige", "p1occIncomeEst", # parent 1 SES
  "p2edu", "p2occPrestige", "p2occIncomeEst") # parent 2 SES

# only retain spi-135 items and demographic vars
data_clean <- data %>% 
  select(c(RID, 
           diabetes, 
           all_of(demographic_vars), 
           all_of(spi_135_names)))

# Save data -------------------------------------------------------

# create output directory if it doesn't already exist
output_dir <- here("output", "tidymodels")
if(!dir.exists(output_dir)){dir.create(output_dir)}

save(list = c("spi_5_names", "spi_27_names", "spi_135_names", "all_spi_names"), file = here("output", "tidymodels", "spi_names.Rdata"))
saveRDS(demographic_vars, file = here("output", "tidymodels", "demographic_vars.RDS"))
saveRDS(data_clean, file = here("output", "tidymodels", "data_clean.RDS"))
saveRDS(keys, file = here("output", "tidymodels", "keys.RDS"))
