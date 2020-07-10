
# Load libraries
library(tidyverse)
library(tidymodels)
library(here)
library(janitor)
library(missMDA) # for imputing missing data
library(themis) # for SMOTE subsampling

# Import data -------------------------------------------------------------

# load raw data
load(here("../SAPAdata07feb2017thru18nov2019.rdata"))

# rename to 'data' and convert to tibble
data <- as_tibble(SAPAdata07feb2017thru18nov2019)

# convert data types
data <- data %>% 
  mutate(RID = as.character(RID)) %>% 
  mutate_at(c("p1occPrestige", 
              "p1occIncomeEst",
              "p2occPrestige",
              "p2occIncomeEst"),
            as.numeric)

# remove all objects except data 
rm(list=setdiff(ls(), "data"))

# sample fraction of data for more speed
set.seed(123)
data <- data %>% 
  sample_frac(.2)

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
data <- data %>% 
  select(c(RID, 
           diabetes, 
           all_of(demographic_vars), 
           all_of(spi_135_names)))

# Split data --------------------------------------------------------------
set.seed(123)

# split data and stratify by diabetes 
# i.e. ensure equal proportions of diabetes groups in training and testing
data_split <- initial_split(data, strata = diabetes)

# extract training and testing data
data_train <- training(data_split)
data_test <- testing(data_split)

# Save cleaned data -------------------------------------------------------
saveRDS(spi_names, file = here("output/tidymodels/spi_names.RDS"))
saveRDS(data_train, file = here("output/tidymodels/data_train.RDS"))
saveRDS(data_test, file = here("output/tidymodels/data_test.RDS"))
saveRDS(data_split, file = here("output/tidymodels/data_split.RDS"))
