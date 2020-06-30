# This is a template script to work through a tidymodels workflow for a single model (let's start with multinomial logistic regression) 

# Load libraries
library(tidyverse)
library(tidymodels)
library(here)
library(janitor)

# Import data -------------------------------------------------------------

# load raw data
load(here("../SAPAdata07feb2017thru18nov2019.rdata"))

# rename to 'data'
data <- SAPAdata07feb2017thru18nov2019

# remove all objects except data 
rm(list=setdiff(ls(), "data"))

# sample 10% of data for more speed
data <- data %>% 
  sample_frac(.1)


# Basic data cleaning -----------------------------------------------------

# source function for extracting spi names
source(here("scripts/preprocessing/get_spi_names.R"))

# read in keys
keys = read.csv(here("data/superKey.csv"), header = TRUE, row.names = 1)

# get SPI names
spi_names = get_spi_names(keys)
spi_5_names = spi_names$spi_5
spi_27_names = spi_names$spi_27
spi_135_names = spi_names$spi_135
all_spi_names = unlist(spi_names, use.names = FALSE)

# min number of responses to SPI-135 items required to be included in analysis
min_n_valid = 27

data = data %>% 
  mutate(n_valid_135 = apply(.[,spi_135_names], 1, function(x) sum(!is.na(x)))) %>%  
  filter(!is.na(diabetes), # only people who responsed to diabetes question
         country == "USA", # only USA data
         n_valid_135 >= min_n_valid) %>%  # only people with at least 27 responses on SPI-135 items
  select(-n_valid_135)

# only retain SPI items that are part of the SPI-135
data = data %>% 
  select(all_of(spi_135_names)) %>% 
  cbind(select(data, -starts_with("q_")), .)


# Split data --------------------------------------------------------------
set.seed(123)

data_split <- initial_split(data, strata = diabetes)

data_train <- training(data_split)
data_test <- testing(data_split)

# Pre-processing ----------------------------------------------------------

source(here("scripts", "step_score_spi.R"))


rec <- recipe(diabetes ~ ., 
       data_train) %>% 
  step_score_spi(all_of(spi_5_names), keys = keys) # custom step to score spi variables (start with just spi 5 for now)

# prep the recipe
rec_prepped <- prep(rec)

# check out the prepped data with `bake()`
bake(rec_prepped, newdata = data_train)

