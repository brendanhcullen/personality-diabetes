# This is a template script to work through a tidymodels workflow for a single model (let's start with multinomial logistic regression) 

# Load libraries
library(tidyverse)
library(tidymodels)
library(here)
library(janitor)
library(missMDA)

# Import data -------------------------------------------------------------

# load raw data
load(here("../SAPAdata07feb2017thru18nov2019.rdata"))

# rename to 'data' and convert to tibble
data <- as_tibble(SAPAdata07feb2017thru18nov2019)

data <- data %>% 
  mutate(RID = as.character(RID)) %>% 
  mutate_at(c("p1occPrestige", 
              "p1occIncomeEst",
              "p2occPrestige",
              "p2occIncomeEst"),
            as.numeric)

# remove all objects except data 
rm(list=setdiff(ls(), "data"))

# sample 1% of data for more speed
set.seed(123)
data <- data %>% 
  sample_frac(.01)


# Basic data cleaning -----------------------------------------------------

# source function for extracting spi names
source(here("scripts/preprocessing/get_spi_names.R"))

# read in keys
keys <- read.csv(here("data/superKey.csv"), header = TRUE, row.names = 1)

# get SPI names
spi_names <- get_spi_names(keys)
spi_5_names <- spi_names$spi_5
spi_27_names <- spi_names$spi_27
spi_135_names <- spi_names$spi_135
all_spi_names <- unlist(spi_names, use.names = FALSE)

# min number of responses to SPI-135 items required to be included in analysis
min_n_valid <- 27

data <- data %>% 
  mutate(n_valid_135 = apply(.[,spi_135_names], 1, function(x) sum(!is.na(x)))) %>%  
  filter(!is.na(diabetes), # only people who responsed to diabetes question
         country == "USA", # only USA data
         n_valid_135 >= min_n_valid) %>%  # only people with at least 27 responses on SPI-135 items
  select(-n_valid_135)

# only retain SPI items that are part of the SPI-135

demographic_vars <- c(
  "age", # age
  "ethnic",  # ethnicity
  "jobstatus", # current job status
  "education", "occPrestige", "occIncomeEst", # self SES
  "p1edu", "p1occPrestige", "p1occIncomeEst", # parent 1 SES
  "p2edu", "p2occPrestige", "p2occIncomeEst") # parent 2 SES

data <- data %>% 
  select(c(RID, 
          diabetes, 
          all_of(demographic_vars), 
          all_of(spi_135_names)))

# Split data --------------------------------------------------------------
set.seed(123)

data_split <- initial_split(data, strata = diabetes)

data_train <- training(data_split)
data_test <- testing(data_split)

# Pre-processing ----------------------------------------------------------

# source scripts containing custom recipe steps
custom_recipe_scripts <- list.files(here("scripts", "recipe_steps"), full.names = TRUE)
walk(custom_recipe_scripts, source)

rec <- recipe(diabetes ~ ., data_train) %>% 
  update_role(RID, new_role = "ID") %>%
  update_role(demographic_vars, new_role = "covariate") %>% 
  step_score_spi_5(spi_135_names, # score spi_5 (mean scoring)
                   keys = keys) %>%
  step_score_spi_27(spi_135_names, # score spi_27 (IRT scoring)
                    keys = keys,
                    IRT_path = here("../IRTinfoSPI27.rdata")) %>%
  step_impute_pca(spi_135_names) %>% 
  step_residualize(all_spi_names, vtc = demographic_vars, id_var = "RID")
  
# prep the recipe
rec_prep <- prep(rec)

# check out the prepped data with `bake()`
bake(rec_prep, new_data = data_train)

