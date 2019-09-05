# This script prepares data for model traning and hyperparameter tuning. 

# Load libraries ----------------------------------------------------------

library(here)
library(tidyverse)
library(caret)
library(janitor)

# Load data ---------------------------------------------------------------

load(here("output/data_cleaned.Rdata"))

set.seed(081919)

# Wrangle data and partition -----------------------------------------------

# get SPI names
keys = read.table(url("https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/TZJGAT/YGMHBT"), 
                  header = TRUE, 
                  row.names = 1)

keys = keys[names(data_scored), ] %>% 
  clean_names() %>% 
  select(contains("spi_135")) %>% 
  names()

spi_names = gsub("spi_135_27_5_", "", keys)
spi_5_names = spi_names[1:5]
spi_27_names = spi_names[6:32]
rm(keys, spi_names)

# convert diagnosis to a factor
data_scored = data_scored %>% 
  mutate(diagnosis = as.factor(diagnosis))

# create 3 datasets 
data_spi135 = data_scored %>% 
  select(diagnosis, starts_with("q_"))

data_spi5 = data_scored %>% 
  select(diagnosis, spi_5_names)

data_spi27 = data_scored %>% 
  select(diagnosis, spi_27_names)
 
# partition into training (80%) and testing (20%)
partition = createDataPartition(data_ml$diagnosis,
                                   times = 1,
                                   p = .8,
                                   list = FALSE)

train_data = data_ml[partition, ] # training data (note: this will be iteratively split into train and validation sets during k-fold cross-validation.)
test_data = data_ml[-partition, ] # holdout test data. Don't use this until evaluating final model performance.

# specify cross-validation and resampling parameters
train_control = trainControl(method = "repeatedcv",
                   number = 2, # number of folds = 10
                   repeats = 2, # cross-validation is repeated 10 times
                   sampling = "smote", # use for resolving class imbalances
                   returnResamp = "final") # only return results of final model

rm(data_ml, data_scored, partition)

# Impute missing data -----------------------------------------------------

# manually randomly impute for now. Note: imputation will eventually occur as a "pre-processing" step
train_data = train_data %>% 
  mutate_all(Hmisc::impute, fun = "random") %>% 
  filter(row_number() %in% sample(1:nrow(.), size = 2000, replace = FALSE))

test_data = test_data %>%
  mutate_all(Hmisc::impute, fun = "random") 

# Specify tuning grids ----------------------------------------------------

multinom_grid = expand.grid(decay = seq(from = 0, to = 0.5, by = .1))

knn_grid = expand.grid(k = seq(from = 1, to = 5, by = 1))

nnet_grid = expand.grid(size = seq(from = 1, to = 10, by = 1),
                        decay = seq(from = 0.1, to = 0.5, by = 0.1))

# Save test data ----------------------------------------------------------

# keep test data separate for later model evaluation
save(test_data, file = here("output/machine_learning/test_data.Rdata"))
rm(test_data)
