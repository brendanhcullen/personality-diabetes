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

# create 3 spi datasets for training
datasets = list(data_spi_5 = data_scored %>% 
  select(diagnosis, spi_5_names),
  data_spi_27 = data_scored %>% 
  select(diagnosis, spi_27_names),
  data_spi_135 = data_scored %>% 
    select(diagnosis, starts_with("q_")))

# function to split datasets into training and testing

partition_spi_data = function(dataset) {
  partition = createDataPartition(dataset$diagnosis,
                      times = 1,
                      p = .8,
                      list = FALSE)
  
  train_data = dataset[partition, ] # training data 
  test_data = dataset[-partition, ] # holdout test data
  
  partitioned_data = list(train = train_data, test = test_data)

  return(partitioned_data)
}
 
# partition the 3 datasets
train_test_splits = map(datasets, partition_spi_data)


# Impute missing data -----------------------------------------------------

# manually randomly impute for now. Note: imputation will eventually occur as a "pre-processing" step
train_test_splits$data_spi_135$train = train_test_splits$data_spi_135$train %>% 
  mutate_all(Hmisc::impute, fun = "random")

train_test_splits$data_spi_135$test = train_test_splits$data_spi_135$test %>%
  mutate_all(Hmisc::impute, fun = "random") 


# Specify resampling parameters -------------------------------------------

# 10-fold repeated cross-validation with SMOTE subsampling
train_control = trainControl(method = "repeatedcv",
                             number = 2, # number of folds = 10
                             repeats = 2, # cross-validation is repeated 10 times
                             sampling = "smote") # use for resolving class imbalances

# Specify tuning grids ----------------------------------------------------

multinom_grid = expand.grid(decay = seq(from = 0, to = 0.5, by = .1))

knn_grid = expand.grid(k = seq(from = 1, to = 5, by = 1))

nnet_grid = expand.grid(size = seq(from = 1, to = 10, by = 1),
                        decay = seq(from = 0.1, to = 0.5, by = 0.1))


# Specify additional arguments --------------------------------------------

multinom_args = NULL

knn_args = NULL

nnet_args = list(MaxNWts = as.character(2000),
                 maxit = as.character(200))

# Create master df --------------------------------------------------------

# list of ML algorithms to run
model_list = list("multinom", "knn", "nnet") 

# list of corresponding tuning grids
tuning_list = map(model_list, ~get(paste0(.x, "_grid")))

# list of additonal arguments (these will be unique to each ML algorithm)
add_args_list = map(model_list, ~get(paste0(.x, "_args")))

# create master df
train_master_df = data.frame(ml_model = I(model_list), # use I() to use lists "as is"
                       tuning_grid = I(tuning_list),
                       add_args = I(add_args_list)) 

train_master_df = train_master_df[rep(1:nrow(train_master_df), times = length(train_test_splits)),]

spi_dataset_names = c("spi_5", "spi_27", "spi_135")

train_master_df = train_master_df %>% 
  mutate(spi_scoring = rep(spi_dataset_names, each = nrow(.)/length(spi_dataset_names))) %>% 
  mutate(train_data = rep(map(train_test_splits, "train"), each = nrow(.)/length(spi_dataset_names)))

# Save test data ----------------------------------------------------------

# save test data for later model evaluation
saveRDS(train_test_splits$data_spi_5$test, file = here("output/machine_learning/test_data/test_data_spi_5.RDS"))
saveRDS(train_test_splits$data_spi_27$test, file = here("output/machine_learning/test_data/test_data_spi_27.RDS"))
saveRDS(train_test_splits$data_spi_135$test, file = here("output/machine_learning/test_data/test_data_spi_135.RDS"))


# Save training info ------------------------------------------------------

saveRDS(train_master_df, file = here("output/machine_learning/training/train_master_df.RDS"))
saveRDS(train_control, file = here("output/machine_learning/training/train_control.RDS"))

