# This script prepares data for model traning and hyperparameter tuning. 

# Load libraries ----------------------------------------------------------

library(here)
library(tidyverse)
library(caret)

# Load training data ------------------------------------------------------

spi_names = readRDS(here("output/spi_names.RDS"))
train_data_pp = readRDS(here("/output/machine_learning/training/train_data_pp.RDS"))

# Split training data into 3 subsets --------------------------------------

# convert diabetes to a factor
train_data_pp = train_data_pp %>% 
  mutate(diabetes = as.factor(diabetes))

train_data_split = list(train_spi_5 = train_data_pp %>% select(diabetes, spi_names$spi_5), 
                        train_spi_27 = train_data_pp %>% select(diabetes, spi_names$spi_27),
                        train_spi_135 = train_data_pp %>% select(diabetes, spi_names$spi_135))

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

train_master_df = train_master_df[rep(1:nrow(train_master_df), times = length(train_data_split)),]

spi_dataset_names = c("spi_5", "spi_27", "spi_135")

train_master_df = train_master_df %>% 
  mutate(spi_scoring = rep(spi_dataset_names, each = nrow(.)/length(spi_dataset_names))) %>% 
  mutate(train_data = rep(train_data_split, each = nrow(.)/length(spi_dataset_names)))

# Save training info ------------------------------------------------------

saveRDS(train_master_df, file = here("output/machine_learning/training/train_master_df.RDS"))
saveRDS(train_control, file = here("output/machine_learning/training/train_control.RDS"))

