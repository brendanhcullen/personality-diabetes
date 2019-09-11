# This script applies the machine learning algorithm 'multinom' using the 'spi_135' dataset as input features

# load libraries
library(here)
library(tidyverse)
library(caret)

# load in relevant info for model training
train_master_df = readRDS(here('/output/machine_learning/training/train_master_df.RDS'))
train_control = readRDS(here('/output/machine_learning/training/train_control.RDS'))

# select training data 
train_data = train_master_df %>% 
  filter(ml_model == deparse(substitute(multinom)) & spi_scoring == deparse(substitute(spi_135))) %>% 
  select(train_data) %>% 
  map_df(1)

# select tuning grid  
tuning_grid = train_master_df %>% 
  filter(ml_model == deparse(substitute(multinom)) & spi_scoring == deparse(substitute(spi_135))) %>% 
  select(tuning_grid) %>% 
  map_df(1)

# train the model 
model = train(diagnosis ~ .,
               data = train_data,
               method = deparse(substitute(multinom)), 
               trControl = train_control,
               tuneGrid = tuning_grid,
               metric = 'Kappa')

# specify where to save model output
filename = 'multinom_spi_135_fit.RDS'
output_dir = here('output/machine_learning/training/model_fits/')

# save model output
saveRDS(model, file = paste0(output_dir, filename)) 
