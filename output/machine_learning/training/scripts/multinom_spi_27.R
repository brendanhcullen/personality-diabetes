# This script applies the machine learning algorithm 'multinom' using the 'spi_27' dataset as input features

# load libraries
library(here)
library(tidyverse)
library(caret)
library(doParallel)
library(DMwR) # for smote sub-sampling
library(e1071) # seems to be required for all ML algorithms

# load in relevant info for model training
train_master_df = readRDS(here('/output/machine_learning/training/train_master_df.RDS'))
train_control = readRDS(here('/output/machine_learning/training/train_control.RDS'))

# select training data 
train_data = train_master_df %>% 
  filter(ml_model == deparse(substitute(multinom)) & spi_scoring == deparse(substitute(spi_27))) %>% 
  select(train_data) %>% 
  map_df(1)

# Set up parallelization
number_of_cores = 4
cluster = makePSOCKcluster(number_of_cores)
registerDoParallel(cluster)

 # set seed for reproducibility
set.seed(010320)
  
# train the model 
model = train(diabetes ~ .,
              data = train_data,
              method = deparse(substitute(multinom)), 
              trControl = train_control,
              metric = 'Kappa') 
  
# specify where to save model output
filename = 'multinom_spi_27_fit.RDS'
output_dir = here('output/machine_learning/training/model_fits/')

# save model output
saveRDS(model, file = paste0(output_dir, filename)) 

# Stop the parallelization
stopCluster(cluster)
