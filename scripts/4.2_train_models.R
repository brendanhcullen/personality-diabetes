
library(here)
library(tidyverse)
library(caret)
library(nnet)
library(Hmisc)
library(e1071)
library(DMwR) # for smote sampling

source(here("scripts/4.1_prep_for_training.R"))

# Train models ------------------------------------------------------------

# list of ML algorithms to run
model_list = list("multinom", "knn", "nnet") 

# list of corresponding tuning grids
tuning_list = map(model_list, ~get(paste0(.x, "_grid")))

# function to train models automatically
run_training = function(model_name, tuning_grid) {
  set.seed(081919)
  model = train(diagnosis ~ .,
                 data = train_data,
                 method = model_name, 
                 trControl = train_control,
                 tuneGrid = tuning_grid,
                 metric = "Kappa")
  
  return(model)
}
 
# train the models
trained_models = map2(model_list, tuning_list, run_training)

# name each model in output list
names(trained_models) = model_list

# Save model output -------------------------------------------------------

# save list of trained models
save(trained_models, file = here("output/machine_learning/trained_models.Rdata"))
