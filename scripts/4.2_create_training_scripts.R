
library(here)
library(tidyverse)
library(glue)

# load in relevant informatiom
train_master_df = readRDS(here("/output/machine_learning/training/train_master_df.RDS"))
train_control = readRDS(here("/output/machine_learning/training/train_control.RDS"))

# function to add additional arguments (wherever needed) when specifying the options for model training
insert_add_args = function(add_args) { 
  default_text = "
# train the model 
model = train(diagnosis ~ .,
              data = train_data,
              method = deparse(substitute(ml_model_name)), 
              trControl = train_control,
              tuneGrid = tuning_grid,
              metric = 'Kappa'"
  
  for (i in 1:length(add_args)) { 
    arg_name = names(add_args[i])
    a = add_args[i]
    default_text = glue(default_text, ", \n {arg_name} = {a}")
    }
  glue(default_text, ")")
  }

# test the function
insert_add_args(add_args_list[[3]])
map(add_args_list, insert_add_args)


# function to create individual model training scripts for each ML algorithm
create_script = function(ml_model, tuning_grid, spi_scoring, train_data) {
  script = "# This script applies the machine learning algorithm 'ml_model_name' using the 'spi_scoring_name' dataset as input features

# load libraries
library(here)
library(tidyverse)
library(caret)

# load in relevant info for model training
train_master_df = readRDS(here('/output/machine_learning/training/train_master_df.RDS'))
train_control = readRDS(here('/output/machine_learning/training/train_control.RDS'))

# select training data 
train_data = train_master_df %>% 
  filter(ml_model == deparse(substitute(ml_model_name)) & spi_scoring == deparse(substitute(spi_scoring_name))) %>% 
  select(train_data) %>% 
  map_df(1)

# select tuning grid  
tuning_grid = train_master_df %>% 
  filter(ml_model == deparse(substitute(ml_model_name)) & spi_scoring == deparse(substitute(spi_scoring_name))) %>% 
  select(tuning_grid) %>% 
  map_df(1)

# train the model 
model = train(diagnosis ~ .,
               data = train_data,
               method = deparse(substitute(ml_model_name)), 
               trControl = train_control,
               tuneGrid = tuning_grid,
               metric = 'Kappa')

# specify where to save model output
filename = 'ml_model_name_spi_scoring_name_fit.RDS'
output_dir = here('output/machine_learning/training/model_fits/')

# save model output
saveRDS(model, file = paste0(output_dir, filename)) 
"
  # substitute relevant strings
  new_script = gsub("ml_model_name", ml_model, script) %>% 
    gsub("spi_scoring_name", spi_scoring, .) 
    
  # specify where to save resulting .R script
  filename = paste0(ml_model, "_", spi_scoring, ".R")
  output_dir = here("output/machine_learning/training/scripts/")
  
  # create .R script
  write_file(new_script, path = paste0(output_dir, filename))
}

# create .R scripts for all machine learning models
pmap(train_master_df, create_script)

