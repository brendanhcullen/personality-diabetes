
library(here)
library(tidyverse)
library(glue)

# load in relevant informatiom
train_master_df = readRDS(here("output/machine_learning/training/train_master_df.RDS"))
train_control = readRDS(here("output/machine_learning/training/train_control.RDS"))

# function to add additional arguments (wherever needed) when specifying the options for model training
insert_add_args = function(add_args) {
  # this is the default code when no additional arguments are necessary
  text = "
# set seed for reproducibility
set.seed(010320)
  
# train the model 
model = train(diabetes ~ .,
              data = train_data,
              method = deparse(substitute(ml_model_name)), 
              trControl = train_control,
              metric = 'Kappa'"
  
  for (i in seq_along(add_args)) { 
    arg_name = names(add_args[i])
    arg_value = add_args[i]
    text = glue(text, ", \n {arg_name} = {arg_value}") # modify default text by adding additional arguments (if there are any)
  }
  
  text = glue(text, ")") # add closing parenthesis
  
  return(text)
  }

# function to create individual model training scripts for each ML algorithm
create_script = function(ml_model, add_args, spi_scoring, train_data) {
  script_chunk_1 = "# This script applies the machine learning algorithm 'ml_model_name' using the 'spi_scoring_name' dataset as input features

# load libraries
library(here)
library(tidyverse)
library(caret)
library(doParallel)
library(DMwR) # for smote sub-sampling
library(e1071) # seems to be required for all ML algorithms
"

# Load additional libraries for each ML algorithm as needed
script_chunk_2 = "" 

if(ml_model == "multinom") {
  script_chunk_2 = "library(nnet)"
}
else if(ml_model == "rf") {
  script_chunk_2 = "library(randomForest)"
}
else if (ml_model == "svmRadial") {
  script_chunk_2 = "library(kernlab)"
}
else if (ml_model == "rpart2") {
  script_chunk_2 = "library(rpart)"
}
else if (ml_model == "rpart") {
  script_chunk_2 = "library(rpart)"
}

script_chunk_3 = "

# load in relevant info for model training
train_master_df = readRDS(here('output/machine_learning/training/train_master_df.RDS'))
train_control = readRDS(here('output/machine_learning/training/train_control.RDS'))

# select training data 
train_data = train_master_df %>% 
  filter(ml_model == deparse(substitute(ml_model_name)) & spi_scoring == deparse(substitute(spi_scoring_name))) %>% 
  select(train_data) %>% 
  map_df(1)

# Set up parallelization
number_of_cores = 6
cluster = makePSOCKcluster(number_of_cores)
registerDoParallel(cluster)

"

  script_chunk_4 = insert_add_args(add_args)

  script_chunk_5 = "
  
# specify where to save model output
filename = 'ml_model_name_spi_scoring_name_fit.RDS'
output_dir = here('output/machine_learning/training/model_fits/')

# save model output
saveRDS(model, file = paste0(output_dir, filename)) 

# Stop the parallelization
stopCluster(cluster)
"
  # combine 3 parts of scripts into one full script
  full_script = paste(script_chunk_1, script_chunk_2, script_chunk_3, script_chunk_4, script_chunk_5)
  
  # substitute relevant strings
  new_script = gsub("ml_model_name", ml_model, full_script) %>% 
    gsub("spi_scoring_name", spi_scoring, .) 
    
  # specify where to save resulting .R script
  filename = paste0(ml_model, "_", spi_scoring, ".R")
  output_dir = here("output/machine_learning/training/scripts/")
  
  # create .R script
  write_file(new_script, path = paste0(output_dir, filename))
}

# create .R scripts for all machine learning models
pmap(train_master_df, create_script)

