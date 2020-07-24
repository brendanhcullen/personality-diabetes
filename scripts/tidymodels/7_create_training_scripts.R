# This script reads in train_master_df (created by prep_for_train.R) and interates across this data frame
# to create individual scripts that will conduct model training and hyperparameter tuning for each 
# unique combination of machine learning model and spi dataset. These scripts can be deployed in parallel on 
# a high performace computing (HPC) cluster for greater computational efficiency. 

library(tidyverse)
library(glue)
library(here)

# Load info ---------------------------------------------------------------

train_master_df <- readRDS(here("output", "tidymodels", "train_master_df.RDS"))


# Define function to create scripts ---------------------------------------

create_script <- function(model_name, 
                          spi_scoring, 
                          workflow,
                          script_output_dir = here::here("output", "tidymodels", "training_scripts"),
                          fit_output_dir = here::here("output", "tidymodels", "model_fits")){ 
  
  # create output directories if they don't already exist
  if(!dir.exists(script_output_dir)){dir.create(script_output_dir)}
  if(!dir.exists(fit_output_dir)){dir.create(fit_output_dir)}
  
  # load info needed for all scripts
  setup <- 
    glue("
    # load libraries
    library(here)
    library(tidyverse)
    library(tidymodels)
    library(doParallel)
    library(janitor)
    library(themis)
    
    # load required info
    data_train <- readRDS(here('output', 'tidymodels', 'data_train.RDS'))
    cv_folds <- readRDS(here('output', 'tidymodels', 'cv_folds.RDS'))
    train_master_df <- readRDS(here('output', 'tidymodels', 'train_master_df.RDS'))
    keys <- readRDS(here('output', 'tidymodels', 'keys.RDS'))
    demographic_vars <- readRDS(here('output', 'tidymodels', 'demographic_vars.RDS'))
    load(here('output', 'tidymodels', 'spi_names.Rdata'))
    
    # source scripts containing custom recipe steps
    custom_recipe_scripts <- list.files(here('scripts', 'tidymodels', 'custom_rec_steps'), pattern = 'step_*', full.names = TRUE)
    walk(custom_recipe_scripts, source)
    
        ")

  # extract workflow
  workflow <- 
    glue("
    # select workflow
    wflow <- 
      train_master_df %>% 
      filter(model_name == '{model_name}' & spi_scoring == '{spi_scoring}') %>% 
      pull(workflow) %>% 
      pluck(1)
      
        ")
  
  # extract tuning grid
  tuning_grid <- 
    glue("
    # select tuning grid
    tuning_grid <- 
      train_master_df %>% 
      filter(model_name == '{model_name}' & spi_scoring == '{spi_scoring}') %>% 
      pull(tuning_grid) %>% 
      pluck(1)
      
        ")

  # load additional libraries
  extra_libs <- ""
  
  if(model_name == "lda") {
    extra_libs = "library(discrim)"
  }
  
  # perform hyperparameter tuning  
  tune <- 
    glue("
  # hyperparamter tuning
  
  ## initialize parallelization
  num_cores <- parallel::detectCores(logical = FALSE)
  cl <- parallel::makeForkCluster(num_cores)
  doParallel::registerDoParallel(cl)
  
  set.seed(123)
  tune_res <- tune_grid(
    wflow,
    resamples = cv_folds,
    grid = tuning_grid,
    metrics = metric_set(kap, accuracy, roc_auc),
    control = control_resamples(verbose = TRUE,
                              save_pred = TRUE))
  
  ## stop parallelization                            
  stopCluster(cl)
                              
         ")
  
  # save results from model tuning
  save_fit <- 
    glue("
    # specify where to save results of model tuning
    filename <- '{model_name}_{spi_scoring}_fit.RDS'
    
    # save results of model tuning
    saveRDS(tune_res, file = paste('{fit_output_dir}', filename, sep = '/')) 
    
        ")

  # combine script chunks together
  full_script <- paste(setup, 
                       workflow, 
                       tuning_grid, 
                       extra_libs,
                       tune, 
                       save_fit, 
                       sep = "\n")
  
  # specify where to save resulting .R script
  filename <-  paste0(model_name, "_", spi_scoring, ".R")
  
  # create .R script
  write_file(full_script, path = paste(script_output_dir, filename, sep = "/"))
}


# Create scripts ----------------------------------------------------------

# create separate training scripts for each unique model/spi combination
train_master_df <- train_master_df %>% 
  select(model_name, spi_scoring, workflow)

pmap(train_master_df, create_script)

