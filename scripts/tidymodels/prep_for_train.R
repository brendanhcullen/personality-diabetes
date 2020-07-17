# This script prepares data for model traning and hyperparameter tuning. 

# Load libraries ----------------------------------------------------------

library(here)
library(tidyverse)
library(tidymodels)

# Load recipes and model specs --------------------------------------------

recipes <- readRDS(here("output", "tidymodels", "recipes.RDS"))
model_specs <- readRDS(here("output", "tidymodels", "model_specs.RDS"))
tuning_grids <- readRDS(here("output", "tidymodels", "tuning_grids.RDS"))

# Create master df --------------------------------------------------------

# pull abbreviated model names out of model specs
model_names <- map_chr(names(model_specs), ~str_replace(., "_spec", ""))

# spi scoring versions
spi_versions <- c("spi_5", "spi_27", "spi_135")

# create master data frame that has all the info required to generate individual training scripts
train_master_df <- 
  # create all combinations of models and spi scorings
  expand_grid(model_name = model_names, spi_scoring = spi_versions) %>% 
  # add list column with recipes
  mutate(recipe = rep_len(recipes, length.out = length(spi_scoring))) %>% 
  # add list column with model specs
  mutate(model_spec = rep(model_specs, each = length(spi_versions))) %>% 
  # add list column with workflows that bundle recipes and model specs
  mutate(workflow = map2(model_spec, recipe, ~workflow() %>% add_model(.x) %>% add_recipe(.y))) %>% 
  # add tuning grids
  left_join(tuning_grids)

# add unique names to each workflow object
names(train_master_df$workflow) <- 
  map2_chr(train_master_df$model_name, 
           train_master_df$spi_scoring, 
           ~paste(.x, .y, "wflow", sep = "_"))

# Save data ---------------------------------------------------------------
saveRDS(train_master_df, file = here("output", "tidymodels", "train_master_df.RDS"))
