
library(here)
library(tidyverse)
library(caret)
library(nnet)
library(Hmisc)
library(e1071)
library(DMwR) # for smote sampling

train_master_df = readRDS(here("/output/machine_learning/training/train_master_df.RDS"))
train_control = readRDS(here("/output/machine_learning/training/train_control.RDS"))


create_script = function(ml_model, tuning_grid, spi_scoring, train_data) {
  script = "
library(caret)

model = train(diagnosis ~ .,
               data = train_data,
               method = ml_model, 
               trControl = train_control,
               tuneGrid = tuning_grid,
               metric = 'Kappa')

filename = 'ml_model_spi_scoring_fit.RDS'
output_dir = here('output/machine_learning/training/')

saveRDS(model, file = paste0(output_dir, filename)) 
"
  new_script = gsub("train_data", train_data, script, fixed = TRUE) %>% 
    gsub("ml_model", ml_model, ., fixed = TRUE) %>% 
    gsub("tuning_grid", tuning_grid, ., fixed = TRUE) %>% 
    gsub("spi_scoring", spi_scoring, ., fixed = TRUE)
  
  filename = paste0(ml_model, "_", spi_scoring, ".R")
  output_dir = here("output/machine_learning/training/")
  
  write_file(new_script, path = paste0(output_dir, filename))
}

pmap(train_master_df, create_script)
