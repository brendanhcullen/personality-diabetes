
# list of ML algorithms to run
model_list = list("multinom", "knn") 

# list of corresponding tuning grids
tuning_list = map(model_list, ~get(paste0(.x, "_grid")))

# function to 
run_training <- function(model_name, tuning_grid) {
  set.seed(081919)
  model <- train(diagnosis ~ .,
                 data = train_data,
                 method = model_name, 
                 trControl = train_control,
                 tuneGrid = tuning_grid,
                 metric = "Kappa")
  
  #assign(paste0(model_name, "_fit"), model)
  
  return(model)
}
 
# train the models
trained_models = map2(model_list, tuning_list, run_training)
names(trained_models) <- model_list

# save list of trained models
save(trained_models, file = here("output/ml_training/trained_models.Rdata"))


# file info for saving trained models
trained_model_names = map_chr(trained_models, "method")
model_output_dir = here("output/ml_training/")
filenames = map_chr(trained_model_names, ~paste0(model_output_dir, .x, ".Rds"))

# save trained models to output folder
walk2(trained_models, filenames, ~saveRDS(.x, file = .y))
