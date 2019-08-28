


# Train models ------------------------------------------------------------

# list of ML algorithms to run
model_list = list("multinom", "knn", "nnet") 

# list of corresponding tuning grids
tuning_list = map(model_list, ~get(paste0(.x, "_grid")))

# function to train models automatically
run_training <- function(model_name, tuning_grid) {
  set.seed(081919)
  model <- train(diagnosis ~ .,
                 data = train_data,
                 method = model_name, 
                 trControl = train_control,
                 tuneGrid = tuning_grid,
                 metric = "Kappa")
  
  return(model)
}
 
# train the models
trained_models = map2(model_list, tuning_list, run_training)
names(trained_models) <- model_list


# Save model output -------------------------------------------------------

# save list of trained models
save(trained_models, file = here("output/ml_training/trained_models.Rdata"))
