
library(here)
library(tidyverse)
library(caret)

# Load holdout test data --------------------------------------------------

best_model = readRDS(here("output/machine_learning/training/best_model.RDS"))
test_data_pp = readRDS(here("output/machine_learning/testing/test_data_pp.RDS"))

# Evaluate best model on test data ----------------------------------------

# pull out final model
best_model = best_model$finalModel

# use model to predict diabetes status in test data
predicted <- predict(best_model, test_data_pp)

# confusion matrix
confusion_matrix <- confusionMatrix(predicted, test_data_pp$diabetes)
confusion_matrix$table
confusion_matrix$byClass
