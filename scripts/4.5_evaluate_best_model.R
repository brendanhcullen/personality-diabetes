
library(here)
library(tidyverse)
library(caret)

# Load holdout test data --------------------------------------------------

load(here("output/machine_learning/best_model.Rdata"))
load(here("output/machine_learning/test_data.Rdata"))

# Evaluate best model on test data ----------------------------------------

# pull out final model
best_model = best_model$finalModel

# use model to predict diagnosis values in test data
predicted <- predict(best_model, test_data)

# confusion matrix
confusion_matrix <- confusionMatrix(predicted, test_data$diagnosis)
confusion_matrix$table
confusion_matrix$byClass
