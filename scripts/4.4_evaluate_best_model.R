

# Load holdout test data --------------------------------------------------



# Evaluate best model on test data ----------------------------------------

# specify which model is superior based on training
best_model = best_model$finalModel

# use model to predict diagnosis values in test data
predicted <- predict(best_model, test_data)

# confusion matrix
confusion_matrix <- confusionMatrix(predicted, test_data$diagnosis)
confusion_matrix$table
confusion_matrix$byClass