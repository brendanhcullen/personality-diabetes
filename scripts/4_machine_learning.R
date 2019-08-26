
library(here)
library(tidyverse)
library(caret)
library(nnet)
library(Hmisc)
library(e1071)
library(DMwR) # for smote sampling


# Load data ---------------------------------------------------------------

load(here("output/data_cleaned.Rdata"))

set.seed(081919)

# Wrangle data and partition -----------------------------------------------

# data wrangling
data_ml <- data_scored %>% 
  select(c(diagnosis, starts_with("q_"))) %>% 
  mutate(diagnosis = as.factor(diagnosis))

# partition into training (80%) and testing (20%)
partition <- createDataPartition(data_ml$diagnosis,
                                   times = 1,
                                   p = .8,
                                   list = FALSE)

train_data <- data_ml[partition, ] # training data (note: this will be iteratively split into train and validation sets during k-fold cross-validation.)
test_data <- data_ml[-partition, ] # holdout test data. Don't use this until evaluating final model performance.

# specify resampling parameters
train_control <- trainControl(method = "repeatedcv",
                   number = 2, # number of folds = 10
                   repeats = 2, # cross-validation is repeated 10 times
                   sampling = "up", # use for resolving class imbalances (up, down, smote) 
                   returnResamp = "final") # only return final 

# Impute missing data -----------------------------------------------------

# manually randomly impute for now. Note: imputation will eventually occur as a "pre-processing" step
train_data <- train_data %>% 
  mutate_all(Hmisc::impute, fun = "random") %>% 
  filter(row_number() %in% sample(1:nrow(.), size = 2000, replace = FALSE))

test_data <- test_data %>%
  mutate_all(Hmisc::impute, fun = "random") 
  
# Logistic regression -----------------------------------------------------
multinom_grid = expand.grid(decay = seq(from = 0, to = 0.5, by = .1))

set.seed(081919)
multinom_fit <- train(diagnosis ~ .,
      data = train_data,
      method = "multinom", 
      trControl = train_control,
      tuneGrid = multinom_grid,
      metric = "Kappa") # select the best model based on Kappa

multinom_fit

# info about tuning parameters?
getModelInfo("multinom", FALSE)[[1]]$grid



# K-nearest neighbors -----------------------------------------------------

knn_grid = expand.grid(k = seq(from = 1, to = 5, by = 1))

set.seed(081919)
knn_fit <- train(diagnosis ~ .,
                      data = train_data,
                      method = "knn", 
                      trControl = train_control,
                      tuneGrid = knn_grid,
                      metric = "Kappa") # select the best model based on Kappa

knn_fit


# Compare models ----------------------------------------------------------

resamps <- resamples(list(multinom = multinom_fit,
                          knn = knn_fit))

trellis.par.set(trellis.par.get())
bwplot(resamps, layout = c(2, 1))

trellis.par.set(caretTheme())
dotplot(resamps, metric = "Kappa")


# Evaluate best model on test data ----------------------------------------

# specify which model is superior based on training
best_model <- multinom_fit$finalModel 

# use model to predict diagnosis values in test data
predicted <- predict(best_model, test_data)

# confusion matrix
confusion_matrix <- confusionMatrix(predicted, test_data$diagnosis)
confusion_matrix$table
confusion_matrix$byClass
