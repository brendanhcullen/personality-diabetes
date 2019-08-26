
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
  filter(row_number() %in% sample(1:nrow(.), size = 5000, replace = FALSE))
  
# Logistic regression -----------------------------------------------------
pmr_grid = expand.grid(decay = seq(from = 0, to = 0.5, by = .1))

pmr_fit <- train(diagnosis ~ .,
      data = train_data,
      method = "multinom", 
      trControl = train_control,
      tuneGrid = pmr_grid,
      metric = "Kappa") # select the best model based on Kappa

confusionMatrix(pmr_fit)

pmr_fit

# info about tuning parameters?
getModelInfo("multinom", FALSE)[[1]]$grid
