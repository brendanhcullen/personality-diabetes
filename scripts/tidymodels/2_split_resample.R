# This script takes cleaned data (e.g. filtered according to inclusion criteria) and 
# performs the initial split into training and testing along with resampling via 
# 10-fold cross-validation

library(tidyverse)
library(tidymodels)
library(here)

# Read in cleaned data ----------------------------------------------------

data_clean <- readRDS(here("output", "tidymodels", "data_clean.RDS"))

# Split data --------------------------------------------------------------

# split data and stratify by diabetes 
# i.e. ensure equal proportions of diabetes groups in training and testing
set.seed(123)
data_split <- initial_split(data_clean, strata = diabetes)

# extract training and testing data
data_train <- training(data_split)
data_test <- testing(data_split)

# Resampling --------------------------------------------------------------

# 10-fold cross-validation
set.seed(123)
cv_folds <- vfold_cv(data_train, 
                     v = 5, # 10 folds
                     strata = diabetes) # maintain diabetes proportions within each fold

# Save data ---------------------------------------------------------------

saveRDS(data_train, file = here("output", "tidymodels", "data_train.RDS"))
saveRDS(data_test, file = here("output", "tidymodels", "data_test.RDS"))
saveRDS(data_split, file = here("output", "tidymodels", "data_split.RDS"))
saveRDS(cv_folds, file = here("output", "tidymodels", "cv_folds.RDS"))
