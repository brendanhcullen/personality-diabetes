# This script takes cleaned data (e.g. filtered according to inclusion criteria) and 
# performs the initial split into training and testing along with resampling via 
# 10-fold cross-validation

library(tidyverse)
library(tidymodels)
library(here)
library(janitor)
library(psych)
library(missMDA)

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


# Initial pre-processing --------------------------------------------------

# The following pre-processing is done *outside* of the recipe because models were
# taking far too long to run when trying to pre-process the data fold-by-fold

# Source pre-processing functions
source(here("scripts/tidymodels/preprocessing/preprocess.R"))
# read in keys for SPI scoring
keys <- readRDS(here("output", "tidymodels", "keys.RDS"))
# load spi names
load(here("output", "tidymodels", "spi_names.Rdata"))
# load demographic vars
demographic_vars <- readRDS(here("output", "tidymodels", "demographic_vars.RDS"))

# pre-process train data
data_train_ppc <- preprocess_sapa(data = data_train, 
                                keys = keys, 
                                id = "RID", 
                                VOI = all_spi_names, 
                                covariates = demographic_vars, 
                                IRT_path = here("../IRTinfoSPI27.rdata"), 
                                order = c("score", "impute", "residualize"))

# pre-process test data
data_test_ppc <- preprocess_sapa(data = data_test, 
                               keys = keys, 
                               id = "RID", 
                               VOI = all_spi_names, 
                               covariates = demographic_vars, 
                               IRT_path = here("../IRTinfoSPI27.rdata"), 
                               order = c("score", "impute", "residualize"))

# Resampling --------------------------------------------------------------

# 10-fold cross-validation
set.seed(123)
cv_folds <- vfold_cv(data_train_ppc, 
                     v = 10, # 10 folds
                     strata = diabetes) # maintain diabetes proportions within each fold

# Save data ---------------------------------------------------------------

saveRDS(data_train_ppc, file = here("output", "tidymodels", "data_train_ppc.RDS"))
saveRDS(data_test_ppc, file = here("output", "tidymodels", "data_test_ppc.RDS"))
saveRDS(data_split, file = here("output", "tidymodels", "data_split.RDS"))
saveRDS(cv_folds, file = here("output", "tidymodels", "cv_folds.RDS"))
