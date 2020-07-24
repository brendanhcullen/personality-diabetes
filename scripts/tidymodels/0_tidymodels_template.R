# This is a template script to work through a tidymodels workflow for a single model (let's start with multinomial logistic regression) 

# Load libraries
library(tidyverse)
library(tidymodels)
library(here)
library(janitor)
library(missMDA) # for imputing missing data
library(themis) # for SMOTE subsampling

# Import data -------------------------------------------------------------

# load raw data
load(here("../SAPAdata07feb2017thru18nov2019.rdata"))

# rename to 'data' and convert to tibble
data <- as_tibble(SAPAdata07feb2017thru18nov2019)

data <- data %>% 
  mutate(RID = as.character(RID)) %>% 
  mutate_at(c("p1occPrestige", 
              "p1occIncomeEst",
              "p2occPrestige",
              "p2occIncomeEst"),
            as.numeric)

# remove all objects except data 
rm(list=setdiff(ls(), "data"))

# sample 1% of data for more speed
# set.seed(123)
# data <- data %>% 
#   sample_frac(.2)


# Basic data cleaning -----------------------------------------------------

# source function for extracting spi names
source(here("scripts/tidymodels/get_spi_names.R"))

# read in keys
keys <- read.csv(here("data/superKey.csv"), header = TRUE, row.names = 1)

# get SPI names
spi_names <- get_spi_names(keys)
spi_5_names <- spi_names$spi_5
spi_27_names <- spi_names$spi_27
spi_135_names <- spi_names$spi_135
all_spi_names <- unlist(spi_names, use.names = FALSE)

# min number of responses to SPI-135 items required to be included in analysis
min_n_valid <- 27

data <- data %>% 
  mutate(n_valid_135 = apply(.[,spi_135_names], 1, function(x) sum(!is.na(x)))) %>%  
  filter(!is.na(diabetes), # only people who responsed to diabetes question
         country == "USA", # only USA data
         n_valid_135 >= min_n_valid) %>%  # only people with at least 27 responses on SPI-135 items
  select(-n_valid_135)

# only retain SPI items that are part of the SPI-135

demographic_vars <- c(
  "age", # age
  "ethnic",  # ethnicity
  "jobstatus", # current job status
  "education", "occPrestige", "occIncomeEst", # self SES
  "p1edu", "p1occPrestige", "p1occIncomeEst", # parent 1 SES
  "p2edu", "p2occPrestige", "p2occIncomeEst") # parent 2 SES

data <- data %>% 
  select(c(RID, 
          diabetes, 
          all_of(demographic_vars), 
          all_of(spi_135_names)))

# Split data --------------------------------------------------------------
set.seed(123)

# split data and stratify by diabetes (i.e. ensure equal proportions of diabetes groups in training and testing)
data_split <- initial_split(data, strata = diabetes)

# extract training and testing data
data_train <- training(data_split)
data_test <- testing(data_split)

# Resample ----------------------------------------------------------------

# 10-fold cross-validation
set.seed(123)
cv_folds <- vfold_cv(data_train, v = 10, strata = diabetes)

# Pre-processing ----------------------------------------------------------

# source scripts containing custom recipe steps
custom_recipe_scripts <- list.files(here("scripts", "tidymodels", "custom_rec_steps"), full.names = TRUE)
walk(custom_recipe_scripts, source)

# # general recipe template
# rec <- recipe(diabetes ~ ., data = data_train) %>% 
#   update_role(RID, new_role = "id") %>%
#   update_role(demographic_vars, new_role = "covariate") %>%
#   # score spi_5 (mean scoring)
#   step_score_spi_5(spi_135_names, 
#                    keys = keys,
#                    role = "predictor") %>%
#   # score spi_27 (IRT scoring)
#   step_score_spi_27(spi_135_names, 
#                     keys = keys,
#                     IRT_path = here("../IRTinfoSPI27.rdata"),
#                     role = "predictor") %>%
#   # impute missing values in all spi variables
#   step_impute_pca(all_spi_names) %>% 
#   # residualize all spi vars (extract variance due to demographic vars)
#   step_residualize(all_spi_names, vtc = demographic_vars, id_var = "RID") %>%
#   # remove non-numeric vars before SMOTE
#   # see here for why you need to remove ID var as well: https://github.com/tidymodels/themis/issues/20
#   step_rm(has_role("id"), has_role("covariate")) %>% 
#   # need to center and scale all numeric predictors before SMOTE
#   step_normalize(all_predictors()) %>% 
#   # `step_smote()` generates new examples of the minority classes using nearest neighbors algorithm 
#   # Note: skip = TRUE because we don't want to apply SMOTE to the test data
#   step_smote(diabetes, skip = TRUE) 

# recipe for spi_5
rec_spi_5 <- recipe(diabetes ~ ., data = data_train) %>% 
  update_role(RID, new_role = "id") %>%
  update_role(demographic_vars, new_role = "covariate") %>%
  step_score_spi_5(spi_135_names, 
                   keys = keys,
                   role = "predictor") %>%
  step_residualize(spi_5_names, vtc = demographic_vars, id_var = "RID") %>%
  step_rm(has_role("id"), has_role("covariate")) %>% 
  step_rm(spi_135_names) %>% 
  step_normalize(all_predictors()) %>% 
  step_smote(diabetes, skip = TRUE) 

# recipe for spi_27
rec_spi_27 <- recipe(diabetes ~ ., data = data_train) %>% 
  update_role(RID, new_role = "id") %>%
  update_role(demographic_vars, new_role = "covariate") %>%
  step_score_spi_27(spi_135_names, 
                    keys = keys,
                    IRT_path = here("../IRTinfoSPI27.rdata"),
                    role = "predictor") %>%
  #step_impute_pca(spi_27_names) %>% 
  step_residualize(spi_27_names, vtc = demographic_vars, id_var = "RID") %>%
  step_rm(has_role("id"), has_role("covariate")) %>% 
  step_rm(spi_135_names) %>% 
  step_normalize(all_predictors()) %>% 
  step_smote(diabetes, skip = TRUE) 

# recipe for spi_135
rec_spi_135 <- recipe(diabetes ~ ., data = data_train) %>% 
  update_role(RID, new_role = "id") %>%
  update_role(demographic_vars, new_role = "covariate") %>%
  step_impute_pca(spi_135_names) %>% 
  step_residualize(spi_135_names, vtc = demographic_vars, id_var = "RID") %>%
  step_rm(has_role("id"), has_role("covariate")) %>% 
  step_normalize(all_predictors()) %>% 
  step_smote(diabetes, skip = TRUE) 

# Test out recipe(s) ------------------------------------------------------

# # prep
# prepped <- prep(rec_spi_27)
# 
# # juice
# juiced <- juice(prepped)
# 
# # bake
# baked <- bake(prepped, new_data = data_train)

# Model specification -----------------------------------------------------

# penalized multinomial logistic regression
multinom_reg_spec <- multinom_reg() %>% 
  set_engine("nnet") %>% 
  set_mode("classification") %>% 
  set_args(penalty = tune())


# Create workflow ---------------------------------------------------------

# bundle model and recipe into a workflow

multinom_reg_spi_5_wflow <- 
  workflow() %>% 
  add_recipe(rec_spi_5) %>% 
  add_model(multinom_reg_spec)


# multinom_reg_spi_27_wflow <- 
#   workflow() %>% 
#   add_recipe(rec_spi_27) %>% 
#   add_model(multinom_reg_spec)
# 
# multinom_reg_spi_135_wflow <- 
#   workflow() %>% 
#   add_recipe(rec_spi_135) %>% 
#   add_model(multinom_reg_spec)

# Hyperparameter tuning ---------------------------------------------------

# no cross-validation

# multinom reg tuning grid
multinom_grid <- grid_regular(penalty(), 
                              levels = 10)



## WITHOUT PARALLELIZATION

tune_res_multinom_reg_spi_5 <- tune_grid(
  multinom_reg_spi_5_wflow,
  resamples = cv_folds,
  grid = multinom_grid,
  control = control_grid(verbose = TRUE,
                         save_pred = TRUE)
)

## WITH PARALLELIZATION

library(doParallel)
library(tictoc)

all_cores <- parallel::detectCores(logical = FALSE)
#cl <- makePSOCKcluster(all_cores)
cl <- makeForkCluster(all_cores)
registerDoParallel(cl)
# foreach::getDoParWorkers()
# clusterEvalQ(cl, {library(janitor)})
# clusterExport(cl, varlist = ls())

# cross-validation + tuning

tune_res_multinom_reg_spi_5 <- tune_grid(
  multinom_reg_spi_5_wflow,
  resamples = cv_folds,
  grid = multinom_grid,
  control = control_grid(verbose = TRUE,
                         save_pred = TRUE)
)

stopCluster(cl)


# Finalize workflow -------------------------------------------------------

# extract best tuning parameters

multinom_reg_spi_5_best <-
  tune_res_spi_5 %>% 
  select_best(metric = "roc_auc")


last_multinom_reg_spi_5_wflow <- 
  multinom_reg_spi_5_wflow %>%
  finalize_workflow(multinom_reg_spi_5_best)

# Predict on test set -----------------------------------------------------

# use tune::last_fit()

# fit to entire training data
final_multinom <- 
  last_multinom_reg_spi_5_wflow %>% 
  fit(data = data_train)



## predict on test set 

# multinom 
last_fit(last_multinom_reg_spi_5_wflow ,
         split = data_split) %>% 
  collect_predictions() %>% 
  roc_curve(diabetes, .pred_type1, .pred_type2, .pred_none) %>% 
  autoplot()
