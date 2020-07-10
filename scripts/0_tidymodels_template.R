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
set.seed(123)
data <- data %>% 
  sample_frac(.2)


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
cv_folds <- vfold_cv(data_train, v = 2, strata = diabetes)

# Pre-processing ----------------------------------------------------------

# source scripts containing custom recipe steps
custom_recipe_scripts <- list.files(here("scripts", "tidymodels", "recipe_steps"), full.names = TRUE)
walk(custom_recipe_scripts, source)

# general recipe template
rec <- recipe(diabetes ~ ., data = data_train) %>% 
  update_role(RID, new_role = "id") %>%
  update_role(demographic_vars, new_role = "covariate") %>%
  # score spi_5 (mean scoring)
  step_score_spi_5(spi_135_names, 
                   keys = keys,
                   role = "predictor") %>%
  # score spi_27 (IRT scoring)
  step_score_spi_27(spi_135_names, 
                    keys = keys,
                    IRT_path = here("../IRTinfoSPI27.rdata"),
                    role = "predictor") %>%
  # impute missing values in all spi variables
  step_impute_pca(all_spi_names) %>% 
  # residualize all spi vars (extract variance due to demographic vars)
  step_residualize(all_spi_names, vtc = demographic_vars, id_var = "RID") %>%
  # remove non-numeric vars before SMOTE
  # see here for why you need to remove ID var as well: https://github.com/tidymodels/themis/issues/20
  step_rm(has_role("id"), has_role("covariate")) %>% 
  # need to center and scale all numeric predictors before SMOTE
  step_normalize(all_predictors()) %>% 
  # `step_smote()` generates new examples of the minority classes using nearest neighbors algorithm 
  # Note: skip = TRUE because we don't want to apply SMOTE to the test data
  step_smote(diabetes, skip = TRUE) 

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
  step_impute_pca(spi_27_names) %>% 
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
# prepped <- prep(rec_spi_135)
# 
# # juice
# juiced <- juice(prepped)
# 
# # bake
# baked <- bake(prepped, new_data = data_train)

# Model specification -----------------------------------------------------

multinom_reg_spec_notune <- multinom_reg() %>%
  set_engine("nnet") %>%
  set_mode("classification")

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

multinom_reg_spi_27_wflow <- 
  workflow() %>% 
  add_recipe(rec_spi_27) %>% 
  add_model(multinom_reg_spec)

multinom_reg_spi_135_wflow <- 
  workflow() %>% 
  add_recipe(rec_spi_135) %>% 
  add_model(multinom_reg_spec)

multinom_reg_spi_5_wflow_notune <-
  workflow() %>%
  add_recipe(rec_spi_5) %>%
  add_model(multinom_reg_spec_notune)


# Hyperparameter tuning ---------------------------------------------------

# no cross-validation

fit_res_nocv <- fit(
  multinom_reg_spi_5_wflow_notune,
  data = data_train
)

# cross-validation + no tuning
fit_res_spi_5_notune <- fit_resamples(
  multinom_reg_spi_5_wflow_notune,
  resamples = cv_folds,
  control = control_resamples(verbose = TRUE,
                              save_pred = TRUE)
)

doParallel::registerDoParallel()
set.seed(123)
# cross-validation + tuning
tune_res_spi_5 <- tune_grid(
  multinom_reg_spi_5_wflow,
  resamples = cv_folds,
  grid = 2,
  control = control_resamples(verbose = TRUE,
                              save_pred = TRUE)
)

# cross-validation + tuning
tune_res_spi_27 <- tune_grid(
  multinom_reg_spi_27_wflow,
  resamples = cv_folds,
  grid= 2,
  control = control_resamples(verbose = TRUE,
                              save_pred = TRUE)
)

# cross-validation + tuning
tune_res_spi_135 <- tune_grid(
  multinom_reg_spi_135_wflow,
  resamples = cv_folds,
  grid= 2,
  control = control_resamples(verbose = TRUE,
                              save_pred = TRUE)
)

# Finalize workflow -------------------------------------------------------

# extract best tuning parameters

multinom_reg_spi_5_best <-
  tune_res_spi_5 %>% 
  select_best(metric = "roc_auc")

last_multinom_reg_spi_5_wflow <- 
  multinom_reg_spi_5_wflow %>%
  finalize_workflow(multinom_reg_spi_5_best)

# Other models specs ------------------------------------------------------

# decision tree
tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")


# random forest
cores <- parallel::detectCores()
cores

rf_spec <- 
  rand_forest(mtry = tune(),
              min_n = tune(),
              trees = 1000) %>% 
  set_engine("ranger", 
             num.threads = cores) %>% 
  set_mode("classification")

knn_spec <- nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("classification")

lda_spec <- discrim_linear() %>% 
  set_engine("MASS") %>% 
  set_mode("classification")



