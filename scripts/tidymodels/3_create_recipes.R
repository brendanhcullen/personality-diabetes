# This script creates recipes, which are blueprints for how to pre-process the data before modelling. 
# There is a separate recipe for each dataset (i.e. spi-5, spi-27, spi-135).
# Note that these recipes use some custom recipe steps (namely, for scoring the spi data, imputing
# missing data, and residualizing scored spi variables)

library(tidyverse)
library(tidymodels)
library(here)
library(themis)
library(janitor)

# Set-up ----------------------------------------------------------

# load required info
data_train_ppc <- readRDS(here("output", "tidymodels", "data_train_ppc.RDS"))
keys <- readRDS(here("output", "tidymodels", "keys.RDS"))
load(here("output", "tidymodels", "spi_names.Rdata"))

# General notes:
# -spi-5 scoring is done via mean scoring
# -spi-27 scoring is done via IRT scoring
# -imputation is needed after `score_spi_27` because there are some missing values after scoring spi-27 
# because some people probably skipped all questions for some factors.
# -must remove ID and non-numeric vars before SMOTE (more info here: https://github.com/tidymodels/themis/issues/20)
# -must center and scale predictors before SMOTE
# -`step_smote()` generates new examples of the minority classes using nearest neighbors algorithm
# -skip = TRUE for `step_smote()` because we don't want to apply SMOTE to the test data


# Create recipes ----------------------------------------------------------

# recipe for spi_5
rec_spi_5 <- recipe(diabetes ~ ., data = data_train_ppc) %>% 
  step_rm(-has_role("outcome"), -spi_5_names) %>% 
  step_normalize(all_predictors()) %>% 
  step_smote(diabetes, skip = TRUE) 

# recipe for spi_27
rec_spi_27 <- recipe(diabetes ~ ., data = data_train_ppc) %>% 
  step_rm(-has_role("outcome"), -spi_27_names) %>% 
  step_normalize(all_predictors()) %>% 
  step_smote(diabetes, skip = TRUE) 

# recipe for spi_135
rec_spi_135 <- recipe(diabetes ~ ., data = data_train_ppc) %>% 
  step_rm(-has_role("outcome"), -spi_135_names) %>%  
  step_normalize(all_predictors()) %>% 
  step_smote(diabetes, skip = TRUE) 

# Test out recipe(s) ------------------------------------------------------

# # prep
#prepped <- prep(rec_spi_5)
# 
# # juice
# juiced <- juice(prepped)
# 
# # bake
# baked <- bake(prepped, new_data = data_train)

# Save recipes ------------------------------------------------------------

output_dir <- here("output", "tidymodels")
if(!dir.exists(output_dir)){dir.create(output_dir, recursive = TRUE)}

# put recipes into a list
recipes <- list(rec_spi_5 = rec_spi_5,
                rec_spi_27 = rec_spi_27,
                rec_spi_135 = rec_spi_135)

saveRDS(recipes, file = here("output", "tidymodels", "recipes.RDS"))

