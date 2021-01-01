# This script automates the process of running the 3 random forest models. 
# Model fits are saved to the directory `personality-diabetes/output/tidymodels/model_fits`
# Be sure to have the most recent version of tidymodels installed (otherwise might
# run into errors)

library(here)

# prep for training (this will create training scripts for all models, but only 
# random forest models are run in the next section)
source(here("scripts/tidymodels/1_clean.R"))
source(here("scripts/tidymodels/2_split_ppc_resample.R"))
source(here("scripts/tidymodels/3_create_recipes.R"))
source(here("scripts/tidymodels/4_create_model_specs.R"))
source(here("scripts/tidymodels/5_create_tuning_grids.R"))
source(here("scripts/tidymodels/6_prep_for_train.R"))
source(here("scripts/tidymodels/7_create_training_scripts.R"))

# run random forest models
source(here("output/tidymodels/training_scripts/rf_spi_5.R"))
source(here("output/tidymodels/training_scripts/rf_spi_27.R"))
source(here("output/tidymodels/training_scripts/rf_spi_135.R"))
