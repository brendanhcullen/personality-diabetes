library(here)

# prep for training
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

# model fits saved to 