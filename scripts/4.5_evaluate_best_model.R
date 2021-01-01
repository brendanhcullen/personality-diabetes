library(here)
library(tidyverse)
library(caret)

# Load holdout test data --------------------------------------------------

best_mods_fits <- readRDS(here("output/machine_learning/training/best_mods_fits.RDS"))
test_data_pp <- readRDS(here("output/machine_learning/testing/test_data_pp.RDS"))

# convert diabetes to factor
test_data_pp$diabetes <- as.factor(test_data_pp$diabetes)

# Evaluate best model on test data ----------------------------------------

# pull out final models
spi_5_final_mod <- best_mods_fits$rf_spi_5$finalModel
spi_27_final_mod <- best_mods_fits$rf_spi_27$finalModel
spi_135_final_mod <- best_mods_fits$rf_spi_135$finalModel

# NOTE: using predict() here is not generating the right number of rows
# use model to predict diabetes status in test data
pred_spi_5 <- predict(spi_5_final_mod, new_data = test_data_pp)

# This is currently giving an error since the number of rows don't match
# confusion matrix
conf_mat_spi_5 <- confusionMatrix(pred_spi_5, test_data_pp$diabetes)

