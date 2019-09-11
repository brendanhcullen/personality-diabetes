library(here)
library(tidyverse)
library(caret)


# Load in list of model fits ----------------------------------------------
model_fits_dir = here("output/machine_learning/training/model_fits")
model_fits_names = list.files(model_fits_dir, pattern = "_fit.RDS") %>% 
  gsub("_fit.RDS", "", .)

model_fits <- list.files(model_fits_dir, pattern = "_fit.RDS", full.names = TRUE) %>%
  map(readRDS) %>% 
  set_names(model_fits_names)


# Visualize model comparisons ---------------------------------------------

resamps <- resamples(model_fits)

# plot accuracy and kappa 
trellis.par.set(trellis.par.get())
bwplot(resamps, layout = c(2, 1))

# plot kappa only
trellis.par.set(caretTheme())
dotplot(resamps, metric = "Kappa")

# Identify best model -----------------------------------------------------

model_fit_df = data.frame(model = names(model_fits), stringsAsFactors = FALSE)
model_fit_df$output = model_fits 
model_fit_df = model_fit_df %>% 
  separate(model, c("model_name", "spi_scoring"), sep = "_", extra = "merge")

kappas = model_fit_df %>%
  mutate(results = map(output, "results")) %>%
  mutate(kappa = map(results, "Kappa")) %>%
  mutate(kappa = map_dbl(kappa, max, na.rm = TRUE)) %>%
  dplyr::select(-c(output, results)) %>% 
  group_by(model_name, spi_scoring) %>% 
  arrange(desc(kappa)) # NEED TO FIGURE THIS OUT...

best_model_name = model.df_kappas$model[[1]] # model with largest Kappa

best_model = model_fits[[best_model_name]]


cat("The best model is", best_model_name, "with tuning parameter(s) of")
best_model$bestTune


# Save best model ---------------------------------------------------------

saveRDS(best_model, file = here("output/machine_learning/training/best_model.RDS"))
    