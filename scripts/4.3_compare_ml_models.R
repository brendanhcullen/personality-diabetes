library(here)
library(tidyverse)
library(caret)

# load in list of trained models
load(here("output/machine_learning/trained_models.Rdata"))

model_fits_dir = here("output/machine_learning/training/model_fits")
model_fits_names = list.files(model_fits_dir, pattern = "_fit.RDS") %>% 
  gsub("_fit.RDS", "", .)

model_fits <- list.files(model_fits_dir, pattern = "_fit.RDS", full.names = TRUE) %>%
  map(readRDS) %>% 
  set_names(model_fits_names)

resamps <- resamples(model_fits)

# plot accuracy and kappa 
trellis.par.set(trellis.par.get())
bwplot(resamps, layout = c(2, 1))

# plot kappa only
trellis.par.set(caretTheme())
dotplot(resamps, metric = "Kappa")

# Identify best model -----------------------------------------------------

model.df = data.frame(model = names(model_fits), stringsAsFactors = FALSE)
model.df$output = model_fits

model.df_kappas = model.df %>%
  mutate(results = map(output, "results")) %>%
  mutate(Kappa = map(results, "Kappa")) %>%
  mutate(Kappa = map_dbl(Kappa, max, na.rm = TRUE)) %>%
  dplyr::select(model, Kappa) %>%
  arrange(desc(Kappa))

best_model_name = model.df_kappas$model[[1]] # model with largest Kappa

best_model = model_fits[[best_model_name]]


cat("The best model is", best_model_name, "with tuning parameter(s) of")
best_model$bestTune


# Save best model ---------------------------------------------------------

save(best_model, file = here("output/machine_learning/best_model.Rdata"))
    