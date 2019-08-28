library(here)
library(tidyverse)
library(caret)

# load in list of trained models
load(here("output/machine_learning/trained_models.Rdata"))

resamps <- resamples(trained_models)

# plot accuracy and kappa 
trellis.par.set(trellis.par.get())
bwplot(resamps, layout = c(2, 1))

# plot kappa only
trellis.par.set(caretTheme())
dotplot(resamps, metric = "Kappa")

# Identify best model -----------------------------------------------------

model.df = data.frame(model = names(trained_models), stringsAsFactors = FALSE)
model.df$output = trained_models

model.df_kappas = model.df %>%
  mutate(results = map(output, "results")) %>%
  mutate(Kappa = map(results, "Kappa")) %>%
  mutate(Kappa = map_dbl(Kappa, max, na.rm = TRUE)) %>%
  dplyr::select(model, Kappa) %>%
  arrange(desc(Kappa))

best_model_name = model.df_kappas$model[[1]] # model with largest Kappa

best_model = trained_models[[best_model_name]]


cat("The best model is", best_model_name, "with tuning parameter(s) of")
best_model$bestTune


# Save best model ---------------------------------------------------------

save(best_model, file = here("output/machine_learning/best_model.Rdata"))
    