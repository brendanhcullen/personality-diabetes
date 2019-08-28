library(here)
library(tidyverse)

# load in list of trained models
load(here("output/ml_training/trained_models.Rdata"))

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

best_model = model.df_kappas$model[[1]]

best_model = model.df %>% 
  filter(model == best_model) %>% 
  select(output)



cat("The best model is", model.df$model[1], "with tuning parameter(s) of")
best_model$bestTune

    