
library(tidyverse)
library(tidymodels)

# list all model fits
model_fits_dir <- "~/Desktop/sapa_diabetes_fits/nov_2020/model_fits/"

model_fits <- list.files(path = model_fits_dir, pattern = "*.RDS", full.names = TRUE)

model_fits_names <-  list.files(model_fits_dir, pattern = ".RDS") %>% 
  str_replace(".RDS", "")

# read in model fits
model_fits <- model_fits %>% 
  map(readRDS) %>% 
  set_names(model_fits_names)

## racing lanes plot

# wrangle roc_auc data
roc_auc <- map_df(model_fits, ~show_best(.x, metric = "roc_auc", n = 1) %>% pull(mean)) %>%
  pivot_longer(cols = everything(), names_to = "model", values_to = "roc_auc") %>% 
  separate(model, into = c("model", "scoring"), sep = "_", extra = "merge")

# plot
roc_auc_plot <- roc_auc %>% 
  ggplot(aes(x = 0, y = roc_auc, color = scoring)) + 
  geom_point(size = 4, alpha = 0.7) + 
  facet_grid(model ~ ., scales = "free_x") + 
  scale_color_viridis_d(end = 0.9) +
  scale_x_continuous("", breaks = NULL) + 
  coord_flip() + 
  theme(legend.title=element_blank())

ggsave("roc_auc_racing_lanes.png", path = "~/Desktop/sapa_diabetes_fits/nov_2020")

# tibble of fits 

fits <- tibble(model_name = model_fits_names, 
               model_fit = model_fits)

# plot roc curves
fits <- fits %>% 
  rowwise() %>% 
  mutate(plot = list(model_fit %>% 
                       collect_predictions() %>% 
                       roc_curve(truth = diabetes, .pred_type1, .pred_type2, .pred_none) %>% 
                       autoplot() + 
                       ggtitle(model_name)),
         filename = paste0(str_replace(model_name, "_fit", ""), ".png")) 

# save roc curve plots 
fits %>% 
  select(filename, plot) %>% 
  pwalk(ggsave, path = "~/Desktop/sapa_diabetes_fits/nov_2020/roc_curves")