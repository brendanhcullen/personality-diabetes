library(here)
library(tidyverse)
library(caret)
library(pROC)
library(randomForest)


# Load in list of model fits ----------------------------------------------
model_fits_dir = here("output/machine_learning/training/model_fits")
model_fits_names = list.files(model_fits_dir, pattern = "_fit.RDS") %>% 
  gsub("_fit.RDS", "", .)

model_fits <- list.files(model_fits_dir, pattern = "_fit.RDS", full.names = TRUE) %>%
  map(readRDS) %>% 
  set_names(model_fits_names)

rm(model_fits_dir, model_fits_names)

# Visualize model comparisons ---------------------------------------------

resamps <- resamples(model_fits)

# plot accuracy and kappa 
trellis.par.set(trellis.par.get())
bwplot(resamps, layout = c(2, 1))

# plot kappa only
trellis.par.set(caretTheme())
dotplot(resamps, metric = "Kappa")


# Accuracy ----------------------------------------------------------------

width = .5
library(RColorBrewer)

# racing lanes
accuracy_racing_lanes_plot <- resamps$values %>% 
  gather("key", "value") %>%
  separate(key, into = c("key", "metric"), sep = "~") %>%
  mutate(key = as.factor(gsub("spi_", "", key))) %>%
  separate(key, into = c("key", "num"), sep = "_") %>%
  filter(metric == "Accuracy") %>% 
  group_by(key, num) %>%
  summarize(accuracy = mean(as.numeric(value)), na.rm=T) %>%
  ggplot(aes(x = 0, y = accuracy, color = num)) +
  geom_point(size = 3) +
  facet_grid(key ~ ., scales = "free_x") +
  scale_color_manual( values= brewer.pal(3, name = "Dark2")) +
  scale_x_continuous("", breaks = NULL)+
  coord_flip() +
  theme(legend.title=element_blank())

ggsave(file = here::here("output/machine_learning/training/figs/accuracy_racing_lanes_plot.png"), plot = accuracy_racing_lanes_plot)



# Kappa -------------------------------------------------------------------

#racing lanes

kappa_racing_lanes_plot <- resamps$values %>% 
  gather("key", "value") %>%
  filter(key != "Resample") %>%
  mutate(key = gsub("rpart2", "rparttwo", key)) %>%
  #separate(key, into = c("key", "metric"), sep = "\\~") %>%
  mutate(metric = ifelse(grepl("Accuracy", key), "Accuracy", "Kappa"),
         num = parse_number(key),
         key = gsub("_.*", "", key)) %>%
  filter(!is.nan(value)) %>%
  #separate(key, into = c("key", "num"), sep = "_") %>%
  filter(metric == "Kappa") %>%
  group_by(key, num) %>%
  summarise(kappa = mean(as.numeric(value)), na.rm=T) %>% 
  mutate(num = as.factor(num)) %>% 
  ggplot(aes(x = 0, y = kappa, color = num)) +
  geom_point(size = 3) +
  facet_grid(key ~ ., scales = "free_x") +
  scale_color_manual( values= brewer.pal(3, name = "Dark2")) +
  scale_x_continuous("", breaks = NULL)+
  coord_flip() +
  theme(legend.title=element_blank())

ggsave(file = here::here("output/machine_learning/training/figs/kappa_racing_lanes_plot.png"), plot = kappa_racing_lanes_plot)

# ROC AUC -----------------------------------------------------------------

# calculate multiclass auc values
roc_auc <- 
  tibble(model = names(model_fits)) %>% 
  mutate(output = model_fits) %>% 
  separate(model, c("model_name", "spi_scoring"), sep = "_", extra = "merge") %>% 
  filter(!grepl("svm", model_name)) %>%
  mutate(predicted = map(output, predict, type = "prob"),
         actual = map(output, "trainingData"),
         actual = map(actual, function(x) x[,".outcome"]),
         multiclass_roc = map2(actual, predicted, multiclass.roc),
         auc = map_dbl(multiclass_roc, "auc"))

#racing lanes
auc_racing_lanes_plot <- 
  roc_auc %>%
  select(model_name, spi_scoring, auc) %>% 
  ggplot(aes(x = 0, y = auc, color = spi_scoring)) +
  geom_point(size = 3) +
  facet_grid(model_name ~ ., scales = "free_x") +
  scale_color_viridis_d(end = 0.9) +
  scale_x_continuous("", breaks = NULL) +
  coord_flip() +
  theme(legend.title=element_blank())

ggsave(file = here::here("output/machine_learning/training/figs/auc_racing_lanes_plot.png"), plot = auc_racing_lanes_plot)


# ROC curves --------------------------------------------------------------

library(tidymodels)

# plot roc curves using yardstick::roc_curve()
roc_curves <- 
  tibble(model_name = names(model_fits), 
         model_fit = model_fits,
         preds = modify_depth(model_fit, 1, "pred")) %>% 
  rowwise() %>% 
  mutate(plot = list(preds %>% 
                       roc_curve(truth = obs, none, type1, type2) %>% 
                       autoplot() + 
                       ggtitle(model_name)),
         filename = paste0(str_replace(model_name, "_fit", ""), ".png")) 

# save roc curve plots 
roc_curves %>% 
  select(filename, plot) %>% 
  pwalk(ggsave, path = here("output/machine_learning/training/figs/roc_curves"))

# Variable importance -----------------------------------------------------

var_imps <- tibble(var_imp = map(model_fits, varImp)) %>% 
  mutate(model = names(model_fits)) %>% 
  filter(str_detect(model, "rf")) %>% 
  mutate(n_to_plot = case_when(str_detect(model, "spi_5") ~ 5, 
                               str_detect(model, "spi_27") ~ 27,
                               str_detect(model, "spi_135") ~ 30)) %>% 
  mutate(plot = map2(var_imp, n_to_plot, ~plot(.x, .y)),
         file = paste0(here("output/machine_learning/training/figs/var_imp"), model, "_var_imp.png"))

# Identify best models -----------------------------------------------------

find_best_model <- function(metrics_df, spi) {
  metrics_df %>% 
    filter(spi_scoring == spi) %>% 
    arrange(desc(auc)) %>% 
    slice(1) %>% 
    unite(full_model_name, model_name, spi_scoring) %>% 
    pull(full_model_name)
}

best_mods <- map_chr(c("spi_5", "spi_27", "spi_135"), ~find_best_model(roc_auc, .x))

best_mods_fits <- model_fits[names(model_fits) %in% best_mods]


# Save best models ---------------------------------------------------------

saveRDS(best_mods_fits, file = here("output/machine_learning/training/best_mods_fits.RDS"))


