library(here)
library(tidyverse)
library(caret)
library(pROC)
library(randomForest)


# Load in list of model fits ----------------------------------------------
#model_fits_dir = here("output/machine_learning/training/model_fits")
model_fits_dir = "~/Desktop/soc-brownbag-pres/output/machine_learning/training/model_fits/"
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

accuracy_lollipop_plot <- resamps$values %>% 
  gather("key", "value") %>%
  separate(key, into = c("key", "metric"), sep = "~") %>%
  mutate(key = as.factor(gsub("spi_", "", key))) %>%
  separate(key, into = c("key", "num"), sep = "_") %>%
  filter(metric == "Accuracy") %>% 
  group_by(key, num) %>%
  summarise(accuracy = mean(as.numeric(value)), na.rm=T) %>%
  ungroup() %>% 
  arrange(desc(accuracy)) %>% 
  ggplot(aes(x = key, y = accuracy, color = num)) +
  geom_point(size = 3, position = position_dodge(width = width)) +
  geom_segment(aes(xend = key, yend = 0), position = position_dodge(width = width)) +
  #facet_wrap(~num) +
  scale_color_manual( values= brewer.pal(3, name = "Dark2")) +
  theme_minimal() +
  coord_flip()

ggsave(file = here::here("output/machine_learning/training/figs/accuracy_lollipop_plot.png"), plot = accuracy_lollipop_plot)

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

kappa_lollipop_plot <- resamps$values %>% 
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
  ungroup() %>% 
  arrange(desc(kappa)) %>% 
  ggplot(aes(x = key, y = kappa, color = as.factor(num))) +
  geom_point(size = 3, position = position_dodge(width = width)) +
  geom_segment(aes(xend = key, yend = 0), position = position_dodge(width = width)) +
  #facet_wrap(~num) +
  scale_color_manual( values= brewer.pal(3, name = "Dark2")) +
  theme_minimal() +
  coord_flip()

ggsave(file = here::here("output/machine_learning/training/figs/kappa_lollipop_plot.png"), plot = kappa_lollipop_plot)

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
pred.v.actual <- data.frame(model = names(model_fits), stringsAsFactors = FALSE) %>% 
  mutate(output = model_fits) %>% 
  separate(model, c("model_name", "spi_scoring"), sep = "_", extra = "merge") %>% 
  filter(!grepl("svm", model_name)) %>%
  mutate(predicted = map(output, predict, type = "prob")) %>% 
  mutate(actual = map(output, "trainingData")) %>%
  mutate(actual = map(actual, function(x) x[,".outcome"])) %>% 
  mutate(multiclass_roc = map2(actual, predicted, multiclass.roc))


#lollipops
auc_lollipop_plot <- pred.v.actual %>%
  select(model_name, spi_scoring, multiclass_roc) %>%
  mutate(auc = map_dbl(multiclass_roc, "auc")) %>%
  ggplot(aes(x = reorder(model_name, auc), y = auc, color = spi_scoring)) +
  geom_point(size = 3, position = position_dodge(width = width)) +
  geom_segment(aes(xend = model_name, yend = 0), position = position_dodge(width = width)) +
  #facet_wrap(~spi_scoring) +
  scale_color_manual( values= brewer.pal(3, name = "Dark2")) +
  theme_minimal() +
  coord_flip()

ggsave(file = here::here("output/machine_learning/training/figs/auc_lollipop_plot.png"), plot = auc_lollipop_plot)

#racing lanes
auc_racing_lanes_plot <- pred.v.actual %>%
  select(model_name, spi_scoring, multiclass_roc) %>%
  mutate(auc = map_dbl(multiclass_roc, "auc")) %>% 
  ggplot(aes(x = 0, y = auc, color = spi_scoring)) +
  geom_point(size = 3) +
  facet_grid(model_name ~ ., scales = "free_x") +
  scale_color_manual( values= brewer.pal(3, name = "Dark2")) +
  scale_x_continuous("", breaks = NULL) +
  coord_flip() +
  theme(legend.title=element_blank())

ggsave(file = here::here("output/machine_learning/training/figs/auc_racing_lanes_plot.png"), plot = auc_racing_lanes_plot)

# Identify best models -----------------------------------------------------

kappas = data.frame(model = names(model_fits), stringsAsFactors = FALSE) %>% 
  mutate(output = model_fits) %>% 
  separate(model, c("model_name", "spi_scoring"), sep = "_", extra = "merge") %>% 
  mutate(results = map(output, "results")) %>%
  mutate(kappa = map(results, "Kappa")) %>%
  mutate(kappa = map_dbl(kappa, max, na.rm = TRUE)) %>%
  dplyr::select(-output, -results)

best_model_name = kappas %>% 
  arrange(desc(kappa)) %>% 
  slice(1) %>% 
  unite(full_model_name, model_name, spi_scoring) %>% 
  select(full_model_name) %>% 
  map_chr(1) %>% 
  unname()

best_model = model_fits[[best_model_name]]


cat("The best model is", best_model_name, "with tuning parameter(s) of")
best_model$bestTune



# Variable importance -----------------------------------------------------



# Save best model ---------------------------------------------------------

saveRDS(best_model, file = here("output/machine_learning/training/best_model.RDS"))


