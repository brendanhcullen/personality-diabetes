

# Load spi names ----------------------------------------------------------

load(here('output', 'tidymodels', 'spi_names.Rdata'))


# Multinomial logistic regression -----------------------------------------

multinom_spi_5_grid <- grid_regular(penalty(),
                                    levels = 10)

multinom_spi_27_grid <- grid_regular(penalty(),
                                     levels = 10)

multinom_spi_135_grid <- grid_regular(penalty(),
                                      levels = 10)


# K-nearest neighbors -----------------------------------------------------

knn_spi_5_grid <- grid_regular(neighbors(),
                               weight_func(),
                               levels = 10)

knn_spi_27_grid <- grid_regular(neighbors(),
                                weight_func(),
                                levels = 10)

knn_spi_135_grid <- grid_regular(neighbors(),
                                 weight_func(),
                                 levels = 10)


# Neural network ----------------------------------------------------------

nnet_spi_5_grid <- grid_regular(hidden_units(),
                                penalty(),
                                levels = 10)

nnet_spi_27_grid <- grid_regular(hidden_units(),
                                 penalty(),
                                 levels = 10)

nnet_spi_135_grid <- grid_regular(hidden_units(),
                                  penalty(),
                                  levels = 10)


# Support vector machine --------------------------------------------------

svm_spi_5_grid <- grid_regular(cost(),
                               rbf_sigma(),
                               levels = 10)

svm_spi_27_grid <- grid_regular(cost(),
                                rbf_sigma(),
                                levels = 10)

svm_spi_135_grid <- grid_regular(cost(),
                                 rbf_sigma(),
                                 levels = 10)


# Linear discriminant analysis --------------------------------------------

lda_spi_5_grid <- grid_regular(penalty(),
                               levels = 10)

lda_spi_27_grid <- grid_regular(penalty(),
                               levels = 10)

lda_spi_135_grid <- grid_regular(penalty(),
                               levels = 10)


# Decision tree -----------------------------------------------------------

tree_spi_5_grid <- grid_regular(cost_complexity(), 
                                tree_depth(),
                                min_n(),
                                levels = 10)

tree_spi_27_grid <- grid_regular(cost_complexity(), 
                                tree_depth(),
                                min_n(),
                                levels = 10)

tree_spi_135_grid <- grid_regular(cost_complexity(), 
                                tree_depth(),
                                min_n(),
                                levels = 10)

# Random forest -----------------------------------------------------------
# NOTE: mtry() is dependent on number of predictors

rf_spi_5_grid <- grid_regular(mtry() %>% range_set(c(1, length(spi_5_names))),
                              min_n(),
                              levels = 10)

rf_spi_27_grid <- grid_regular(mtry() %>% range_set(c(1, length(spi_27_names))),
                               min_n(),
                               levels = 10)

rf_spi_135_grid <- grid_regular(mtry() %>% range_set(c(1, length(spi_135_names))),
                                min_n(),
                                levels = 10)


# Save tuning grids -------------------------------------------------------

# put tuning grid into tibble as list column
grid_names <- ls(pattern = "*_grid")

tuning_grids <- tibble(tuning_grid = map(grid_names, ~get(.x))) %>% 
  mutate(name = str_remove(grid_names, "_grid")) %>% 
  separate(name, into = c("model_name", "spi", "scoring"), sep = "_") %>% 
  unite("spi_scoring", spi, scoring, sep = "_")

# save
saveRDS(tuning_grids, file = here("output", "tidymodels", "tuning_grids.RDS"))
