# This script prepares data for model traning and hyperparameter tuning. 

# Load libraries ----------------------------------------------------------

library(here)
library(tidyverse)
library(tidymodels)

# Model specs -------------------------------------------------------------

# decision tree
tree_spec <- 
  decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

# random forest
rf_spec <- 
  rand_forest() %>% 
  set_engine("ranger", 
             num.threads = cores) %>% 
  set_mode("classification") %>% 
  set_args(mtry = tune(),
           min_n = tune(),
           trees = 1000)

# k-nearest neighbor
knn_spec <- 
  nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("classification")

# linear discriminant analysis
lda_spec <- 
  discrim_linear() %>% 
  set_engine("MASS") %>% 
  set_mode("classification")

# Create master df --------------------------------------------------------

spi_scoring <- c(
  "spi_5",
  "spi_27",
  "spi_135"
)

rec_names <- map_chr(spi_scoring, ~paste("rec", ., sep = "_"))

model_names <- c( 
  "multinom",
  "rf"
  )

expand_grid(spi_scoring, model_names)
