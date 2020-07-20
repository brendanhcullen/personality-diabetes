# This script creates a specification for each machine learning model using the {parsnip}
# package from tidymodels. A URL has been included for each model that links to a webpage 
# with more information. 

library(tidyverse)
library(tidymodels)
library(here)
library(discrim)

# Model specs -------------------------------------------------------------

# multinomial logistic regression
# https://parsnip.tidymodels.org/reference/multinom_reg.html

multinom_spec <- 
  multinom_reg() %>% 
  set_engine("nnet") %>% 
  set_mode("classification") %>% 
  set_args(penalty = tune())

# k-nearest neighbor
# https://parsnip.tidymodels.org/reference/nearest_neighbor.html
knn_spec <- 
  nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("classification") %>% 
  set_args(neighbors = tune(), 
           weight_func = tune(),
           dist_power = tune())

# neural network
# https://parsnip.tidymodels.org/reference/mlp.html
nnet_spec <- 
  mlp() %>% 
  set_engine("nnet") %>% 
  set_mode("classification") %>% 
  set_args(hidden_units = tune(),
           penalty = tune(),
           epochs = 10000) # by default epochs (maxit) = 100

# support vector machine (radial basis)
# https://parsnip.tidymodels.org/reference/svm_rbf.html
svm_spec <- 
  svm_rbf() %>% 
  set_engine("kernlab") %>% 
  set_mode("classification") %>% 
  set_args(cost = tune(),
           rbf_sigma= tune())

# linear discriminant analysis
# https://discrim.tidymodels.org/reference/discrim_linear.html
# NOTE: need to specify discrim_linear() comes from {discrim}, 
# otherwise the function won't be found

lda_spec <- 
  discrim::discrim_linear() %>% 
  set_engine("mda") %>% 
  set_mode("classification") %>% 
  set_args(penalty = tune())

# decision tree
# https://parsnip.tidymodels.org/reference/decision_tree.html
tree_spec <- 
  decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification") %>% 
  set_args(cost_complexity = tune(),
           tree_depth = tune(),
           min_n = tune())

# random forest
rf_spec <- 
  rand_forest() %>% 
  set_engine("ranger") %>% 
  set_mode("classification") %>% 
  set_args(mtry = tune(),
           min_n = tune(),
           trees = 1000)


# Save model specs --------------------------------------------------------

# put model specs into a list
model_specs <- list(multinom_spec = multinom_spec,
                    knn_spec = knn_spec,
                    nnet_spec = nnet_spec, 
                    svm_spec = svm_spec,
                    lda_spec = lda_spec,
                    tree_spec = tree_spec,
                    rf_spec = rf_spec)

saveRDS(model_specs, file = here("output", "tidymodels", "model_specs.RDS"))
