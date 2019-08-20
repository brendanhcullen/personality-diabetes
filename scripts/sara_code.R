
library(here)
library(tidyverse)
library(caret)


# Load data ---------------------------------------------------------------

load(here("output/data_cleaned.Rdata"))

set.seed(081919)

# Wrangle data and partition -----------------------------------------------

# data wrangling
data_ml <- data_scored %>% 
  select(c(RID, diagnosis, starts_with("q_"))) %>% 
  mutate(diagnosis = as.factor(diagnosis))

# partition into training (80%) and testing (20%)
partition <- createDataPartition(data_ml$diagnosis,
                                 times = 1,
                                 p = .8,
                                 list = FALSE)

dev_set <- data_ml[partition, ] # development set
holdout_set <- data_ml[-partition, ] # holdout set

# partiton initial development data into train and test data
partition2 <- createDataPartition(dev_set$diagnosis,
                                  times = 1,
                                  p = .7,
                                  list = FALSE)

training <- dev_set[partition2, ]
testing <- dev_set[-partition2, ]

kfold <- trainControl(method = "repeatedcv",
                      number = 2, 
                      repeats = 2, 
                      sampling = "up") # up, down, or smote

# Impute missing data -----------------------------------------------------

training = training %>%
  dplyr::select(-RID) %>%
  mutate_all(impute, fun = "random") %>%
  filter(row_number() %in% sample(1:38682, size = 5000, replace = FALSE))

testing = testing %>%
  mutate_all(impute, fun = "random") 


# Logistic regression -----------------------------------------------------

pmr.grid = expand.grid(decay = seq(from = 0, to = .5, by = .1))

pmr <- train(diagnosis ~ .,
             data = training, 
             method = "multinom",
             trControl = kfold, 
             tuneGrid = pmr.grid,
             na.action = "na.exclude",
             metric = "Kappa") # select the best model based on Kappa

predicted_pmr = predict(pmr$finalModel, newdata = testing)
CM_pmr = confusionMatrix(predicted_pmr, testing$diagnosis)
View(CM_pmr)
