
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
                      number = 10, 
                      repeats = 10)

# Logistic regression -----------------------------------------------------


