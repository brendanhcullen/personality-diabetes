
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
train_index <- createDataPartition(data_ml$diagnosis,
                                   times = 1,
                                   p = .8,
                                   list = FALSE)

train_data <- data_ml[train_index, ] # training data
test_data <- data_ml[-train_index, ] # holdout testing data

# Logistic regression -----------------------------------------------------


