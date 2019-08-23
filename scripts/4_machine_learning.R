
library(here)
library(tidyverse)
library(caret)
library(nnet)
library(Hmisc)
library(e1071)


# Load data ---------------------------------------------------------------

load(here("output/data_cleaned.Rdata"))

set.seed(081919)

# Wrangle data and partition -----------------------------------------------

# data wrangling
data_ml <- data_scored %>% 
  select(c(diagnosis, starts_with("q_"))) %>% 
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

cv <- trainControl(method = "repeatedcv",
                      number = 2, 
                      repeats = 2)

# Impute missing data -----------------------------------------------------

# randomly impute for now...update this real imputation code later!
training <- training %>% 
  mutate_all(Hmisc::impute, fun = "random") %>% 
  filter(row_number() %in% sample(1:nrow(.), size = 5000, replace = FALSE))
  

# Logistic regression -----------------------------------------------------

pmr <- train(diagnosis ~ .,
      data = training,
      method = "multinom", 
      trControl = cv,
      na.action = "na.exclude")

pmr

summary(pmr)

