
library(here)
library(tidyverse)
library(caret)


# Load data ---------------------------------------------------------------

load(here("output/data_cleaned.Rdata"))

# Wrangle data ------------------------------------------------------------

data_ml <- data_scored %>% 
  select(c(RID, diagnosis, starts_with("q_")))

