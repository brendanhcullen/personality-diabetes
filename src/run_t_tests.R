
# load libraries
library(here)
library(tidyverse)

# load cleaned data
load(here("output/data_cleaned.Rdata"))

# Create nested dataframes ------------------------------------------------

df <- data_scored %>% 
  select(-contains("q_")) %>% # remove raw SPI items
  select(-("gender":"p2occIncomeEst")) %>% # remove demographic vars
  gather(-diagnosis, key = trait, value = score) %>% 
  group_by(trait) %>% 
  nest()




