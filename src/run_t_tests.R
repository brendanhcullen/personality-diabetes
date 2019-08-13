
# load libraries
library(here)
library(tidyverse)
library(broom)

# load cleaned data
load(here("output/data_cleaned.Rdata"))

# Create nested dataframes ------------------------------------------------

# convert to long format and nest
data_nested <- data_scored %>% 
  select(-contains("q_")) %>% # remove raw SPI items
  select(-("gender":"p2occIncomeEst")) %>% # remove demographic vars
  gather(-diagnosis, key = trait, value = score) %>% 
  group_by(trait) %>% 
  nest()

# organize dataframe to run t-tests
data_ <- expand.grid(
  comparison = c("t1.v.healthy", "t2.v.healthy", "t1.v.t2"),
  trait = df$trait,
  stringsAsFactors = FALSE) %>% 
  left_join(df) %>% 
  mutate(data = case_when(comparison == "t1.v.t2" ~ map(data, ~filter(.x, !diagnosis == "healthy")),
                          comparison == "t1.v.healthy" ~ map(data, ~filter(.x, !diagnosis == "t2d")),
                          comparison == "t2.v.healthy" ~ map(data, ~filter(.x, !diagnosis == "t1d"))))


t.test(agree ~ diagnosis, data = data_scored %>% filter(!diagnosis == "t2d")) %>% 
  tidy()
