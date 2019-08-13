
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
data_nested <- expand.grid(
  comparison = c("t1.v.healthy", "t2.v.healthy", "t1.v.t2"),
  trait = data_nested$trait,
  stringsAsFactors = FALSE) %>% 
  left_join(data_nested) %>% 
  mutate(data = case_when(comparison == "t1.v.t2" ~ map(data, ~filter(.x, !diagnosis == "healthy")),
                          comparison == "t1.v.healthy" ~ map(data, ~filter(.x, !diagnosis == "t2d")),
                          comparison == "t2.v.healthy" ~ map(data, ~filter(.x, !diagnosis == "t1d"))))



# Run multiple t-tests ----------------------------------------------------

# run t-test for each personality trait variable
t_test_output <- data_nested %>% 
  mutate(t_test = map(data, ~broom::tidy(t.test(score ~ diagnosis, data = .)))) %>% 
  select(-data) %>% 
  unnest() 

adj_p <- t_test_output %>% 
  mutate(p.value = p.adjust(p.value, method = "holm"))
