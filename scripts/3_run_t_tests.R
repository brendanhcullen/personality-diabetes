# This script takes cleaned and scored SPI data and runs a two-sample t test
# to compare T1 vs. Healthy, T2 vs. Healthy, and T1 vs. T2 on each 

# load libraries
library(here)
library(tidyverse)
library(broom)
library(effsize)
library(rsample)
library(janitor)
library(psych)

# load data that has been filtered based on exclusion criteria
data_filtered = readRDS(here("output/t_tests/data_filtered.RDS"))

# Score SPI data ----------------------------------------------------------

source(here("scripts/preprocessing/score_spi.R"))
spi_names = readRDS(here("output/spi_names.RDS"))
IRT_path = here("data/IRTinfoSPI27.rdata")
keys = read.csv(here("data/superKey.csv"), header = TRUE, row.names = 1)

data_scored = score(data = data_filtered, keys = keys, IRT_path = IRT_path)

# Wrangle data for iteration ------------------------------------------------

# convert to long format and nest
data_nested = data_scored %>% 
  select(diabetes, spi_names$spi_5, spi_names$spi_27) %>% # remove demographic vars and raw items
  gather(-diabetes, key = trait, value = score) %>% # convert to long format
  group_by(trait) %>% 
  nest()

# organize dataframe by group comparison and trait
data_nested = expand.grid(
  comparison = c("t1.v.healthy", "t2.v.healthy", "t1.v.t2"), # create all possible group comparions
  trait = data_nested$trait,
  stringsAsFactors = FALSE) %>% 
  left_join(data_nested) %>% # join with nested dataframe
  # filter nested data frames according to group comparison:
  mutate(data = case_when(comparison == "t1.v.t2" ~ map(data, ~filter(.x, !diabetes == "healthy")),
                          comparison == "t1.v.healthy" ~ map(data, ~filter(.x, !diabetes == "t2d")),
                          comparison == "t2.v.healthy" ~ map(data, ~filter(.x, !diabetes == "t1d"))))


# Iterate t-tests ----------------------------------------------------

# run t-test for each personality trait variable
t_test_output = data_nested %>% 
  mutate(t_test = map(data, ~broom::tidy(t.test(score ~ diabetes, data = .))), # iterate t-tests
         cohens_d = map(data, ~effsize::cohen.d(score ~ diabetes, data = .)) %>% # iterate cohen's d
           map_dbl("estimate")) %>% # extract just Cohen's d estimate from list output
  select(-data) %>% 
  unnest() %>% 
  mutate(p.adj = p.adjust(p.value, method = "holm")) %>% # Holm correction for multiple comparisons
  select(comparison, trait, statistic, p.value, p.adj, cohens_d) # select relevant vars

# Bootstrap Cohen's D ----------------------------------------------------

# number of bootstraps
#boot.n = 100
boot.n = 10000

#helper function
d_boot = function(split){
  effsize::cohen.d(score ~ diabetes, data = analysis(split)) 
  }

# iterate cohen's d confidence intervals
d_confidence = data_nested %>%
  mutate(boots = map(data, rsample::bootstraps, times = boot.n)) %>%
  mutate(boots = map(boots, .f =  function(x) mutate(x, d = map(splits, d_boot)))) %>% #maps in maps!
  mutate(boots = map(boots, .f = function(x) mutate(x, d = map_dbl(d, "estimate")))) %>%
  mutate(boots = map(boots, "d")) %>%
  unnest(boots) %>%
  group_by(comparison, trait) %>%
  dplyr::summarise(d_conf_low = quantile(boots, probs = c(.025)),
            d_conf_high = quantile(boots, probs = c(.975)))

# add to t-test output
t_test_output = full_join(t_test_output, d_confidence)


# Save t-test output ------------------------------------------------------

saveRDS(t_test_output, file = here("output/t_tests/t_test_output.RDS"))
