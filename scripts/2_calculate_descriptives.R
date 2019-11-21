# This script takes cleaned data and calculates descriptive statistics for all variables.

#load packages
library(here)
library(tidyverse)
library(psych)

# Load data ---------------------------------------------------------------

load(here("output/data_cleaned.Rdata"))


# Calculate descriptives --------------------------------------------------

descriptives = data_scored %>% 
  select(-starts_with("q_")) %>% 
  group_by(diabetes) %>% 
  summarise_all(list(~mean(., na.rm = TRUE),
                     ~sd(., na.rm = TRUE))) %>% 
  gather(stat, value, -diabetes) %>% 
  separate(stat, c("variable", "stat"), sep = "[_$]") # can't figure out this regexp

#descriptives = describeBy(data_scored, group = "diabetes")

# Save descriptives output ------------------------------------------------

save(descriptives, file = here("output/descriptives.Rdata"))
