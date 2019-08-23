
library(here)
library(gt)
library(tidyverse)

# load descriptives
load(here("output/descriptives.Rdata"))


descriptives <- data.frame(diagnosis = c("t1d", "t2d", "healthy")) %>% 
  mutate(desc = descriptives) %>% 
  mutate(desc = map(desc, ~rownames_to_column(as.data.frame(.x)))) %>% 
  unnest() %>% 
  rename(variable = rowname) %>% 
  select(variable, diagnosis, n, mean, sd) %>% 
  gather(key, value, -variable, -diagnosis) %>% 
  spread(variable, diagnosis)



ex <- rownames_to_column(as.data.frame(descriptives[[2]][[1]]))
