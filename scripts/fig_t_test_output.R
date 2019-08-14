# This script plots the output from t-tests comparing diabetes groups on 5- and 27-factor SPI scores.

library(here)
library(tidyverse)


# Load output from t-tests ------------------------------------------------

load(here("output/t_test_output.Rdata"))

big5 <- c("open", "consc", "extra", "agree", "neuro")

t_test_output <- t_test_output %>% 
  mutate(trait = as.factor(trait))

# plot results for big 5 traits
t_test_output %>% 
  filter(trait %in% big5) %>% 
  ggplot(aes(fct_reorder(trait, statistic), statistic)) +
  geom_point(aes(color = trait), size = 4) +
  coord_flip() + 
  facet_wrap(~comparison) + 
  theme_minimal() + 
  theme(legend.position = "none")

# plot results for spi_27 traits
t_test_output %>% 
  filter(!trait %in% big5) %>% 
  #ggplot(aes(fct_reorder(trait, cohens_d), cohens_d)) +
  ggplot(aes(trait, cohens_d)) +
  geom_point(aes(color = trait), size = 4) +
  coord_flip() + 
  facet_wrap(~comparison) + 
  labs(x = "", y = "Effect size (d)") +
  theme_minimal() + 
  theme(legend.position = "none")
