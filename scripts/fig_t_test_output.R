# This script plots the output from t-tests comparing diabetes groups on 5- and 27-factor SPI scores.

library(here)
library(tidyverse)


# Load output from t-tests ------------------------------------------------

load(here("output/t_test_output.Rdata"))

# Build plot --------------------------------------------------------------

# set ggplot theme
theme_set(theme_minimal(base_size = 18))

# wrangle data for plotting
t_test_output <- t_test_output %>% 
  mutate(trait = factor(trait) %>% 
           fct_reorder(cohens_d))

big5 <- c("open", "consc", "extra", "agree", "neuro")

# plot results for big 5 traits
t_test_output %>% 
  filter(trait %in% big5) %>% 
  ggplot(aes(trait, cohens_d)) +
  geom_errorbar(aes(ymin = d_conf_low,
                    ymax = d_conf_high,
                    width = 0.15)) +
  geom_point(aes(color = trait), size = 4) +
  coord_flip() + 
  facet_wrap(~comparison) + 
  labs(x = "", y = "Effect size (d)") +
  #theme_minimal(base_size = 17) + 
  theme(legend.position = "none",
        panel.grid.minor = element_blank())


# Save plot ---------------------------------------------------------------

ggsave("effect_sizes.png", width = 15, height = 10, path = here("figs"))
