# This script plots the output from t-tests comparing diabetes groups on 5- and 27-factor SPI scores.

# load libraries
library(here)
library(tidyverse)

# Load output from t-tests ------------------------------------------------

readRDS(here("output/t_tests/t_test_output.RDS"))

# Build plot --------------------------------------------------------------

# set ggplot theme
theme_set(theme_minimal(base_size = 18))

# wrangle data for plotting
t_test_output = t_test_output %>% 
  mutate(trait = factor(trait) %>% 
           fct_reorder(cohens_d))

spi5 = c("open", "consc", "extra", "agree", "neuro")
spi27 = c(
  "compassion",
  "trust",
  "honesty",
  "conservatism",
  "authoritarianism",
  "easy_goingness",
  "perfectionism",
  "order",
  "industry",
  "impulsivity",
  "self_control",
  "emotional_stability",
  "anxiety",
  "irritability",
  "well_being",
  "emotional_expressiveness",
  "sociability",
  "adaptability",
  "charisma",
  "humor",
  "attention_seeking",
  "sensation_seeking",
  "conformity",
  "introspection",
  "art_appreciation",
  "creativity",
  "intellect"
)

# plot results for big 5 traits
t_test_output %>% 
  filter(trait %in% spi5) %>% 
  ggplot(aes(trait, cohens_d)) +
  geom_errorbar(aes(ymin = d_conf_low,
                    ymax = d_conf_high,
                    width = 0.15)) +
  geom_point(aes(color = trait), size = 4) +
  coord_flip() + 
  facet_wrap(~comparison) + 
  labs(x = "", y = "Effect size (d)") +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())


plot_d = function(spi_traits) {
  plot = t_test_output %>% 
  filter(trait %in% spi_traits) %>% 
  ggplot(aes(trait, cohens_d)) +
  geom_errorbar(aes(ymin = d_conf_low,
                    ymax = d_conf_high,
                    width = 0.15)) +
  geom_point(aes(color = trait), size = 3) +
  coord_flip() + 
  facet_wrap(~comparison) + 
  labs(x = "", y = "Effect size (d)") +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())
  
  return(plot)
}

spi5_plot = plot_d(spi5)
spi27_plot = plot_d(spi27)

# Save plot ---------------------------------------------------------------

save_plot = function(num_traits) {
 # plot = paste0("spi", num_traits, "_plot")
  
  ggsave(paste0("spi_", num_traits, "_effsizes", ".png"),
         plot = get(paste0("spi", num_traits, "_plot")),
         width = 15,
         height = 10, 
         path = here("figs"))
}

save_plot(5)
save_plot(27)

