# need to run scripts resample_random.R and (4.4_compare_ml_models OR 4.4.1_compare_ml_models_nosub.R) first

resamps$values %>% 
  gather("key", "value") %>%
  separate(key, into = c("key", "metric"), sep = "~") %>%
  mutate(key = gsub("spi_", "", key)) %>%
  separate(key, into = c("key", "num"), sep = "_") %>%
  filter(metric == "Accuracy") %>%
  group_by(key, num) %>%
  summarise(accuracy = mean(as.numeric(value)), na.rm=T) %>%
  ggplot(aes(x = key, y = accuracy)) +
  geom_point() +
  geom_hline(aes(yintercept = mean(acc))) + 
  coord_flip() +
  facet_wrap(~num)
