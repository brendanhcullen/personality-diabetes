
library(tidyverse)
library(here)

scripts_dir = here("output/machine_learning/training/scripts")
logs_dir = here("output/machine_learning/training/logs")

# 
pattern = "spi_135*"
#pattern = "spi_5"

scripts = list.files(scripts_dir, full.names = TRUE, pattern = pattern)

safe_source = safely(source)

safe_source_list = map(scripts, safe_source)

