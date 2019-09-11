
library(tidyverse)
library(here)

scripts_dir = here("output/machine_learning/training/scripts")
logs_dir = here("output/machine_learning/training/logs")

# use to indicate which set of scripts you want to run. 
pattern = "spi_5"
#pattern = "*"

scripts = list.files(scripts_dir, full.names = TRUE, pattern = pattern)
script_names = script_names = list.files(scripts_dir, full.names = FALSE, pattern = pattern)

# create 'safe' version of 'source' function that outputs error messages
safe_source = safely(source)

# execute all selected .R scripts and create list of error messages
map(scripts, safe_source)
