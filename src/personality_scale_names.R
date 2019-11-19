# This script reads in superKey.csv and extracts the names of the raw 135 SPI items as
# well as the names of the SPI scored with 5 factors and 27 factors.

# load libraries
library(here)
library(janitor)
library(tidyverse)

# Read in superKey data
keys = read.csv("src/superKey.csv", header = TRUE, row.names = 1)

# extract SPI names
spi_names = keys %>% 
  clean_names() %>% 
  select(contains("spi_135_27_5")) %>% 
  names() %>% 
  gsub("spi_135_27_5_", "", .)

spi_5_names = spi_names[1:5]

spi_27_names = spi_names[6:32]

spi_135_names = keys %>% 
  clean_names() %>% 
  select(contains("spi_135_27_5")) %>% 
  rownames_to_column() %>% 
  filter(rowname %in% grep("q_*", rownames(keys), value = TRUE)) %>% 
  mutate_if(is.numeric, abs) %>% 
  mutate(row_sum = rowSums(.[,-1])) %>% 
  filter(!row_sum == 0) %>% 
  pull(rowname)

rm(keys, spi_names)
