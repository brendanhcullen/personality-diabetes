# This script takes in the raw data and formats it for the t-test analysis (Aim 1).

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(psych)
library(here)
library(janitor)

# Load dataset ------------------------------------------------------------
source(here("src/build_toy_data.R"))

# Clean SPI data ----------------------------------------------------------

# names of personality vars
spi_135_items <- toydata %>% 
  select(starts_with("q_")) %>% 
  names() %>% 
  subset(. %in% spi.dictionary$item_id)

# only retain personality items that corespond to SPI 135
spi_135_indices <- which(names(toydata) %in% spi_135_items)

toydata_demographics <- toydata %>% select(-starts_with("q_")) # demographic vars only
toydata_spi_135 <- toydata %>% select(spi_135_indices) # spi items only

toydata2 <- cbind(toydata_demographics, toydata_spi_135)
rm(list = c("toydata_demographics", "toydata_spi_135", "spi_135_items", "spi_135_indices"))

# score SPI_5 and SPI_27
pers_scores <- scoreVeryFast(spi.keys, toydata2) %>% # note: psych::scoreItems gives lots of reliability stats
  as.data.frame() %>% 
  clean_names()

# combine
toydata_scored <- cbind(toydata2, pers_scores) 
rm(list = c("toydata2", "pers_scores", "toydata"))

