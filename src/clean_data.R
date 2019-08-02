# This script takes in the raw data and formats it for the t-test analysis (Aim 1).

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(psych)
library(here)
library(janitor)


# Load dataset ------------------------------------------------------------
source(here("src/toy_dataset.R"))

# Clean SPI data ----------------------------------------------------------

# names of personality vars
spi_135_items <- toydata %>% 
  select(starts_with("q_")) %>% 
  names() %>% 
  subset(. %in% spi.dictionary$item_id)

# only retain personality items that corespond to SPI 135
spi_135_indices <- which(names(toydata) %in% spi_135_items)

toydata_demographics <- toydata %>% select(-starts_with("q_"))
toydata_spi_135 <- toydata %>% select(spi_135_indices) 

toy2 <- cbind(toydata_demographics, toydata_spi_135)

# score SPI_5 and SPI_27
pers_scores <- scoreVeryFast(spi.keys, toy2) %>% 
  as.data.frame() %>% 
  clean_names()

pers_scores2 <- scoreItems(spi.keys, toy2) %>% 
  as.data.frame() %>% 
  clean_names()


# combine
toy3 <- cbind(toy2, pers_scores)

