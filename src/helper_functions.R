
library(tidyverse)
library(dataverse)
library(data.table)
library(psych)


# Get data from dataverse -------------------------------------------------

retrieve_data <- function(doi, dataset_name){
  dataset <- get_dataset(doi)
  writeBin(get_file(dataset_name, doi), dataset_name)
  dataset <- fread(dataset_name, na.strings=getOption("<NA>","NA")) %>%
    as.data.frame() %>% 
    subset(select = -c(1))
  file.remove(dataset_name)
  return(dataset)
}

# Score SPI data ----------------------------------------------------------

score_spi <- function(raw_data) {
  # names of personality vars
  spi_135_items <- raw_data %>% 
    select(starts_with("q_")) %>% 
    names() %>% 
    subset(. %in% spi.dictionary$item_id)
  
  # only retain personality items that corespond to SPI 135
  spi_135_indices <- which(names(raw_data) %in% spi_135_items)
  d1 <- raw_data %>% 
    select(-starts_with("q_")) 
  d2 <- raw_data %>% 
    select(spi_135_indices) #
  d3 <- cbind(d1, d2)
  
  # score SPI data
  d4 <- scoreVeryFast(spi.keys, d3) %>% # note: psych::scoreItems gives lots of reliability stats
    as.data.frame() %>% 
    clean_names()
  
  # dataset with 135 items, 5-factor, and 27-factor scoring
  d5 <- cbind(d3, d4) 
  return(d5)
}
  
# Test out the functions --------------------------------------------------
data <- retrieve_data("doi:10.7910/DVN/TZJGAT", "sapaTempData696items22dec2015thru07feb2017.tab")
scored_data <- score_spi(data)