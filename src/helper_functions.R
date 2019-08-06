
library(tidyverse)
library(dataverse)
library(data.table)

# get data from dataverse
retrieve_data <- function(doi, dataset_name){
  dataset <- get_dataset(doi)
  writeBin(get_file(dataset_name, doi), dataset_name)
  dataset <- fread(dataset_name, na.strings=getOption("<NA>","NA")) %>%
    as.data.frame() %>% 
    subset(select = -c(1))
  file.remove(dataset_name)
  return(dataset)
}

