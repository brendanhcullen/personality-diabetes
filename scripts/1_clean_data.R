# This script imports raw SAPA data and performs basic cleaning operations:
# 1) score raw SPI items according to 5- and 27-factor solutions
# 2) convert varaibles to correct type (e.g. factors)
# 3) create composite demographic variables???
# 4) impute missing data???

library(here)
library(tidyverse)
library(psych)
library(janitor)

# import helper functions
source(here("src/helper_functions.R"))

# Import data -------------------------------------------------------------

# import toydataset to use for writing analysis code
source(here("src/build_toy_data.R"))

data = toydata %>% 
  rownames_to_column() %>% 
  rename(RID = rowname)

# when importing real dataset use this:
#data = retrieve_data("doi:10.7910/DVN/TZJGAT", "sapaTempData696items22dec2015thru07feb2017.tab")

# Filter data -------------------------------------------------------------

# remove who did not respond to diabetes question
data = data %>% 
  filter(!is.na(diagnosis))

# Score SPI-135 data ----------------------------------------------------------

#data_scored = score_spi(data)

keys = read.table(url("https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/TZJGAT/YGMHBT"), 
                  header = TRUE, 
                  row.names = 1)

# select just the rows that correspond to variables in the current SAPA dataset
keys = keys[names(data), ]

# select just the scales that are scored using the SPI_135 form
keys = keys %>%
  select(contains("SPI_135"))

spi_135_indices = grepl("q_", levels(spi.dictionary$item_id))
spi_135_names = subset(levels(spi.dictionary$item_id), spi_135_indices)

tmp1 <- data %>% 
  select(-starts_with("q_")) 
tmp2 <- data %>% 
  select(spi_135_names) #
data <- cbind(tmp1, tmp2)


# score the items (this contains item and scale statistics too!)
scored = scoreItems(keys, data)
spi_scores = as.data.frame(scored$scores)
names(spi_scores) = gsub("SPI_135_27_5_", "", names(spi_scores))

# add scores to data
data_scored = cbind(data, spi_scores) %>% 
  clean_names()

spi_names = gsub("SPI_135_27_5_", "", names(keys))
spi_5_names = spi_names[1:5]
spi_27_names = spi_names[6:32]

# Fix variable types ------------------------------------------------------

# change char vars to factors 
data_scored = fix_var_types(data_scored)


# Save cleaned data -------------------------------------------------------

save(data_scored, file = here("output/data_cleaned.Rdata"))
