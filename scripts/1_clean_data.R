# This script imports raw SAPA data and performs basic cleaning operations:
# 1) score raw SPI items according to 5- and 27-factor solutions
# 2) convert varaibles to correct type (e.g. factors)
# 3) create composite demographic variables???
# 4) impute missing data???

# import helper functions
source(here("src/helper_functions.R"))


# Import data -------------------------------------------------------------

# import toydataset to use for writing analysis code
source(here("src/build_toy_data.R"))
data <- toydata %>% 
  rownames_to_column() %>% 
  rename(RID = rowname)

# when importing real dataset use this:
#data <- retrieve_data("doi:10.7910/DVN/TZJGAT", "sapaTempData696items22dec2015thru07feb2017.tab")


# Filter data -------------------------------------------------------------

# filter out individuals who did not respond to diabetes question
data <- data %>% 
  filter(!is.na(diagnosis))

# Score SPI-135 data ----------------------------------------------------------

data_scored <- score_spi(data)

# Fix variable types ------------------------------------------------------

# change char vars to factors 
data_scored <- fix_var_types(data_scored)


# Save cleaned data -------------------------------------------------------

save(data_scored, file = here("output/data_cleaned.Rdata"))
