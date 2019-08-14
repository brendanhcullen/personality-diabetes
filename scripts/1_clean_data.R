
# import helper functions
source(here("src/helper_functions.R"))

# import toydataset to use for writing analysis code
source(here("src/build_toy_data.R"))
data <- toydata

# score SPI data
data_scored <- scored_data <- score_spi(data)

# change char vars to factors 
data_scored <- fix_var_types(data_scored)

save(data_scored, file = here("output/data_cleaned.Rdata"))
