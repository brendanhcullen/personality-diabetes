
# import helper functions
source(here("src/helper_functions.R"))

# import toydataset to use for writing analysis code
source(here("src/build_toy_data.R"))
data <- toydata

data_scored <- scored_data <- score_spi(data)

save(data_scored, file = here("output/data_cleaned.Rdata"))
