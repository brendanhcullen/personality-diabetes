

# Load libraries ----------------------------------------------------------

library(missMDA)
library(psych)
library(tidyverse)

load(here("output/data_cleaned.Rdata"))

# spi_135_items <- datasets$data_spi_135 %>% 
#   select_if(is.numeric)

spi_135_items <- datasets$data_spi_135 %>% 
  select_if(is.numeric) %>% 
  sample_n(500) # randomly sample 500 rows to reduce run-time

# fa.parallel
#pc_number <- fa.parallel(spi_135_items)$ncomp
pc_number <- 30

# single imputation
sipca_local = function(data_df, num_components)
{
  single_impute = imputePCA(X = data_df,
                             ncp = num_components,
                             method = "Regularized", # default
                             coeff.ridge = 1, # default (see documentation)
                             maxiter = 1000 # default
  )
  
  imputed_data = as.data.frame(single_impute$completeObs)
  
  return(imputed_data)
}

spi_135_items_imputed = sipca_local(data_df = spi_135_items, num_components = pc_number)
