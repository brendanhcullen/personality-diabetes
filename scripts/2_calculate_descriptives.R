
#load packages
library(here)
library(tidyverse)
library(psych)

# Load data ---------------------------------------------------------------

load(here("output/data_cleaned.Rdata"))

# Calculate descriptives --------------------------------------------------

descriptives <- describeBy(data_scored, group = "diagnosis")

# Save descriptives output ------------------------------------------------

save(descriptives, file = here("output/descriptives.Rdata"))
