set.seed(072219)

packages = c("tidyverse", "dataverse", "data.table", "here")
lapply(packages, library, character.only = TRUE)
rm(packages)

# load data from Dataverse
Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")
dataset4 <- get_dataset("doi:10.7910/DVN/TZJGAT")
writeBin(get_file("sapaTempData696items22dec2015thru07feb2017.tab", "doi:10.7910/DVN/TZJGAT"), "sapaTempData696items22dec2015thru07feb2017.tab")
sapaTempData696items22dec2015thru07feb2017 <- fread("sapaTempData696items22dec2015thru07feb2017.tab", na.strings=getOption("<NA>","NA"))
sapaTempData696items22dec2015thru07feb2017 <- as.data.frame(sapaTempData696items22dec2015thru07feb2017)
sapaTempData696items22dec2015thru07feb2017 <- subset(sapaTempData696items22dec2015thru07feb2017, select = -c(1))
rm(dataset4)

# add a diabetes condition variable
  # known rates of diabetes from http://www.diabetes.org/diabetes-basics/statistics/
  # 9.4% of population has diabetes
  # 1.25 million have Type 1; 30.3 million total
  
  p_d = .094
  p_t1d_ifd = (1.25/30.3) #probability of T1 if have diabetes diagnosis
  #use these values to calculate probability of each diagnosis
  p_t1d = p_t1d_ifd*p_d
  p_t2d = p_d - p_t1d
  p_healthy = 1-p_d
  
#rename dataset and add random diabetes variable  
toydata = sapaTempData696items22dec2015thru07feb2017 %>%
  mutate(diabetes = sample(x = c("t1d", "t2d", "healthy"),
                            size = nrow(.), 
                            replace = TRUE, 
                            prob = c(p_t1d, p_t2d, p_healthy)))
  
rm(sapaTempData696items22dec2015thru07feb2017)
rm(list = ls(pattern = "p_"))

# remove raw datafile
file.remove("sapaTempData696items22dec2015thru07feb2017.tab")

# save toydata to file
save(toydata, file = here("data/toydata.Rdata"))