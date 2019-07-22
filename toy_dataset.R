packages = c("tidyverse")

lapply(packages, library, character.only = TRUE)

# load data from Dataverse
load("~/Google Drive/Work/Research/ongoing/Diabetes x Personality/toy data/sapaTempData696items22dec2015thru07feb2017.RData")

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
  
#rename dataset and add random diagnosis variable  
toydata = sapaTempData696items22dec2015thru07feb2017 %>%
  mutate(diagnosis = sample(x = c("t1d", "t2d", "healthy"),
                            size = nrow(.), 
                            replace = TRUE, 
                            prob = c(p_t1d, p_t2d, p_healthy)))
  
  
save(toydata, file = "toy data/toy.Rdata")
