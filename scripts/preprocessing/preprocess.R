
library(here)

source(here("scripts/preprocessing/get_spi_names.R"))
source(here("scripts/preprocessing/score_spi.R"))
source(here("scripts/preprocessing/impute.R"))
source(here("scripts/preprocessing/residualize.R"))

preprocess_sapa = function(data, keys = keys, id ="RID", VOI = all_spi_names, covariates, IRT_path = here("data/IRTinfoSPI27.rdata"), order){

  VOI_items = all_spi_names[grepl("q_", all_spi_names)]
  resid_place = which(order == "residualize")
  impute_place = which(order == "impute")
  score_place = which(order == "score")
  resid_items = FALSE
  impute_items = FALSE
  if (resid_place < score_place) { resid_items = TRUE}
  if (impute_place < score_place) { impute_items = TRUE}
    
  if(order[1] == "residualize" & !resid_items){
    data = residualize(VOI = VOI, VTC = covariates, data = data, id = id)
  }
  if(order[1] == "residualize" & resid_items){
    data = residualize(VOI = VOI_items, VTC = covariates, data = data, id = id)
  }
  if(order[1] == "impute" & !impute_items){
    data = impute_missing(data = data, vars_to_impute = VOI)
  }
  if(order[1] == "impute" & impute_items){
    data = impute_missing(data = data, vars_to_impute = VOI_items)
  }
  if(order[1] == "score"){
    data = score(data, keys = keys, IRT_path = IRT_path)
  }
  
  if(order[2] == "residualize" & !resid_items){
    data = residualize(VOI = VOI, VTC = covariates, data = data, id = id)
  }
  if(order[2] == "residualize" & resid_items){
    data = residualize(VOI = VOI_items, VTC = covariates, data = data, id = id)
  }
  if(order[2] == "impute" & !impute_items){
    data = impute_missing(data = data, vars_to_impute = VOI)
  }
  if(order[2] == "impute" & impute_items){
    data = impute_missing(data = data, vars_to_impute = VOI_items)
  }
  if(order[2] == "score"){
    data = score(data, keys = keys, IRT_path = IRT_path)
  }
  
  if(order[3] == "residualize"){
    data = residualize(VOI = VOI, VTC = covariates, data = data, id = id)
  }
  if(order[3] == "impute"){
    data = impute_missing(data = data, vars_to_impute = VOI)
  }
  if(order[3] == "score"){
    data = score(data, keys = keys, IRT_path = IRT_path)
  }
  
  return(data)
}

# test the function

demographic_vars = c(
  "age", # age
  "ethnic",  # ethnicity
  "jobstatus", # current job status
  "education", "occPrestige", "occIncomeEst", # self SES
  "p1edu", "p1occPrestige", "p1occIncomeEst", # parent 1 SES
  "p2edu", "p2occPrestige", "p2occIncomeEst") # parent 2 SES

# newdata = preprocess_sapa(data = data, 
#                           keys = keys, 
#                           id = "RID", 
#                           VOI = all_spi_names, 
#                           covariates = demographic_vars, 
#                           IRT_path = here("data/IRTinfoSPI27.rdata"), 
#                           order = c("score", "impute", "residualize"))


