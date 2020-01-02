
library(here)

source(here("scripts/0.2_residualize.R"))
source(here("scripts/0.3_score_spi.R"))
source(here("scripts/0.4_impute.R"))

preprocess_sapa = function(data, keys = keys, id ="RID", VOI = all_spi_names, covariates, path = here("data/IRTinfoSPI27.rdata"), order){

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
    data = score(data, keys = keys, path = path)
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
    data = score(data, keys = keys, path = path)
  }
  
  if(order[3] == "residualize"){
    data = residualize(VOI = VOI, VTC = covariates, data = data, id = id)
  }
  if(order[3] == "impute"){
    data = impute_missing(data = data, vars_to_impute = VOI)
  }
  if(order[3] == "score"){
    data = score(data, keys = keys, path = path)
  }
  
  return(data)
}
