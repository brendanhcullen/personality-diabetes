score = function(data, keys, path){
  spi_5_scores = score_spi_5(data = data, keys = keys)
  
  # add SPI-5 scores to data
  data = cbind(select(data, -starts_with("q_")),
               spi_5_scores, 
               select(data, starts_with("q_")))
  
  # Score SPI-27 (using IRT) 
  spi_27_scores = score_spi_27(data = data, 
                               keys = keys, 
                               path_to_IRT_calibrations = path)
  
  # add IRT scores to data
  data = cbind(select(data, -starts_with("q_")),
               spi_27_scores,
               select(data, starts_with("q_")))
  return(data)
}

preprocess = function(data, keys = keys, id ="RID", VOI = all_spi_names, covariates, path = here("data/IRTinfoSPI27.rdata"), order){

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