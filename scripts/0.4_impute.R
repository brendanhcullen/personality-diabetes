
# create a function to impute missing SPI data

impute_missing = function(data = NULL, vars_to_impute = NULL){
  newdata = data
  
  # check to see if variables to impute are all numeric
  if(length(vars) > 1) vars_numeric = apply(data[,vars], 2, is.numeric)
  if(length(vars) == 1) vars_numeric = is.numeric(data[,vars])
  vars_not_numeric = which(!vars_numeric)
  try(if(length(vars_not_numeric) > 0) stop("Some variables to impute are not numeric."))
  
  # select only variables to impute
  data_to_impute = data %>% 
    select(vars_to_impute)
  
  # fa.parallel
  pc_number <- fa.parallel(data_to_impute)$ncomp
  
  # single imputation
  sipca_local = function(data_df, num_components)
  {
    single_impute = imputePCA(X = data_df,
                              ncp = num_components,
                              method = "Regularized", # default
                              coeff.ridge = 1, # default (see documentation)
                              maxiter = 1000 # default; INCREASE THIS???
    )
    
    imputed_data = as.data.frame(single_impute$completeObs)
    
    return(imputed_data)
  }
  
  data_imputed = sipca_local(data_df = data_to_impute, num_components = pc_number)
  
  newdata[,vars_to_impute] = data_imputed
  
  return(newdata)
}

