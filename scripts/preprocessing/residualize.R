# this script creates a function to residualize variables of interest (e.g., personality items or scores) 
# controlling for demographic variables (e.g., race, SES)

# function to return centered variables
center = function(x){
  m = mean(x, na.rm=T)
  centered = x-m
  return(centered)
}

# function to write and build linear model
build.lm = function(y, x, data){
  form = as.formula(paste(y, x, sep = " ~ "))
  mod = lm(form, data = data)
  coef = coef(mod)
  return(coef)
}

# function to esimate predicted values from raw data and coefficients
estimate.pred = function(data.mat, coef, id){
  var = names(coef)
  id_vals = row.names(data.mat)
  var[1] = id
  data.mat = data.mat[, var[-1]]
  data.mat = cbind(1, data.mat)
  pred = data.mat %*% as.matrix(coef)
  row.names(pred) = id_vals
  return(pred)
}

#function to replace NA with 0
na.0 = function(x){
  x[is.na(x)] = 0
  return(x)
}

residualize = function(VOI = NULL, VTC = NULL, id = NULL, data = NULL){
  newdata = data
  #check to see VOI are all numeric
  if(length(VOI) > 1) VOI_numeric = apply(data[,VOI], 2, is.numeric)
  if(length(VOI) == 1) VOI_numeric = is.numeric(data[,VOI])
  VOI_not_numeric = which(!VOI_numeric)
  try(if(length(VOI_not_numeric) > 0) stop("Some variables of interest are not numeric."))
  
  try(if(!(is.data.frame(data))) stop ("data must be a data.frame."))
  try(if(!(is.character(id))) stop ("Please provide the variable name, as a character string, for id."))
  
  # build model formula
  cov.model = paste(VTC, collapse = " + ")
  
  # center predictors if numeric
  data_pred = data %>%
    select(VTC) %>%
    mutate_if(is.numeric, center)
  data[,VTC] = data_pred

  
  # build model across VOI and return coefficients
  if(length(VOI) > 1) models = sapply(VOI, FUN = function(y) build.lm(y = y, x = cov.model, data = data))
  if(length(VOI) == 1) models = build.lm(y = VOI, x = cov.model, data = data)
  # get matrix of predictors
  predictors = data[,c(id, VTC)] 
  predictors[, id] = as.character(predictors[, id])
  numeric = predictors %>%
    select_if(is.numeric)
  if(ncol(numeric) > 0) {
    numeric = numeric %>%
      mutate_all(center) %>%
      mutate_all(na.0)
    }
  num_var = names(numeric)
  factor_var = names(predictors)[which(!(names(predictors) %in% num_var))]
  if(length(factor_var) > 1) {predictors = predictors %>% 
    select(factor_var) %>%
    gather("variable", "value", -id) %>%
    unite(variable, variable, value, sep = "") %>%
    mutate(value = 1) %>%
    spread(variable, value, fill = 0)}
  if (length(factor_var) <= 1){
    predictors = data.frame(id = predictors[,id])
    names(predictors)[1] = id
  }
  if(ncol(numeric) > 0) predictors = cbind(predictors, numeric)
  
  predictors = predictors[order(predictors[,id]), ]
  data = data[order(data[,id]), ]
  
  # use coefficients to estimate predicted values for everyone
  pred.mat = predictors %>% select(-id)
  pred.mat = as.matrix(pred.mat)
  row.names(pred.mat) = predictors$RID
  
  #use matrix algebra to apply each linear transformation (coef vector) to columns
  if(is.matrix(models)){
    predicted.values = apply(models, 2, FUN = function(x) estimate.pred(pred.mat, coef = x, id = id))
    predicted.values = cbind(as.numeric(as.character(row.names(pred.mat))), predicted.values)
    predicted.values = as.data.frame(predicted.values)
    names(predicted.values)[1] = id
    predicted.values[,id] = as.factor(predicted.values[,id])
  }
  if(is.list(models)){ 
    predicted.values = lapply(models, FUN = function(x) estimate.pred(pred.mat, coef = x, id = id))
    #make that a data.frame
    predicted.values <- data.frame(lapply(predicted.values, function(x) unlist(x)))
    colnames(predicted.values) = names(models)
    predicted.values[,id] = row.names(pred.mat)
  }
  #if(is.vector(models)) predicted.values = estimate.pred(pred.mat, coef = models, id = id)
  
  # calculate mean of each VOI
  means = sapply(VOI, FUN = function(x) mean(data[,x], na.rm=T))
  
  # for each VOI, take that vector in the original data and subtract the predicted value; 
  # then add the mean of that variable back in
  if(length(VOI) > 1){
    resid = predicted.values
    resid[,VOI] = sapply(seq_along(VOI), FUN = function(x) data[,VOI[x]] - predicted.values[,VOI[x]] + means[VOI[x]])
    }
  if(length(VOI) == 1){
    resid = predicted.values
    resid[,VOI] = data[,VOI] - predicted.values + means
    resid = as.data.frame(resid)
    }
  # replace the existing variables with the residualized ones
  newdata = newdata[,!(names(newdata) %in% VOI)]
  newdata = full_join(newdata, resid, by = id)
  # return the new data frame
  return(newdata)
}
