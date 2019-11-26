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
  var[1] = id
  data.mat = data.mat[, var]
  pred = data.mat %*% as.matrix(coef)
  return(pred)
}

residualize = function(VOI = NULL, VTC = NULL, id = NULL, data = NULL){
  #check to see VOI are all numeric
  VOI_numeric = apply(data[,VOI], 2, is.numeric)
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
  models = sapply(VOI, FUN = function(y) build.lm(y = y, x = cov.model, data = data))
  # get matrix of predictors
  predictors = data[,c(id, VTC)] 
  predictors[, id] = as.character(predictors[, id])
  numeric = predictors %>%
    select_if(is.numeric) %>%
    mutate_all(center)
  num_var = names(numeric)
  predictors = predictors[,which(!(names(predictors) %in% num_var))]
  predictors = predictors %>% 
    gather("variable", "value", -id) %>%
    unite(variable, variable, value, sep = "") %>%
    mutate(value = 1) %>%
    spread(variable, value, fill = 0) 
  predictors = cbind(predictors, numeric)
  
  # use coefficients to estimate predicted values for everyone
  pred.mat = predictors
  pred.mat[,id] = 1
  pred.mat = as.matrix(pred.mat)
  
  #use matrix algebra to apply each linear transformation (coef vector) to columns
  predicted.values = apply(models, 2, FUN = function(x) estimate.pred(pred.mat, coef = x, id = id))
  #make that a data.frame
  #predicted.values <- data.frame(matrix(unlist(predicted.values), ncol=length(predicted.values), byrow=F))
  
  # calculate mean of each VOI
  means = sapply(VOI, FUN = function(x) mean(data[,x], na.rm=T))
  
  # for each VOI, take that vector in the original data and subtract the predicted value; 
  # then add the mean of that variable back in
  resid = sapply(seq_along(VOI), FUN = function(x) data[,VOI[x]] - predicted.values[,VOI[x]] + means[VOI[x]])
  # replace the existing variables with the residualized ones
  newdata = data
  newdata[,VOI] = resid
  # return the new data frame
  return(newdata)
}
