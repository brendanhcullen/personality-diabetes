# this script creates a function to residualize variables of interest (e.g., personality items or scores) 
# controlling for demographic variables (e.g., race, SES)


# function to write and build linear model
build.lm = function(y, x, data){
  form = as.formula(paste(y, x, sep = " ~ "))
  mod = lm(form, data = data)
  coef = coef(mod)
  return(coef)
}

# function to esimate predicted values from raw data and coefficients
estimate.pred = function(data.mat, coef){
  var = names(coef)
  var[1] = id
  data.mat = data.mat[, var]
  pred = data.mat %*% as.matrix(coef)
  return(pred)
}


VOI = names(data)[grepl("q_", names(data))] #variables of interest
VTC = c("age", "ethnic", "education") # variables to control
id = "RID"


residualize = function(voi = NULL, vtc = NULL, id = NULL, data = NULL){
  #check to see VOI are all numeric
  VOI_numeric = apply(data[,VOI], 2, is.numeric)
  VOI_not_numeric = which(!VOI_numeric)
  try(if(length(VOI_not_numeric) > 0) stop("Some variables of interest are not numeric."))
  
  # build model formula
  cov.model = paste(VTC, collapse = " + ")
  
  # center predictors if numeric
  data_pred = data %>%
    select(VTC) %>%
    mutate_if(is.numeric, scale)
  data[,VTC] = data_pred
  
  # build model across VOI and return coefficients
  models = sapply(VOI, FUN = function(y) build.lm(y = y, x = cov.model, data = data))
  # get matrix of predictors
  predictors = data[,c(id, VTC)] %>%
    mutate_at("age", scale) %>%
    gather("variable", "value", -id, -age) %>%
    unite(variable, variable, value, sep = "") %>%
    mutate(value = 1) %>%
    spread(variable, value, fill = 0) 
  # use coefficients to estimate predicted values for everyone
  pred.mat = predictors
  pred.mat$RID = 1
  pred.mat = as.matrix(pred.mat)
  
  #use matrix algebra to apply each linear transformation (coef vector) to columns
  predicted.values = lapply(models, FUN = function(x) estimate.pred(pred.mat, coef = x))
  #make that a data.frame
  predicted.values <- data.frame(matrix(unlist(predicted.values), ncol=length(predicted.values), byrow=F))
  
  # calculate mean of each VOI
  means = sapply(VOI, FUN = function(x) mean(data[,x], na.rm=T))
  
  # for each VOI, take that vector in the original data and subtract the predicted value; then add the mean of that variable back in
  resid = sapply(seq_along(VOI), FUN = function(x) (data[,VOI[x]] - predicted.values[,x]) + means[x])
  # replace the existing variables with the residualized ones
  newdata = data
  newdata[,VOI] = resid
  # return the new data frame
  return(newdata)
}