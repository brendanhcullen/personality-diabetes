# `step_residualize` creates a *specification* of a recipe
# step that uses linear models to extract residuals from a set of 
# 'variables of interest' (voi) after controlling for a set of 
# 'variables to control' for (vtc)

# define user-facing function
step_residualize <- function(
  recipe, 
  ..., 
  role = NA, 
  trained = FALSE, 
  skip = FALSE,
  id = rand_id("residualize"),
  voi = NULL,
  vtc = NULL,
  id_var = NULL
) { 
  
  terms <- ellipse_check(...)
  
  add_step(
    recipe, 
    step_residualize_new(
      terms = terms, 
      trained = trained,
      role = role, 
      skip = skip,
      id = id,
      voi = voi,
      vtc = vtc,
      id_var = id_var)
  )
}

# define the constructor function
step_residualize_new <- function(terms, trained, role, skip, id, voi, vtc, id_var) {
  step(
    subclass = "residualize",
    terms = terms, 
    trained = trained,
    role = role, 
    skip = skip,
    id = id,
    voi = voi,
    vtc = vtc,
    id_var = id_var
  )
}



# create the prep method 
prep.step_residualize <- function(x, training, info = NULL, ...) { 
  
  voi <- terms_select(terms = x$terms, info = info) 
  
  ## Use the constructor function to return the updated object. 
  ## Note that `trained` is now set to TRUE
  
  step_residualize_new(
    terms = x$terms, 
    trained = TRUE,
    role = x$role, 
    skip = x$skip,
    id = x$id,
    voi = voi,
    vtc = x$vtc,
    id_var = x$id_var
  )
}


# helper functions --------------------------------------------------------

# function to return centered variables
center <- function(x){
  m <- mean(x, na.rm = TRUE)
  centered <- x - m
  return(centered)
}

# function to write and build linear model
build.lm <- function(y, x, data){
  form <- as.formula(paste(y, x, sep = " ~ "))
  mod <- lm(form, data = data)
  coef <- coef(mod)
  return(coef)
}

# function to esimate predicted values from raw data and coefficients
estimate.pred <- function(data.mat, coef, id_var){
  var <- names(coef)
  id_var_vals <- row.names(data.mat)
  var[1] <- id_var
  data.mat <- data.mat[, var[-1]]
  data.mat <- cbind(1, data.mat)
  pred <- data.mat %*% as.matrix(coef)
  row.names(pred) <- id_var_vals
  return(pred)
}

#function to replace NA with 0
na.0 <- function(x){
  x[is.na(x)] <- 0
  return(x)
}

# create the bake method 
bake.step_residualize <- function(object, new_data, ...) {

  voi <- object$voi
  vtc <- object$vtc
  id_var <- object$id_var
    
    #check to see voi are all numeric
    if(length(voi) > 1) voi_numeric <- apply(new_data[,voi], 2, is.numeric)
    if(length(voi) == 1) voi_numeric <- is.numeric(new_data[,voi])
    voi_not_numeric <- which(!voi_numeric)
    try(if(length(voi_not_numeric) > 0) stop("Some variables of interest are not numeric."))
    
    # check if new_data is a data frame
    try(if(!(is.data.frame(new_data))) stop ("new_data must be a data.frame."))
    
    # check if id_var
    try(if(!(is.character(id_var))) stop ("Please provide the variable name, as a character string, for id."))
    
    # build model formula
    cov.model <- paste(vtc, collapse = " + ")
    
    # center predictors if numeric
    data_pred <- new_data %>%
      select(vtc) %>%
      mutate_if(is.numeric, center)
    new_data[,vtc] <- data_pred
    
    
    # build model across voi and return coefficients
    if(length(voi) > 1) models <- sapply(voi, FUN = function(y) build.lm(y = y, x = cov.model, data = new_data))
    if(length(voi) == 1) models <- build.lm(y = voi, x = cov.model, data = new_data)
    
    # get matrix of predictors
    predictors <- new_data[,c(id_var, vtc)] 
    
    predictors <- predictors %>% 
      mutate_at(id_var, as.character)
    
    numeric <- predictors %>%
      select_if(is.numeric)
    
    if(ncol(numeric) > 0) {
      numeric <- numeric %>%
        mutate_all(center) %>%
        mutate_all(na.0)
    }
    num_var <- names(numeric)
    factor_var <- names(predictors)[which(!(names(predictors) %in% num_var))]
    
    if(length(factor_var) > 1) {
      predictors <- predictors %>% 
      select(factor_var) %>%
      gather("variable", "value", -id_var) %>%
      unite(variable, variable, value, sep = "") %>%
      mutate(value = 1) %>%
      spread(variable, value, fill = 0)}
    
    if (length(factor_var) <= 1){
      predictors <- data.frame(id_var = predictors[,id_var])
      names(predictors)[1] <- id_var
    }
    
    if(ncol(numeric) > 0) predictors <- cbind(predictors, numeric)
    
    predictors <- predictors[order(predictors[,id_var]), ]
    new_data <- new_data[order(as.data.frame(new_data)[,id_var]), ]
    
    # use coefficients to estimate predicted values for everyone
    pred.mat <- predictors %>% select(-id_var)
    pred.mat <- as.matrix(pred.mat)
    row.names(pred.mat) <- predictors[[id_var]] 
    
    #use matrix algebra to apply each linear transformation (coef vector) to columns
    if(is.matrix(models)){
      predicted.values <- apply(models, 2, FUN = function(x) estimate.pred(pred.mat, coef = x, id_var = id_var))
      predicted.values <- cbind(as.numeric(as.character(row.names(pred.mat))), predicted.values)
      predicted.values <- as.data.frame(predicted.values)
      names(predicted.values)[1] <- id_var
      predicted.values[,id_var] <- as.factor(predicted.values[,id_var])
    }
    if(is.list(models)){ 
      predicted.values <- lapply(models, FUN = function(x) estimate.pred(pred.mat, coef = x, id_var = id_var))
      #make that a data.frame
      predicted.values <- data.frame(lapply(predicted.values, function(x) unlist(x)))
      colnames(predicted.values) <- names(models)
      predicted.values[,id_var] <- row.names(pred.mat)
    }
    #if(is.vector(models)) predicted.values = estimate.pred(pred.mat, coef = models, id_var = id_var)
    
    # calculate mean of each voi
    means <- sapply(voi, FUN = function(x) mean(as.data.frame(new_data)[,x], na.rm = TRUE))
    
    # for each voi, take that vector in the original data and subtract the predicted value; 
    # then add the mean of that variable back in
    if(length(voi) > 1){
      resid <- predicted.values
      resid[,voi] <- sapply(seq_along(voi), FUN = function(x) new_data[,voi[x]] - predicted.values[,voi[x]] + means[voi[x]])
    }
    if(length(voi) == 1){
      resid <- predicted.values
      resid[,voi] <- new_data[,voi] - predicted.values + means
      resid <- as.data.frame(resid)
    }
    # replace the existing variables with the residualized ones
    new_data <- new_data[,!(names(new_data) %in% voi)]
    new_data <- full_join(new_data, resid, by = id_var)
  
    ## Always convert to tibbles on the way out
    tibble::as_tibble(new_data)
}
