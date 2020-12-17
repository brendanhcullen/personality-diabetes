# `step_impute_pca` creates a *specification* of a recipe
# step that will impute missing values in the spi_135 items 
# using `imputePCA()` from the {missMDA} package

# define user-facing function
step_impute_pca <- function(
  recipe, 
  ..., 
  role = NA, 
  trained = FALSE, 
  skip = FALSE,
  id = rand_id("impute_pca"),
  columns = NULL
) { 
  
  terms <- ellipse_check(...)
  
  add_step(
    recipe, 
    step_impute_pca_new(
      terms = terms, 
      trained = trained,
      role = role, 
      skip = skip,
      id = id,
      columns = columns)
  )
}

# define the constructor function
step_impute_pca_new <- function(terms, trained, role, skip, id, columns) {
  step(
    subclass = "impute_pca",
    terms = terms, 
    trained = trained,
    role = role, 
    skip = skip,
    id = id,
    columns = columns
  )
}

# create the prep method 
prep.step_impute_pca <- function(x, training, info = NULL, ...) { 
  
  col_names <- terms_select(x$terms, info = info)
  
  ## Use the constructor function to return the updated object. 
  ## Note that `trained` is now set to TRUE
  
  step_impute_pca_new(
    terms = x$terms, 
    trained = TRUE,
    role = x$role, 
    skip = x$skip,
    id = x$id,
    columns = col_names
  )
}

# create the bake method 
bake.step_impute_pca <- function(object, new_data, ...) {
  
  # specify which variables to impute
  col_names <- object$columns
  
  # check to see if variables to impute are all numeric
  if(length(col_names) > 1) vars_numeric <- apply(new_data[,col_names], 2, is.numeric)
  if(length(col_names) == 1) vars_numeric <- is.numeric(new_data[,col_names])
  vars_not_numeric <- which(!vars_numeric)
  try(if(length(vars_not_numeric) > 0) stop("Some variables to impute are not numeric."))
  
  # select only variables to impute
  data_to_impute <- new_data[, col_names]
  
  # coerce to data.frame to avoid weird error message
  data_to_impute <- as.data.frame(data_to_impute)
  
  # use fa.parallel to determine number of principal components
  pc_number <- psych::fa.parallel(data_to_impute)$ncomp
  
  # single imputation
  sipca_local <- function(data_df, num_components)
  {
    single_impute <- missMDA::imputePCA(X = data_df,
                                        ncp = num_components,
                                        method = "Regularized", # default
                                        coeff.ridge = 1, # default (see documentation)
                                        maxiter = 10000 # default = 1000
    )
    
    imputed_data <- as.data.frame(single_impute$completeObs)
    
    return(imputed_data)
  }
  
  # actually impute the missing values in data_to_impute
  data_imputed <- sipca_local(data_df = data_to_impute, 
                              num_components = pc_number)
  
  # replace the columns containing missing data with imputed data
  new_data[, col_names] <- data_imputed
  
  ## Always convert to tibbles on the way out
  tibble::as_tibble(new_data)
}
