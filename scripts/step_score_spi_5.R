# `step_score_spi_5` creates a *specification* of a recipe
# step that will score the spi_5 variables with sum scoring
# and add them to the data

# define user-facing function
step_score_spi_5 <- function(
  recipe, 
  ..., 
  role = NA, 
  trained = FALSE, 
  keys = NULL, 
  skip = FALSE,
  id = rand_id("score_spi_5")
) { 
  
  terms <- ellipse_check(...)
  
  add_step(
    recipe, 
    step_score_spi_5_new(
      terms = terms, 
      trained = trained,
      keys = keys,
      role = role, 
      skip = skip,
      id = id)
  )
}

# define the constructor function
step_score_spi_5_new <- function(terms, trained, role, skip, id, keys) {
  step(
    subclass = "score_spi_5",
    terms = terms, 
    trained = trained,
    role = role, 
    skip = skip,
    id = id,
    keys = keys
  )
}

# function to extract spi names from keys
get_spi_names <- function(keys){
  
  # extract SPI names
  spi_names <- keys %>% 
    clean_names() %>% 
    select(contains("spi_135_27_5")) %>% 
    names() %>% 
    gsub("spi_135_27_5_", "", .)
  
  spi_5_names <- spi_names[1:5]
  
  spi_27_names <- spi_names[6:32]
  
  spi_135_names <- keys %>% 
    clean_names() %>% 
    select(contains("spi_135_27_5")) %>% 
    rownames_to_column() %>% 
    filter(rowname %in% grep("q_*", rownames(keys), value = TRUE)) %>% 
    mutate_if(is.numeric, abs) %>% 
    mutate(row_sum = rowSums(.[,-1])) %>% 
    filter(!row_sum == 0) %>% 
    pull(rowname)
  
  return(list(spi_5 = spi_5_names, 
              spi_27 = spi_27_names,
              spi_135 = spi_135_names))
}


# create the prep method 
prep.step_score_spi_5 <- function(x, training, info = NULL, ...) { 
  
  # translate the specification listed in the terms argument to column names in the current data
  col_names <- terms_select(terms = x$terms, info = info) 
  
  spi_5_names <- get_spi_names(x$keys)$spi_5
  
  # select only the spi_135 items
  data_spi_135 <- training[, col_names]
  
  # subset keys
  keys <- x$keys[names(data_spi_135), ] %>% 
    select(contains("spi_135"))
  
  spi_5_keys <- keys %>% 
    select(contains(spi_5_names))
  
  x$keys <- spi_5_keys
  
  ## Use the constructor function to return the updated object. 
  ## Note that `trained` is now set to TRUE
  
  step_score_spi_5_new(
    terms = x$terms, 
    trained = TRUE,
    role = x$role, 
    skip = x$skip,
    id = x$id,
    keys = x$keys
  )
}

# create the bake method 
bake.step_score_spi_5 <- function(object, new_data, ...) {
  
  # score the Big 5 scales
  scored <- psych::scoreItems(object$keys, new_data)
  
  # extract just the data frame of scores
  spi_5_scores <- as_tibble(scored$scores)
  
  # get spi_5 names
  spi_5_names <- get_spi_names(object$keys)$spi_5
  
  # assign names to spi_5 scores
  names(spi_5_scores) <- spi_5_names
  
  # add spi_5 scores to data
  new_data <- cbind(new_data %>% select(-starts_with("q_")),
                    spi_5_scores, 
                    new_data %>% select(starts_with("q_")))
  
  ## Always convert to tibbles on the way out
  tibble::as_tibble(new_data)
  }
