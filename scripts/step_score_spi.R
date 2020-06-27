
# define user-facing function
step_score_spi <- function(
  recipe, 
  ..., 
  role = NA, 
  trained = FALSE, 
  skip = FALSE,
  id = rand_id("score_spi"),
  keys = NULL, 
) { 
  
  terms <- ellipse_check(...)
  
  add_step(
    recipe, 
    step_score_spi_new(
      terms = terms, 
      trained = trained,
      role = role, 
      skip = skip,
      id = id,
      keys = keys)
  )
}

# define the constructor function
step_score_spi_new <- function(terms, trained, role, skip, id, keys) {
  step(
    subclass = "score_spi",
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
  spi_names = keys %>% 
    clean_names() %>% 
    select(contains("spi_135_27_5")) %>% 
    names() %>% 
    gsub("spi_135_27_5_", "", .)
  
  spi_5_names = spi_names[1:5]
  
  spi_27_names = spi_names[6:32]
  
  spi_135_names = keys %>% 
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
prep.step_score_spi <- function(x, training, info = NULL, ...) { 
  
  spi_names <- get_spi_names(keys)
  
  keys <- keys[names(data_spi_135), ] %>% 
    select(contains("spi_135"))
  
  spi_5_keys <- keys %>% 
    select(1:5)
  
  ## Use the constructor function to return the updated object. 
  ## Note that `trained` is now set to TRUE
  
  step_score_spi_new(
    terms = x$terms, 
    trained = TRUE,
    role = x$role, 
    skip = x$skip,
    id = x$id,
    keys = x$keys
  )
}

# create the bake method 
bake.step_score_spi <- function(object, new_data, ...) {

  ## Always convert to tibbles on the way out
  tibble::as_tibble(new_data)
  }
