
# define user-facing function
step_score_spi_27 <- function(
  recipe, 
  ..., 
  role = NA, 
  trained = FALSE, 
  keys = NULL, 
  IRT_path = NULL,
  skip = FALSE,
  id = rand_id("score_spi_27")
) { 
  
  terms <- ellipse_check(...)
  
  add_step(
    recipe, 
    step_score_spi_27_new(
      terms = terms, 
      trained = trained,
      keys = keys,
      IRT_path = IRT_path,
      role = role, 
      skip = skip,
      id = id)
  )
}

# define the constructor function
step_score_spi_27_new <- function(terms, trained, role, skip, id, keys, IRT_path) {
  step(
    subclass = "score_spi_27",
    terms = terms, 
    trained = trained,
    role = role, 
    skip = skip,
    id = id,
    keys = keys,
    IRT_path = IRT_path
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
prep.step_score_spi_27 <- function(x, training, info = NULL, ...) { 
  
  # # translate the specification listed in the terms argument to column names in the current data
  # col_names <- terms_select(terms = x$terms, info = info) 
  # 
  # # select only the spi_135 items
  # data_spi_135 <- training[, col_names]
  # 
  # # load IRT calibrations
  # load(x$IRT_path)
  # 
  # # IRT score
  # dataSet <- subset(data_spi_135, select = c(orderForItems))
  # 
  # # create empty matrix
  # SPIirtScores <- matrix(nrow=dim(dataSet)[1], ncol=27)
  # 
  # # spi_27 names
  # scaleNames <- gsub("SPI27_", "", names(IRToutputSPI27))
  
  # wrangle keys
  spi_keys <- x$keys %>%
    select(matches("SPI_135")) %>%
    select(-c(1:5)) %>%
    mutate(item = rownames(.)) %>%
    gather("scale", "key", -item) %>%
    filter(key != 0)
  
  # update keys
  x$keys <- spi_keys
  
  
  
  ## Use the constructor function to return the updated object. 
  ## Note that `trained` is now set to TRUE
  
  step_score_spi_27_new(
    terms = x$terms, 
    trained = TRUE,
    role = x$role, 
    skip = x$skip,
    id = x$id,
    keys = x$keys,
    IRT_path = x$IRT_path
  )
}

# create the bake method 
bake.step_score_spi_27 <- function(object, new_data, ...) {
  
  # load IRT calibrations
  load(object$IRT_path)
  
  # IRT score
  dataSet <- subset(new_data, select = c(orderForItems))
  
  # create empty matrix
  SPIirtScores <- matrix(nrow=dim(dataSet)[1], ncol=27)
  
  # spi_27 names
  scaleNames <- gsub("SPI27_", "", names(IRToutputSPI27))

  # calculate IRT scores
  for (i in 1:length(IRToutputSPI27)) {
    data1 <- subset(dataSet, select = c(rownames(IRToutputSPI27[[i]]$irt$difficulty[[1]])))
    calibrations <- IRToutputSPI27[[i]]
    #check calibration direction
    loadings = calibrations$fa$loadings[,1]
    loadings = ifelse(loadings < 0, -1, 1)
    loadings = data.frame(item = names(loadings), loadings = loadings)
    keys_direction = object$keys %>%
      filter(grepl(scaleNames[i], scale)) %>%
      full_join(loadings)
    same = sum(keys_direction$key == keys_direction$loadings)
    if(same == 0) data1[,1:ncol(data1)] = apply(data1[,1:ncol(data1)], 2, function(x) max(x, na.rm=TRUE) + 1 - x)
    if (same > 0 & same < 5) print("Error in loadings")
    scored <- psych::scoreIrt(calibrations, data1, keys = NULL, cut = 0)
    SPIirtScores[,i] <- scored$theta1
  }
  
  # put IRT scores into a tibble
  SPIirtScores <- as_tibble(SPIirtScores)
  
  colnames(SPIirtScores) <- (scaleNames)
  
  SPIirtScores <- SPIirtScores %>% 
    janitor::clean_names()
  
  # add spi_27 scores to data
  new_data <- cbind(new_data %>% select(-starts_with("q_")),
                    SPIirtScores, 
                    new_data %>% select(starts_with("q_")))
  
  ## Always convert to tibbles on the way out
  tibble::as_tibble(new_data)
}
