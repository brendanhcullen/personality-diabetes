
# Function: Get SPI names -------------------------------------------------

get_spi_names = function(keys){
  
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


# Function: Score SPI-5 ---------------------------------------------------

score_spi_5 = function(data, keys){
  
  spi_names = get_spi_names(keys)
  
  data_spi_135 = data %>%
      select(spi_names$spi_135)
    
  keys = keys[names(data_spi_135), ] %>% 
      select(contains("spi_135"))
    
  spi_5_keys = keys %>% 
      select(1:5)
    
  # score the Big 5 scales
  scored = scoreVeryFast(as.matrix(spi_5_keys), data_spi_135)
  spi_5_scores = as.data.frame(scored)
  names(spi_5_scores) = spi_names$spi_5
    
  return(spi_5_scores)
}



# Function: Score SPI-27 (using IRT) --------------------------------------

######## ALL IRT CODE WAS COPIED FROM SARA'S SAPA BMI PROJECT ######## 
# more info here: https://github.com/sjweston/SAPA_BMI/tree/master/irt_troubleshoot

score_spi_27 = function(data, keys, IRT_path){

  # load IRT calibrations
  load(IRT_path)
  
  # IRT score
  dataSet <- subset(data, select = c(orderForItems))
  
  SPIirtScores <- matrix(nrow=dim(dataSet)[1], ncol=27)
  
  scaleNames = gsub("SPI27_", "", names(IRToutputSPI27))
  spi_keys = keys %>%
    select(matches("SPI_135")) %>%
    select(-c(1:5)) %>%
    mutate(item = rownames(.)) %>%
    gather("scale", "key", -item) %>%
    filter(key != 0)
  
  for (i in 1:length(IRToutputSPI27)) {
    data1 <- subset(dataSet, select = c(rownames(IRToutputSPI27[[i]]$irt$difficulty[[1]])))
    calibrations <- IRToutputSPI27[[i]]
    #check calibration direction
    loadings = calibrations$fa$loadings[,1]
    loadings = ifelse(loadings < 0, -1, 1)
    loadings = data.frame(item = names(loadings), loadings = loadings)
    keys_direction = spi_keys %>%
      filter(grepl(scaleNames[i], scale)) %>%
      full_join(loadings)
    same = sum(keys_direction$key == keys_direction$loadings)
    if(same == 0) data1[,1:ncol(data1)] = apply(data1[,1:ncol(data1)], 2, function(x) max(x, na.rm=TRUE) + 1 - x)
    if (same > 0 & same < 5) print("Error in loadings")
    scored <- scoreIrt(calibrations, data1, keys = NULL, cut = 0)
    SPIirtScores[,i] <- scored$theta1
  }
  
  SPIirtScores <- as.data.frame(SPIirtScores)
  spi_names = get_spi_names(keys) 
  colnames(SPIirtScores) = spi_names$spi_27
  
 return(SPIirtScores)
}


score = function(data, keys, IRT_path){
  spi_5_scores = score_spi_5(data = data, keys = keys)
  
  # add SPI-5 scores to data
  data = cbind(select(data, -starts_with("q_")),
               spi_5_scores, 
               select(data, starts_with("q_")))
  
  # Score SPI-27 (using IRT) 
  spi_27_scores = score_spi_27(data = data, 
                               keys = keys, 
                               IRT_path = IRT_path)
  
  # add IRT scores to data
  data = cbind(select(data, -starts_with("q_")),
               spi_27_scores,
               select(data, starts_with("q_")))
  return(data)
}
