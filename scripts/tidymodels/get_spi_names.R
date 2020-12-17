
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
