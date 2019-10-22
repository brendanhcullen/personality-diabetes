dataSet <- subset(sapa, select = c(orderForItems))

SPIirtScores <- matrix(nrow=dim(dataSet)[1], ncol=27)

scaleNames = gsub("SPI27_", "", names(IRToutputSPI27))
spi_keys = keys %>%
  select(matches("SPI_135")) %>%
  select(-c(1:5)) %>%
  mutate(item = rownames(.)) %>%
  gather("scale", "key", -item) %>%
  filter(key != 0)

for (i in 1:length(IRToutputSPI27)) {
  data <- subset(dataSet, select = c(rownames(IRToutputSPI27[[i]]$irt$difficulty[[1]])))
  calibrations <- IRToutputSPI27[[i]]
  #check calibration direction
  loadings = calibrations$fa$loadings[,1]
  loadings = ifelse(loadings < 0, -1, 1)
  loadings = data.frame(item = names(loadings), loadings = loadings)
  keys_direction = spi_keys %>%
    filter(grepl(scaleNames[i], scale)) %>%
    full_join(loadings)
  same = sum(keys_direction$key == keys_direction$loadings)
  if(same == 0) data[,1:ncol(data)] = apply(data[,1:ncol(data)], 2, function(x) max(x, na.rm=T) + 1 - x)
  if (same > 0 & same < 5) print("Error in loadings")
  scored <- scoreIrt(calibrations, data, keys = NULL, cut = 0)
  SPIirtScores[,i] <- scored$theta1
}

SPIirtScores <- as.data.frame(SPIirtScores)
colnames(SPIirtScores) <- paste0("SPI_135_27_5_", scaleNames)

#add to sapa dataset
sapa = cbind(sapa, SPIirtScores)