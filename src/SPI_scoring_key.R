SPI_5_Scales <- c("SPI_5_1", "SPI_5_2", "SPI_5_3", "SPI_5_4", "SPI_5_5")
SPI_27_Scales <- c("SPI_27_1", "SPI_27_2", "SPI_27_3", "SPI_27_4", "SPI_27_5", "SPI_27_6", "SPI_27_7", "SPI_27_8", "SPI_27_9", "SPI_27_10", "SPI_27_11", "SPI_27_12", "SPI_27_13", "SPI_27_14", "SPI_27_15", "SPI_27_16", "SPI_27_17", "SPI_27_18", "SPI_27_19", "SPI_27_20", "SPI_27_21", "SPI_27_22", "SPI_27_23", "SPI_27_24", "SPI_27_25", "SPI_27_26", "SPI_27_27")

autoKeying <- function (EFAn = EFAn)
  
{
  
  EFAn = EFAn
  
  keying.list <- list(list())
  item.list <- list(list())
  n <- 0
  
  for (i in 1:EFAn) {
    n <- n+1
    expLdgs <- eval(parse(text=paste("expSampleCor696autoFA$FAoutput$fa", EFAn, "$fa$loadings", sep="")))
    repLdgs <- eval(parse(text=paste("repSampleCor696autoFA$FAoutput$fa", EFAn, "$fa$loadings", sep="")))
    if (EFAn == 5) {
      rows <- eval(parse(text=SPI_5_Scales[i]))
      colExp <- SPI_5exploratoryFactorCorrespondence[i]
      colRep <- SPI_5replicationFactorCorrespondence[i]
      named <- SPI_5_Scales
    }
    if (EFAn == 27) {
      rows <- eval(parse(text=SPI_27_Scales[i]))
      colExp <- SPI_27exploratoryFactorCorrespondence[i]
      colRep <- SPI_27replicationFactorCorrespondence[i]
      named <- SPI_27_Scales
    }
    expVals <- expLdgs[rows, colExp]
    repVals <- repLdgs[rows, colRep]
    expVals[expVals < 0] <- -1
    expVals[expVals > 0] <- 1
    repVals[repVals < 0] <- -1
    repVals[repVals > 0] <- 1
    ifelse(sum(repVals-expVals) == 0, x <- expVals, ifelse(sum(repVals+expVals) == 0, x <- expVals, x <- c("NA")))
    z <- list()
    for (j in 1:length(x)) {
      z[[j]] <- paste("", names(x[j]), sep="")
    }
    z <- unlist(z)
    item.list[[i]] <- z
    y <- list()
    for (j in 1:length(x)) {
      ifelse(x[j] == -1, y[[j]] <- paste("-", names(x[j]), sep=""), y[[j]] <- paste("", names(x[j]), sep=""))
    }
    y <- unlist(y)
    keying.list[[i]] <- y
    rm(colExp, colRep, expLdgs, expVals, i, j, repLdgs, repVals, rows)
  }
  names(keying.list) <- named
  names(item.list) <- named
  rm(n)
  results <- list(keys = keying.list, items = item.list)
  return(results)
} 

keys.list27 <- autoKeying(EFAn = 27)

keys.list5 <- autoKeying(EFAn = 5)

# Some manual changes to the direction of scoring are needed.
# The following 7 lines of code were added post-hoc,
# after the crowd-sourced names were derived.
# All we are doing here is flipping the direction of the items.
keys.list27$keys$SPI_27_3 <- c("q_1904", "-q_312", "-q_684", "q_4243", "-q_1923")
keys.list27$keys$SPI_27_4 <- c("-q_578", "-q_811", "q_1371", "q_2765", "q_820")
keys.list27$keys$SPI_27_7 <- c("-q_4296", "q_1812", "q_2853", "-q_1896", "-q_501")
keys.list27$keys$SPI_27_8 <- c("-q_904", "q_1744", "q_1979", "-q_1452", "-q_1444")
keys.list27$keys$SPI_27_18 <- c("q_219", "-q_1081", "q_1706", "-q_1635", "q_803")
keys.list27$keys$SPI_27_19 <- c("-q_607", "q_348", "-q_610", "q_1132", "-q_612")
keys.list5$keys$SPI_5_4 <- c("q_90", "q_1763", "q_253", "-q_1896", "q_851", "q_1832", "-q_501", "q_377", "-q_871", "q_1855", "-q_4296", "-q_142", "q_379", "q_4289")