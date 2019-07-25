options(width=110)
ItemTable$SPI27 <- ifelse(rownames(ItemTable) %in% rownames(allItems), 1, 0)

SPI_5exploratoryFactorCorrespondence = c(1, 2, 3, 4, 5)
SPI_5replicationFactorCorrespondence = c(1, 2, 4, 3, 5)

EFAn = 5
matchrep = SPI_5replicationFactorCorrespondence

exp <- eval(parse(text=paste("expSampleCor696autoFA$FAoutput$fa", EFAn, "$fa$loadings", sep="")))
rep <- eval(parse(text=paste("repSampleCor696autoFA$FAoutput$fa", EFAn, "$fa$loadings", sep="")))

for (n in 1:EFAn) {
  
  stable <- eval(parse(text=paste("replicationEFA", EFAn, "_", n, "$stableItems", sep="")))
  pass <- eval(parse(text=paste("replicationEFA", EFAn, "_", n, "$structuralPass", sep="")))
  qual <- eval(parse(text=paste("replicationEFA", EFAn, "_", n, "$qualItems", sep="")))
  ldgs <- eval(parse(text=paste("replicationEFA", EFAn, "_", n, "$magldgs", sep="")))
  
  # Sort the items in descending order by the magnitude of the average primary loading 
  sortedLdgs <- sort(ldgs, decreasing=TRUE)
  factorTable <- ItemTable[names(sortedLdgs),]
  factorTable$sortedLdgs <- sortedLdgs
  factorTable$stable <- ifelse(rownames(factorTable) %in% stable, 1, 0)
  factorTable$pass <- ifelse(rownames(factorTable) %in% pass, 1, 0)
  factorTable$qual <- ifelse(rownames(factorTable) %in% qual, 1, 0)
  factorTable$stability <- ifelse(factorTable$stable==1 & factorTable$pass==1 & factorTable$qual==1 & factorTable$SPI27==1, 1, ifelse(factorTable$stable==1 & factorTable$pass==1 & factorTable$qual==1, 2, ifelse(factorTable$pass==1 & factorTable$qual==1, 3, ifelse(factorTable$qual==1, 4, 5))))
  factorTable$qualitativeRank <- round((scale(factorTable$clarity)+scale(factorTable$salience)-scale(factorTable$readgrade)-scale(factorTable$readage)-scale(factorTable$lexile))[,1], 2)
  factorTable <- subset(factorTable, select = c(Item, sortedLdgs, stable, pass, qual, SPI27, stability, clarity, salience, lexile, readage, readgrade, qualitativeRank))
  assign(paste("factors5Table", n, sep=""), factorTable)
  write.csv(factorTable, file=paste(filepathdata, "factorTable/factors5Table", n, ".csv", sep=""))
}

# Factor 1 items - prelim label: Extraversion
SPI_5_1 <- c("q_1904", "q_4243", "q_312", "q_565", "q_1416", "q_1923", "q_1027", "q_684", "q_254", "q_1296", "q_901", "q_1243", "q_803", "q_1244")
# all spi27 items invariant on this factor:
# q_1904, q_4243, q_312, q_565, q_1416, q_1923, q_1027, q_684, q_254, q_1296, q_901, q_1243, q_803, q_1244, q_1635, q_219, q_296, q_1781, q_1248, q_598, q_1662, q_1045, q_1371, q_1052, q_1081, q_1242, q_1555
# Note: all of the above were also stable
# qualified (but not inv.) in Exp or Rep sample: q_2765, q_39
x1_5 <- data.frame(factors5Table1[SPI_5_1,c(1,2,7:12)],ItemInfo696[SPI_5_1,c(2:4,6,7)])
y1_5 <- psych::describe(x1_5[,c(3:7)], fast=TRUE)

# Factor 2 items - prelim label: Neuroticism
SPI_5_2 <- c("q_979", "q_4252", "q_1989", "q_1505", "q_4249", "q_808", "q_793", "q_1840", "q_811", "q_1585", "q_578", "q_176", "q_797", "q_1683")
# all spi27 items invariant on this factor:
# q_979, q_4252, q_1989, q_1505, q_4249, q_808, q_793, q_1840, q_811, q_1585, q_578, q_176, q_797, q_1683, q_820, q_1706, q_566, q_1357
# Note: q_1357 was not stable
# qualified (but not inv.) in Exp or Rep sample: q_174, q_689, q_56
x2_5 <- data.frame(factors5Table2[SPI_5_2,c(1,2,7:12)],ItemInfo696[SPI_5_2,c(2:4,6,7)])
y2_5 <- psych::describe(x2_5[,c(3:7)], fast=TRUE)

# Factor 3 items - prelim label: Conscientiousness
SPI_5_3 <- c("q_1290", "q_1744", "q_1979", "q_1452", "q_1915", "q_1201", "q_530", "q_904", "q_1867", "q_1694", "q_369", "q_1444", "q_1483", "q_1254")
# all spi27 items invariant on this factor:
# q_1290, q_1744, q_1979, q_1452, q_1915, q_1201, q_530, q_904, q_1867, q_1694, q_369, q_1444, q_1483, q_1254, q_571, q_398, q_169, q_736, q_1024
# Note: all of the above were also stable
# qualified (but not inv.) in Exp or Rep sample: q_4223, q_1424, q_1328
x3_5 <- data.frame(factors5Table3[SPI_5_3,c(1,2,7:12)],ItemInfo696[SPI_5_3,c(2:4,6,7)])
y3_5 <- psych::describe(x3_5[,c(3:7)], fast=TRUE)

# Factor 4 items - prelim label: Agreeableness
SPI_5_4 <- c("q_90", "q_1763", "q_253", "q_1896", "q_851", "q_1832", "q_501", "q_377", "q_871", "q_1855", "q_4296", "q_142", "q_379", "q_4289")
# all spi27 items invariant on this factor:
# q_90, q_1763, q_253, q_1896, q_851, q_1832, q_501, q_377, q_871, q_1855, q_4296, q_142, q_379, q_4289, q_1367, q_1664
# Note: all of the above were also stable
# qualified (but not inv.) in Exp or Rep sample: q_2853
x4_5 <- data.frame(factors5Table4[SPI_5_4,c(1,2,7:12)],ItemInfo696[SPI_5_4,c(2:4,6,7)])
y4_5 <- psych::describe(x4_5[,c(3:7)], fast=TRUE)

# Factor 5 items - prelim label: Openness
SPI_5_5 <- c("q_128", "q_2745", "q_2754", "q_1392", "q_1058", "q_240", "q_1738", "q_422", "q_1389", "q_1310", "q_1880", "q_747", "q_1609", "q_1834")
# all spi27 items invariant on this factor:
# q_128, q_2745, q_2754, q_1392, q_1058, q_240, q_1738, q_422, q_1389, q_1310, q_1880, q_747, q_1609, q_1834, q_1132, q_493, q_1303, q_610, q_755, q_1300, q_1653, q_2005, q_607, q_612, q_152, q_1253, q_1542
# Note: all of the above were also stable
# qualified (but not inv.) in Exp or Rep sample: q_348, q_1825
x5_5 <- data.frame(factors5Table5[SPI_5_5,c(1,2,7:12)],ItemInfo696[SPI_5_5,c(2:4,6,7)])
y5_5 <- psych::describe(x5_5[,c(3:7)], fast=TRUE)

SPI_5_Items <- c(SPI_5_1, SPI_5_2, SPI_5_3, SPI_5_4, SPI_5_5)

# How many SPI items in total?
length(SPI_5_Items)
# Confirm they're all unique...
length(unique(SPI_5_Items))
# What are the item labels?
unique(SPI_5_Items)
# How many of them overlap with 27 factor items?
length(intersect(SPI_5_Items, SPI_27_Items))