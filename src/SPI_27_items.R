options(width=110)
#SPI_27
# `prelim' labels are mine, not the final crowd-sourced names.
# Factor 1 items - prelim label: compassion
SPI_27_1 <- c("q_90", "q_253", "q_1763", "q_851", "q_1832")
# other acceptable items: q_844, q_298, q_460, q_98, q_1962
x1 <- factorTable1[SPI_27_1,]
y1 <- psych::describe(x1[,c(10:14)], fast=TRUE)

# Factor 2 items - prelim label: irritability
SPI_27_2 <- c("q_952", "q_1357", "q_1585", "q_1683", "q_176")
# other acceptable items: q_926, q_1591, q_4300, q_965, q_4235
x2 <- factorTable2[SPI_27_2,]
y2 <- psych::describe(x2[,c(10:14)], fast=TRUE)

# Factor 3 items - prelim label: sociability
SPI_27_3 <- c("q_1904", "q_312", "q_684", "q_4243", "q_1923")
# other acceptable items: q_1899, q_1531, q_4247, q_4267, q_314
x3 <- factorTable3[SPI_27_3,]
y3 <- psych::describe(x3[,c(10:14)], fast=TRUE)

# Factor 4 items - prelim label: well-being
SPI_27_4 <- c("q_578", "q_811", "q_1371", "q_2765", "q_820")
# other acceptable items: q_285, q_1044, q_1038, q_1578, q_832
x4 <- factorTable4[SPI_27_4,]
y4 <- psych::describe(x4[,c(10:14)], fast=TRUE)

# Factor 5 items - prelim label: sensation-seeking
SPI_27_5 <- c("q_1367", "q_1664", "q_1662", "q_1781", "q_598")
# other acceptable items: q_1306, q_292, q_1780, q_2011, q_804
x5 <- factorTable5[SPI_27_5,]
y5 <- psych::describe(x5[,c(10:14)], fast=TRUE)

# Factor 6 items - prelim label: worry
SPI_27_6 <- c("q_4249", "q_1989", "q_4252", "q_1505", "q_808")
# other acceptable items: q_4256, q_1596, q_986, q_85, q_332
x6 <- factorTable6[SPI_27_6,]
y6 <- psych::describe(x6[,c(10:14)], fast=TRUE)

# Factor 7 items - prelim label: honesty
SPI_27_7 <- c("q_4296", "q_1812", "q_2853", "q_1896", "q_501")
# other acceptable items: q_1765, q_1808, q_2021, q_393, q_2003
x7 <- factorTable7[SPI_27_7,]
y7 <- psych::describe(x7[,c(10:14)], fast=TRUE)

# Factor 8 items - prelim label: industry
SPI_27_8 <- c("q_904", "q_1744", "q_1979", "q_1452", "q_1444")
# other acceptable items: q_1087, q_992, q_985, q_637, q_602
x8 <- factorTable8[SPI_27_8,]
y8 <- psych::describe(x8[,c(10:14)], fast=TRUE)

# Factor 9 items - prelim label: intellect
SPI_27_9 <- c("q_1253", "q_240", "q_493", "q_422", "q_1834")
# other acceptable items: q_722, q_1235, q_423, q_132, q_157
x9 <- factorTable9[SPI_27_9,]
y9 <- psych::describe(x9[,c(10:14)], fast=TRUE)

# Factor 10 items - prelim label: imaginative
SPI_27_10 <- c("q_128","q_2745", "q_2754", "q_1392", "q_1058")
# other acceptable items: q_516, q_1090, q_609, q_1083, q_1232
x10 <- factorTable10[SPI_27_10,]
y10 <- psych::describe(x10[,c(10:14)], fast=TRUE)

# Factor 11 items - prelim label: impulsivity
SPI_27_11 <- c("q_35", "q_1424", "q_1173", "q_4223","q_4276")
# other acceptable items: q_636, q_1638, q_4286, q_708, q_22
x11 <- factorTable11[SPI_27_11,]
y11 <- psych::describe(x11[,c(10:14)], fast=TRUE)

# Factor 12 items - prelim label: attention-seeking
SPI_27_12 <- c("q_1416", "q_1296", "q_565", "q_1555", "q_1027")
# other acceptable items: q_1667, q_1871, q_690, q_698, q_154
x12 <- factorTable12[SPI_27_12,]
y12 <- psych::describe(x12[,c(10:14)], fast=TRUE)

# Factor 13 items - prelim label: orderliness
SPI_27_13 <- c("q_1201", "q_1254", "q_1483", "q_169", "q_1290")
# other acceptable items: q_1333, q_1255, q_170, q_4254, q_1374
x13 <- factorTable13[SPI_27_13,]
y13 <- psych::describe(x13[,c(10:14)], fast=TRUE)

# Factor 14 items - prelim label: conventionalism
SPI_27_14 <- c("q_369", "q_398", "q_1624", "q_1867", "q_1609")
# other acceptable items: q_4236, q_1752, q_4244, q_1657, q_623
x14 <- factorTable14[SPI_27_14,]
y14 <- psych::describe(x14[,c(10:14)], fast=TRUE)

# Factor 15 items - prelim label: charisma
# This is a weak factor.
# The first 3 items listed below are great: q_254, q_1045, and q_131.
# q_901 loads -.45 in the exp sample and -.29 in the rep sample but has cross-loadings with sociability of -.30 in the rep sample (only .14 in the exp sample).
# q_1242 is retained for the same reasons: loading of -.38 in the rep sample but only -.25 in the exp sample
SPI_27_15 <- c("q_254", "q_901", "q_1045", "q_131", "q_1242")
# other marginally acceptable items: q_158, q_3090, q_1205, q_1110, and q_241
x15 <- factorTable15[SPI_27_15,]
y15 <- psych::describe(x15[,c(10:14)], fast=TRUE)

# Factor 16 items - prelim label: trust
SPI_27_16 <- c("q_1855", "q_4289", "q_377", "q_871", "q_379")
# other acceptable items: q_1854, q_594, q_1758, q_343, q_286
x16 <- factorTable16[SPI_27_16,]
y16 <- psych::describe(x16[,c(10:14)], fast=TRUE)

# Factor 17 items - prelim label: humor
SPI_27_17 <- c("q_1243", "q_1244", "q_1248", "q_296", "q_1685")
# other acceptable items: q_4265, q_1064, q_1709, q_4295, q_799
x17 <- factorTable17[SPI_27_17,]
y17 <- psych::describe(x17[,c(10:14)], fast=TRUE)

# Factor 18 items - prelim label: expressiveness
SPI_27_18 <- c("q_219", "q_1081", "q_1706", "q_1635", "q_803")
# other acceptable items: q_403, q_1810, q_55, q_1668, q_140
x18 <- factorTable18[SPI_27_18,]
y18 <- psych::describe(x18[,c(10:14)], fast=TRUE)

# Factor 19 items - prelim label: culture
SPI_27_19 <- c("q_607", "q_348", "q_610", "q_1132", "q_612")
# other acceptable items: q_1369, q_776, q_1388, q_608, q_964
x19 <- factorTable19[SPI_27_19,]
y19 <- psych::describe(x19[,c(10:14)], fast=TRUE)

# Factor 20 items - prelim label: reflection
SPI_27_20 <- c("q_1389", "q_1738", "q_1880", "q_755", "q_1310")
# other acceptable items: q_778, q_1587, q_322, q_197, q_4232
x20 <- factorTable20[SPI_27_20,]
y20 <- psych::describe(x20[,c(10:14)], fast=TRUE)

# Factor 21 items - prelim label: perfectionism
# This is a problematic factor. In the exploratory sample, this factor has considerable overlap with the 13th factor (order); the overlap is far less in the replication sample (in fact, this is a very strong factor in the replication sample).
SPI_27_21 <- c("q_571", "q_1915", "q_1694", "q_530", "q_142")
# other acceptable items: maybe q_4278, q_554, q_71, q_1984, q_604
x21 <- factorTable21[SPI_27_21,]
y21 <- psych::describe(x21[,c(10:14)], fast=TRUE)

# Factor 22 items - prelim label: selfdiscipline
# A weak factor in both samples (few items with strong loadings); stability across samples is okay.
SPI_27_22 <- c("q_1462", "q_1590", "q_1461", "q_56", "q_736")
# other acceptable items: maybe q_1120, q_1474
x22 <- factorTable22[SPI_27_22,]
y22 <- psych::describe(x22[,c(10:14)], fast=TRUE)

# Factor 23 items - prelim label: modesty
# This factor is problematic bc it was much stronger in the replication sample than the exploratory sample.
SPI_27_23 <- c("q_1300", "q_2005", "q_747", "q_152", "q_1653")
# other somewhat acceptable items: q_1691, q_525, q_1151, q_1913, q_319
x23 <- factorTable23[SPI_27_23,]
y23 <- psych::describe(x23[,c(10:14)], fast=TRUE)

# Factor 24 items - prelim label: adjustability
SPI_27_24 <- c("q_566", "q_689", "q_1542", "q_39", "q_1303")
# other acceptable items: q_38, q_582, q_47, q_77, q_1540
x24 <- factorTable24[SPI_27_24,]
y24 <- psych::describe(x24[,c(10:14)], fast=TRUE)

# Factor 25 items - prelim label: inactivity
# This factor is has few items in both samples
SPI_27_25 <- c("q_1329", "q_1281", "q_1052", "q_1024", "q_1280")
# other acceptable items: q_1372
x25 <- factorTable25[SPI_27_25,]
y25 <- psych::describe(x25[,c(10:14)], fast=TRUE)

# Factor 26 items - prelim label: emotionlability
# This factor is problematic, predictably related to Factor 2 (irritability).  They are correlated and there may be an argument for collapsing the two, though it will depend on the evidence for discriminant validity.
SPI_27_26 <- c("q_1840", "q_797", "q_979", "q_793", "q_174")
# other acceptable items: q_1099, q_4224, q_497, q_52
x26 <- factorTable26[SPI_27_26,]
y26 <- psych::describe(x26[,c(10:14)], fast=TRUE)

# Factor 27 items - prelim label: conservatism
# This factor replicates in both samples.  That said, the administration of these items is likely to be problematic in many settings, particularly those which are governed by legal oversight (employment and possibly healthcare settings).  These items explicitly ask about political views and religiosity.
SPI_27_27 <- c("q_1824", "q_660", "q_345", "q_1825","q_1328")
# other acceptable items: q_4260, q_394, q_398, q_1301, q_1223
x27 <- factorTable27[SPI_27_27,]
y27 <- psych::describe(x27[,c(10:14)], fast=TRUE)

SPI_27_Items <- c(SPI_27_1, SPI_27_2, SPI_27_3, SPI_27_4, SPI_27_5, SPI_27_6, SPI_27_7, SPI_27_8, SPI_27_9, SPI_27_10, SPI_27_11, SPI_27_12, SPI_27_13, SPI_27_14, SPI_27_15, SPI_27_16, SPI_27_17, SPI_27_18, SPI_27_19, SPI_27_20, SPI_27_21, SPI_27_22, SPI_27_23, SPI_27_24, SPI_27_25, SPI_27_26, SPI_27_27)