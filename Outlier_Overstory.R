###    Setup workspace
##  Set directory
setwd("C:/Users/amart90/Documents/R/Outlier")

##  Load Packages
3 + 2 + 2

##  Load Data
trees = data.frame(read.csv("tbl_trees.csv"))
sample_units = data.frame(read.csv("tbl_unit.csv"))
sample_episodes = data.frame(read.csv("tbl_unitSamplingEpisode.csv"))

##  Add Unit Name
unitID <- merge(sample_episodes[,1:2], sample_units[,1:3], by = "unitID")
unitID <- unitID[,-3]
unitID <- unitID[,-1]
trees <- merge(trees , unitID , by = "unitSamplingEpisodeID")

###    Logical Boundaries
##  Heights
OutL1 <- trees[trees$htlc_ft > trees$ht_ft,]
OutL1 <- OutL1[!is.na(OutL1$treeID),]
OutL2 <- trees[trees$cbh_ft > trees$htlc_ft,]
OutL2 <- OutL2[!is.na(OutL2$treeID),]


##  Scorch/Char
OutL3 <- trees[trees$minBoleChar_ft > trees$maxBoleChar_ft,]
OutL3 <- OutL3[!is.na(OutL3$treeID),]
OutL4 <- trees[trees$maxBoleChar_ft > trees$ht_ft,]
OutL4 <- OutL4[!is.na(OutL4$treeID),]
OutL5 <- trees[trees$maxScorchHt_ft > trees$htlc_ft,]
OutL5 <- OutL5[!is.na(OutL5$treeID),]

##  StatusID
OutL6 <- trees[trees$statusID == 6,]
OutL6 <- OutL6[OutL6$htlc_ft > 0,]
OutL6 <- OutL6[!is.na(OutL6$htlc_ft),]
OutL7 <- trees[trees$statusID == 1,]
OutL7 <- OutL7[OutL7$htlc_ft == 0,]
OutL7 <- OutL7[!is.na(OutL7$htlc_ft),]
OutL8 <- trees[trees$statusID == 6,]
OutL8 <- OutL8[!is.na(OutL8$maxScorchHt_ft) | !is.na(OutL8$minBoleChar_ft) | !is.na(OutL8$maxBoleChar_ft) | !is.na(OutL8$burnSeverity) | !is.na(OutL8$crownScorch_. != 0),]
OutL8 <- OutL8[!is.na(OutL8$treeID),]

##  Burn Severity
OutL9 <- trees[trees$burnSeverity == 3,]
OutL9 <- OutL9[OutL9$crownScorch_. < 100,]
OutL9 <- OutL9[!is.na(OutL9$burnSeverity),]
OutL10 <- trees[trees$burnSeverity == 2,]
OutL10 <- OutL10[OutL10$crownScorch_. > 99 | OutL10$crownScorch_.  < 1,]
OutL10 <- OutL10[!is.na(OutL10$burnSeverity),]
OutL11 <- trees[trees$burnSeverity == 1,]
OutL11 <- OutL11[OutL11$crownScorch_. > 0,]
OutL11 <- OutL11[!is.na(OutL11$burnSeverity),]

##  Add Error Message and Output to OutLog
if(nrow(OutL1) > 0) {OutL1$Error <- "Logical Bound (Htlc > Ht)"; OutLog <- rbind(OutL1)}
if(nrow(OutL2) > 0) {OutL2$Error <- "Logical Bound (Cbh > Htlc)"; OutLog <- rbind(OutLog, OutL2)}
if(nrow(OutL3) > 0) {OutL3$Error <- "Logical Bound (MinChar > MaxChar)"; OutLog <- rbind(OutLog, OutL3)}
if(nrow(OutL4) > 0) {OutL4$Error <- "Logical Bound (MaxChar > Ht)"; OutLog <- rbind(OutLog, OutL4)}
if(nrow(OutL5) > 0) {OutL5$Error <- "Logical Bound (MaxScorch > Htlc)"; OutLog <- rbind(OutLog, OutL5)}
if(nrow(OutL6) > 0) {OutL6$Error <- "Logical Bound (Dead w/ Live Crown)"; OutLog <- rbind(OutLog, OutL6)}
if(nrow(OutL7) > 0) {OutL7$Error <- "Logical Bound (Live w/o Live Crown)"; OutLog <- rbind(OutLog, OutL7)}
if(nrow(OutL8) > 0) {OutL8$Error <- "Logical Bound (Dead Trees w/ improper data)"; OutLog <- rbind(OutLog, OutL8)}
if(nrow(OutL9) > 0) {OutL9$Error <- "Logical Bound (BrnSev 3 Crwn Srch < 100)"; OutLog <- rbind(OutLog, OutL9)}
if(nrow(OutL10) > 0) {OutL10$Error <- "Logical Bound (BrnSev 2 Crwn Srch 0 or 100)"; OutLog <- rbind(OutLog, OutL10)}
if(nrow(OutL11) > 0) {OutL11$Error <- "Logical Bound (BrnSev1 Crwn Srch > 0)"; OutLog <- rbind(OutLog, OutL11)}

###    Regression: Ht over DBH
mod1 <- lm(trees$ht_ft ~ trees$dbh_in)
summary(mod1) #R-squared: 0.8035; p < 0.001

##  Calculate residuals and output these potential outliers to OutR1
res1 <- rstandard(mod1)
OutR1 <-trees
OutR1 <- OutR1[abs(res1) > 3.5, ]
res2 <- res1[abs(res1) > 3.5]

##  Plots

#Height Vs. DBH
plot(trees$dbh_in, trees$ht_ft, xlab = "DBH (in)", ylab = "Height (ft)", main = "Height vs. DBH")
abline(mod1)
r2 = summary(mod1)$adj.r.squared
p = summary(mod1)$coefficients[2,4]
p = 0.2
rp = vector('expression', 2)
rp[1] = substitute(expression(italic(R)^2 == MYVALUE), list(MYVALUE = format(r2, dig = 3)))[2]
rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), list(MYOTHERVALUE = format(p, digits = 3))) [2]
legend('bottomright', legend = rp, bty = 0)

#Outliers with Standardized Residuals > |3.5|
plot(trees$dbh_in, res1, xlab = "DBH (in)", ylab = "Standardized Residuals", main = "Outliers with Standardized Residuals > |3.5|")
abline(3.5, 0)
abline(-3.5, 0)
points(OutR1$dbh_in, res2, pch = 21, col = "red", bg = "red")

##  Add Error Message and Output to OutReg
if(nrow(OutR1) > 0) {OutR1$Error <- "Regression (StdRes > |3.5|)"; OutReg <- rbind(OutR1)}


###Compile and Export All Outliers to Overstory_Outliers.xls
if(nrow(OutLog) > 0) {Overstory_Outliers <- rbind(OutLog)}
if(nrow(OutReg) > 0) {Overstory_Outliers <- rbind(Overstory_Outliers, OutReg)}
if(nrow(Overstory_Outliers) > 0) {write.csv(Overstory_Outliers, file = "Overstory_Outliers.csv")}
  