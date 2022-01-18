########################################################
########################################################
#     Fuel Moisture: Quality Control/Data Cleaning     #
#                   Anthony Martinez                   #
#                       May 2017                       #
########################################################
########################################################

###   Initial setup   ###
# Identify user
username <- Sys.getenv("USERNAME")

#Create file path to Git repository
dir <- paste("C:/Users/", username, "/Documents/GitHub/Quality-Control---Overstory", sep = "")

# Set working directory
setwd(dir)

# Load Packages

# Load Data
trees = data.frame(read.csv("tbl_trees.csv"))
sample_units = data.frame(read.csv("tbl_unit.csv"))
sample_episodes = data.frame(read.csv("tbl_unitSamplingEpisode.csv"))

###   Data Cleanning   ###
# Remove unnecessary data from sample_units object
#    This will make file easier to view
sample_units <- sample_units[,-c(5:12, 14:17, 21:24)]

# Add Unit Name
unitID <- merge(sample_episodes[,1:2], sample_units[,1:3], by = "unitID")
unitID <- unitID[,-3]
unitID <- unitID[,-1]
trees <- merge(trees , unitID , by = "unitSamplingEpisodeID")
#during this step 202 records are deleted (because they have an unrecognized UnitSamplingEpisodeID of 18).  It appears that they are duplicates from UnitSamplingID 11 (Angel)

###   Logical Boundaries   ###
# Heights
OutL1 <- trees[trees$htlc_ft > trees$ht_ft,]
OutL1 <- OutL1[!is.na(OutL1$treeID),]
OutL2 <- trees[trees$cbh_ft > trees$htlc_ft,]
OutL2 <- OutL2[!is.na(OutL2$treeID),]

# Scorch/Char
OutL3 <- trees[trees$minBoleChar_ft > trees$maxBoleChar_ft,]
OutL3 <- OutL3[!is.na(OutL3$treeID),]
OutL4 <- trees[trees$maxBoleChar_ft > trees$ht_ft,]
OutL4 <- OutL4[!is.na(OutL4$treeID),]
OutL5 <- trees[trees$maxScorchHt_ft > trees$htlc_ft,]
OutL5 <- OutL5[!is.na(OutL5$treeID),]

# StatusID
OutL6 <- trees[trees$statusID == 6,]
OutL6 <- OutL6[OutL6$htlc_ft > 0,]
OutL6 <- OutL6[!is.na(OutL6$htlc_ft),]
OutL7 <- trees[trees$statusID == 1,]
OutL7 <- OutL7[OutL7$htlc_ft == 0,]
OutL7 <- OutL7[!is.na(OutL7$htlc_ft),]
OutL8 <- trees[trees$statusID == 6,]
OutL8 <- OutL8[!is.na(OutL8$maxScorchHt_ft) | !is.na(OutL8$minBoleChar_ft) | !is.na(OutL8$maxBoleChar_ft) | !is.na(OutL8$burnSeverity) | !is.na(OutL8$crownScorch_. != 0),]
OutL8 <- OutL8[!is.na(OutL8$treeID),]

# Burn Severity
OutL9 <- trees[trees$burnSeverity == 3,]
OutL9 <- OutL9[OutL9$crownScorch_. < 100,]
OutL9 <- OutL9[!is.na(OutL9$burnSeverity),]
OutL10 <- trees[trees$burnSeverity == 2,]
OutL10 <- OutL10[OutL10$crownScorch_. > 99 | OutL10$crownScorch_.  < 1,]
OutL10 <- OutL10[!is.na(OutL10$burnSeverity),]
OutL11 <- trees[trees$burnSeverity == 1,]
OutL11 <- OutL11[OutL11$crownScorch_. > 0,]
OutL11 <- OutL11[!is.na(OutL11$burnSeverity),]

# Add Error Message and Output to Error Log (OutLog)
if(nrow(OutL1) > 0) {OutL1$Error <- "Logical Bound (Htlc > Ht)"; OutLog <- rbind(OutL1)}
if(nrow(OutL2) > 0) {OutL2$Error <- "Logical Bound (Cbh > Htlc)"; ifelse(exists("OutLog"), OutLog <- rbind(OutLog, OutL2), OutLog <- rbind(OutL2))}
if(nrow(OutL3) > 0) {OutL3$Error <- "Logical Bound (MinChar > MaxChar)"; ifelse(exists("OutLog"), OutLog <- rbind(OutLog, OutL3), OutLog <- rbind(OutL3))}
if(nrow(OutL4) > 0) {OutL4$Error <- "Logical Bound (MaxChar > Ht)"; ifelse(exists("OutLog"), OutLog <- rbind(OutLog, OutL4), OutLog <- rbind(OutL4))}
if(nrow(OutL5) > 0) {OutL5$Error <- "Logical Bound (MaxScorch > Htlc)"; ifelse(exists("OutLog"), OutLog <- rbind(OutLog, OutL5), OutLog <- rbind(OutL5))}
if(nrow(OutL6) > 0) {OutL6$Error <- "Logical Bound (Dead w/ Live Crown)"; ifelse(exists("OutLog"), OutLog <- rbind(OutLog, OutL6), OutLog <- rbind(OutL6))}
if(nrow(OutL7) > 0) {OutL7$Error <- "Logical Bound (Live w/o Live Crown)"; ifelse(exists("OutLog"), OutLog <- rbind(OutLog, OutL7), OutLog <- rbind(OutL7))}
if(nrow(OutL8) > 0) {OutL8$Error <- "Logical Bound (Dead Trees w/ improper data)"; ifelse(exists("OutLog"), OutLog <- rbind(OutLog, OutL8), OutLog <- rbind(OutL8))}
if(nrow(OutL9) > 0) {OutL9$Error <- "Logical Bound (BrnSev 3 Crwn Srch < 100)"; ifelse(exists("OutLog"), OutLog <- rbind(OutLog, OutL9), OutLog <- rbind(OutL9))}
if(nrow(OutL10) > 0) {OutL10$Error <- "Logical Bound (BrnSev 2 Crwn Srch 0 or 100)"; ifelse(exists("OutLog"), OutLog <- rbind(OutLog, OutL10), OutLog <- rbind(OutL10))}
if(nrow(OutL11) > 0) {OutL11$Error <- "Logical Bound (BrnSev1 Crwn Srch > 0)"; ifelse(exists("OutLog"), OutLog <- rbind(OutLog, OutL11), OutLog <- rbind(OutL11))}

###   Regression: Ht over DBH   ###
mod_Ht <- lm(trees$ht_ft ~ trees$dbh_in)
summary(mod_Ht) #R-squared: 0.8035; p < 0.001

# Calculate residuals and output these potential outliers to OutR1
res_Ht <- rstandard(mod_Ht)
OutR1 <-trees
OutR1 <- OutR1[abs(res_Ht) > 3.5, ]
res_Ht2 <- res_Ht[abs(res_Ht) > 3.5]

##  Plots  ##
# Height Vs. DBH
plot(trees$dbh_in, trees$ht_ft, xlab = "DBH (in)", ylab = "Height (ft)", main = "Height vs. DBH")
abline(mod_Ht)
r2_Ht = summary(mod_Ht)$adj.r.squared
p_Ht = summary(mod_Ht)$coefficients[2,4]
rp_Ht = vector('expression', 2)
rp_Ht[1] = substitute(expression(italic(R)^2 == MYVALUE), list(MYVALUE = format(r2_Ht, dig = 3)))[2]
rp_Ht[2] = substitute(expression(italic(p) == MYOTHERVALUE), list(MYOTHERVALUE = format(p_Ht, digits = 3))) [2]
legend('bottomright', legend = rp_Ht, bty = 0)

# Outliers with Standardized Residuals > |3.5|
plot(trees$dbh_in, res_Ht, xlab = "DBH (in)", ylab = "Standardized Residuals", main = "Outliers with Standardized Residuals > |3.5|")
abline(3.5, 0)
abline(-3.5, 0)
points(OutR1$dbh_in, res_Ht2, pch = 21, col = "red", bg = "red")

###    Regression: Htlc over DBH
# Prep data by removing trees without Htlc
premodHtlc <- trees[!is.na(trees$htlc_ft),]
premodHtlc <- premodHtlc[premodHtlc$htlc_ft > 0,]
mod_Htlc <- lm(formula = premodHtlc$htlc_ft ~ premodHtlc$ht_ft)
summary(mod_Htlc)

# Calculate residuals and output these potential outliers to OutR2
res_Htlc <- rstandard(mod_Htlc)
OutR2 <-premodHtlc
OutR2 <- OutR2[abs(res_Htlc) > 3.5, ]
res_Htlc2 <- res_Htlc[abs(res_Htlc) > 3.5]

##  Plots  ##
#Htlc Vs. Ht
plot(premodHtlc$ht_ft, premodHtlc$htlc_ft, xlab = "Ht (ft)", ylab = "Height to live crown (ft)", main = "Htlc vs. Ht")
abline(mod_Htlc)
r2_Htlc = summary(mod_Htlc)$adj.r.squared
p_Htlc = summary(mod_Htlc)$coefficients[2,4]
rp_Htlc = vector('expression', 2)
rp_Htlc[1] = substitute(expression(italic(R)^2 == MYVALUE), list(MYVALUE = format(r2_Htlc, dig = 3)))[2]
rp_Htlc[2] = substitute(expression(italic(p) == MYOTHERVALUE), list(MYOTHERVALUE = format(p_Htlc, digits = 3))) [2]
legend('bottomright', legend = rp_Htlc, bty = 0)

# Outliers with Standardized Residuals > |3.5|
plot(premodHtlc$ht_ft, res_Htlc, xlab = "Ht (ft)", ylab = "Standardized Residuals", main = "Outliers with Standardized Residuals > |3.5|")
abline(3.5, 0)
abline(-3.5, 0)
points(OutR2$ht_ft, res_Htlc2, pch = 21, col = "red", bg = "red")

# Add Error Message and Output to Error Log (OutLog)
if(nrow(OutR1) > 0) {OutR1$Error <- "Regression: Ht~DBH (StdRes > |3.5|)"; OutReg <- rbind(OutR1)}
if(nrow(OutR2) > 0) {OutR2$Error <- "Regression: Htlc~Ht (StdRes > |3.5|)"; ifelse(exists("OutReg"), OutReg <- rbind(OutReg, OutR2), OutLog <- rbind(OutR2))}


### Decimal places- Check to make sure there are not more than 1 decimal places ###
# Add training data
trees[4,"dbh_in"] <- 2.333
trees[5,"ht_ft"] <- 12.12345
trees[7,"htlc_ft"] <- 107.3571465

# Function 1
num.digits <- function(column) {
  q <- as.character(trees[,column])
  q1 <- as.numeric(q)
  q2 <- (q1 %% 1)
  q3 <- (q2 !=0)
  q4 <- q1[q3]
  q5 <- q[q3]
  q6 <- gsub("^.*\\.","",q5)
  q7 <- nchar(q6, type = "chars")
  q8 <- q7 > 1
  outlist <- list2env(list(q3 = q3, q8 = q8), .GlobalEnv)
  return(outlist)
}

# Function 2 (This is where I am having trouble)
dec_error <- function(column) {
  num.digits(column)
  assign(paste0("Out_", column), trees)
  assign(paste0("Out_", column), trees[q3, ])
  assign(paste0("Out_", column), get(paste0("Out_", column))[q8, ])
  assign(paste0("Out_", column), get(paste0("Out_", column))[!is.na(get(paste0("Out_", column))[,column]),])
  if(nrow(get(paste0("Out_", column))) > 0) {
    ErrorCol <- matrix(data = paste("Precision overstated: ", column), nrow = nrow(get(paste0("Out_", column))), ncol= (1))
    colnames(ErrorCol) <- "Error"
    assign(paste0("Out_", column), cbind(get(paste0("Out_", column)), ErrorCol))
    ifelse(exists("OutDec"), OutDec <- rbind(OutDec, get(paste0("Out_", column))), OutDec <- rbind(get(paste0("Out_", column))))
  }
  OutDec <- list2env(list(OutDec = OutDec), .GlobalEnv)
  return(OutDec)
}

# Run functions on columns (datasets)
dec_error("dbh_in")
dec_error("ht_ft")
dec_error("htlc_ft")
dec_error("cbh_ft")
dec_error("cbh_ft")
dec_error("maxScorchHt_ft")
dec_error("minBoleChar_ft")
dec_error("maxBoleChar_ft")


###   Compile and Export All Outliers to Overstory_Outliers.xls   ###
if(nrow(OutLog) > 0) {Overstory_Outliers <- rbind(OutLog)}
if(nrow(OutReg) > 0) {ifelse(exists("Overstory_Outliers"), Overstory_Outliers <- rbind(Overstory_Outliers, OutReg), Overstory_Outliers <- rbind(OutReg))}
if(nrow(OutDec) > 0) {ifelse(exists("Overstory_Outliers"), Overstory_Outliers <- rbind(Overstory_Outliers, OutDec), Overstory_Outliers <- rbind(OutDec))}

# Date and time stamp on output file
dt <- Sys.Date()
tm <- format(Sys.time(), format = "%H.%M.%S", 
             tz = "", usetz = FALSE)                                                        
if(nrow(Overstory_Outliers) > 0) {write.csv(Overstory_Outliers, file = paste(dt, "_", tm, "_", username, 
                                                                             "_Overstory_Outliers.csv", 
                                                                             sep = ""))}
