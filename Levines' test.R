version
if(!require(installr)) {
  + install.packages("installr"); 
  + require(installr)
}
updateR()
#if in Rstudio check in Help for package updates (no code for that)

setwd("/Users/llamasp/Documents/gray dataloggers")
list.files()
#Requires R.utils
library(R.utils)
#check rows number
nL <- countLines("SEF102_K26.csv")
#HOBO data loggers produce a file with data logger name in first column
#It is necesary to delet it and add a new first column with name
SEF102_K26_2 <- read.table(file="SEF102_K26.csv", header= FALSE, skip= 2, sep=",")
#to add a new column with name and subset the time series we will use sqldf library, 
#be sure that files are tables before to continue
#Requires SQLDF package
library(sqldf)
#creating database for this data
DB1<-dbConnect(SQLite(), dbname= "DB1")
#next command creates an data base file, also, it generates a warning message but it works
sqldf("attach 'DB1.sqlite' as new")
SEF102_K26_3 <-read.csv.sql("SEF102_K26_2", 
             sql = "SELECT v2,V3 FROM SEF102_K26_2")
dbWriteTable (conn = DB1, name = 'SEF102_K26', value = SEF102_K26_3 ,row.names = FALSE, header = FALSE)
#checking that this data table is in the data base
dbListTables(DB1)
#reading what is in specific data tables
dbReadTable(DB1, 'SEF102_K26')
#removing (IF NEED IT)
dbRemoveTable(DB1, 'SEF102_K26_2')

######NEED TO DO THIS BEFORE OR AFTER SEF102_K26_3. I will keep working on this tomorrow.
SEF102_K26_4 <- read.csv.sql("SEF102_K26_3", 
                             sql = "SELECT v2,V3 FROM SEF102_K26_2"
                             "WHERE "V2" BETWEEN 8/17/2017  4:05:03 PM AND 8/17/2017  4:50:27 PM")
