# Example 4 - The number of Cases in Melaka from 2010 to 2015 (monthly and yearly)
# 1. Bar Chart

# clear console
cat("\014") 

# clear workspace
rm(list=ls())

# Install Packages offline 
setpath="C:/Users/R/Datasets - MDeC/R Packages/"
packages=c("ggplot2_1.0.1.zip",
           "gridExtra_0.9.1.zip",
           "reshape_0.8.5.zip",
           "rJava_0.9-6.zip",
           "scales_0.2.4.zip",
           "XLConnect_0.2-11.zip",
           "xlsx_0.5.7.zip")

pack <- paste(setpath, packages, sep="")
sapply(1:length(pack), function(x) install.packages(pack[x], repos = NULL))


# enable libraries

library(rJava)
library(xlsxjars)
library(gridExtra)
library(XLConnect)
library(ggplot2)
library(reshape)
library(scales)
library(xlsx)
require(plyr)


# import from local drive
destfile1 <- paste("C:/Users/R/Datasets - MDeC/",
                   "Denggue Cases+Mortality 2010-2015.xlsx", sep="")

# load the Excel workbook as an R object from local directory 
df <- readWorksheetFromFile(destfile1, sheet = "Kes 2010", 
                            startCol = which(LETTERS=="B"), 
                            startRow = 4, 
                            endCol = grep("BB",c(LETTERS, sapply(LETTERS, paste0, LETTERS))),
                            endRow = 19,
                            header = TRUE, rownames=TRUE)


# get data from 2011 to 2015 from different sheet/pages
for (i in 2:6) {
  A <-  readWorksheetFromFile(destfile1, sheet = (i-1)*2+1, 
                              startCol = which(LETTERS=="C"), 
                              startRow = 4, 
                              endCol = grep("BB",c(LETTERS, sapply(LETTERS, paste0, LETTERS))),
                              endRow = 19 ,
                              header = TRUE, rownames=TRUE) 
  
  # store subsequent years into initial data frame df
  df <- data.frame(df,A)  
  
}


new_df = melt(df)
# Generate daily dates from the start of 2010 until the end of 2015
new_df$daily <- rep(seq(as.Date('2010/01/08'), by='weeks',length=52*6),each=15)

# Create column Year based on the generated dates
new_df$Year <- format(as.Date(cut(new_df$daily, breaks = "year")),format="%Y")

new_df$NEGERI <- factor(new_df$NEGERI)
new_df$Year <- factor(new_df$Year)


new_df[is.na(new_df)] <- 0 #replace NA with 0
new_df2 <- ddply(new_df,.(NEGERI,Year),summarise,Total=sum(value)) #sum by year by state

#example subsetting dataframe
a<-which(new_df2$Year == 2013 & new_df2$NEGERI == "JOHOR")
new_df2[2,3]
