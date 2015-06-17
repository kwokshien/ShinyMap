## Plot of Case vs Week for Week 1 - 312, forecasting
# clear console
cat("\014") 

# clear workspace
rm(list=ls())

# clear all figures
graphics.off()

# set your home path
setpath="C:/Users/R/Datasets - MDeC/R Packages/"

packages1 = c("xlsx_0.5.7.zip",
              "forecast_5.9.zip",
              "zoo_1.7-12.zip",
              "rJava_0.9-6.zip",
              "xlsxjars_0.6.1.zip")

# get the absolute path
# select packages1 or packages2 or package 3
p <- paste(setpath, packages1, sep="")

# install packages
sapply(p, function(x) install.packages(x, repos = NULL, type = "source"))


library(xlsx)
library(forecast)
library(rJava)
library(xlsxjars)
library(zoo)



# import from local drive
destfile1 <- paste("C:/Users/R/Datasets - MDeC/",
                   "Denggue Cases+Mortality 2010-2015.xlsx", sep="")

## Get dengue case data from the Excel file
case.df = read.xlsx(destfile1, 
                    sheetName=1, rowIndex=c(4,12), colIndex=c(3:54), as.data.frame=TRUE, header=TRUE)
for (i in 2:5) {
  case.df[, ((i-1)*52+1):(i*52)] = read.xlsx(destfile1, 
                                            sheetName=(i-1)*2+1, rowIndex=c(4,12), colIndex=c(3:54), as.data.frame=TRUE, header=TRUE) 
}
totw = ncol(case.df) # Total weeks in 2010-2014
case.df = case.df[, c(8:totw)] # Get data only from week 8, year 2010 to week 52, year 2014 

## Polynomial regression (PR)
week = c(8:totw) # Vector of week number 8-260
case = c(unlist(case.df))
fit = lm(case~poly(week,20)) # PR of 20th degree
preds = predict(fit, newdata=list(week), se=TRUE) # Generate PR o/p
plot(week, case, col=2, pch=20, xaxt="n") # Plot dengue cases vs week
axis(1, xaxp=c(0, totw, 5), las=2)
title(main='Malacca Dengue Cases in  Year 2010 - 2014')
lines(week, preds$fit, lwd=2, col="blue") # Plot PR

## Create time series (TS)
v1 = as.matrix(case.df[1,])
v2 = as.matrix(v1[1,]) # To create 1 column data for TS generation
v3 = v2
caseNA.ts = ts(v2, frequency=52, start=c(2010,8)) # TS with NA's
preds = predict(fit, newdata=list(week[is.na(v2)]), se=TRUE) # Generate PR o/p to replace NA's
v2[is.na(v2)] = preds$fit[which(is.na(v2))] # Replace NA's with PR o/p (otherwise can't create Arima model)
v3[is.na(v3)] = 0 # Replace NA's with 0 (for comparison)
casepf.ts = ts(v2, frequency=52, start=c(2010,8)) # TS with NA's replaced with PR o/p
plot(ts(v3, frequency=52, start=c(2010,8)), type='o', col='red', pch=20, ylab=NULL) # Plot data with NA's replaced with 0's
lines(casepf.ts, type='o', col='blue', pch=20) # Plot data with NA's replaced with PR o/p
title(main='Malacca Dengue Cases in  Year 2010 - 2014', ylab='Cases')

## Decomposition plot
plot(decompose(casepf.ts))

## Create autoregressive model and forecast
ps = 2 # Prediction step
fweekfc = 0*c(1:(52-ps)) # Vector of 0's to store forecasts
for (i in 1:(52-ps)) { # To forecast dengue cases from Week 3, 2013 - Week 52, 2013
  train.ts = window(casepf.ts, start=c(2011,1), end=c(2013,i), frequency=52) # Use data from Week 1, 2011 - Week i, 2013 for training
  dengue.mdl = auto.arima(train.ts) # Create autoregressive model
  case.forecast = forecast.Arima(dengue.mdl, h=ps) # Generate 2-week-ahead forecast 
  fweekfc[i] = case.forecast$mean[ps]
}
fweekfc.ts = ts(fweekfc, frequency=52, start=c(2013,ps+1))
plot(caseNA.ts, type='o', lwd=2, col='red', pch=20, ylab=NULL) # Plot observation
lines(fweekfc.ts, type='o', lwd=2, col='blue', pch=20) # Plot forecast
abline(v=(seq(2010,2015,1/26)), col='lightgray', lty='dotted') # x grid every 2 weeks
legend('topright', c('Observation','Forecast'), lty=1, lwd=2, col=c('red', 'blue'), bty='n', cex=.75)
title(main=paste(ps, '-Week-Ahead Forecast of Dengue Cases in Malacca'), ylab='Cases')

## In-sample model accuracy
accuracy(dengue.mdl)

## Get rain data from Excel file

rain.df = read.xlsx('C:/Users/R/Datasets - MDeC/sun-rain-wind 2008-2014/Malacca Daily RR, MSPD, Solar 2008 - Aug 2014.xls', 
                    sheetName=1, startRow=788, endRow=2447, colIndex=c(2:7), as.data.frame=TRUE, header=FALSE)

# Convert Year, Month, Day into POSIX Dates
fulldate=paste(rain.df$X2, formatC(rain.df$X3, width=2, flag="0"), formatC(rain.df$X4, width=2, flag="0"),sep="-")
dates <- strptime(fulldate, format="%Y-%m-%d")
x <- as.POSIXlt(dates)

# Date-time Conversion Functions to Character 
rain.df$X9 <- strftime(x,format="%V") 

# Week of the year as decimal number (00-53). 
# If the week (starting on Monday) containing 1 January has four or more days in the new year, 
# then it is considered week 1. Otherwise, it is the last week of the previous year, and the next week is week 1.

# conversion to numeric data
rain.df$X9=as.numeric(rain.df$X9)

rain.df$X5[rain.df$X5==-33.3] = 0 # Rainfall less than 0.1mm
rain.df$X5[rain.df$X5==-1.1] = NA # Defective measurement
rain = NA*c(1:totw)
k = 1;
for(i in 2010:2014){ # Compute the weekly mean of the rainfall
  for(j in 1:52){
    if((i==2014)&(j>36)){break}  # No rain and solar data after Week 36, 2014
    if(j==1){
      v4 = c(rain.df$X5[(rain.df$X2==i)&(rain.df$X9==j)],     
             rain.df$X5[(rain.df$X2==i-1)&(rain.df$X9==53)])
    }
    else{
      v4 = rain.df$X5[(rain.df$X2==i)&(rain.df$X9==j)]
    }
    rain[k]=mean(v4, trim=0, na.rm=TRUE)
    k = k+1
  }
}
rain = rain[c(8:totw)] # Get data only from week 8, year 2010 to week 52, year 2014
rain.ts = ts(rain, frequency=52, start=c(2010,8)) # Time series of rainfall
plot(decompose(rain.ts)$trend, type='l', lwd=2, col='blue', xlab=NULL, ylab=NULL, xaxt='n', yaxt='n')
par(new=T)
plot(decompose(casepf.ts)$trend, type='l', lwd=2, col='red', ylab=NULL, yaxt='n')
legend('topright', c('Cases','Rainfall'), lty=1, lwd=2, col=c('red', 'blue'), bty='n', cex=.5)
title(main='Trends of Dengue Cases & Rainfall in Malacca')

plot(decompose(rain.ts)$trend, type='l', lwd=2, col='gray', xlab=NULL, ylab=NULL, xaxt='n', yaxt='n')
lines(lag(decompose(rain.ts)$trend,-26), type='l', lwd=2, col='blue')
par(new=T)
plot(decompose(casepf.ts)$trend, type='l', lwd=2, col='red', ylab=NULL, yaxt='n')
abline(v=(seq(2010,2015,1/4)), col='lightgray', lty='dotted') # x grid every 2 weeks
legend('topright', c('Cases','Rainfall', 'Lagged rainfall'), lty=1, lwd=2, col=c('red','gray','blue'), bty='n', cex=.5)
title(main='Trends of Dengue Cases & Rainfall in Malacca')



 