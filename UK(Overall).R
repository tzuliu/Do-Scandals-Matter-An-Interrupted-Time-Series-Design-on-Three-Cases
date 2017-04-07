rm(list=ls())
library(foreign)
library(TSA)
library(zoo)
library(eventstudies)
library(tseries)
library(strucchange)
library(urca)
library(changepoint)
library(MASS)

## set working directory
setwd("/Users/tpliu/Desktop/Projects/Interrupted Time Series and Scandal/Data")

## read the data (only germany)
ukc <- read.dta("DAYPOLLS_UKC.dta")
ukl <- read.dta("DAYPOLLS_UKL.dta")

## create the date (based on stata)
ukc$date <- seq(as.Date("1935-11-15"),as.Date("2010-05-06"), by="day")
ukl$date <- seq(as.Date("1935-11-15"),as.Date("2010-05-06"), by="day")
uk <- cbind(ukc$ipoll_, ukl$ipoll_, ukl$poll_)
colnames(uk) <- c("Con","Lab", "poll")
uk <- zoo(uk, ukc$date)

## subset the data
ukoa <- uk[time(uk) >= "2000-01-01",]

## plot ts data
## plot(ger1r$CDU)
## plot(ger1r$FDP)

## searching for the index of the date when scandal happend
which(time(ukoa)=="2008-01-08")
# it's 2930

## create values for vline, one for each panel
vlines <- c(v=time(ukoa)[2930], v=time(ukoa)[2930])

## panel function that loops over panels
v.panel <- function(x, ...){
	lines(x, ...)
	panel.number <- parent.frame()$panel.number
	abline(v = vlines[panel.number], col = "red", lty=2)
}

## plot multiple zoo plots
pdf("LPwhole.pdf", onefile=FALSE, width=6.75, height=4)
plot(ukoa$Lab, panel=v.panel, main="Labour Party after 2000", xlab="Time", ylab="Approval Rate")
abline(v=time(ukoa)[2930], lty=2, col="red")
dev.off()



########################################
########################################
########################################

## Go to UK(Labour)2M1