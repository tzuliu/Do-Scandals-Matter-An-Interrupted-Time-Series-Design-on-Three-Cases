rm(list=ls())
library(foreign)
library(TSA)
library(zoo)
library(eventstudies)
library(tseries)
library(strucchange)
library(urca)
library(changepoint)
library(forecast)
library(MASS)

## set working directory
setwd("/Users/tpliu/Desktop/Projects/Interrupted Time Series and Scandal/Data")

## read the data (only germany)
ger <- read.dta("DAYPOLLS_GER.dta")

## create the date (based on stata)
ger$date <- seq(as.Date("1957-09-16"),as.Date("2013-09-22"), by="day")

## subset the data
geroa <- ger[ger$date >= "2000-01-01",]

## reducing the data
geroar <- cbind(geroa$poll_p1_ipo, geroa$poll_p4_ipo)

## create the daily times series data
geroar <- zoo(geroar, geroa$date)

## name the column (don't need for date)
colnames(geroar) <- c("CDU/CSU", "FDP")

## plot ts data
## plot(ger1r$CDU)
## plot(ger1r$FDP)

## searching for the index of the date when scandal happend
which(time(geroar)=="2010-12-02")
which(time(geroar)=="2011-02-16")
## 3989
## 4065

## create values for vline, one for each panel
vlines <- c(v=time(geroar)[3995], v=time(geroar)[3995])

## panel function that loops over panels
v.panel <- function(x, ...){
	lines(x, ...)
	panel.number <- parent.frame()$panel.number
	abline(v = vlines[panel.number], col = "red", lty=2)
}

## plot multiple zoo plots
pdf("CDUwhole.pdf", onefile=FALSE, width=6.75, height=4)
plot(geroar$CDU, main="CDU/CSU after 2000", xlab="Time", ylab="Approval Rate")
abline(v=time(geroar$CDU)[3989], lty=2, col="red")
abline(v=time(geroar$CDU)[4065], lty=2, col="red")
dev.off()

## plot multiple zoo plots
pdf("FDPwhole.pdf", onefile=FALSE, width=6.75, height=4)
plot(geroar$FDP, main="FDP after 2000", xlab="Time", ylab="Approval Rate")
abline(v=time(geroar$CDU)[3989], lty=2, col="red")
abline(v=time(geroar$CDU)[4065], lty=2, col="red")
dev.off()

########################################
########################################
########################################

## data after AM became PM
geram <- ger[ger$date >= "2005-11-22",]

## reducing the data
geramr <- cbind(geram$poll_p1_ipo, geram$poll_p4_ipo)

## create the daily times series data
geramr <- zoo(geramr, geram$date)

## name the column (don't need for date)
colnames(geramr) <- c("CDU/CSU", "FDP")

## plot ts data
## plot(ger2r$CDU)
## plot(ger2r$FDP)

## searching for the index of the date when scandal happend
which(time(geramr)=="2010-11-28")
## 1833

## create values for vline, one for each panel
vlines <- c(v=time(geramr)[1833], v=time(geramr)[1833])

## plot multiple zoo plots
pdf("Geram.pdf", onefile=FALSE, width=6.75, height=4)
plot(geramr, panel=v.panel, main="CDU/CSU & FDP after Merkel's Government", xlab="Time")
dev.off()

########################################
########################################
########################################

## Go to German(CDU)2M1
