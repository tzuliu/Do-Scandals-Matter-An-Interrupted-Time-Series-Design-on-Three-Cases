#####################################################################################
## Don't need this unless necessary                                                ##
## set working directory                                                           ##
## setwd("/Users/tpliu/Desktop/Projects/Interrupted Time Series and Scandal/Data") ##
##                                                                                 ##
## read the data (only germany)                                                    ##
## ger <- read.dta("DAYPOLLS_GER.dta")                                             ##
##                                                                                 ##
## create the date (based on stata)                                                ##
## ger$date <- seq(as.Date("1957-09-16"),as.Date("2013-09-22"), by="day")          ##
#####################################################################################

########################################
########################################
########################################
## 2008閏年
## data 2 month before and after
## 45 days before is the trick
uk21 <- uk[time(uk) >= "2007-11-10",]
uk21 <- uk21[time(uk21) <= "2008-03-08",]
dateuk21 <- seq(as.Date("2007-11-10"),as.Date("2008-03-08"), by="day")

set.seed(1431232)
lperror1 <- rnorm(120, 0, lpsd1)

uk21$Lab_p <- uk21$Lab/100

## add error into the imputed data
for (i in 1:nrow(uk21)){
	if (is.na(uk21$poll[i])==TRUE) {
	uk21$Lab_p[i] <- uk21$Lab_p[i] + lperror1[i]
	} else {
		uk21$Lab_p[i] <- uk21$Lab_p[i]
	}
}

uk21$Lab <- uk21$Lab_p*100

## create the daily times series data
## time indicator
uk21$t <- seq(1, nrow(uk21), 1)
uk21r <- zoo(uk21, dateuk21)

# plot(ger22r$CDU)
# plot(ger22r$FDP)

## searching for the index of the date when scandal happend
which(time(uk21r)=="2008-01-09")
## 44
## 61

## panel function that loops over panels
v.panel <- function(x, ...){
	lines(x, ...)
	panel.number <- parent.frame()$panel.number
	abline(v = vlines[panel.number], col = "red", lty=2)
}

## create values for vline, one for each panel
vlines <- c(v=time(uk21r)[61], v=time(uk21r)[61])

## plot multiple zoo plots
pdf("Lab2month1.pdf", onefile=FALSE, width=6.75, height=4)
plot(time(uk21r), uk21r$Lab, main="2-Month before and after Donation Scandal", xlab="Time", type="l",
	 ylab="Labour Party")
abline(v=time(uk21r)[61], lty=2, col="red")
dev.off()

## create event indicator
uk21r$scandal1 <- c(rep(0, 60), 1, rep(0, nrow(uk21r)-61))
uk21r$scandal2 <- c(rep(0, 60), rep(1, nrow(uk21r)-60))
uk21r$scandal3 <- c(rep(0, 120))

## testing the model specification for CDU
summary(ur.df(uk21r$Lab, type="drift", selectlags="BIC"))
## It's more like drift under 10%

acf(uk21r$Lab)
pacf(uk21r$Lab)
## looks like AR(1)

## testing for dgs
uk21r.lab.ar1 <- arimax(uk21r$Lab[1:60], order=c(1,0,0))
uk21r.lab.ar2 <- arimax(uk21r$Lab[1:60], order=c(2,0,0))
uk21r.lab.ar3 <- arimax(uk21r$Lab[1:60], order=c(3,0,0))
uk21r.lab.ar4 <- arimax(uk21r$Lab[1:60], order=c(4,0,0))
uk21r.lab.ar5 <- arimax(uk21r$Lab[1:60], order=c(5,0,0))

BIC(uk21r.lab.ar1, uk21r.lab.ar2, uk21r.lab.ar3, uk21r.lab.ar4, uk21r.lab.ar5)
## BIC shows AR(1) is the best for CDU

## estimating the model
uk21r.lab <- arimax(uk21r$Lab, order=c(1,0,0),
					xreg=cbind(uk21r$t, uk21r$scandal2, uk21r$t*uk21r$scandal2)); uk21r.lab

##
acf(uk21r.lab$residual)
pacf(uk21r.lab$residual)
Box.test(uk21r.lab$residual, lag=20, type="Ljung-Box")

uk21r.lab.pred1 <- uk21r.lab$coef[2] + uk21r.lab$coef[3]*uk21r$t + uk21r.lab$coef[4]*uk21r$scandal2 + 
				   uk21r.lab$coef[5]*uk21r$t*uk21r$scandal2

uk21r.lab.pred2 <- uk21r.lab$coef[2] + uk21r.lab$coef[3]*uk21r$t + uk21r.lab$coef[4]*uk21r$scandal3 + 
				   uk21r.lab$coef[5]*uk21r$t*uk21r$scandal3		  

uk21r.lab.diff <- uk21r.lab.pred1[61] - uk21r.lab.pred2[61]; uk21r.lab.diff
## difference is -0.4777

## Parametric Monte Carlo Simulation for 95% CIs
lab1coef <- uk21r.lab$coef
lab1vcov <- vcov(uk21r.lab)
lab1draw <- mvrnorm(5000, lab1coef, lab1vcov)
uk21r$intercept <- rep(1, 120)
lab1xs <- cbind(uk21r$intercept,uk21r$t,uk21r$scandal2,uk21r$t*uk21r$scandal2)
lab1mcci <- lab1xs%*%t(lab1draw[,2:5])
lab1lb <- apply(lab1mcci, 1, function(x) quantile(x, probs=c(.025)))
lab1ub <- apply(lab1mcci, 1, function(x) quantile(x, probs=c(.975)))

# Graph for Causal Effect
pdf("Lablf21.pdf", onefile=FALSE, width=6.75, height=4)                                                    
plot(y=uk21r$Lab, x=time(uk21r), type='l', ylab="Labour Party Approval Rate", xlab="Time",
	main="Causal Effect of the Donation Scandal (The Labour Party)")
points(time(uk21r),uk21r$Lab,
       col="pink",
       pch=20)	
lines(y=uk21r.lab.pred2[1:61], x=time(uk21r)[1:61], lty=1, col="blue")
lines(y=uk21r.lab.pred2[62:120], x=time(uk21r)[62:120], lty=2, col="blue")
lines(y=uk21r.lab.pred1[61:length(uk21r.lab.pred1)], x=time(uk21r)[61:length(uk21r.lab.pred1)], 
	  lty=1, col="red")
lines(y=lab1lb[61:120], x=time(uk21r)[61:120], lty=2, col="red")
lines(y=lab1ub[61:120], x=time(uk21r)[61:120], lty=2, col="red")
abline(v=time(uk21r)[61], lty=2)
abline(v=time(uk21r)[76], lty=2)
text(x = time(uk21r)[76], y = 27, paste("Retirement\n", "Date"), col="#3C3C3C", cex=0.8)
# Add a box to show the period between two events
rect(time(uk21r)[61], 25, time(uk21r)[76], 40, border=NA, col="#00000011")
Corner_text <- function(text, location="bottomright"){
	legend(location, legend=text, bty="n", pch=NA)
}
Corner_text(text=c("Difference: -0.4777", "Date: 01/09/2008"))
dev.off()

## autocorrelation
lab2m1e <- uk21r.lab$residual
plot(lab2m1e)
acf(lab2m1e)
pacf(lab2m1e)

## Nwey-West standard errors
library(sandwich)
library(lmtest)
uk21r.lablm <- lm(uk21r$Lab[2:120] ~ uk21r$Lab[1:119] + uk21r$t[2:120])
coeftest(uk21r.lab, vcov = vcovHC(uk21r.lab, "HC3"))

###############sensitivity analysis----changing the range of the rate in 2008-01-08
laborig <- uk21r$Lab
labsens1 <- seq(32, 34, 0.01)
labfitsens1 <- list()
for(i in 1 : length(labsens1)){
	uk21r$Lab[60] <- labsens1[i]
	uk21r.labt <- arimax(uk21r$Lab, order=c(1,0,0), 
					xreg=cbind(uk21r$t, uk21r$scandal2, uk21r$t*uk21r$scandal2))
	labfitsens1[[i]] <- uk21r.labt$coef[2] + uk21r.labt$coef[3]*uk21r$t + 	
						uk21r.labt$coef[4]*uk21r$scandal3 + uk21r.labt$coef[5]*uk21r$t*uk21r$scandal3
}

###sensitivity plot 1
pdf("Labsens121.pdf", onefile=FALSE, width=6.75, height=4)     
plot(y=uk21r$Lab, x=time(uk21r), type='l', ylab="Labour", xlab="Time",
	main="Sensitivity Plot 1 (Labour Party)")
for(i in 1 : length(labsens1)){
	lines(y=labfitsens1[[i]][1:61], x=time(uk21r)[1:61], col="#00000008")
}
lines(y=uk21r.lab.pred2[1:61], x=time(uk21r)[1:61], lty=2, col="blue")
lines(y=uk21r.lab.pred1[61:length(uk21r.lab.pred1)], x=time(uk21r)[61:length(uk21r.lab.pred1)], 
	  lty=2, col="red")
abline(v=time(uk21r)[61], lty=2)
abline(v=time(uk21r)[76], lty=2)
text(x = time(uk21r)[76], y = 29, paste("Retirement\n", "Date"), col="#3C3C3C", cex=0.8)
# Add a box to show the period between two events
rect(time(uk21r)[61], 25, time(uk21r)[76], 39, border=NA, col="#00000011")
dev.off()
#####

###############sensitivity analysis----comparing 2008-01-09 and 2008-01-10
###############
uk21r$Lab[60] <- laborig
uk21r$scandal4 <- c(rep(0, 62), rep(1, nrow(uk21r)-62))

## estimating the model
uk21r.labsens2 <- arimax(uk21r$Lab, order=c(1,0,0), 
					xreg=cbind(uk21r$t, uk21r$scandal3, uk21r$t*uk21r$scandal3)); uk21r.labsens2

uk21r.labsens.pred1 <- (uk21r.labsens2$coef[2] + uk21r.labsens2$coef[3]*uk21r$t 
					    + uk21r.labsens2$coef[4]*uk21r$scandal3 					  
 					    + uk21r.labsens2$coef[5]*uk21r$t*uk21r$scandal3)

uk21r.labsens.pred2 <- (uk21r.labsens2$coef[2] + uk21r.labsens2$coef[3]*uk21r$t 
					    + uk21r.labsens2$coef[4]*uk21r$scandal4 					  
 					    + uk21r.labsens2$coef[5]*uk21r$t*uk21r$scandal4)		  

uk21r.labsens.diff <- uk21r.labsens.pred1[62] - uk21r.labsens.pred2[62]; uk21r.labsens.diff
## difference is 0.9131

### sensitivity plot 2
pdf("Labsens221.pdf", onefile=FALSE, width=6.75, height=4)                                                    
plot(y=uk21r$Lab, x=time(uk21r), type='l', ylab="Labour", xlab="Time",
	main="Sensitivity Plot 2 (Labour Party)")
lines(y=uk21r.labsens.pred2[1:62], x=time(uk21r)[1:62], lty=2, col="blue")
lines(y=uk21r.labsens.pred1[62:length(uk21r.labsens.pred1)],x=time(uk21r)	
	  [62:length(uk21r.labsens.pred1)], lty=2, col="red")
points(time(uk21r),uk21r$Lab,
       col="pink",
       pch=20)
abline(v=time(uk21r)[62], lty=2)
abline(v=time(uk21r)[76], lty=2)
text(x = time(uk21r)[76], y = 29, paste("Retirement\n", "Date"), col="#3C3C3C", cex=0.8)
# Add a box to show the period between two events
rect(time(uk21r)[62], 25, time(uk21r)[76], 39, border=NA, col="#00000011")
Corner_text <- function(text, location="topleft"){
	legend(location, legend=text, bty="n", pch=NA)
}
Corner_text(text=c("Difference: -0.1256", "Date: 01/10/2008"))
dev.off()

###############sensitivity analysis----changing window to 1 month
###############無解

########################################
########################################
########################################
## 2008閏年
## data 2 month before and after
## 47 days before is the trick
uk11 <- uk[time(uk) >= "2007-12-10",]
uk11 <- uk11[time(uk) < "2008-02-07",]
dateuk11 <- seq(as.Date("2007-12-10"),as.Date("2008-02-07"), by="day")

## create the daily times series data
## time indicator
uk11$t <- seq(1, nrow(uk11), 1)
uk11r <- zoo(uk11, dateuk11)
Labd <- uk11r$Lab - lag(uk11r$Lab) 

# plot(ger22r$CDU)
# plot(ger22r$FDP)

## searching for the index of the date when scandal happend
which(time(uk11r)=="2008-01-09")

## create the daily times series data
## time indicator
uk11$t <- seq(1, nrow(uk11), 1)
uk11r <- zoo(uk11, dateuk11)

# plot(ger22r$CDU)
# plot(ger22r$FDP)

## searching for the index of the date when scandal happend
which(time(uk11r)=="2008-01-09")
## 16

## panel function that loops over panels
v.panel <- function(x, ...){
	lines(x, ...)
	panel.number <- parent.frame()$panel.number
	abline(v = vlines[panel.number], col = "red", lty=2)
}

## create values for vline, one for each panel
vlines <- c(v=time(uk11r)[21], v=time(uk11r)[31])

## plot multiple zoo plots
pdf("Lab1month1.pdf", onefile=FALSE, width=6.75, height=4)
plot(time(uk11r), uk11r$Lab, main="Data 1-Month before and after Scandal", xlab="Time", type="l",
	 ylab="Labour Party")
abline(v=time(uk11r)[31], lty=2, col="red")
dev.off()

## create event indicator
uk11r$scandal1 <- c(rep(0, 17), 1, rep(0, nrow(uk11r)-18))
uk11r$scandal2 <- c(rep(0, 17), rep(1, nrow(uk11r)-17))
uk11r$scandal3 <- c(rep(0, 18), rep(1, nrow(uk11r)-18))

## testing the model specification for CDU
summary(ur.df(Labd[1:30], type="none", selectlags="BIC"))
## It's more like drift under 10%

acf(uk21r$Lab[1:60])
pacf(uk21r$Lab[1:60])
## looks like AR(1)

## testing for dgs
uk21r.lab.ar1 <- arimax(uk21r$Lab[1:60], order=c(1,0,0))
uk21r.lab.ar2 <- arimax(uk21r$Lab[1:60], order=c(2,0,0))
uk21r.lab.ar3 <- arimax(uk21r$Lab[1:60], order=c(3,0,0))
uk21r.lab.ar4 <- arimax(uk21r$Lab[1:60], order=c(4,0,0))

BIC(uk21r.lab.ar1, uk21r.lab.ar2, uk21r.lab.ar3, uk21r.lab.ar4)
## BIC shows AR(1) is the best for CDU

## estimating the model
uk21r.lab <- arimax(uk21r$Lab, order=c(1,0,0), 
					xreg=cbind(uk21r$t, uk21r$scandal2, uk21r$t*uk21r$scandal2)); uk21r.lab

uk21r.lab.pred1 <- uk21r.lab[2] - uk21r.lab[3]*uk21r$t + uk21r.lab[4]*uk21r$scandal2 - 
				   uk21r.lab[5]*uk21r$t*uk21r$scandal2

uk21r.lab.pred1 <- uk21r.lab[2] - uk21r.lab[3]*uk21r$t + uk21r.lab[4]*uk21r$scandal3 - 
				   uk21r.lab[5]*uk21r$t*uk21r$scandal3			  

uk21r.lab.diff <- uk21r.lab.pred1[61] - uk21r.lab.pred2[61]; uk21r.lab.diff
## difference is -0.1433

pdf("Lablf21.pdf", onefile=FALSE, width=6.75, height=4)                                                    
plot(y=uk21r$Lab, x=time(uk21r), type='l', ylab="Labour", xlab="Time",
	main="Local Effect of 2 Month before and after Scandal (Labour Party)")
lines(y=uk21r.lab.pred2[1:61], x=time(uk21r)[1:61], lty=2, col="blue")
lines(y=uk21r.lab.pred1[61:length(uk21r.lab.pred1)], x=time(uk21r)[61:length(uk21r.lab.pred1)], 
	  lty=2, col="red")
points(time(uk11r),uk11r$Lab,
       col="pink",
       pch=20)
abline(v=time(uk21r)[61], lty=2)
abline(v=time(uk21r)[76], lty=2)
text(x = time(uk21r)[76], y = 29, paste("Retirement\n", "Date"), col="#3C3C3C", cex=0.8)
# Add a box to show the period between two events
rect(time(uk21r)[61], 25, time(uk21r)[76], 39, border=NA, col="#00000011")
Corner_text <- function(text, location="topleft"){
	legend(location, legend=text, bty="n", pch=NA)
}
Corner_text(text=c("Difference: -0.1433", "Date: 01/09/2008"))
dev.off()

## autocorrelation
lab2m1e <- uk21r.lab$residual
plot(lab2m1e)
acf(lab2m1e)
pacf(lab2m1e)
                    			          