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

## data 2 month before and after
ger22 <- ger[ger$date >= "2010-12-18",]
ger22 <- ger22[ger22$date <= "2011-04-16",]

## generate error component
set.seed(19820222)
cduerror2 <- rnorm(120, 0, cdusd2)
set.seed(19831010)
fdperror2 <- rnorm(120, 0, fdpsd2)

## create poll is decimal
ger22$poll_p1_p <- ger22$poll_p1_ipo/100
ger22$poll_p4_p <- ger22$poll_p4_ipo/100

## add error into the imputed data
for (i in 1:nrow(ger22)){
	if (is.na(ger22$poll_p1[i])==TRUE) {
	ger22$poll_p1_p[i] <- ger22$poll_p1_p[i] + cduerror2[i]
	} else {
		ger22$poll_p1_p[i] <- ger22$poll_p1_p[i]
	}
}

for (i in 1:nrow(ger22)){
	if (is.na(ger22$poll_p4[i])==TRUE) {
	ger22$poll_p4_p[i] <- ger22$poll_p4_p[i] + fdperror2[i]
	} else {
		ger22$poll_p4_p[i] <- ger22$poll_p4_p[i]
	}
}

## transform decimal to percentage
ger22$poll_p1_ipo <- ger22$poll_p1_p*100
ger22$poll_p4_ipo <- ger22$poll_p4_p*100

## reducing the data
ger22r <- cbind(ger22$poll_p1_ipo, ger22$poll_p4_ipo)

## create the daily times series data
ger22r <- zoo(ger22r, ger22$date)

## name the column (don't need for date)
colnames(ger22r) <- c("CDU/CSU", "FDP")

# plot(ger22r$CDU)
# plot(ger22r$FDP)

## searching for the index of the date when scandal happend
## searching for the index of the date when scandal happend
## NOTICE: Ther is NO REAL data for 2011-02-14, 2011-02-17, 2011-02-18, and 2011-02-19
## NOTICE: There IS REAL data for 2011-02-15 and 2011-02-16
which(time(ger22r)=="2011-02-16")
## 61

## panel function that loops over panels
v.panel <- function(x, ...){
	lines(x, ...)
	panel.number <- parent.frame()$panel.number
	abline(v = vlines[panel.number], col = "red", lty=2)
}

## create values for vline, one for each panel
vlines <- c(v=time(ger22r)[61], v=time(ger22r)[61])

## plot multiple zoo plots
pdf("CDU2month2.pdf", onefile=FALSE, width=6.75, height=4)
plot(ger22r, panel=v.panel, main="2-Month before and after the Plagiarism  Scandal", xlab="Time")
dev.off()

## time indicator
ger22r$t <- seq(1,nrow(ger22r),1)
## total 120 points

## create event indicator
ger22r$scandal1 <- c(rep(0, 60), 1, rep(0, nrow(ger22r)-61))
ger22r$scandal2 <- c(rep(0, 60), rep(1, nrow(ger22r)-60))
ger22r$scandal3 <- c(rep(0, 120))

## testing the model specification for CDU
summary(ur.df(ger22r$CDU, type="drift", selectlags="BIC"))
## It's more like drift umder 1% critical value

acf(ger22r$CDU[1:60])
pacf(ger22r$CDU)
## looks like AR(1)

## testing for dgs
ger22r.cdu.ar1 <- arimax(ger22r$CDU, order=c(1,0,0), xreg=ger22r$t)
ger22r.cdu.ar2 <- arimax(ger22r$CDU, order=c(2,0,0), xreg=ger22r$t)
ger22r.cdu.ar3 <- arimax(ger22r$CDU, order=c(3,0,0), xreg=ger22r$t)
ger22r.cdu.ar4 <- arimax(ger22r$CDU, order=c(4,0,0), xreg=ger22r$t)

BIC(ger22r.cdu.ar1, ger22r.cdu.ar2, ger22r.cdu.ar3, ger22r.cdu.ar4)
## BIC shows AR(1) is the best for CDU

## estimating the model
ger22r.cdu <- arimax(ger22r$CDU, order=c(1,0,0), 
					 xreg=cbind(ger22r$t, ger22r$scandal2, ger22r$t*ger22r$scandal2)); ger22r.cdu 

##
acf(ger22r.cdu$residual)
pacf(ger22r.cdu$residual) 
Box.test(ger22r.cdu$residuals, lag=20, type="Ljung-Box")

ger22r.cdu.pred1 <- ger22r.cdu$coef[2] + ger22r.cdu$coef[3]*ger22r$t + 													ger22r.cdu$coef[4]*ger22r$scandal2 + ger22r.cdu$coef[5]*ger22r$t*ger22r$scandal2

ger22r.cdu.pred2 <- ger22r.cdu$coef[2] + ger22r.cdu$coef[3]*ger22r$t + 													ger22r.cdu$coef[4]*ger22r$scandal3 + ger22r.cdu$coef[5]*ger22r$t*ger22r$scandal3

ger22r.cdu.diff <- ger22r.cdu.pred1[61] - ger22r.cdu.pred2[61]; ger22r.cdu.diff
## difference is 0.5625

## Parametric Monte Carlo Simulation for 95% CIs
cdu2coef <- ger22r.cdu$coef
cdu2vcov <- vcov(ger22r.cdu)
cdu2draw <- mvrnorm(5000, cdu2coef, cdu2vcov)
ger22r$intercept <- rep(1, 120)
cdu2xs <- cbind(ger22r$intercept,ger22r$t,ger22r$scandal2,ger22r$t*ger22r$scandal2)
cdu2mcci <- cdu2xs%*%t(cdu2draw[,2:5])
cdu2lb <- apply(cdu2mcci, 1, function(x) quantile(x, probs=c(.025)))
cdu2ub <- apply(cdu2mcci, 1, function(x) quantile(x, probs=c(.975)))

## Graph
pdf("CDUlf22.pdf", onefile=FALSE, width=6.75, height=4)                                                    
plot(y=ger22r$CDU, x=time(ger22r), type='l', ylab="CDU/CSU Approval Rate", xlab="Time",
	main="Causal Effect of the Plagiarism Scandal (CDU/CSU)")
points(time(ger22r), ger22r$CDU,
       col="pink",
       pch=20)
lines(y=ger22r.cdu.pred2[1:61], x=time(ger22r)[1:61], lty=1, col="blue")
lines(y=ger22r.cdu.pred2[62:120], x=time(ger22r)[62:120], lty=2, col="blue")
lines(y=ger22r.cdu.pred1[61:length(ger22r.cdu.pred1)], x=time(ger22r)[61:length(ger22r.cdu.pred1)], 
	  lty=1, col="red")
lines(y=cdu2lb[61:120], x=time(ger22r)[61:120], lty=2, col="red")
lines(y=cdu2ub[61:120], x=time(ger22r)[61:120], lty=2, col="red")
abline(v=time(ger22r)[61], lty=2)
abline(v=time(ger22r)[74], lty=2)
text(x = time(ger22r)[74], y = 31, paste("Retirement\n", "Date"), col="#3C3C3C", cex=0.8)
# Add a box to show the period between two events
rect(time(ger22r)[61], 30, time(ger22r)[74], 40, border=NA, col="#00000011")
Corner_text <- function(text, location="topright"){
	legend(location, legend=text, bty="n", pch=NA)
}
Corner_text(text=c("Difference: 0.5625", "Date: 02/16/2011"))
dev.off()

## structure change detect
## using the supF script
## plot w/ the breakpoint
ger22r$scandal1 <- c(rep(1, 88), rep(0, (120-88)))
ger22r$scandal2 <- c(rep(0, 87), rep(1, (120-87)))
ger22r.cdu1 <- arimax(ger22r$CDU[1:88], order=c(1,0,0)); ger22r.cdu1 
ger22r.cdu2 <- arimax(ger22r$CDU[89:120], order=c(1,0,0)); ger22r.cdu2 
pdf("CDU22bp.pdf", onefile=FALSE, width=6.75, height=4)                                                    
plot(y=ger22r$CDU, x=time(ger22r), type='l', ylab="CDU/CSU Approval Rate", xlab="Time",
	main="Estimated Break Point(s) of the Plagiarism Scandal (CDU/CSU)")
points(time(ger22r), ger22r$CDU,
       col="pink",
       pch=20)
lines(y=ger22r.cdu1$coef[2]*ger22r$scandal1[1:88], x=time(ger22r)[1:88], lty=1, col="red")
lines(y=ger22r.cdu2$coef[2]*ger22r$scandal2[88:120], x=time(ger22r)[88:120], lty=1, col="red")
abline(v=time(ger22r)[61], lty=2)
abline(v=time(ger22r)[80], lty=2)
abline(v=time(ger22r)[88], lty=2)
abline(v=time(ger22r)[95], lty=2)
text(x = time(ger22r)[61], y = 38, paste("Scandal\n", "Date"), col="#3C3C3C", cex=0.8)
text(x = time(ger22r)[88], y = 37, paste("Break\n", "Point"), col="#3C3C3C", cex=0.8)
# Add a box to show the period between two events
rect(time(ger22r)[80], 30, time(ger22r)[95], 40, border=NA, col="#00000011")
Corner_text <- function(text, location="bottomleft"){
	legend(location, legend=text, bty="n", pch=NA)
}
Corner_text(text=c("Scandal Date: 02/16/2011", "Break Point: 03/15/2011"))
dev.off()


##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
###############sensitivity analysis----comparing 2010-02-16 and 2010-02-17
###############
ger22r$CDU[60] <- cduorig
ger22r$scandal4 <- c(rep(0, 62), rep(1, nrow(ger22r)-62))

## estimating the model
ger22r.cdusens2 <- arimax(ger22r$CDU, order=c(1,0,0), 
					xreg=cbind(ger22r$t, ger22r$scandal3, ger22r$t*ger22r$scandal3)); ger22r.cdusens2

ger22r.cdusens.pred1 <- (ger22r.cdusens2$coef[2] + ger22r.cdusens2$coef[3]*ger22r$t 
					    + ger22r.cdusens2$coef[4]*ger22r$scandal3 					  
 					    + ger22r.cdusens2$coef[5]*ger22r$t*ger22r$scandal3)

ger22r.cdusens.pred2 <- (ger22r.cdusens2$coef[2] + ger22r.cdusens2$coef[3]*ger22r$t 
					    + ger22r.cdusens2$coef[4]*ger22r$scandal4 					  
 					    + ger22r.cdusens2$coef[5]*ger22r$t*ger22r$scandal4)	  

ger22r.cdusens.diff <- ger22r.cdusens.pred1[62] - ger22r.cdusens.pred2[62]; ger22r.cdusens.diff
## difference is -0.0959

### sensitivity plot 1
pdf("CDUsens122.pdf", onefile=FALSE, width=6.75, height=4)                                                    
plot(y=ger22r$CDU, x=time(ger22r), type='l', ylab="CDU/CSU", xlab="Time",
	main="Sensitivity Plot 1 (CDU/CSU)")
points(time(ger22r),ger22r$CDU,
       col="pink",
       pch=20)
lines(y=ger22r.cdusens.pred2[1:62], x=time(ger22r)[1:62], lty=2, col="blue")
lines(y=ger22r.cdusens.pred1[62:length(ger22r.cdusens.pred1)],x=time(ger22r)	
	  [62:length(ger22r.cdusens.pred1)], lty=2, col="red")
abline(v=time(ger22r)[62], lty=2)
abline(v=time(ger22r)[74], lty=2)
text(x = time(ger22r)[74], y = 32, paste("Retirement\n", "Date"), col="#3C3C3C", cex=0.8)
# Add a box to show the period between two events
rect(time(ger22r)[62], 30, time(ger22r)[74], 38, border=NA, col="#00000011")
Corner_text <- function(text, location="bottomleft"){
	legend(location, legend=text, bty="n", pch=NA)
}
Corner_text(text=c("Difference: -0.0959", "Date: 02/16/2010"))
dev.off()

### The breakioint is the 95. which is 2011-03-22
time(ger22r)[95]