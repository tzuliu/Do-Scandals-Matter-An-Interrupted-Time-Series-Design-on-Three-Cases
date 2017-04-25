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
	ger22$gerpoll_p1_p[i] <- ger22$poll_p1_p[i] + cduerror2[i]
	} else {
		ger22$gerpoll_p1_p[i] <- ger22$poll_p1_p[i]
	}
}

for (i in 1:nrow(ger22)){
	if (is.na(ger22$poll_p4[i])==TRUE) {
	ger22$gerpoll_p4_p[i] <- ger22$poll_p4_p[i] + fdperror2[i]
	} else {
		ger22$gerpoll_p4_p[i] <- ger22$poll_p4_p[i]
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
plot(ger22r, panel=v.panel, main="2-Month before and after Scandal(2)", xlab="Time")
dev.off()

## time indicator
ger22r$t <- seq(1,nrow(ger22r),1)
## total 120 points

## create event indicator
ger22r$scandal1 <- c(rep(0, 60), 1, rep(0, nrow(ger22r)-61))
ger22r$scandal2 <- c(rep(0, 60), rep(1, nrow(ger22r)-60))
ger22r$scandal3 <- c(rep(0, 61), rep(1, nrow(ger22r)-61))

## testing the model specification for CDU
summary(ur.df(ger22r$FDP, type="drift", selectlags="BIC"))
## It's more like drift under 5% critical value

acf(ger22r$FDP)
pacf(ger22r$FDP)
## looks like AR(2)

## testing for dgs
ger22r.fdp.ar1 <- arimax(ger22r$FDP, order=c(1,0,0))
ger22r.fdp.ar2 <- arimax(ger22r$FDP, order=c(2,0,0))
ger22r.fdp.ar3 <- arimax(ger22r$FDP, order=c(3,0,0))
ger22r.fdp.ar4 <- arimax(ger22r$FDP, order=c(4,0,0))
ger22r.fdp.ar5 <- arimax(ger22r$FDP, order=c(1,0,1))
ger22r.fdp.ar6 <- arimax(ger22r$FDP, order=c(1,0,2))
ger22r.fdp.ar7 <- arimax(ger22r$FDP, order=c(2,0,1))
ger22r.fdp.ar8 <- arimax(ger22r$FDP, order=c(5,0,0))
ger22r.fdp.ar9 <- arimax(ger22r$FDP, order=c(6,0,0))

BIC(ger22r.fdp.ar1, ger22r.fdp.ar2, ger22r.fdp.ar3, ger22r.fdp.ar4, ger22r.fdp.ar5, ger22r.fdp.ar6,
    ger22r.fdp.ar7, ger22r.fdp.ar8, ger22r.fdp.ar9)
## BIC shows AR(5) is the best for CDU

## estimating the model
ger22r.fdp <- arimax(ger22r$FDP, order=c(5,0,0), 
					 xreg=cbind(ger22r$t, ger22r$scandal2, ger22r$t*ger22r$scandal2)); ger22r.fdp
					 
##
acf(ger22r.fdp$residaul)
pacf(ger22r.fdp$residual)  
Box.test(ger22r.fdp$residuals, lag=20, type="Ljung-Box")

ger22r.fdp.pred1 <- ger22r.fdp$coef[6] + ger22r.fdp$coef[7]*ger22r$t + 													ger22r.fdp$coef[8]*ger22r$scandal2 + ger22r.fdp$coef[9]*ger22r$t*ger22r$scandal2

ger22r.fdp.pred2 <- ger22r.fdp$coef[6] + ger22r.fdp$coef[7]*ger22r$t + 													ger22r.fdp$coef[8]*ger22r$scandal3 + ger22r.fdp$coef[9]*ger22r$t*ger22r$scandal3

ger22r.fdp.diff <- ger22r.fdp.pred1[61] - ger22r.fdp.pred2[61]; ger22r.fdp.diff
## difference is 0.7227

## Parametric Monte Carlo Simulation for 95% CIs
fdp2coef <- ger22r.fdp$coef
fdp2vcov <- vcov(ger22r.fdp)
fdp2draw <- mvrnorm(5000, fdp2coef, fdp2vcov)
ger22r$intercept <- rep(1, 120)
fdp2xs <- cbind(ger22r$intercept,ger22r$t,ger22r$scandal2,ger22r$t*ger22r$scandal2)
fdp2mcci <- fdp2xs%*%t(fdp2draw[,6:9])
fdp2lb <- apply(fdp2mcci, 1, function(x) quantile(x, probs=c(.025)))
fdp2ub <- apply(fdp2mcci, 1, function(x) quantile(x, probs=c(.975)))

# Graph for Causal Effect
pdf("FDPlf22.pdf", onefile=FALSE, width=6.75, height=4)                                                    
plot(y=ger22r$FDP, x=time(ger22r), type='l', ylab="FDP Approval Rate", xlab="Time",
	main="Causal Effect of the Plagiarism Scandal (FDP)")
points(time(ger22r),ger22r$FDP,
       col="pink",
       pch=20)	
lines(y=ger22r.fdp.pred2[1:61], x=time(ger22r)[1:61], lty=1, col="blue")
lines(y=ger22r.fdp.pred2[62:120], x=time(ger22r)[62:120], lty=2, col="blue")
lines(y=ger22r.fdp.pred1[61:length(ger22r.fdp.pred1)], x=time(ger22r)[61:length(ger22r.fdp.pred1)], 
	  lty=1, col="red")
lines(y=fdp2lb[61:120], x=time(ger22r)[61:120], lty=2, col="red")
lines(y=fdp2ub[61:120], x=time(ger22r)[61:120], lty=2, col="red")
abline(v=time(ger22r)[61], lty=2)
abline(v=time(ger22r)[74], lty=2)
text(x = time(ger22r)[74], y = 4, paste("Retirement\n", "Date"), col="#3C3C3C", cex=0.8)
# Add a box to show the period between two events
rect(time(ger22r)[61], 1, time(ger22r)[74], 14, border=NA, col="#00000011")
Corner_text <- function(text, location="topright"){
	legend(location, legend=text, bty="n", pch=NA)
}
Corner_text(text=c("Difference: 0.7227", "Date: 02/16/2011"))
dev.off()

## structure change detect
## using the supF script
## plot w/ the breakpoint
ger22r$scandal1 <- c(rep(1,39), rep(0,(120-39)))
ger22r$scandal2 <- c(rep(0, 38), rep(1, (85-38)), rep(0,(120-85)))
ger22r$scandal3 <- c(rep(0, 84), rep(1,(120-84)))
ger22r.fdp1 <- arimax(ger22r$FDP[1:39], order=c(5,0,0)); ger22r.fdp1
ger22r.fdp2 <- arimax(ger22r$FDP[40:84], order=c(5,0,0)); ger22r.fdp2
ger22r.fdp3 <- arimax(ger22r$FDP[85:120], order=c(5,0,0)); ger22r.fdp3
pdf("FDP22bp.pdf", onefile=FALSE, width=6.75, height=4)                                                    
plot(y=ger22r$FDP, x=time(ger22r), type='l', ylab="FDP Approval Rate", xlab="Time",
	main="Estimated Break Point(s) of the Plagiarism Scandal (FDP)")
points(time(ger22r),ger22r$FDP,
       col="pink",
       pch=20)	
lines(y=ger22r.fdp1$coef[6]*ger22r$scandal1[1:39], x=time(ger22r)[1:39], lty=1, col="red")
lines(y=ger22r.fdp2$coef[6]*ger22r$scandal2[39:85], x=time(ger22r)[39:85], lty=1, col="red")
lines(y=ger22r.fdp3$coef[6]*ger22r$scandal3[85:120], x=time(ger22r)[85:120], lty=1, col="red")
abline(v=time(ger22r)[61], lty=2)
abline(v=time(ger22r)[31], lty=2)
abline(v=time(ger22r)[39], lty=2)
abline(v=time(ger22r)[46], lty=2)
abline(v=time(ger22r)[69], lty=2)
abline(v=time(ger22r)[85], lty=2)
abline(v=time(ger22r)[100], lty=2)
text(x = time(ger22r)[61], y = 3, paste("Scandal\n", "Date"), col="#3C3C3C", cex=0.8)
text(x = time(ger22r)[39], y = 3, paste("Break\n", "Point(1)"), col="#3C3C3C", cex=0.8)
text(x = time(ger22r)[85], y = 3, paste("Break\n", "Point(2)"), col="#3C3C3C", cex=0.8)
# Add a box to show the period between two events
rect(time(ger22r)[31], 1, time(ger22r)[46], 14, border=NA, col="#00000011")
rect(time(ger22r)[69], 1, time(ger22r)[100], 14, border=NA, col="#00000011")
Corner_text <- function(text, location="topleft"){
	legend(location, legend=text, bty="n", pch=NA)
}
Corner_text(text=c("Scandal Date: 02/16/2011","Break Point(1): 01/25/2011", "Break Point(2): 03/23/2011"))
dev.off()

################################################################################################
################################################################################################
################################################################################################
################################################################################################

###############sensitivity analysis----comparing 2010-02-16 and 2010-02-17
###############
ger22r$FDP[60] <- cduorig
ger22r$scandal4 <- c(rep(0, 62), rep(1, nrow(ger22r)-62))

## estimating the model
ger22r.fdpsens2 <- arimax(ger22r$FDP, order=c(2,0,0), 
					xreg=cbind(ger22r$t, ger22r$scandal3, ger22r$t*ger22r$scandal3)); ger22r.fdpsens2

ger22r.fdpsens.pred1 <- (ger22r.fdpsens2$coef[3] + ger22r.fdpsens2$coef[4]*ger22r$t 
					    + ger22r.fdpsens2$coef[5]*ger22r$scandal3 					  
 					    + ger22r.fdpsens2$coef[6]*ger22r$t*ger22r$scandal3)

ger22r.fdpsens.pred2 <- (ger22r.fdpsens2$coef[3] + ger22r.fdpsens2$coef[4]*ger22r$t 
					    + ger22r.fdpsens2$coef[5]*ger22r$scandal4 					  
 					    + ger22r.fdpsens2$coef[6]*ger22r$t*ger22r$scandal4)	  

ger22r.fdpsens.diff <- ger22r.fdpsens.pred1[62] - ger22r.fdpsens.pred2[62]; ger22r.fdpsens.diff
## difference is 0.3291

### sensitivity plot 1
pdf("FDPsens122.pdf", onefile=FALSE, width=6.75, height=4)                                                    
plot(y=ger22r$FDP, x=time(ger22r), type='l', ylab="FDP", xlab="Time",
	main="Sensitivity Plot 1 (FDP)")
points(time(ger22r),ger22r$FDP,
       col="pink",
       pch=20)
lines(y=ger22r.fdpsens.pred2[1:62], x=time(ger22r)[1:62], lty=2, col="blue")
lines(y=ger22r.fdpsens.pred1[62:length(ger22r.fdpsens.pred1)],x=time(ger22r)	
	  [62:length(ger22r.fdpsens.pred1)], lty=2, col="red")
abline(v=time(ger22r)[62], lty=2)
abline(v=time(ger22r)[74], lty=2)
text(x = time(ger22r)[74], y = 4, paste("Retirement\n", "Date"), col="#3C3C3C", cex=0.8)
# Add a box to show the period between two events
rect(time(ger22r)[62], 1, time(ger22r)[74], 14, border=NA, col="#00000011")
Corner_text <- function(text, location="topright"){
	legend(location, legend=text, bty="n", pch=NA)
}
Corner_text(text=c("Difference: 0.3291", "Date: 02/16/2010"))
dev.off()

