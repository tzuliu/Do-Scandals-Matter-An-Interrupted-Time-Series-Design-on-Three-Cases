## data 2 month before and after
ger21 <- ger[ger$date >= "2010-10-03",]
ger21 <- ger21[ger21$date <= "2011-01-30",]

## generate error component
set.seed(710222)
cduerror1 <- rnorm(120, 0, cdusd1)
set.seed(721010)
fdperror1 <- rnorm(120, 0, fdpsd1)

## create poll is decimal
ger21$poll_p1_p <- ger21$poll_p1_ipo/100
ger21$poll_p4_p <- ger21$poll_p4_ipo/100

## add error into the imputed data
for (i in 1:nrow(ger21)){
	if (is.na(ger21$poll_p1[i])==TRUE) {
	ger21$gerpoll_p1_p[i] <- ger21$poll_p1_p[i] + cduerror1[i]
	} else {
		ger21$gerpoll_p1_p[i] <- ger21$poll_p1_p[i]
	}
}

for (i in 1:nrow(ger21)){
	if (is.na(ger21$poll_p4[i])==TRUE) {
	ger21$gerpoll_p4_p[i] <- ger21$poll_p4_p[i] + fdperror1[i]
	} else {
		ger21$gerpoll_p4_p[i] <- ger21$poll_p4_p[i]
	}
}

## transform decimal to percentage
ger21$poll_p1_ipo <- ger21$poll_p1_p*100
ger21$poll_p4_ipo <- ger21$poll_p4_p*100


## reducing the data
ger21r <- cbind(ger21$poll_p1_ipo, ger21$poll_p4_ipo)

## create the daily times series data
ger21r <- zoo(ger21r, ger21$date)

## name the column (don't need for date)
colnames(ger21r) <- c("CDU/CSU", "FDP")



## plot ts data
## plot(ger4r$CDU)
## plot(ger4r$FDP)

## searching for the index of the date when scandal happend
## searching for the index of the date when scandal happend
## NOTICE: Ther is NO REAL data for 2010-12-02 and 2010-12-04
## NOTICE: There IS REAL data for 2010-12-01 and 2010-12-03
## NOTICE: There IS REAL data for 2010-11-28, 2010-11-30, and 2010-12-01
## NOTICE: Ther is NO REAL data for 2010-11-25~2010-11-27 and 2010-11-29
which(time(ger21r)=="2010-12-02")
## it shows the 61

## create values for vline, one for each panel
vlines <- c(v=time(ger21r)[61], v=time(ger21r)[61])

## plot multiple zoo plots
pdf("CDU2month1.pdf", onefile=FALSE, width=8, height=8)
plot(ger21r, panel=v.panel, main="Data 2-Month before and after Scandal", xlab="Time")
dev.off()

## time indicator
nrow(ger21r)
## total 120 data points
ger21r$t <- seq(1,120,1)

## create event indicator
ger21r$scandal1 <- c(rep(0, 60), 1, rep(0, nrow(ger21r)-61))
ger21r$scandal2 <- c(rep(0, 60), rep(1, nrow(ger21r)-60))
ger21r$scandal3 <- c(rep(0, 120))

## testing the model specification for CDU
summary(ur.df(ger21r$FDP, type="drift", selectlags="BIC"))
## it shows that this time series is stationary under 5% critical value
acf(ger21r$FDP[1:60])
pacf(ger21r$FDP)
## looks like it's an AR(1)

## testing for dgs
ger21r.fdp.ar1 <- arimax(ger21r$FDP, order=c(1,0,0))
ger21r.fdp.ar2 <- arimax(ger21r$FDP, order=c(2,0,0))
ger21r.fdp.ar3 <- arimax(ger21r$FDP, order=c(3,0,0))
ger21r.fdp.ar4 <- arimax(ger21r$FDP, order=c(4,0,0))

BIC(ger21r.fdp.ar1, ger21r.fdp.ar2, ger21r.fdp.ar3, ger21r.fdp.ar4)
## BIC shows ar1 is the best for FDP2M1

## estimating the model
ger21r.fdp <- arimax(ger21r$FDP, order=c(1,0,0), xreg=cbind(ger21r$t, ger21r$scandal2, 	
					 ger21r$t*ger21r$scandal2)); ger21r.fdp

##
acf(ger21r.fdp$residual)
pacf(ger21r.fdp$residual)
Box.test(ger21r.fdp$residuals, lag=20, type="Ljung-Box")

ger21r.fdp.pred1 <- ger21r.fdp$coef[2] + ger21r.fdp$coef[3]*ger21r$t +
                    ger21r.fdp$coef[4]*ger21r$scandal2 + ger21r.fdp$coef[5]*ger21r$t*ger21r$scandal2

ger21r.fdp.pred2 <- ger21r.fdp$coef[2] + ger21r.fdp$coef[3]*ger21r$t +
                    ger21r.fdp$coef[4]*ger21r$scandal3 + ger21r.fdp$coef[5]*ger21r$t*ger21r$scandal3

ger21r.fdp.diff <- ger21r.fdp.pred1[61] - ger21r.fdp.pred2[61];ger21r.fdp.diff
## difference is -0.8135

## Parametric Monte Carlo Simulation for 95% CIs
fdp1coef <- ger21r.fdp$coef
fdp1vcov <- vcov(ger21r.fdp)
fdp1draw <- mvrnorm(5000, fdp1coef, fdp1vcov)
ger21r$intercept <- rep(1, 120)
fdp1xs <- cbind(ger21r$intercept,ger21r$t,ger21r$scandal2,ger21r$t*ger21r$scandal2)
fdp1mcci <- fdp1xs%*%t(fdp1draw[,2:5])
fdp1lb <- apply(fdp1mcci, 1, function(x) quantile(x, probs=c(.025)))
fdp1ub <- apply(fdp1mcci, 1, function(x) quantile(x, probs=c(.975)))

## Graph for Local Effect
pdf("FDPlf21.pdf", onefile=FALSE, width=6.75, height=4)
plot(y=ger21r$FDP, x=time(ger21r), type='l', ylab="FDP Approval Rate", xlab="Time",
	 main="Causal Effect of the WikiLeaks Scandal (FDP)")
points(time(ger21r),ger21r$FDP,
       col="pink",
       pch=20)
lines(y=ger21r.fdp.pred2[1:61], x=time(ger21r)[1:61], lty=1, col="blue")
lines(y=ger21r.fdp.pred2[62:120], x=time(ger21r)[62:120], lty=2, col="blue")
lines(y=ger21r.fdp.pred1[61:length(ger21r.fdp.pred1)], x=time(ger21r)[61:length(ger21r.fdp.pred1)], 
	  lty=1, col="red")
lines(y=fdp1lb[61:120], x=time(ger21r)[61:120], lty=2, col="red")
lines(y=fdp1ub[61:120], x=time(ger21r)[61:120], lty=2, col="red")	  
abline(v=time(ger21r)[61], lty=2)
Corner_text <- function(text, location="topleft"){
	legend(location, legend=text, bty="n", pch=NA)
}
Corner_text(text=c("Difference: -0.8135", "Date: 12/02/2010"))
dev.off()	  

## structure change detect
## using the supF script
## plot w/ the breakpoint
pdf("FDPlf21bp.pdf", onefile=FALSE, width=6.75, height=4)                                                    
plot(y=ger21r$FDP, x=time(ger21r), type='l', ylab="FDP", xlab="Time",
	 main="Local Effect of 2 Month before and after Scandal (FDP) (1)")
points(time(ger21r),ger21r$FDP,
       col="pink",
       pch=20)
lines(y=ger21r.fdp.pred2[1:61], x=time(ger21r)[1:61], lty=2, col="blue")
lines(y=ger21r.fdp.pred1[61:length(ger21r.fdp.pred1)], x=time(ger21r)[61:length(ger21r.fdp.pred1)], 
	  lty=2, col="red")
abline(v=time(ger21r)[61], lty=2)
abline(v=time(ger21r)[12], lty=2)
text(x = time(ger21r)[13], y = 4, paste("Detected\n", "Breakpoint"), col="#3C3C3C", cex=0.8)
Corner_text <- function(text, location="topleft"){
	legend(location, legend=text, bty="n", pch=NA)
}
Corner_text(text=c("Difference: -0.8135", "Date: 12/02/2010"))
dev.off()


########################################################################################			########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################	
###############sensitivity analysis----changing the range of the rate in 2010-12-02
fdporig <- ger21r$FDP
fdpsens1 <- seq(3, 6, 0.01)
fdpfitsens1 <- list()
for(i in 1 : length(fdpsens1)){
	ger21r$FDP[60] <- fdpsens1[i]
	ger21r.fdpt <- arimax(ger21r$FDP, order=c(2,0,0), 
					xreg=cbind(ger21r$t, ger21r$scandal2, ger21r$t*ger21r$scandal2))
	fdpfitsens1[[i]] <- ger21r.fdpt$coef[3] + ger21r.fdpt$coef[4]*ger21r$t + 	
						ger21r.fdpt$coef[5]*ger21r$scandal2 + 																ger21r.fdpt$coef[6]*ger21r$t*ger21r$scandal2
}

###sensitivity plot 1
pdf("FDPsens121.pdf", onefile=FALSE, width=6.75, height=4)     
plot(y=ger21r$FDP, x=time(ger21r), type='l', ylab="FDP", xlab="Time",
	main="Sensitivity Plot 1 (FDP)")
for(i in 1 : length(fdpsens1)){
	lines(y=fdpfitsens1[[i]][61:120], x=time(ger21r)[61:120], col="#00000008")
}
lines(y=ger21r.fdp.pred2[1:61], x=time(ger21r)[1:61], lty=2, col="blue")
lines(y=ger21r.fdp.pred1[61:length(ger21r.fdp.pred1)], x=time(ger21r)[61:length(ger21r.fdp.pred1)], 
	  lty=2, col="red")
abline(v=time(ger21r)[61], lty=2)
dev.off()

###############sensitivity analysis----comparing 2010-12-02 and 2010-12-03
###############
ger21r$FDP[60] <- fdporig
ger21r$scandal4 <- c(rep(0, 62), rep(1, nrow(ger21r)-62))

## estimating the model
ger21r.fdpsens2 <- arimax(ger21r$FDP, order=c(2,0,0), 
					xreg=cbind(ger21r$t, ger21r$scandal3, ger21r$t*ger21r$scandal3)); ger21r.fdpsens2

ger21r.fdpsens.pred1 <- (ger21r.fdpsens2$coef[3] + ger21r.fdpsens2$coef[4]*ger21r$t 
					    + ger21r.fdpsens2$coef[5]*ger21r$scandal3 					  
 					    + ger21r.fdpsens2$coef[6]*ger21r$t*ger21r$scandal3)

ger21r.fdpsens.pred2 <- (ger21r.fdpsens2$coef[3] + ger21r.fdpsens2$coef[4]*ger21r$t 
					    + ger21r.fdpsens2$coef[5]*ger21r$scandal4 					  
 					    + ger21r.fdpsens2$coef[6]*ger21r$t*ger21r$scandal4)	  

ger21r.fdpsens.diff <- ger21r.fdpsens.pred1[62] - ger21r.fdpsens.pred2[62]; ger21r.fdpsens.diff
## difference is 0.5264

### sensitivity plot 2
pdf("FDPsens221.pdf", onefile=FALSE, width=6.75, height=4)                                                    
plot(y=ger21r$FDP, x=time(ger21r), type='l', ylab="FDP", xlab="Time",
	main="Sensitivity Plot 2 (FDP)")
points(time(ger21r),ger21r$FDP,
       col="pink",
       pch=20)
lines(y=ger21r.fdpsens.pred2[1:62], x=time(ger21r)[1:62], lty=2, col="blue")
lines(y=ger21r.fdpsens.pred1[62:length(ger21r.fdpsens.pred1)],x=time(ger21r)	
	  [62:length(ger21r.fdpsens.pred1)], lty=2, col="red")
abline(v=time(ger21r)[62], lty=2)
Corner_text <- function(text, location="topleft"){
	legend(location, legend=text, bty="n", pch=NA)
}
Corner_text(text=c("Difference: 0.5264", "Date: 01/10/2008"))
dev.off()