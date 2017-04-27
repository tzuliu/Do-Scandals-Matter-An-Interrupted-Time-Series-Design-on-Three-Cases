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
	ger21$poll_p1_p[i] <- ger21$poll_p1_p[i] + cduerror1[i]
	} else {
		ger21$poll_p1_p[i] <- ger21$poll_p1_p[i]
	}
}

for (i in 1:nrow(ger21)){
	if (is.na(ger21$poll_p4[i])==TRUE) {
	ger21$poll_p4_p[i] <- ger21$poll_p4_p[i] + fdperror1[i]
	} else {
		ger21$poll_p4_p[i] <- ger21$poll_p4_p[i]
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
## NOTICE: Ther is NO REAL data for 2010-12-02 and 2010-12-04
## NOTICE: There IS REAL data for 2010-12-01 and 2010-12-03
## NOTICE: There IS REAL data for 2010-11-28, 2010-11-30, and 2010-12-01
## NOTICE: Ther is NO REAL data for 2010-11-25~2010-11-27 and 2010-11-29
which(time(ger21r)=="2010-12-02")
## it shows the 61

## create values for vline, one for each panel
vlines <- c(v=time(ger21r)[61], v=time(ger21r)[61])

## plot multiple zoo plots
pdf("CDU2month1.pdf", onefile=FALSE, width=6.75, height=4)
plot(ger21r, panel=v.panel, main="2-Month before and after the WikiLeaks Scandal", xlab="Time")
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
summary(ur.df(ger21r$CDU, type="drift", selectlags="BIC"))
## it shows that it's stationary under 5% critical value
acf(ger21r$CDU)
pacf(ger21r$CDU)
## looks like it's an AR(1)

## testing for dgs
ger21r.cdu.ar1 <- arimax(ger21r$CDU, order=c(1,0,0))
ger21r.cdu.ar2 <- arimax(ger21r$CDU, order=c(2,0,0))
ger21r.cdu.ar3 <- arimax(ger21r$CDU, order=c(3,0,0))
ger21r.cdu.ar4 <- arimax(ger21r$CDU, order=c(4,0,0))
ger21r.cdu.ar5 <- arimax(ger21r$CDU, order=c(2,0,5))

BIC(ger21r.cdu.ar1, ger21r.cdu.ar2, ger21r.cdu.ar3, ger21r.cdu.ar4, ger21r.cdu.ar5)
## BIC shows AR(3) is the best for CDU2M1

## estimating the model
ger21r.cdu <- arimax(ger21r$CDU, order=c(3,0,0), xreg=cbind(ger21r$t, ger21r$scandal2, 	
					 ger21r$t*ger21r$scandal2)); ger21r.cdu

##testing autocorrelation
acf(ger21r.cdu$residual)
pacf(ger21r.cdu$residual)
Box.test(ger21r.cdu$residuals, lag=20, type="Ljung-Box")


ger21r.cdu.pred1 <- ger21r.cdu$coef[4] + ger21r.cdu$coef[5]*ger21r$t +
					ger21r.cdu$coef[6]*ger21r$scandal2 + ger21r.cdu$coef[7]*ger21r$t*ger21r$scandal2

ger21r.cdu.pred2 <- ger21r.cdu$coef[4] + ger21r.cdu$coef[5]*ger21r$t +
					ger21r.cdu$coef[6]*ger21r$scandal3 + ger21r.cdu$coef[7]*ger21r$t*ger21r$scandal3

ger21r.cdu.diff <- ger21r.cdu.pred1[61] - ger21r.cdu.pred2[61]; ger21r.cdu.diff
## difference is 0.4562

## Parametric Monte Carlo Simulation for 95% CIs
cdu1coef <- ger21r.cdu$coef
cdu1vcov <- vcov(ger21r.cdu)
cdu1draw <- mvrnorm(5000, cdu1coef, cdu1vcov)
ger21r$intercept <- rep(1, 120)
cdu1xs <- cbind(ger21r$intercept,ger21r$t,ger21r$scandal2,ger21r$t*ger21r$scandal2)
cdu1mcci <- cdu1xs%*%t(cdu1draw[,4:7])
cdu1lb <- apply(cdu1mcci, 1, function(x) quantile(x, probs=c(.025)))
cdu1ub <- apply(cdu1mcci, 1, function(x) quantile(x, probs=c(.975)))

## Graph for Local Effect
pdf("CDUlf21.pdf", onefile=FALSE, width=6.75, height=4)
plot(y=ger21r$CDU, x=time(ger21r), type='l', ylab="CDU/CSU Approval Rate", xlab="Time",
	 main="Causal Effect of the WikiLeaks Scandal (CDU/CSU)")
points(time(ger21r), ger21r$CDU,
       col="pink",
       pch=20)
lines(y=ger21r.cdu.pred2[1:61], x=time(ger21r)[1:61], lty=1, col="blue")
lines(y=ger21r.cdu.pred2[62:120], x=time(ger21r)[62:120], lty=2, col="blue")
lines(y=ger21r.cdu.pred1[61:length(ger21r.cdu.pred1)], x=time(ger21r)[61:length(ger21r.cdu.pred1)], 
	  lty=1, col="red")
lines(y=cdu1lb[61:120], x=time(ger21r)[61:120], lty=2, col="red")
lines(y=cdu1ub[61:120], x=time(ger21r)[61:120], lty=2, col="red")
abline(v=time(ger21r)[61], lty=2)
Corner_text <- function(text, location="topleft"){
	legend(location, legend=text, bty="n", pch=NA)
}
Corner_text(text=c("Difference: 0.4562", "Date: 12/02/2010"))
dev.off()
	  
###plot of break point
ger21r$scandal1 <- c(rep(1,26), rep(0,(120-26)))
ger21r$scandal2 <- c(rep(0, 25), rep(1, (40-25)), rep(0,(120-40)))
ger21r$scandal3 <- c(rep(0, 39), rep(1,(120-39)))
ger21r.cdu1 <- arimax(ger21r$CDU[1:26], order=c(3,0,0)); ger21r.cdu1
ger21r.cdu2 <- arimax(ger21r$CDU[27:39], order=c(3,0,0)); ger21r.cdu2
ger21r.cdu3 <- arimax(ger21r$CDU[40:120], order=c(3,0,0)); ger21r.cdu3
pdf("CDU21bp.pdf", onefile=FALSE, width=6.75, height=4)
plot(y=ger21r$CDU, x=time(ger21r), type='l', ylab="CDU/CSU Approval Rate", xlab="Time",
	 main="Estimated Break Point(s) of the WikiLeaks Scandal (CDU/CSU)")
points(time(ger21r), ger21r$CDU,
       col="pink",
       pch=20)
lines(y=ger21r.cdu1$coef[4]*ger21r$scandal1[1:26], x=time(ger21r)[1:26], lty=1, col="red")
lines(y=ger21r.cdu2$coef[4]*ger21r$scandal2[26:40], x=time(ger21r)[26:40], lty=1, col="red")
lines(y=ger21r.cdu3$coef[4]*ger21r$scandal3[40:120], x=time(ger21r)[40:120], lty=1, col="red")
abline(v=time(ger21r)[61], lty=2)
abline(v=time(ger21r)[2], lty=2)
abline(v=time(ger21r)[26], lty=2)
abline(v=time(ger21r)[47], lty=2)
abline(v=time(ger21r)[49], lty=2)
abline(v=time(ger21r)[40], lty=2)
abline(v=time(ger21r)[53], lty=2)
rect(time(ger21r)[2], 25, time(ger21r)[53], 40, border=NA, col="#00000011")
Corner_text <- function(text, location="bottomright"){
	legend(location, legend=text, bty="n", pch=NA)
}
Corner_text(text=c("Scandal Date: 12/02/2010","Break Point(1): 11/18/2010","Break Point(2): 10/28/2010"))
text(x = time(ger21r)[61], y = 29, paste("Scandal\n", "Date"), col="#3C3C3C", cex=0.8)
text(x = time(ger21r)[26], y = 29, paste("Break\n", "Point(2)"), col="#3C3C3C", cex=0.8)
text(x = time(ger21r)[40], y = 29, paste("Break\n", "Point(1)"), col="#3C3C3C", cex=0.8)
dev.off()
 				
###############sensitivity analysis----changing the range of the rate in 2010-12-02
cduorig <- ger21r$CDU
cdusens1 <- seq(32, 34, 0.01)
cdufitsens1 <- list()
for(i in 1 : length(cdusens1)){
	ger21r$CDU[60] <- cdusens1[i]
	ger21r.cdut <- arimax(ger21r$CDU, order=c(1,0,0), 
					xreg=cbind(ger21r$t, ger21r$scandal2, ger21r$t*ger21r$scandal2))
	cdufitsens1[[i]] <- ger21r.cdut$coef[2] + ger21r.cdut$coef[3]*ger21r$t + 												ger21r.cdut$coef[4]*ger21r$scandal2 + 																ger21r.cdut$coef[5]*ger21r$t*ger21r$scandal2
}


###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###sensitivity plot 1
pdf("CDUsens121.pdf", onefile=FALSE, width=6.75, height=4)     
plot(y=ger21r$CDU, x=time(ger21r), type='l', ylab="CDU/CSU", xlab="Time",
	main="Sensitivity Plot 1 (CDU/CSU)")
for(i in 1 : length(cdusens1)){
	lines(y=cdufitsens1[[i]][61:120], x=time(ger21r)[61:120], col="#00000008")
}
lines(y=ger21r.cdu.pred2[1:61], x=time(ger21r)[1:61], lty=2, col="blue")
lines(y=ger21r.cdu.pred1[61:length(ger21r.cdu.pred1)], x=time(ger21r)[61:length(ger21r.cdu.pred1)], 
	  lty=2, col="red")
abline(v=time(ger21r)[61], lty=2)
dev.off()

###############sensitivity analysis----comparing 2010-12-02 and 2010-12-03
###############
ger21r$CDU[60] <- cduorig
ger21r$scandal4 <- c(rep(0, 62), rep(1, nrow(ger21r)-62))

## estimating the model
ger21r.cdusens2 <- arimax(ger21r$CDU, order=c(1,0,0), 
					xreg=cbind(ger21r$t, ger21r$scandal3, ger21r$t*ger21r$scandal3)); ger21r.cdusens2

ger21r.cdusens.pred1 <- (ger21r.cdusens2$coef[2] + ger21r.cdusens2$coef[3]*ger21r$t 
					    + ger21r.cdusens2$coef[4]*ger21r$scandal3 					  
 					    + ger21r.cdusens2$coef[5]*ger21r$t*ger21r$scandal3)

ger21r.cdusens.pred2 <- (ger21r.cdusens2$coef[2] + ger21r.cdusens2$coef[3]*ger21r$t 
					    + ger21r.cdusens2$coef[4]*ger21r$scandal4 					  
 					    + ger21r.cdusens2$coef[5]*ger21r$t*ger21r$scandal4)	  

ger21r.cdusens.diff <- ger21r.cdusens.pred1[62] - ger21r.cdusens.pred2[62]; ger21r.cdusens.diff
## difference is 0.4562

### sensitivity plot 2
pdf("CDUsens221.pdf", onefile=FALSE, width=6.75, height=4)                                                    
plot(y=ger21r$CDU, x=time(ger21r), type='l', ylab="CDU/CSU", xlab="Time",
	main="Sensitivity Plot 2 (CDU/CSU)")
points(time(ger21r),ger21r$CDU,
       col="pink",
       pch=20)
lines(y=ger21r.cdusens.pred2[1:62], x=time(ger21r)[1:62], lty=2, col="blue")
lines(y=ger21r.cdusens.pred1[62:length(ger21r.cdusens.pred1)],x=time(ger21r)	
	  [62:length(ger21r.cdusens.pred1)], lty=2, col="red")
abline(v=time(ger21r)[62], lty=2)
Corner_text <- function(text, location="topleft"){
	legend(location, legend=text, bty="n", pch=NA)
}
Corner_text(text=c("Difference: 0.4562", "Date: 12/02/2010"))
dev.off()
