## Create Error Component for CDU 1 and FDP 1
gererror1 <- ger[ger$electionyr==2013,]
cdumean1 <- mean(gererror1$poll_p1, na.rm=TRUE)
fdpmean1 <- mean(gererror1$poll_p4, na.rm=TRUE)
cdumean1 <- cdumean1/100
fdpmean1 <- fdpmean1/100
cdusd1 <- sqrt((cdumean1*(1-cdumean1))/600)
fdpsd1 <- sqrt((fdpmean1*(1-fdpmean1))/600)

## Create Error Component for CDU 2 and FDP 2
gererror2 <- ger[ger$electionyr==2013,]
cdumean2 <- mean(gererror2$poll_p1, na.rm=TRUE)
fdpmean2 <- mean(gererror2$poll_p4, na.rm=TRUE)
cdumean2 <- cdumean2/100
fdpmean2 <- fdpmean2/100
cdusd2 <- sqrt((cdumean2*(1-cdumean2))/600)
fdpsd2 <- sqrt((fdpmean2*(1-fdpmean2))/600)


## Create Error Component for LP
ukerror1 <- ukl[ukl$electionyr==2010,]
lpmean1 <- mean(ukerror1$poll_, na.rm=TRUE)
lpmean1 <- lpmean1/100
lpsd1 <- sqrt((lpmean1*(1-lpmean1))/600)