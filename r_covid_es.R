taiwan <- scan()
42145 67639 65836 45493 51158 45751 48326 52961 48068 38025 38798 41373 43948 44463 60429 43690 45751
41115 36738 46266 52446 51416 58111 58626 52188 54506 59914 13047  1974  6866 15107 24635 24635 31587
33133 22317 27210 30815 26180 34420 34935 34935 40085 49613 51416 53733 50128 50643 47038 61459 51931
51931 56309 46008 51158

taiwan.ts <- ts(taiwan,frequency = 12,start = c(2001,01))
taiwan.ts_presars <- window(taiwan.ts, end = c(2003,3))

plot(taiwan.ts)

library("smooth")


omega <- seq(0.6,0.99,0.01)

aic_omega <- matrix(NA,length(omega),2)


for (i in seq_along(omega)) {
  
  tc <- filter(1 * (seq.int(length(taiwan.ts)) == 31), filter = omega[i], method = "rec", 
               sides = 1)
  
  ls <- 1 * (seq_along(taiwan.ts) > length(taiwan.ts_presars) & seq_along(taiwan.ts) <=length(taiwan.ts_presars)+3)
  
  xreg <- data.frame(tc,ls)
  
  taiwan.ts_test <- window(taiwan.ts,end=c(2004,11))
  
  es.f <- es(taiwan.ts_test,xreg = xreg,h =0)
  
  aic_omega[i,1] <- omega[i]
  aic_omega[i,2] <- AIC(es.f)
  
  
}

plot(aic_omega,xlab="omega",ylab="AIC",col="blue")
abline(v=aic_omega[which.min(aic_omega[,2]),1],col="red")


tc <- filter(1 * (seq.int(length(taiwan.ts)) == 31), filter =omg, method = "rec", 
             sides = 1)

ls <- 1 * (seq_along(taiwan.ts) > length(taiwan.ts_presars) & seq_along(taiwan.ts) <=length(taiwan.ts_presars)+3)

xreg <- data.frame(tc,ls)

es.f <- es(taiwan.ts,xreg = xreg,holdout = T,h=8)

plot(forecast(es.f,h=8))



