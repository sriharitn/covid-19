
## Input Data
taiwan <- scan()
42145 67639 65836 45493 51158 45751 48326 52961 48068 38025 38798 41373 43948 44463 60429 43690 45751
41115 36738 46266 52446 51416 58111 58626 52188 54506 59914 13047  1974  6866 15107 24635 24635 31587
33133 22317 27210 30815 26180 34420 34935 34935 40085 49613 51416 53733 50128 50643 47038 61459 51931
51931 56309 46008 51158

## Convert Data to Time Series
taiwan.ts <- ts(taiwan,frequency = 12,start = c(2001,01))

## Filter Data before SARS Epidemic
taiwan.ts_presars <- window(taiwan.ts, end = c(2003,3))

plot(taiwan.ts)

library("smooth")

## Estimate Delta by mnimizing AIC

### Create Test Data
taiwan.ts_test <- window(taiwan.ts,end=c(2004,11))

delta <- seq(0.6,0.99,0.01)

aic_delta <- matrix(NA,length(delta),2)


for (i in seq_along(delta)) {
  
  tc <- filter(1 * (seq.int(length(taiwan.ts)) == 31), filter = delta[i], method = "rec", 
               sides = 1)
  
  ls <- 1 * (seq_along(taiwan.ts) > length(taiwan.ts_presars) & seq_along(taiwan.ts) <=length(taiwan.ts_presars)+3)
  
  xreg <- data.frame(tc,ls)
  
  
  es.f <- es(taiwan.ts_test,xreg = xreg,h =0)
  
  aic_delta[i,1] <- delta[i]
  aic_delta[i,2] <- AIC(es.f)
  
}

## Plot the data with minimum aic and corresponding delta
plot(aic_delta,xlab="delta",ylab="AIC",col="blue")
abline(v=aic_delta[which.min(aic_delta[,2]),1],col="red")


## Extrct Delta
del <- aic_delta[which.min(aic_delta[,2]),1]

##Create Regressor Variables
tc <- filter(1 * (seq.int(length(taiwan.ts)) == 31), filter =del, method = "rec", 
             sides = 1)

## Level Shift in April May June
ls <- 1 * (seq_along(taiwan.ts) > length(taiwan.ts_presars) & seq_along(taiwan.ts) <=length(taiwan.ts_presars)+3)

## Combine Regressors
xreg <- data.frame(tc,ls)


## Final Model with Regressors
es.f <- es(taiwan.ts,xreg = xreg,holdout = T,h = 8)

## Plot Forecasts and output
taiwan.f <-(forecast(es.f,h=8))
plot(taiwan.ts,ylim = c(0,80000),ylab="Taiwan Tourism")
lines(taiwan.f$fitted,col="blue",type="o")
lines(taiwan.f$forecast,col="red",type="o")
lines(taiwan.f$upper,col="red")
lines(taiwan.f$lower,col="red")
abline(v=(2004+11/12),col="orange")
text(2005.3,5000,"Hold Out")

