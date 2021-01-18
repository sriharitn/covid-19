
## Input Data
taiwan <- scan()
42145 67639 65836 45493 51158 45751 48326 52961 48068 38025 38798 41373 43948 44463 60429 43690 45751
41115 36738 46266 52446 51416 58111 58626 52188 54506 59914 13047  1974  6866 15107 24635 24635 31587
33133 22317 27210 30815 26180 34420 34935 34935 40085 49613 51416 53733 50128 50643 47038 61459 51931
51931 56309 46008 51158

## Convert Data to Time Series
taiwan.ts <- ts(taiwan,frequency = 12,start = c(2001,01))

## Filter Data before SARS Epidemic
taiwan.ts_presars <- window(taiwan.ts, start = c(2004,12))

#plot(taiwan.ts)


## SAS fit data 

taiwan.sas <- scan()
51499	60250	59273	48243	51314	48383	49779	52292	49639	44194	44613	46009	
47405	47684	56341	47265	48383	45869	43496	48662	52013	51454	55084	55363	
51873	53130	10770	9916	3912	15121	19120	25722	26995	31892	33731	28753	
32192	34843	32948	37963	38728	39158	42332	47836	49113	50635

taiwan.f.sas <- scan()
48916.14	48467.91	48410.01	48542.73	48760.17	49007.04	49255.23	49491.14

taiwan.f.sas.l <- scan()
36308.6	34126.25	33597.08	33594.07	33771.84	34007.05	34251.81	34486.71

taiwan.f.sas.u <- scan()
61525.08	62810.87	63224.17	63492.58	63749.67	64008.17	64259.77	64496.69


## Filter Data before SARS Epidemic
taiwan.ts_presars <- window(taiwan.ts, end = c(2003,3))


taiwan.sas.ts <- ts(taiwan.sas,start = c(2001,02),frequency = 12)
taiwan.f.sas.ts <- ts(taiwan.f.sas,start = c(2004,12),frequency = 12)
taiwan.f.sas.l.ts <- ts(taiwan.f.sas.l,start = c(2004,12),frequency = 12)
taiwan.f.sas.u.ts <- ts(taiwan.f.sas.u,start = c(2004,12),frequency = 12)

taiwan.plot <- function(){
  par(mar=c(4.5,4.5,4.5,4.5)+.1)
  cex.s <- 2
  plot(taiwan.ts,ylim = c(0,80000),ylab = " ",xlab = " ",yaxt="n",xaxt="n",cex = cex.s,
       main="Model with LTF/Arima and Predicted vs. Holdout",cex.main=cex.s)
  axis(2,cex.axis=cex.s)
  axis(1,cex.axis=cex.s)
  lines(taiwan.sas.ts,col="blue",type="o",cex = cex.s)
  lines(taiwan.f.sas.ts,col="red",type="o",cex = cex.s)
  lines(taiwan.f.sas.l.ts,col="red")
  lines(taiwan.f.sas.u.ts,col="red")
  abline(v=(2004+11/12),col="darkviolet",lty=2)
  text(2005.3,5000,"Hold Out",cex = cex.s)
  mtext("# of Tourist Arrival from Japan", side=2, line=3, cex=cex.s)
  mtext("Time", side=1, line=2.8, cex=cex.s)
  
}




## SAS Ouput

##Parameter Estimate Standard
##Error t Value Approx
##Pr > |t| Lag Variable Shift 
##MU      51499.5 2517.3 20.46 <.0001 0 tourist 0 
##AR1,1   0.54220 0.12034 4.51 <.0001 1 tourist 0 
##NUM1    -45291.5 5561.2 -8.14 <.0001 0 ls 0 
##NUM2    -36735.0 6657.0 -5.52 <.0001 0 tc 0 
##DEN1,1  0.88649 0.03892 22.78 <.0001 1 tc 0 


##Create Regressor Variables
tc <- -36735.0 * filter(1 * (seq.int(length(taiwan.ts)) == 31), filter =0.88649, method = "rec", 
             sides = 1)

## Level Shift in April May June
ls <- -45291.5 * (seq_along(taiwan.ts) > length(taiwan.ts_presars) & seq_along(taiwan.ts) <=length(taiwan.ts_presars)+3)


effects <- ts(ls+tc,frequency = 12, start = c(2001,01))



effect.plot <- function(){
  
  par(mar=c(4.5,4.5,4.5,4.5)+.1)
  cex.s <- 2
  plot(effects,col="darkred",type="o",ylab = " ",xlab = " ",yaxt="n",xaxt="n",cex=cex.s,
       main="SARS Effects - LTF/Arima",cex.main=cex.s)
  axis(2,cex.axis=cex.s)
  axis(1,cex.axis=cex.s)
  mtext("Reduction in Tourist Arrival from Japan", side=2, line=3, cex=cex.s)
  mtext("Time", side=1, line=2.8, cex=cex.s)
  text(2001.1,-30000,"Due to SARS Pandemic Taiwan \nPermenantly lost ~450K Tourists \nfrom Japan",cex = cex.s,pos = 4)
}




png(
  "LTF_arima_smooth.png",
  width     = 6.1,
  height    = 5.3,
  units     = "in",
  res       = 800,
  pointsize = 4
)
par(mfrow=c(2,1))
taiwan.plot()
effect.plot()
dev.off()




