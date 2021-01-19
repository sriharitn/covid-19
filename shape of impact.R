
x_ls <- ts(rep(0,30),frequency = 1)
x_ls[15:30] <- -1
plot(x_ls,type="o")
x_pu <- ts(rep(0,30),frequency = 1)
x_pu[15] <- -1

del_1 <- (filter(x_ls,filter=1,method = "rec"))
del_75 <- (filter(x_ls,filter=0.75,method = "rec"))
del_5 <- (filter(x_ls,filter=0.5,method = "rec"))
del_0 <- (filter(x_ls,filter=0.0,method = "rec"))


perm <- function(){
  plot(-del_1/min(del_1)*100,type="o",ylab = "Permanent Impact")
  lines(del_75/-min(del_75)*100,col="blue",type="o")
  lines(del_5/-min(del_5)*100,col="red",type="o")
  lines(del_0/-min(del_0)*100,col="darkgreen",type="o")
}



ts_9 <- (filter(x_pu,filter=0.9,method = "rec"))
ts_75 <- (filter(x_pu,filter=0.75,method = "rec"))
ts_5 <- (filter(x_pu,filter=0.5,method = "rec"))
ts_0 <- (filter(x_pu,filter=0.0,method = "rec"))

temp <- function(){
  plot(ts_9*100,type="o",ylab = "Temporary  Impact")
  lines(ts_75*100,col="blue",type="o")
  lines(ts_5*100,col="red",type="o")
  lines(ts_0*100,col="darkgreen",type="o")
}



par(mfrow=c(1,2))
perm()
temp()

dev.off()

## Compund impact

comp_1 <- (filter(x_ls,filter=1,method = "rec"))/min(del_1)*-100
comp_2 <- (filter(x_pu,filter=0.8,method = "rec"))*100
plot(comp_2)

comp_2[which(comp_2==0)] <- -100

plot(ts(c(comp_1,comp_2),frequency = 1),type="o",xlab = "Time", ylab = "Compund Impact",col="blue",main="Compund Impact of Intervention")
lines(ts(comp_2[15:30],start=45,frequency=1),col="red",type="o")
lines(ts(rep(-100,15),start=30,frequency=1),col="purple",type="o")
text(21,-40,"Gradual Decline",col="blue",pos = 4,srt=-58)
text(42,-80,"Exponential Increase",col="red",pos = 4,srt=70)
text(33,-95,"Plateau",col="purple",pos = 4)
