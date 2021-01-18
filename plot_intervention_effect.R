## Plot Intervention Effect

x <- rep(0,20)
x[10] <- 1

plotx <- function(){
par(mfrow=c(3,2))

cex = 2
barplot(-100*as.numeric(filter(x = x, filter = 0.0,method = "rec" )),yaxt="n")
axis(2,cex.axis=cex)
mtext('Pre-Intervention', side=3, line=0, at=0,adj =0,cex=cex)
mtext('Post-Intervention', side=3, line=0, at=11,adj =0,cex=cex)
text(3,-40, expression(paste(delta, "=0")),cex=cex+1)
barplot(-100*as.numeric(filter(x = x, filter = 1.0,method = "rec" )),yaxt="n")
axis(2,cex.axis=cex)
mtext('Pre-Intervention', side=3, line=0, at=0,adj =0,cex=cex)
mtext('Post-Intervention', side=3, line=0, at=11,adj =0,cex=cex)
text(3,-40, expression(paste(delta, "=1.0")),cex=cex+1)
barplot(-100*as.numeric(filter(x = x, filter = 0.25,method = "rec" )),yaxt="n")
axis(2,cex.axis=cex)
mtext('Pre-Intervention', side=3, line=0, at=0,adj =0,cex=cex)
mtext('Post-Intervention', side=3, line=0, at=11,adj =0,cex=cex)
text(3,-40, expression(paste(delta, "=0.25")),cex=cex+1)
barplot(-100*as.numeric(filter(x = x, filter = 0.5,method = "rec" )),yaxt="n")
axis(2,cex.axis=cex)
mtext('Pre-Intervention', side=3, line=0, at=0,adj =0,cex=cex)
mtext('Post-Intervention', side=3, line=0, at=11,adj =0,cex=cex)
text(3,-40, expression(paste(delta, "=0.5")),cex=cex+1)
barplot(-100*as.numeric(filter(x = x, filter = 0.75,method = "rec" )),yaxt="n")
axis(2,cex.axis=cex)
mtext('Pre-Intervention', side=3, line=0, at=0,adj =0,cex=cex)
mtext('Post-Intervention', side=3, line=0, at=11,adj =0,cex=cex)
text(3,-40, expression(paste(delta, "=0.75")),cex=cex+1)
barplot(-100*as.numeric(filter(x = x, filter = 0.9,method = "rec" )),yaxt="n")
axis(2,cex.axis=cex)
mtext('Pre-Intervention', side=3, line=0, at=0,adj =0,cex=cex)
mtext('Post-Intervention', side=3, line=0, at=11,adj =0,cex=cex)
text(3,-40, expression(paste(delta, "=0.9")),cex=cex+1)
}


png("intervention.png",width     = 5,
    height    = 5,
    units     = "in",
    res       = 800,
    pointsize = 4)
plotx()
dev.off()








