rm(list=ls())
library(viridis)


#############################################################################
## fig 5.1?
ir <- c(19,6,9,20,1,4)
mr <- c(32,10,8,23,2,2)
cr <- c(27,14,3,19,1,1)
ds <- matrix(cbind(ir, mr, cr), nrow=6)

j <- rep(NA,6)
ds1 <- matrix(cbind(ir, j, j), nrow=6)
ds2 <- matrix(cbind(j, mr, j), nrow=6)
ds3 <- matrix(cbind(j, j, cr), nrow=6)
vc <- c(viridis(n=50)[2],viridis(n=50)[50],
        viridis(n=50)[36],viridis(n=50)[2],
        viridis(n=50)[50],viridis(n=50)[36])


graphics.off()
##dev.new(height=3, width=6.5)
png(file="itrc/fig5.1.png", width=6.5, height=3, units="in", res=1000)
par(mai=c(0.5, 1, 0.64, 3), yaxs="i")
bp <- barplot(height=ds1[sort.list(ds1[,1], decreasing=TRUE),], beside=TRUE,
              axes=FALSE, ylim=c(0,35), col=vc,
              density=c(-1,40,-1,40,-1,40))
par(new=TRUE)
bp <- barplot(height=ds2[sort.list(ds1[,1], decreasing=TRUE),], beside=TRUE,
              axes=FALSE, ylim=c(0,35), col=vc,
              density=c(-1,40,-1,40,-1,40))
par(new=TRUE)
bp <- barplot(height=ds3[sort.list(ds1[,1], decreasing=TRUE),], beside=TRUE,
              axes=FALSE, ylim=c(0,35), col=vc,
              density=c(-1,40,-1,40,-1,40))
box()
axis(1, at=c(mean(bp[3:4]), mean(bp[9:10]),mean(bp[15:16])), labels=NA,
     cex.axis=.9)
axis(2, at=seq(0,40,5), las=2, font=2, cex.axis=.9)
mtext(side=1, at=c(mean(bp[3:4]), mean(bp[9:10]),mean(bp[15:16])),
      text=c("Industrial", "MS4", "Construction"),
      font=2, line=0.6)
mtext(side=2, text="Number of Respondents", font=2,
      cex=1.05, line=2.6)
par(xpd=TRUE)
legend(x=max(bp)+1, y=30, fill=vc, bty="n",
       cex=0.85, text.font=2,
       legend=c("Required Self Reporting", "Routine Agency Insp.",
                "Effluent Monitoring", "Event Dependent Agency Insp.",
                "None Required", "Digital Recording Devices" ),
       density=c(-1,40,-1,40,-1,40))
par(xpd=TRUE)
text(x=20, y=42, labels="Monitoring Approach for Post Construction",
     font=2, cex=1.1)
text(x=20, y=38.5, labels="Stormwater BMPs",
     font=2, cex=1.1)
graphics.off()


##graphics.off()




#############################################################################
## fig 5.2
ir <- c(15,20,6)
mr <- c(23,17,4)
cr <- c(12,22,5)
ds <- matrix(rbind(ir, mr, cr), nrow=3)

## vc <- c(viridis(n=20, option="magma")[3],
##         viridis(n=20, option="magma")[12],
##         viridis(n=20, option="magma")[7])
vc <- c(viridis(n=50)[2],
        viridis(n=50)[50],
        viridis(n=50)[36])

graphics.off()
##dev.new(height=3, width=6.5)
png(file="itrc/fig5.2.png", width=6.5, height=3, units="in", res=1000)
par(mai=c(0.5, 1, 0.65, 1.5), yaxs="i")
bp <- barplot(height=ds, beside=TRUE, axes=FALSE, ylim=c(0,25),
              col=vc)
box()
axis(1, at=bp[c(2,5,8)], labels=NA)
mtext(side=1, at=bp[c(2,5,8)], text=c("Required", "Not Required",
                                      "Don't Know"),
      font=2, line=0.75)
axis(2, at=seq(0,25,5), labels=seq(0,25,5), las=2, font=2)
## for (i in seq(5,20,5)) {
##     lines(x=c(min(bp)-1, max(bp)+1), y=rep(i,2))
## }
mtext(side=2, text="Number of Respondents", font=2,
      cex=1.25, line=2.6)
mtext(side=3, text="Maintenance Reporting Requirements", font=2,
      cex=1.25, line=1.9)
mtext(side=3, text="By Discharge Category", font=2,
      cex=1.25, line=0.7)
par(xpd=TRUE)
legend(x=max(bp)+1, y=20, pt.bg=vc, bty="n", pch=22,
       pt.cex=2, cex=1.1, text.font=2,
       legend=c("Industrial", "MS4", "Construction"))
graphics.off()


#############################################################################
## fig 3.1
x1 <- c(63.0, 52.2, 54.3, 10.9, 19.6, 17.4, 26.1, 56.5, 34.8, 60.9, 26.1)
l1 <- c("Soil type", "Vegetation", "Topography", "Temperature",
        "Long dry periods", "Saturated media", "Pollutant concentration",
        "Land use", "Space restrictions", "Receiving waters",
        "Other")
l1a <- c("Soil type", "Vegetation", "Topography", "Temperature",
        "Long dry", "Saturated", "Pollutant",
        "Land use", "Space", "Receiving",
        "Other")
l1b <- c(NA, NA, NA, NA,
        "periods", "media", "concentration",
        NA, "restrictions", "waters",
        NA)
ds <- data.frame(f=l1,r=x1, l1a=l1a, l1b=l1b, stringsAsFactors=FALSE)


graphics.off()
##dev.new(height=3, width=6.5)
png(file="itrc/fig3.1.png", width=6.5, height=3, units="in", res=1000)
par(mai=c(.85,.75,0.65,.25), yaxs="i")
##bp <- barplot(ds$r[order(ds$r, decreasing=TRUE)], ylim=c(0,70),
##        col="#73AE57", axes=FALSE)
bp <- barplot(ds$r[order(ds$r, decreasing=TRUE)], ylim=c(0,70),
        col="#6ECE58", axes=FALSE)
box()
axis(2, at=seq(0,70,10), labels=paste0(seq(0,70,10),"%"), las=2)
axis(1, at=bp, labels=NA)
mtext(text="Percent of Survey Respondents Indicating Site",
      side=3, cex=1.1, font=2, line=1.95)
mtext(text="Chacteristics Used in BMP Evaluation",
      side=3, cex=1.1, font=2, line=0.85)
par(xpd=TRUE)
text(x=bp-.75, y=-4, labels=ds$l1a[order(ds$r, decreasing=TRUE)],
     srt=315, pos=4, cex=.8)
text(x=bp-.95, y=-8.5, labels=ds$l1b[order(ds$r, decreasing=TRUE)],
     srt=315, pos=4, cex=.8)
graphics.off()

