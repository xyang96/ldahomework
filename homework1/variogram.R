
## This code plots variogram

## set working directory to file location 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## load files
camp <- read.csv("camp_primary.csv")

## create data with no missingness in posfev
campdat <- camp %>% filter(complete.cases(POSFEV))

# fit gls
fit_vario <- gls(POSFEV ~ visitc + trt:visitc, data = campdat)
est_acf <- ACF(fit_vario, form=~1|id)

## specify resType="response" to obtain the estimated variogram as defined in class
est_vario <- Variogram(fit_vario, form = ~visitc|id, resType="response")
sig2_hat <- var(resid(fit_vario))

## set some defaults for plotting 
textsize <- 22  ## plotting text size
chrsize  <- 2   ## plotting character (point size) parameter
linesize <- 2   ## plotting line width parameter

## plot variogram

par(mfrow=c(1,2),las=1)
scatter.smooth(est_vario$dist,est_vario$variog, ylim=c(0,0.5),
               ylab="",xlab="Distance (months)",main=expression("Variogram: " ~ hat(gamma)(u[jk])),
               pch=16,cex.main=linesize, cex.lab=linesize, cex.axis=linesize, cex=linesize/2, lwd=linesize,
               lpars=list(col='red',lty=1,lwd=linesize),xlim=c(0,50))
abline(h=sig2_hat, lty=2,col='grey',lwd=2)
legend("topleft","Loess smooth", col="red", lty=1,lwd=2,bty='n',cex=linesize)
scatter.smooth(est_vario$dist, 1-est_vario$variog/sig2_hat, pch=16,ylab="",xlab="Distance (Months)",
               main=expression("Estimated Auto-correlation:" ~ hat(rho)(u[jk]) == 1-hat(gamma)(u[jk])/hat(sigma^2)(u[jk])),
               cex.main=linesize, cex.lab=linesize, cex.axis=linesize, cex=linesize/2,
               lpars=list(col='red',lty=1,lwd=linesize),xlim=c(0,50))

dev.off()
