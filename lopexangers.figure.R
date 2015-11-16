library(data.table)
load("lopexangers.results.RData")
load("lopex.RData")
load("angers.RData")

# Convert lopex results to data.table and add sample_id
#angers.results.dt <- results[project == "LOPEX"]
#lopex.results.dt[, sample_id := rownames(lopex.results)]

# Average LOPEX chemistry data
mean.chr <- function(x){
    if(is.numeric(x)) return(mean(x, na.rm=TRUE))
    return(x[1])
}
lopex.davg <- lopex.dat[,lapply(.SD, mean.chr), by=sample_id]

# Merge Lopex and Angers chemistry data
common.cols <- intersect(colnames(angers.dat), colnames(lopex.davg))
setkeyv(angers.dat, common.cols)
setkeyv(lopex.davg, common.cols)
both.dat <- merge(angers.dat, lopex.davg, all=T)

# Merge with results data frame
setkey(results, sample_id)
setkey(both.dat, sample_id)
dat <- results[both.dat]

# Some results plots
png("both.validation.png", width=6, height=6, units="in", res=200)
par(mfrow=c(2,2), pch=20, mar=c(2,2,2,1), oma=c(4,4,1,0))
with(dat,{
    plot(Cab.mu, C_ab, xlab='', ylab='')
    title(main=expression(paste("Total chlorophyll (", mu, "g ", cm^-2, ")")))
    abline(0,1)
    plot(Car.mu, C_car, xlab='', ylab='')#, xlim=c(0, 60))
    title(main=expression(paste("Total carotenoids (", mu, "g ", cm^-2, ")")))
    abline(0,1)
    plot(Cw.mu*10000, EWT*10000, xlab='', ylab='')
    title(main=expression(paste("Leaf water content (g ",m^-2,")")))
    abline(0,1)
    plot(Cm.mu*10000, LMA*10000, xlab='', ylab='', ylim=c(0,200))
    title(main=expression(paste("LMA (g ", m^-2, ")")))
    abline(0,1)
    mtext("Spectral inversion estimate", 1, 1, cex=1.2, outer=TRUE)
    mtext("Direct measurement", 2, 1, cex=1.2, outer=TRUE)
    #title(xlab="Inversion estimate", ylab="Measurement", outer=TRUE, cex=4, line=1)
})
dev.off()

##### I DIDN'T ALTER THE FIGURES BELOW. THEY ARE FOR LOPEX ONLY #####

# Spectral validation
library(PEcAnRTM)
nr <- nrow(lopex)
nwl <- ncol(lopex.reflspec)
wl <- 1:nwl + 399
error.refl <- matrix(NA, ncol=nwl, nrow=nr)
rownames(error.refl) <- lopex[,sample_id]
error.trans <- error.refl
r <- 1
id.rxp <- "LOPEX_(.*)_[[:digit:]]+$"
pdf("lopex.spectra.pdf")
for(r in 1:nrow(lopex)){
    print(r)
    id.full <- lopex[r, sample_id]
    id <- gsub(id.rxp, "\\1", id.full)
    index <- which(grepl(id, rownames(lopex.reflspec)))
    index.t <- which(grepl(id, rownames(lopex.transspec)))
    spec <- t(lopex.reflspec[index,])
    transspec <- t(lopex.transspec[index.t,])
    param.inv <- lopex[r, c("N.mu", "Cab.mu", "Car.mu", "Cw.mu", "Cm.mu"), with=F]
    param.true <- lopex[r, c("N", "C_ab", "C_car", "EWT", "LMA"), with=F]
    if(any(is.na(param.true))) next
    mod.inv <- prospect(param.inv, 5)
    mod.true <- prospect(param.true, 5)
# PDF plot
    matplot(wl, spec, type='l', col=1)
    lines(wl, mod.inv[,1], col=2)
    lines(wl, mod.true[,1], col=3)
    title(main=id.full)
    legend("topright", c("observed", "inversion", "true sim"), lty=1, col=1:3)
# Error matrix
    error.refl[id.full,] <- mod.inv[,1] - rowMeans(spec)
    error.trans[id.full,] <- mod.inv[,2] - rowMeans(transspec)
}
#dev.off()

# Process error matrices
reflerror.mean <- colMeans(error.refl, na.rm=TRUE)
reflerror.q25 <- apply(error.refl, 2, quantile, 0.025, na.rm=TRUE)
reflerror.q975 <- apply(error.refl, 2, quantile, 0.975, na.rm=TRUE)
transerror.mean <- colMeans(error.trans, na.rm=TRUE)
transerror.q25 <- apply(error.trans, 2, quantile, 0.025, na.rm=TRUE)
transerror.q975 <- apply(error.trans, 2, quantile, 0.975, na.rm=TRUE)

pdf("LOPEX.error.pdf")
par(mfrow=c(2,1))
plot(0, 0, type='n', xlim=c(400, 2500), ylim=c(-0.1, 0.1), xlab='', ylab='')
lines(wl, reflerror.mean, col=1)
lines(wl, reflerror.q25, lty=2)
lines(wl, reflerror.q975, lty=2)
abline(h=0, col=2)
title(main="Reflectance", xlab="Wavelength", ylab="Reflectance bias")
plot(0, 0, type='n', xlim=c(400, 2500), ylim=c(-0.2, 0.2), xlab='', ylab='')
lines(wl, transerror.mean, col=1)
lines(wl, transerror.q25, lty=2)
lines(wl, transerror.q975, lty=2)
abline(h=0, col=2)
title(main="Transmittance", xlab="Wavelength", ylab="Transmittance bias")
dev.off()
