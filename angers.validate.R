library(data.table)
load("lopexangers.results.RData")
load("angers.RData")

# Convert angers results to data.table and add sample_id
#angers.results.dt <- results[project == "ANGERS"]
#angers.results.dt[, sample_id := rownames(angers.results)]

# Merge with chemistry data
setkey(results, sample_id)
setkey(angers.dat, sample_id)
angers <- angers.dat[results]

# 1:1 results plots
png("angers.validate.png", width=4, height=6, units="in", res=200)
par(mfrow=c(3,2), pch=20, mar=c(4,4,1,1), oma=c(0,0,2,0))
with(angers,{
    plot(N.mu, N, xlim=range(c(N.q25, N.q975)), xlab='', ylab='', main="N")
    arrows(N.q25, N, N.q975, N, length=0.05, angle=90, code=3)
    abline(0,1)
    plot(Cab.mu, C_ab, xlim=range(c(Cab.q25, Cab.q975)), xlab='', ylab='', main="Chlorophyll (a,b)")
    arrows(Cab.q25, C_ab, Cab.q975, C_ab, length=0.05, angle=90, code=3)
    abline(0,1)
    plot(Car.mu, C_car, xlim=range(c(0, 20)), xlab='', ylab='', main="Carotenoids")
    arrows(Car.q25, C_car, Car.q975, C_car, length=0.05, angle=90, code=3)
    abline(0,1)
    plot(Cw.mu, EWT, xlim=range(c(Cw.q25, Cw.q975)), xlab='', ylab='', main="Leaf water content")
    arrows(Cw.q25, EWT, Cw.q975, EWT, length=0.05, angle=90, code=3)
    abline(0,1)
    plot(Cm.mu, LMA, xlim=range(c(Cm.q25, Cm.q975)), xlab='', ylab='', main="LMA")
    arrows(Cm.q25, LMA, Cm.q975, LMA, length=0.05, angle=90, code=3)
    abline(0,1)
    title(main="angers validation", xlab="Inversion", ylab="Measurement", outer=TRUE)
})
dev.off()

# Spectral validation
library(PEcAnRTM)
nr <- nrow(angers)
nwl <- ncol(angers.reflspec)
wl <- 1:nwl + 399
error.refl <- matrix(NA, ncol=nwl, nrow=nr)
rownames(error.refl) <- angers[,sample_id]
error.trans <- error.refl
r <- 1
id.rxp <- "ANGERS_(.*)_[[:digit:]]+$"
pdf("angers.spectra.pdf")
for(r in 1:nrow(angers)){
    print(r)
    id.full <- angers[r, sample_id]
    id <- gsub(id.rxp, "\\1", id.full)
    index <- which(grepl(id, rownames(angers.reflspec)))
    index.t <- which(grepl(id, rownames(angers.transspec)))
    spec <- angers.reflspec[index,]
    transspec <- angers.transspec[index.t,]
    param.inv <- angers[r, c("N.mu", "Cab.mu", "Car.mu", "Cw.mu", "Cm.mu"), with=F]
    param.true <- angers[r, c("N", "C_ab", "C_car", "EWT", "LMA"), with=F]
    if(any(is.na(param.true))) next
    mod.inv <- prospect(param.inv, 5)[wl-399,]
    mod.true <- prospect(param.true, 5)[wl-399,]
# PDF plot
    matplot(wl, spec, type='l', col=1)
    lines(wl, mod.inv[,1], col=2)
    lines(wl, mod.true[,1], col=3)
    title(main=id.full)
    legend("topright", c("observed", "inversion", "true sim"), lty=1, col=1:3)
# Error matrix
    error.refl[id.full,] <- mod.inv[,1] - spec
    error.trans[id.full,] <- mod.inv[,2] - transspec
}
dev.off()

# Process error matrices
reflerror.mean <- colMeans(error.refl, na.rm=TRUE)
reflerror.q25 <- apply(error.refl, 2, quantile, 0.025, na.rm=TRUE)
reflerror.q975 <- apply(error.refl, 2, quantile, 0.975, na.rm=TRUE)
transerror.mean <- colMeans(error.trans, na.rm=TRUE)
transerror.q25 <- apply(error.trans, 2, quantile, 0.025, na.rm=TRUE)
transerror.q975 <- apply(error.trans, 2, quantile, 0.975, na.rm=TRUE)

pdf("ANGERS.error.pdf")
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
