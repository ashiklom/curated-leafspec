library(data.table)
load("lopex.invert.RData")
load("lopex.RData")

# Convert lopex results to data.table and add sample_id
angers.results.dt <- results[project == "LOPEX"]
#lopex.results.dt[, sample_id := rownames(lopex.results)]

# Average LOPEX chemistry data
mean.chr <- function(x){
    if(is.numeric(x)) return(mean(x, na.rm=TRUE))
    return(x[1])
}
lopex.davg <- lopex.dat[,lapply(.SD, mean.chr), by=sample_id]

# Merge with chemistry data
setkey(lopex.results.dt, sample_id)
setkey(lopex.davg, sample_id)
lopex <- lopex.davg[lopex.results.dt]

# Some results plots
png("lopex.validate.png", width=4, height=6, units="in", res=200)
par(mfrow=c(3,2), pch=20, mar=c(4,4,1,1), oma=c(0,0,2,0))
with(lopex,{
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
    title(main="LOPEX validation", xlab="Inversion", ylab="Measurement", outer=TRUE)
})
dev.off()
