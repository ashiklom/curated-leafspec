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

### Plot the 20 worst fits 
# Calculate parameter errors
angers[, N.error := N.mu - N]
angers[, Cab.error := Cab.mu - C_ab]
angers[, Car.error := Car.mu - C_car]
angers[, Cw.error := Cw.mu - EWT]
angers[, Cm.error := Cm.mu - LMA]
angers[, tot.error := N.error/N + Cab.error/C_ab + Car.error/C_car + Cw.error/EWT + Cm.error/LMA]

angers.bs <- angers[order(tot.error)]
angers.es <- angers[order(tot.error, decreasing=TRUE)]

pdf("angers.badresults.pdf")
for(r in 1:20){
    id <- angers.es[r, sample_id]
    spec <- angers.reflspec[id,]
    wl <- 400:2450
    modpars <- angers.es[r, c("N.mu", "Cab.mu", "Car.mu", "Cw.mu", "Cm.mu"), with=F]
    truepars <- angers.es[r, c("N", "C_ab", "C_car", "EWT", "LMA"), with=F]
    mod <- prospect(modpars, 5)[wl-399,1]
    mod.t <- prospect(truepars, 5)[wl-399,1]
    plot(wl, spec, type='l', ylim=range(c(spec,mod), na.rm=TRUE))
    lines(wl, mod, col=2)
    lines(wl, mod.t, col=3)
    legend("topright", c("Observed", "PROSECT-invert", "PROSPECT-true"), lty=1, col=1:3)
    title(main = sprintf("Invert: N = %.2f Cab = %.2f Car = %.2f Cw = %.4f LMA = %.2f\nTrue: N = %.2f Cab = %.2f Car = %.2f Cw = %.4f LMA = %.2f", 
                          modpars[[1]], modpars[[2]], modpars[[3]], modpars[[4]], modpars[[5]]*10000,
                          truepars[[1]], truepars[[2]], truepars[[3]], truepars[[4]], truepars[[5]]*10000))
}
dev.off()

pdf("angers.goodresults.pdf")
for(r in 1:20){
    id <- angers.bs[r, sample_id]
    spec <- angers.reflspec[id,]
    wl <- 400:2450
    modpars <- angers.bs[r, c("N.mu", "Cab.mu", "Car.mu", "Cw.mu", "Cm.mu"), with=F]
    truepars <- angers.bs[r, c("N", "C_ab", "C_car", "EWT", "LMA"), with=F]
    mod <- prospect(modpars, 5)[wl-399,1]
    mod.t <- prospect(truepars, 5)[wl-399,1]
    plot(wl, spec, type='l', ylim=range(c(spec,mod), na.rm=TRUE))
    lines(wl, mod, col=2)
    lines(wl, mod.t, col=3)
    legend("topright", c("Observed", "PROSECT-invert", "PROSPECT-true"), lty=1, col=1:3)
    title(main = sprintf("Invert: N = %.2f Cab = %.2f Car = %.2f Cw = %.4f LMA = %.2f\nTrue: N = %.2f Cab = %.2f Car = %.2f Cw = %.4f LMA = %.2f", 
                          modpars[[1]], modpars[[2]], modpars[[3]], modpars[[4]], modpars[[5]]*10000,
                          truepars[[1]], truepars[[2]], truepars[[3]], truepars[[4]], truepars[[5]]*10000))
}
dev.off()
