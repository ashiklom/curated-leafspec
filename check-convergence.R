library(coda)
library(PEcAnRTM)
flist <- list.files("samples", full.names=TRUE)
f <- flist[7]

conv.list <- list()
bad.conv.list <- list()

for(f in flist){
    load(f)
    f.rxp <- "samples/(.*)\\.RData"
    f.id <- gsub(f.rxp, "\\1", f)
    sbt <- lapply(samps.list, burnin.thin, burnin=40000)
    smcmc <- as.mcmc.list(lapply(sbt, as.mcmc))
    gd <- gelman.diag(smcmc, autoburnin=FALSE)
    gdmp <- gd$mpsrf
    print(paste(f, round(gdmp, 3), sep="      "))
    conv.list[[f.id]] <- gdmp
    if(gdmp > 3) bad.conv.list[[f.id]] <- list(gelman.diag = gd, samples = smcmc)
}

save(conv.list, file="conv.list.RData")
save(bad.conv.list, file="bad.conv.list.RData")
