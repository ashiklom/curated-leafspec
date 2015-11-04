#' ---
#' title: Perform inversion of LOPEX and ANGERS data
#' author: Alexey Shiklomanov
#' ---

source("common.R")
library(PEcAnRTM)
ngibbs <- 50000
version <- 5
do.mle <- FALSE
quiet <- TRUE

#' Get column names from summary.simple function (a bit of a hack)
samps <- matrix(0, nrow=1, ncol=6)
colnames(samps) <- c(params.prospect5, "residual")
samps.summary <- summary.simple(samps)
cnames <- names(samps.summary)

#' # LOPEX inversion
load("lopex.RData")

lopex.results <- matrix(NA, ncol=length(cnames), nrow=nrow(lopex.dat))
rownames(lopex.results) <- lopex.dat[,sample_id]
colnames(lopex.results) <- cnames
print("Inverting LOPEX")
setkey(lopex.dat, sample_id)
for(r in 1:nrow(lopex.dat)){
    print(r)
    sample.id <- lopex.dat[r, sample_id]
    spec.id <- lopex.dat[sample.id, spec_id]
    refl <- t(lopex.reflspec[spec.id,])
    samps <- default.invert.prospect(refl, "identity", ngibbs, version, do.mle, quiet)
    save(samps, file=sprintf("samples/%s.inv.RData", sample.id))
    samps.bt <- burnin.thin(samps)
    samps.summary <- summary.simple(samps.bt)
    lopex.results[sample.id,] <- as.numeric(samps.summary[cnames])
}
save(lopex.results, file="lopex.invert.RData")

#' # ANGERS inversion
load("angers.RData")

nwl <- ncol(angers.reflspec)
angers.results <- matrix(NA, ncol=length(cnames), nrow=nrow(angers.dat))
rownames(angers.results) <- angers.dat[,sample_id]
colnames(angers.results) <- cnames

#' Set up custom PROSPECT inversion parameters
model <- function(param) prospect(param, version)[1:nwl,1]
prior.params <- prior.defaultvals.prospect(sd.inflate = 3)
prior <- with(prior.params, priorfunc.prospect(mu, sigma))
pm <- c(1, 0, 0, 0, 0)

print("Inverting ANGERS")
print(sprintf("%d spectra", nrow(angers.dat)))
for(r in 1:nrow(angers.dat)){
    print(r)
    id <- angers.dat[r, sample_id]
    refl <- angers.reflspec[id,]
    inits <- with(prior.params, rlnorm(5, mu, sigma))
    inits[1] <- inits[1] + 1
    names(inits) <- params.prospect5
    samps <- invert.custom(observed=refl, inits=inits, ngibbs=ngibbs,
                           prior=prior, pm=pm, model=model, do.lsq=do.mle, quiet=TRUE)
    save(samps, file=sprintf("samples/%s.inv.RData", id))
    samps.bt <- burnin.thin(samps)
    samps.summary <- summary.simple(samps.bt)
    angers.results[id,] <- as.numeric(samps.summary[cnames])
}
save(angers.results, file="angers.invert.RData")
