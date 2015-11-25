# Perform a single inversion as a function of sample ID
invert.id <- function(id){
    require(data.table)
    require(PEcAnRTM)
    require(coda)
    id.rxp <- "([[:alpha:]]+)_(.*)_([[:digit:]]+)$"
    project <- tolower(gsub(id.rxp, "\\1", id))
    sample.name <- gsub(id.rxp, "\\2", id)
    sample.year <- gsub(id.rxp, "\\3", id)
    print(paste0("project: ", project))
    print(paste0("sample.name: ", sample.name))
    print(paste0("sample.year: ", sample.year))
    load(paste0("processed-spec-data/", project, ".RData"))
    dat.dat <- get(paste0(project, ".dat"))
    dat.reflspec <- get(paste0(project, ".reflspec"))

    ngibbs <- 100000
    burnin <- 80000
    nchains <- 5
    version <- 5
    target <- 0.234
    do.lsq <- FALSE
    quiet <- TRUE

    # Get column names from summary.simple function (a bit of a hack)
    samps <- matrix(0, nrow=1, ncol=6)
    colnames(samps) <- c(params.prospect5, "residual")
    samps.summary <- summary.simple(samps)
    cnames <- names(samps.summary)

    wl.all <- as.numeric(colnames(dat.reflspec))
    wl <- wl.all[wl.all >= 400 & wl.all <= 2500]
    wl.vec <- wl-399

    # Set up custom PROSPECT inversion parameters
    model <- function(param) prospect(param, version)[wl.vec,1]
    prior.params <- prior.defaultvals.prospect(sd.inflate = 3)
    prior <- with(prior.params, priorfunc.prospect(mu, sigma))
    pm <- c(1, 0, 0, 0, 0)

    index <- which(grepl(sample.name, rownames(dat.reflspec)))

    refl <- dat.reflspec[index, as.character(wl)]
    if(!is.null(dim(refl))) refl <- t(refl)

    try.again <- TRUE
    i.try <- 1
    n.tries <- 5
    while(try.again & i.try <= n.tries){
        # Randomly draw initial conditions
        inits <- with(prior.params, sapply(1:5, function(x) rlnorm(5, mu, sigma)))
        inits[1,] <- inits[1,] + 1
        rownames(inits) <- params.prospect5
        # Perform inversion and save outputs
        print(try.again)
        print(i.try)
        samps.list <- lapply(1:nchains, function(i)
                             invert.custom(observed=refl, inits=inits[,i], ngibbs=ngibbs,
                                           prior=prior, pm=pm, model=model, do.lsq=do.lsq, quiet=quiet, target=target))
        save(samps.list, file=sprintf("samples/%s.inv.RData", id))
        samps.list.bt <- lapply(samps.list, burnin.thin, burnin=burnin, thin=1)
        # Check for convergence. Repeat if necessary.
        smcmc <- as.mcmc.list(lapply(samps.list.bt, as.mcmc))
        gd <- try(gelman.diag(smcmc, autoburnin=FALSE))
        if(is.character(gd)) {
            i.try <- i.try + 1
            print("Could not calculate Gelman diag. Trying again")
            next
        } else {
            gdmp <- gd$mpsrf
            if(gdmp < 1.1){
                msg <- sprintf("Converged with Gelman diag = %.3f", gdmp)
                print(msg)
                try.again <- FALSE
                samps <- burnin.thin(do.call(rbind, samps.list.bt), burnin=0)
                results <- summary.simple(samps)
                results$gelman.diag <- gdmp
                write.csv(results, file = sprintf("results/%s.inv.csv", id))
            } else {
                msg <- sprintf("Did not converge. GD = %.3f. Trying again.", gdmp)
                print(msg)
                i.try <- i.try + 1
                target <- target * 0.9
                if(i.try > 3) do.lsq <- TRUE
            }
        }
    }
    if((i.try >= n.tries) & try.again) print("Convergence was NOT achieved")

}

id <- commandArgs(trailingOnly=TRUE)
invert.id(id)
