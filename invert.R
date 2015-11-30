# Perform a single inversion as a function of sample ID
invert.id <- function(id, ngibbs=100000){
    require(PEcAnRTM)
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

    burnin <- floor(0.8 * ngibbs)
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

    inits.function <- function(){
        inits <- with(prior.params, rlnorm(5, mu, sigma))
        inits[1] <- inits[1] + 1
        names(inits) <- params.prospect5
        return(inits)
    }

    out <- invert.auto(observed = refl,
                       model = model,
                       ngibbs = ngibbs,
                       nchains = 5,
                       prior.function = prior,
                       inits.function = inits.function,
                       param.mins = pm,
                       burnin = burnin,
                       n.tries = 5,
                       return.samples = TRUE,
                       target = 0.234,
                       target.adj = 0.8,
                       do.lsq.first = FALSE,
                       do.lsq.after = 3,
                       save.samples = NULL)

    return(out)
}

#id <- commandArgs(trailingOnly=TRUE)
id <- "LOPEX_Tri-pra_Leaf01_1993"
invert.id(id)

