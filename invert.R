# Perform a single inversion as a function of sample ID
source("common.R")
invert.id <- function(id, version=5, ngibbs=100000){
    require(PEcAnRTM)
    sep <- paste0("\\", id_separator)
    id.split <- strsplit(id, sep)[[1]]
    project <- tolower(id.split[1])
    sample.name <- id.split[2]
    sample.year <- id.split[3]
    print(paste0("project: ", project))
    print(paste0("sample.name: ", sample.name))
    print(paste0("sample.year: ", sample.year))
    dat.full <- readRDS(paste0("processed-spec-data/", project, ".rds"))
    dat.dat <- dat.full$traits
    dat.reflspec <- dat.full$reflectance

    wl.all <- as.numeric(colnames(dat.reflspec))
    wl <- wl.all[wl.all >= 400 & wl.all <= 2500]
    wl.vec <- wl-399

    index <- which(grepl(sample.name, rownames(dat.reflspec)))
    refl <- dat.reflspec[index, as.character(wl)]
    if (!is.null(dim(refl))) refl <- t(refl)
    if (any(dim(refl) < 1)) stop(sprintf("Reflectance spectrum %s not found", 
                                         sample.name))

    # Get column names from summary.simple function (a bit of a hack)
    samps <- matrix(0, nrow=1, ncol=6)
    colnames(samps) <- c(params.prospect5, "residual")
    samps.summary <- summary.simple(samps)
    cnames <- names(samps.summary)

    # Set up custom PROSPECT inversion parameters
    invert.options <- default.settings.prospect
    invert.options$model <- function(param) prospect(param, version)[wl.vec,1]
    invert.options$ngibbs <- ngibbs
    invert.options$burnin <- floor(ngibbs * 0.8)
    invert.options$n.tries <- 5
    invert.options$nchains <- 5

    out <- invert.auto(observed = refl,
                       invert.options = invert.options,
                       return.samples = TRUE,
                       parallel = TRUE)

    return(out)
}

id <- commandArgs(trailingOnly=TRUE)
if (length(id) < 1){
    #id <- "LOPEX_Tri-pra_Leaf01_1993"
    #id <- "Arctic_Chl|1236spul|1000"
    id <- "ACCP|92CWS20BA2|1992"
}
results <- invert.id(id, ngibbs=1e5)

dir.create("raw_output", showWarnings = FALSE)
saveRDS(results, file = paste0("raw_output/", id, ".rds"))

