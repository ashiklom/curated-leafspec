# Perform a single inversion as a function of sample ID
source("common.R")
invert.id <- function(id, version=5, ...){
    require(PEcAnRTM)
    sep <- paste0("\\", id_separator)
    id.split <- strsplit(id, sep)[[1]]
    project <- tolower(id.split[1])
    sample.name <- id.split[2]
    stopifnot(!is.na(sample.name))
    sample.year <- id.split[3]
    stopifnot(!is.na(sample.year))
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
    if (length(index) < 1){
        message(sprintf("Spectrum %s not found", id))
        return(NULL)
    }

    refl <- dat.reflspec[index, as.character(wl)]
    if (all(is.na(refl))) {
        message(sprintf("%s spectrum is all NA or NAN. Returning NULL.", id))
        return(NULL)
    }
    if (any(is.na(refl))) {
        message(sprintf("%s spectrum contains NA. Returning NULL.", id))
        return(NULL)
    }
    pseudo_reflectance <- FALSE
    if (any(refl > 1)) {
        message(sprintf("%s spectrum contains values > 1. Assuming pseudo-reflectance.",
                        id))
        pseudo_reflectance <- TRUE
    }
    if (!is.null(dim(refl))) refl <- t(refl)
    if (any(dim(refl) < 1)) stop(sprintf("Reflectance spectrum %s not found", 
                                         sample.name))

    # Get column names from summary.simple function (a bit of a hack)
    samps <- matrix(0, nrow=1, ncol=6)
    colnames(samps) <- c(params.prospect5, "residual")
    samps.summary <- summary_simple(samps)
    cnames <- names(samps.summary)

    # Set up custom PROSPECT inversion parameters
    invert.options <- default.settings.prospect
    invert.options$model <- function(param, seed=NULL) prospect(param, version)[wl.vec,1]
    if (pseudo_reflectance) {
        invert.options$model <- function(param, seed=NULL) {
            mod <- prospect(param, version)[wl.vec,1]
            out <- log10(1/mod)
            return(out)
        }
    }
    invert.options$n.tries <- 5
    invert.options$nchains <- 5
    invert.options$do.lsq <- FALSE

    out <- invert.auto(observed = refl,
                       invert.options = invert.options,
                       return.samples = TRUE,
                       parallel = TRUE,
                       ...)

    return(out)
}

dir.create("raw_output", showWarnings = FALSE)

id <- commandArgs(trailingOnly=TRUE)
if (length(id) < 1){
    # Some IDs for testing functionality
    id <- c(
            'ACCP|92HFS19RO3|1992',
            'ACCP|92HFS6LL2|1992',
            'ANGERS|Ace-pse_0071|2003',
            'ANGERS|Ace-pse_0088|2003',
            'ANGERS|Ace-pse_0091|2003',
            'ANGERS|Ace-pse_0155|2003',
            'ANGERS|Hed-hel_0240|2003',
            'ANGERS|Pru-lau_0220|2003',
            'ANGERS|Rob-pse_0180|2003',
            'ANGERS|Rob-pse_0181|2003',
            'ANGERS|Vib-pli_0210|2003',
            'Arctic_Chl|1236spul|1000',
            'Arctic_Chl|1258spun|1000',
            'Arctic_Chl|1400|1000',
            'LOPEX|Vit-vin_Leaf59|1993',
            'YangHF|RO2U1_122|2012',
            'YangHF|RO2U2_122|2012',
            'YangHF|RO1L1_122|2012',
            'YangHF|RO2L1_122|2012'
            )

}
for (ID in id) {
    results <- invert.id(ID)
    if (!is.null(results)) {
        saveRDS(results, file = sprintf("raw_output/%s.rds", ID))
    } else {
        message(sprintf("Results were NULL for %s", ID))
    }
}

