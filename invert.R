# Perform a single inversion as a function of sample ID
library(PEcAnRTM)
library(dtplyr)
library(dplyr)
library(data.table)
library(specobs)

id_separator <- "|"
outdir <- "raw_output"
dir.create(outdir, showWarnings = FALSE)

invert.id <- function(id, version=5, ...) {
    sep <- paste0("\\", id_separator)
    id.split <- strsplit(id, sep)[[1]]
    project <- tolower(id.split[1])
    sample.name <- id.split[2]
    stopifnot(!is.na(sample.name))
    sample.year <- id.split[3]
    stopifnot(!is.na(sample.year))
    message(paste0("project: ", project))
    message(paste0("sample.name: ", sample.name))
    message(paste0("sample.year: ", sample.year))
    dat.full <- readRDS(paste0("processed-spec-data/", project, ".rds")) %>%
        .[SampleName == sample.name]

    if (nrow(dat.full) == 0) {
        message(sprintf("Spectrum %s not found", id))
        return(NULL)
    }

    dat.reflspec <- dat.full[[1, "Reflectance"]]

    wl <- dat.reflspec[,1]
    wl_rng <- wl[wl >= 400][wl <= 2500]
    refl <- dat.reflspec[[wl]][,-1]
    wl.vec <- wl - 399
    wl.vec <- wl.vec[wl.vec > 0]

    if (all(is.na(refl))) {
        message(sprintf("%s spectrum is all NA or NAN. Returning NULL.", id))
        return(NULL)
    }
    if (any(is.na(refl))) {
        message(sprintf("%s spectrum contains NA. Returning NULL.", id))
        return(NULL)
    }
    pseudo_absorbance <- FALSE
    if (!is.null(dat.full$SpecialSpec)) {
        if (!(is.na(dat.full$SpecialSpec))) {
            if (dat.full$SpecialSpec == "PA") {
                pseudo_absorbance <- TRUE
            }
        }
    }
    if (!is.null(dim(refl))) {
        refl <- t(refl)
    }
    if (any(dim(refl) < 1)) {
        stop(sprintf("Reflectance spectrum %s not found", sample.name))
    }

    # Get column names from summary.simple function (a bit of a hack)
    samps <- matrix(0, nrow=1, ncol=6)
    colnames(samps) <- c(params.prospect5, "residual")
    samps.summary <- summary_simple(samps)
    cnames <- names(samps.summary)

    # Set up custom PROSPECT inversion parameters
    invert.options <- default.settings.prospect
    invert.options$model <- function(param, runID=NULL) {
        prospect(param, version)[wl.vec,1]
    }

    if (pseudo_absorbance) {
        message(sprintf("%s spectrum is pseudo-absorbance", id))
        invert.options$model <- function(param, runID=NULL) {
            mod <- prospect(param, version)[wl.vec,1]
            out <- log10(1/mod)
            return(out)
        }
    }
    invert.options$ngibbs.max <- 1e6
    invert.options$nchains <- 5
    invert.options$do.lsq <- FALSE

    save.samples <- sprintf("%s/%s.rds", outdir, ID)

    out <- invert.auto(observed = refl,
                       invert.options = invert.options,
                       return.samples = TRUE,
                       save.samples = save.samples,
                       parallel = TRUE, 
                       ...)
    return(out)
}

id <- commandArgs(trailingOnly=TRUE)
if (length(id) < 1){
    # Some IDs for testing functionality
    id <- c(
#            'ACCP|92HFS19RO3|1992',
#            'ACCP|92HFS6LL2|1992',
#            'ANGERS|Ace-pse_0071|2003',
#            'ANGERS|Ace-pse_0088|2003',
#            'ANGERS|Ace-pse_0091|2003',
#            'ANGERS|Ace-pse_0155|2003',
#            'ANGERS|Hed-hel_0240|2003',
#            'ANGERS|Pru-lau_0220|2003',
#            'ANGERS|Rob-pse_0180|2003',
#            'ANGERS|Rob-pse_0181|2003',
#            'ANGERS|Vib-pli_0210|2003',
#            'Arctic_Chl|1236spul|1000',
#            'Arctic_Chl|1258spun|1000',
#            'Arctic_Chl|1400|1000',
#            'LOPEX|Vit-vin_Leaf59|1993',
             'ngee_tropics|BNL10456|2016',
             'ngee_arctic|BNL1500|2014',
             'yang_pheno|AUN1_159|2011',
#            'yang_pheno|RO2U1_122|2012',
#            'yang_pheno|RO2U2_122|2012',
#            'yang_pheno|RO1L1_122|2012',
            'yang_pheno|RO2L1_122|2012'
            )

}

for (ID in id) {
    results <- invert.id(ID)
    success <- TRUE
    if (is.null(results)) {
        success <- FALSE
    } else if (is.na(results)) {
        success <- FALSE
    }
    if (success) {
        saveRDS(results, file = sprintf("%s/%s.rds", outdir, ID))
        write(ID, file = "finished.txt", append = TRUE)
    } else {
        message(sprintf("Run failed: %s", ID))
        write(ID, file = "failed.txt", append = TRUE)
    }
}

