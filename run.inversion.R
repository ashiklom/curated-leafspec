#!/usr/bin/Rscript
#' ---
#' title: Invert dataset by name
#' author: Alexey Shiklomanov
#' ---

library(data.table)
fname.sh <- "00-run-inversion.sh"
submit.string <- 'qsub -N "%1$s" -v ID=\'%1$s\' run-inversion.sh'
dir.create(path = "logs", showWarnings = FALSE)

raw_args <- commandArgs(trailingOnly = TRUE)

for (arg in raw_args){
    write("#!/bin/bash -l", file = fname.sh)
    if (arg == "no_convergence") {
        dat.idlist <- readLines("no.convergence.txt")
    } else {
        load_fname <- sprintf("processed-spec-data/%s.rds", arg)
        if (file.exists(load_fname)) {
            dat <- readRDS(load_fname)$traits
            dat.idlist <- unique(dat[,sample_id])
        } else {
            warning(paste(load_fname, "doesn't exist. Skipping"))
            next
        }
    }
    write(sprintf(submit.string, dat.idlist), file=fname.sh, append=TRUE)
    system(paste("chmod +x", fname.sh))
    system(paste0("./", fname.sh))
}

