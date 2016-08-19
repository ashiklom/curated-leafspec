#!/usr/bin/Rscript
#' ---
#' title: Perform inversion of LOPEX and ANGERS data
#' author: Alexey Shiklomanov
#' ---

library(data.table)
fname.sh <- "00-run-inversion.sh"
submit.string <- 'qsub -q "geo*" -j y -o logs/ -l h_rt=12:00:00 -v ID=\'%s\' run-inversion.sh'
dir.create(path = "logs", showWarnings = FALSE)

raw_args <- commandArgs(trailingOnly = TRUE)

for (arg in raw_args){
    write("#!/bin/bash", file = fname.sh)
    load_fname <- sprintf("processed-spec-data/%s.rds", arg)
    if (file.exists(load_fname)) {
        dat <- readRDS(load_fname)$traits
        dat.idlist <- unique(dat[,sample_id])
        write(sprintf(submit.string, dat.idlist), file=fname.sh, append=TRUE)
        system(paste("chmod +x", fname.sh))
        system(paste0("./", fname.sh))
        file.remove(fname.sh)
    } else {
        warning(paste(load_fname, "doesn't exist. Skipping"))
        next
    }
}

