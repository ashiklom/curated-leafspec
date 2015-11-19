#' ---
#' title: Perform inversion of FFT data
#' author: Alexey Shiklomanov
#' ---

library(data.table)
fname.sh <- "00-run-inversion.sh"
submit.string <- 'qsub -q "geo*" -j y -o logs/%1$s -l h_rt=12:00:00 -v ID=\'%1$s\' run-inversion.sh'
write("#!/bin/bash", file = fname.sh)

load("processed-spec-data/fft.RData")
fft.idlist <- unique(fft.dat[,sample_id])
write(sprintf(submit.string, fft.idlist), file=fname.sh, append=TRUE)

system("chmod +x 00-run-inversion.sh")
system(paste0("./", fname.sh))

