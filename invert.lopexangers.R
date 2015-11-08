#' ---
#' title: Perform inversion of LOPEX and ANGERS data
#' author: Alexey Shiklomanov
#' ---

library(data.table)
fname.sh <- "00-run-inversion.sh"
submit.string <- 'qsub -q "geo*" -j y -l h_rt=12:00:00 -v ID=%s run-inversion.sh'
write("#!/bin/bash", file = fname.sh)

load("lopex.RData")
lopex.idlist <- unique(lopex.dat[,sample_id])
write(sprintf(submit.string, lopex.idlist), file=fname.sh, append=TRUE)

load("angers.RData")
angers.idlist <- angers.dat[,sample_id]
write(sprintf(submit.string, angers.idlist), file=fname.sh, append=TRUE)

system(paste0("./", fname.sh))

