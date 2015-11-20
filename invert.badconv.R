
library(data.table)
fname.sh <- "00-run-inversion.sh"
submit.string <- 'qsub -q "geo*" -j y -o logs/%1$s -l h_rt=12:00:00 -v ID=\'%1$s\' run-inversion.sh'
write("#!/bin/bash", file = fname.sh)

load("bad.conv.list.RData")
bad.idlist.raw <- names(bad.conv.list)
bad.idlist <- gsub("(.*)\\.inv", "\\1", bad.idlist.raw)
write(sprintf(submit.string, bad.idlist), file=fname.sh, append=TRUE)

system("chmod +x 00-run-inversion.sh")
system(paste0("./", fname.sh))
