library(data.table)
dat <- readRDS("specdat.rds")
outfile <- "all_ids.txt"

write(dat[, FullName], file = outfile)
