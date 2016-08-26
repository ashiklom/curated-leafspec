library(data.table)
flist <- list.files("processed-spec-data/", full.names = TRUE)
outfile <- "id_list.txt"
file.remove(outfile)

for (f in flist) {
    dat <- readRDS(f)$traits
    ids <- dat[,sample_id]
    write(ids, outfile, append=TRUE)
}
