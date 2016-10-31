library(data.table)
flist <- list.files("processed-spec-data/", 
                    pattern = "*.rds",
                    full.names = TRUE)
outfile <- "all_ids.txt"
file.remove(outfile)

for (f in flist) {
    dat <- readRDS(f)
    ids <- dat[,FullName]
    write(ids, outfile, append=TRUE)
}
