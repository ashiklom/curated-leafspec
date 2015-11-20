library(data.table)
flist <- list.files("results")
id.list <- gsub("(.*).inv.csv", "\\1", flist)
names(id.list) <- flist
results.list <- list()
for(f in flist) {
    ff <-file.path("results", f)
    dat <- fread(ff, header=TRUE)
    dat[, sample_id := id.list[f]]
    results.list[[f]] <- dat
}

results <- rbindlist(results.list)
save(results, file="processed-spec-data/all.results.RData")

