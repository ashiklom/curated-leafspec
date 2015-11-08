library(data.table)
flist <- list.files("results")
id.list <- gsub("(.*).csv", "\\1", flist)
names(id.list) <- flist
results.list <- list()
for(f in flist) {
    dat <- fread(f, header=TRUE)
    dat[, sample_id := id.list[f]]
    results.list[[f]] <- dat
}

results <- rbindlist(results.list)
save(results, file="lopexangers.results.RData")

