library(PEcAnRTM)
library(data.table)
outdir <- "raw_output"
flist <- list.files(outdir)
id.list <- gsub("(.*).rds", "\\1", flist)
names(id.list) <- flist
results.list <- list()
did.not.converge <- character()
for (f in flist) {
    print(f)
    ff <- file.path(outdir, f)
    output <- readRDS(ff)
    if (!is.na(output$results)) {
        results <- as.data.table(output$results)
        results[, sample_id := id.list[f]]
        results.list[[f]] <- results
    } else {
        warning(paste0(f, ": Results not found, likely because the run did not converge."))
        did.not.converge <- c(did.not.converge, f)
    }
}

print("The following runs did not converge:")
print(did.not.converge)
write.table(did.not.converge, file = "no.convergence.txt",
            quote = FALSE, col.names = FALSE, row.names = FALSE)

results <- rbindlist(results.list)
saveRDS(results, file="processed-spec-data/all.results.rds")

