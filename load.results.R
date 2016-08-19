library(PEcAnRTM)
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
        output[, sample_id := id.list[f]]
        results.list[[f]] <- output
    } else {
        warning(paste0(f, ": Results not found, likely because the run did not converge."))
        did.not.converge <- c(did.not.converge, f)
    }
}

print("The following runs did not converge:")
print(did.not.converge)

results <- rbindlist(results.list)
saveRDS(results, file="processed-spec-data/all.results.rds")

