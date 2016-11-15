library(PEcAnRTM)
library(data.table)
outdir <- "raw_output"
flist <- list.files(outdir)
nf <- length(flist)
id.list <- gsub("(.*).rds", "\\1", flist)
names(id.list) <- flist
results.list <- list()
did.not.converge <- character()
for (i in seq_along(flist)) {
    f <- flist[i]
    print(paste(i, "of", length(flist),"----", f))
    ff <- file.path(outdir, f)
    exist <- TRUE
    output <- try(readRDS(ff))
    if ("try-error" %in% class(output)) exist <- FALSE
    if (exist) {
        if (is.null(output$results)) exist <- FALSE
    }
    if (exist) {
        if (is.na(output$results)) exist <- FALSE
    }
    if (exist) {
        results <- as.data.table(output$results)
        results[, FullName := id.list[f]]
        results.list[[f]] <- results
    } else {
        #warning(paste0(f, ": Results not found, likely because the run did not converge."))
        did.not.converge <- c(did.not.converge, f)
    }

}

print("The following runs had no results, presumably because they did not converge:")
print(did.not.converge)
write.table(did.not.converge, file = "no.convergence.txt",
            quote = FALSE, col.names = FALSE, row.names = FALSE)

results <- rbindlist(results.list)
saveRDS(results, file="processed-spec-data/all.results.rds")

