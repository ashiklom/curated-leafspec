#' Load packages
suppressPackageStartupMessages({
    library(dtplyr)
    library(data.table)
    library(dplyr)
    library(RPostgreSQL)
    library(reshape2)
    library(specobs)
    library(googlesheets)
})

#' Load tables from Google Sheets


#' Small function to check for values in data.table based on column.
#' Mainly used to make sure the sample_id is actually unique.

check.unique <- function(dat, columns="sample_id"){
    dn <- dat[, .N, by=columns]
    dnu <- dn[N > 1]
    nnu <- nrow(dnu)
    if(nnu > 0) stop(sprintf("%d duplicates found", nnu))
}

#' Replace "-999" (and similar) with NA values
replace.na <- function(column, na.val = -999){
    if(is.numeric(column)){
        na.999 <- column <= na.val + 1
        column[na.999] <- NA
    }
    return(column)
}

id_separator <- "|"

metadata <- readLines("metadata.txt")
traits <- readLines("traits.txt")
spec_cols <- c("Reflectance", "Transmittance")
all_cols <- c(metadata, traits, spec_cols)

subToCols <- function(dat) {
    cnames_dat <- colnames(dat)
    cnames <- all_cols[all_cols %in% cnames_dat]
    out <- dat[, cnames, with = FALSE]
    return(out)
}

mat2list <- function(mat) {
    cnames <- colnames(mat)
    mlist <- split(mat, rep(1:ncol(mat), each = nrow(mat))) %>%
        "names<-"(cnames)
    return(mlist)
}

wlmat2list <- function(wlmat) {
    wl <- wlmat[,1]
    mat <- wlmat[,-1]
    out <- mat2list(mat) %>%
        lapply(function(x) cbind("Wavelength" = wl, x)) %>%
        lapply(specobs)
    return(out)
}

rds_name <- function(projectcode) {
    sprintf("processed-spec-data/%s.rds", projectcode)
}
