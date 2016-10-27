#' Load packages
suppressPackageStartupMessages({
    source("dbFunctions.R")
    library(reshape2)
    library(googlesheets)
})

db <- src_sqlite("specdb.sqlite")

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

#' Define common variables and their units:
trait_names <- tbl(db, "traitInfo") %>%
    distinct(TraitName) %>%
    collect() %>%
    .[[1]]

columns_samples <- tbl(db, 'samples') %>% tbl_vars()
columns_traits <- tbl(db, 'traits') %>% tbl_vars()
columns_specInfo <- tbl(db, 'specInfo') %>% tbl_vars()
columns_spectra <- tbl(db, 'spectra') %>% tbl_vars()
columns_sites <- tbl(db, 'sites') %>% tbl_vars()
columns_plots <- tbl(db, 'plots') %>% tbl_vars()

#' Other functions
getProjectID <- function(project_code) {
    tbl(db, "projects") %>%
        filter(ProjectCode == project_code) %>%
        select(ProjectID) %>%
        .[[1]]
}

