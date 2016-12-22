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

id_separator <- "|"

metadata <- readLines("metadata.txt")
traits <- readLines("traits.txt")
spec_cols <- c("Reflectance", "Transmittance")
all_cols <- c(metadata, traits, spec_cols)

