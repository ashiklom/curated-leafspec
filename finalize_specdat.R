#!/usr/bin/Rscript
library(dtplyr)
library(data.table)
library(dplyr)

message("Loading all data...")
datfiles <- list.files("processed-spec-data", ".rds$", full.names = TRUE)
specdat <- list()
for (f in datfiles) specdat[[f]] <- readRDS(f)

message("Concatenating...")
specdat <- rbindlist(specdat, fill = TRUE)
no_refl <- sapply(specdat[["Reflectance"]], is.null)
specdat_cleaned <- specdat[!no_refl]

saveRDS(specdat_cleaned, "specdat.rds")

message("Merging with species...")
species <- readRDS("species.rds") %>% setDT()

setkey(species, Project, RawSpecies)
setkey(specdat, Project, RawSpecies)
alldat <- species[specdat] %>% select(-RawSpecies, -Database_ID)

saveRDS(alldat, "alldat.rds")

message("Subsetting traits...")
traitdat <- alldat %>% select(-Reflectance, -Transmittance)
saveRDS(traitdat, "traitdat.rds")

message("Done!")
