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
species <- readRDS("species.rds") %>% 
    select(Project, RawSpecies, SpeciesCode) %>%
    setDT()

species_final <- readRDS('species_final.rds')

alldat <- specdat %>% 
    left_join(species) %>%
    left_join(species_final)

saveRDS(alldat, "alldat.rds")

message("Subsetting traits...")
traitdat <- alldat %>% select(-Reflectance, -Transmittance)
saveRDS(traitdat, "traitdat.rds")

message("Done!")
