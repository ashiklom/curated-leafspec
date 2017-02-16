#!/usr/bin/Rscript
library(dtplyr)
library(data.table)
library(dplyr)

# code -- USDA/ThePlantsList code
# codetype -- USDA_Plants or ThePlantsList
# scientificname -- Full name, with variety, etc.
# genus
# species
# subspecies
# variety
# subvariety
# forma
# family
# authority
# tryid

usda_path <- '~/Projects/common-data/usdadb.sqlite3'
try_sqlite_path <- '~/Projects/try/try-sqlite/try.sqlite'
output_path <- 'data/common/usda_try.rds'

usda_full <- src_sqlite(usda_path) %>% tbl("usda")

usda_sub <- usda_full %>%
    select(code = Symbol,
           genus = Genus,
           species = Species,
           subspecies = Subspecies,
           variety = Variety,
           subvariety = Subvariety,
           forma = Forma,
           family = Family,
           authority = Genera_Binomial_Author,
           scientificname = Scientific_Name_x) %>%
    collect() %>%
    setDT() %>%
    mutate(codetype = 'USDA_Plants') %>%
    .[!is.na(genus) & !is.na(species) & species != '',
      AccSpeciesName := paste(genus, species, sep = ' ')] %>%
    .[is.na(AccSpeciesName) & !is.na(genus), 
      AccSpeciesName := paste(genus, 'sp')] %>%
    .[is.na(AccSpeciesName), 
      AccSpeciesName := gsub("(^[[:alpha:]]+ [[:alpha:]]+).*", "\\1", 
                             scientificname)]

try_species_full <- src_sqlite(try_sqlite_path) %>%
    tbl("orig_species") %>%
    select(-N) %>%
    collect() %>%
    setDT()

# First pass -- full merge
setkey(usda_sub, AccSpeciesName)
setkey(try_species_full, AccSpeciesName)
merge1 <- try_species_full[usda_sub]
nmatch <- merge1[!is.na(AccSpeciesID), .N]
ntot <- nrow(merge1)
fracmatch <- nmatch/ntot * 100
message('USDA/TRY matches: ', nmatch, '/', ntot, ' = ', round(fracmatch), '%')

output <- merge1 %>%
    select(-AccSpeciesName) %>%
    rename(tryspeciesid = AccSpeciesID)

saveRDS(output, file = output_path, compress = 'xz')
