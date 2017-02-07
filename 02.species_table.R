library(specprocess)
specdb <- src_postgres('leaf_spectra')

usda_try <- readRDS('data/common/usda_try.rds')
othersp <- fread('data/common/other_species.csv')
species <- rbind(usda_try, othersp, fill = TRUE) %>%
    as.tbl() %>%
    rename(speciescode = code)

species <- db_merge_into(db = specdb, table = 'species', values = species,
                     by = 'speciescode', id_colname = 'speciesid', 
                     backend = 'psql_copy')

print(species)
