library(specprocess)
specdb <- src_sqlite('leaf_spectra.db')

#DBI::dbGetQuery(specdb$con, 'DELETE FROM species')

usda_try <- readRDS('data/common/usda_try.rds')
othersp <- fread('data/common/other_species.csv')
species <- rbind(usda_try, othersp, fill = TRUE) %>%
    as.tbl() %>%
    rename(speciescode = code)

species <- db_merge_into(db = specdb, table = 'species', values = species,
                     by = 'speciescode', id_colname = 'speciesid')

print(species)
