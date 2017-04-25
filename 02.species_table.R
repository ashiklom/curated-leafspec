library(specprocess)
specdb <- src_sqlite('leaf_spectra.db')

#DBI::dbGetQuery(specdb$con, 'DELETE FROM species')
DBI::dbGetQuery(specdb$con, 'DELETE FROM instruments')

usda_try <- readRDS('data/common/usda_try.rds')
othersp <- fread('data/common/other_species.csv')
species <- rbind(usda_try, othersp, fill = TRUE) %>%
    as.tbl() %>%
    rename(speciescode = code)

species <- db_merge_into(db = specdb, table = 'species', values = species,
                     by = 'speciescode', id_colname = NULL)

instruments <- read_csv('data/common/instruments.csv') %>% 
    db_merge_into(db = specdb, table = 'instruments', values = ., by = 'instrumentcode')

conditions <- read.csv('data/common/sample_condition_info.csv', header = TRUE, 
                       stringsAsFactors = FALSE) %>% 
    db_merge_into(db = specdb, table = 'sample_condition_info', values = .,
                  by = 'condition')

print(species)
