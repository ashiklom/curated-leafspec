library(specprocess)
specdb <- src_postgres('leaf_spectra')

#raw2code <- readRDS('species.rds') %>% setDT()
#write.table(raw2code, file = 'data/common/species_dict.csv', 
            #quote = TRUE, row.names = FALSE, col.names = TRUE,
            #sep = ',')
raw2code <- read_csv('data/common/species_dict.csv')

dict <- raw2code %>% 
    select(speciesdatacode = RawSpecies,
           projectcode = Project,
           speciescode = SpeciesCode) %>%
    mutate(projectcode = tolower(projectcode))

dict <- db_merge_into(db = specdb, table = 'species_dict',
                      values = dict, by = c('speciescode'),
                      id_colname = 'speciesdictid')
print(dict)
