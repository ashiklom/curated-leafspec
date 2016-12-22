library(specprocess)

#raw2code <- readRDS('species.rds') %>% setDT()
#write.table(raw2code, file = 'data/common/species_dict.csv', 
            #quote = TRUE, row.names = FALSE, col.names = TRUE,
            #sep = ',')
raw2code <- fread('data/common/species_dict.csv')

dict <- raw2code %>% 
    select(datacode = RawSpecies,
           projectcode = Project,
           speciescode = SpeciesCode) %>%
    mutate(projectcode = tolower(projectcode))

specdb <- src_postgres('leaf_spectra')

mrg <- merge_with_sql(dict, 'species_dict')
