library(dplyr)
library(stringr)

options(stringsAsFactors = FALSE)
species_dict <- as.tbl(read.csv(file = 'species_dict.csv'))

othersp <- species_dict %>%
    filter(Database_ID == 'ThePlantList') %>%
    select(code = SpeciesCode,
           codetype = Database_ID,
           scientificname = ScientificName)

rxp <- paste0('^([[:alpha:]]+)\\b +',   # Genus (1st word)
              '([[:alpha:]]+)\\b +',    # Species (2nd word)
              '(?:(?:var\\. )([[:alpha:]]+) +)?',     # Variety (var. ...)
              '(?:(?:subsp\\. )([[:alpha:]]+) +)?',   # Subspecies (subsp. ...)
              '(.*?)$')
match_mat <- str_match(othersp$scientificname, rxp)
head(match_mat)
unique(match_mat[,4])
unique(match_mat[,5])

othersp <- othersp %>%
    mutate(genus = match_mat[,2],
           species = match_mat[,3],
           variety = match_mat[,4],
           subspecies = match_mat[,5])

write.table(othersp, file = 'other_species.csv', sep = ',',
            row.names = FALSE, col.names = TRUE)
