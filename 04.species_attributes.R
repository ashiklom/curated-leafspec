library(specprocess)
source('common.R')

attribute_path <- 'data/common/attributes.csv'


if ('species_attributes' %in% DBI::dbListTables(specdb$con)) {
    message('Species attributes table already exists. Dropping table...')
    DBI::dbSendQuery(specdb$con, 'DROP TABLE species_attributes')
} else {
    speciesatt <- read_csv(attribute_path) %>%
        mutate(myco_asso = recode(myco_asso, 
                                  `ECM?` = 'ECM',
                                  `AM?` = 'AM',
                                  `EITHER` = 'AM + ECM',
# ABM = Arbutoid, similar to ecto, so grouping them together
# See: http://www.davidmoore.org.uk/assets/mostly_mycology/diane_howarth/arbutoid.htm
                                  `ABM` = 'ECM',
                                  `ABM, ECM` = 'ECM'),
               shade_tolerance = tolower(shade_tolerance)) %>%
    select(-scientific_name, -family) %T>%
    db_insert_into(con = specdb$con, table = 'species_attributes', values = .)
}

#samples <- tbl(specdb, 'samples') %>%
    #select(samplecode, projectcode, speciescode) %>%
    #collect()
#sampatt <- left_join(samples, speciesatt)

#sampatt %>%
    #count(shade_tolerance, sort = TRUE)

#sampatt %>%
    #filter(is.na(ps_type)) %>%
    #count(growth_form)
