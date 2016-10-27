# Delete all specInfo from given project
qry <- "DELETE FROM specInfo
        WHERE SampleID IN (
            SELECT SampleID
            FROM samples
                INNER JOIN projects
                ON samples.ProjectID = projects.ProjectID
            WHERE ProjectCode LIKE '%lopex%')"
dbSendQuery(db$con, qry) 

# Add column to table
qry <- "ALTER TABLE projects
        ADD COLUMN ProjectReference TEXT"
dbSendQuery(db$con, qry) 

qry <- "ALTER TABLE projects
        ADD COLUMN ProjectReferenceDOI TEXT"
dbSendQuery(db$con, qry) 

# Search species
library(stringdist)
species %>%
    mutate(std = stringdist(ScientificName,
                            "Cornus alba 'Elegantissima'")) %>%
    arrange(std) %>%
    head()

species %>%
    slice(which(std == 10))
