source("dbFunctions.R")
source("common.R")

# Connect to remote BETY
#username <- "ashiklom"
##username <- "bety"
#password <- readline(prompt = "Enter password:")
##password <- "bety"
#dbname <- "bety"
#host <- "psql-pecan.bu.edu"

#betydb <- dbConnect(dbDriver("PostgreSQL"),
                    #dbname = dbname, 
                    #user = username,
                    #password = password,
                    #host = host)


bety_species <- fread("raw/bety.species.csv")

cp <- copy_to(db, bety_species, "species", temporary = FALSE,
              unique_indexes = list("id"))
