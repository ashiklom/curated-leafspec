library(dtplyr)
library(data.table)
library(dplyr)

usda <- fread("raw/usda_plants.csv") %>%
    setnames(c("Symbol", 
               "SynonymSymbol", 
               "ScientificName",
               "CommonName",
               "Family"))

species %>%
    filter(grepl("Acer negundo", ScientificName, ignore.case = TRUE),
           grepl("vari", ScientificName, ignore.case = TRUE))



library(httr)

# Test query
usda_url <- "https://plantsdb.xyz"
qtest <- list(data = list(Symbol = "ABBA"))

test <- GET(usda_url, path = "search", 
            query = qtest)
result <- content(test)
