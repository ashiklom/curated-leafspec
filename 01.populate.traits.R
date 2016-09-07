source("dbFunctions.R")

db <- src_sqlite("specdb.sqlite")
table_name <- "traitInfo"
input_data <- read.csv("traitInfo.csv", stringsAsFactors = FALSE) %>% 
    tbl_df

insert <- mergeWithSQL(db, table_name, input_data, "Trait")

# Test
#traits <- db %>% tbl("traitInfo") %>% collect()
