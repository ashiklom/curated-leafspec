library(RSQLite)
library(dplyr)

matchColumns <- function(db, table_name, input_data) {
    # Get table information for matching
    table_sample <- tbl(db, table_name) %>% head(1) %>% collect
    columns <- sapply(table_sample, class)
    column_names <- names(columns)
    missing_columns <- setdiff(column_names, colnames(input_data))
    if (length(missing_columns) > 0) {
        for (cl in missing_columns) {
            input_data[[cl]] <- as(NA, columns[cl])
        }
    }
    input_data <- input_data[, column_names]
    return(input_data)
}

mergeWithSQL <- function(db, table_name, input_data, key) {
    # Get keys from SQL table
    sql_keys <- tbl(db, table_name) %>% 
        select_(key) %>%
        collect() %>%
        .[[key]]
    new_input <- matchColumns(db, table_name, input_data) %>%
        filter(!(.[[key]] %in% sql_keys))
    insert <- db_insert_into(db$con, table_name, new_input)
    return(insert)
}

## Variables for testing
#db <- src_sqlite("specdb.sqlite")
#table_name <- "traitInfo"
#input_data <- read.csv("traitInfo.csv", stringsAsFactors = FALSE) %>% 
    #tbl_df
#key <- "Trait"
#matchdf <- matchColumns(db, table_name, input_data)

