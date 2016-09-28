library(RSQLite)
library(dtplyr)
library(data.table)
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
    out_data <- select_(input_data, .dots = column_names)
    return(out_data)
}

mergeWithSQL <- function(db, table_name, input_data, searchkey = NULL,
                         return.table = TRUE) {
    # Get keys from SQL table
    new_input <- matchColumns(db, table_name, input_data)
    if (!is.null(searchkey)) {
        sql_keys <- tbl(db, table_name) %>% 
            select_(searchkey) %>%
            collect() %>%
            .[[searchkey]]
        new_input <- new_input %>%
            filter(!(.[[searchkey]] %in% sql_keys))
    }
    setDT(new_input)
    insert <- db_insert_into(db$con, table_name, new_input)
    stopifnot(insert)
    if (!return.table) return(insert)
    else {
        if (!is.null(searchkey)) {
            out_table <- tbl(db, table_name) %>%
                select_(1, searchkey) %>%
                collect() %>%
                right_join(input_data)
        } else {
            out_table <- tbl(db, table_name) %>%
                collect()
        }
        setDT(out_table)
        return(out_table)
    }
}


clearTables <- function(db, table_names) {
    for (tb in table_names) {
        qry <- sprintf("DELETE FROM %s", tb)
        dbSendQuery(db$con, qry)
    }
}
