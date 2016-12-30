#' Merge data table into PostgreSQL database
#'
#' @export
#' @examples
#' library(specprocess)
#' tablename <- 'projects'
#' input <- data.table(
#'    code = 'test',
#'    description = 'example')
#' key <- 'code'
#' dbname <- 'leaf_spectra'
#' merge_with_sql(input, tablename, key = 'code')
merge_with_sql <- function(input, tablename, key = 'code',
                           dbname = 'leaf_spectra', ...) {
    db <- src_postgres('leaf_spectra')
    sql_table <- tbl(db, tablename)
    sql_cols <- tbl_vars(sql_table)
    input_cols <- colnames(input)
    keep_cols <- input_cols[input_cols %in% sql_cols]
    input_sel <- select_(input, .dots=keep_cols)
    sql_nrow <- collect(count(sql_table))[['n']]
    if (sql_nrow > 0) {
        sql_keys <- collect(distinct_(sql_table, key))[[key]]
        input_sub <- input_sel[!input_sel[[key]] %in% sql_keys,]
    } else {
        input_sub <- input_sel
    }
    n_added <- nrow(input_sub)
    if (n_added > 0) {
        dt2sql_write(db, tablename, input_sub, ...) 
    }
    message('Added ', n_added, ' rows to table ', tablename)
    return(n_added)
}

