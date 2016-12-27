#' Add data to SQL database.
#'
#' @export
add_to_sql <- function(input, tablename,
                       dbname = 'leaf_spectra', ...) {
    db <- src_postgres(dbname)
    caroline::dbWriteTable2(db$con, tablename, input, append = TRUE, add.id = FALSE, ...)
    }
