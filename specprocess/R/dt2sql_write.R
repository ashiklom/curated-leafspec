#' Write data.table to SQL
#' 
#' @export
#' @examples
#' library(specprocess)
#' tablename <- 'projects'
#' input <- data.table(
#'    id = 1e+08,
#'    code = 'test3',
#'    description = 'example')
#' id_colname <- 'id'
#' add_id <- TRUE
#' db <- src_postgres('leaf_spectra')
#' con <- db$con
#' dt2sql_write(db, tablename, input, key = 'code', add_id = FALSE)
dt2sql_write <- function(db, tablename, input,
                         id_colname = 'id', add_id = TRUE, 
                         ...) {
    stopifnot(is.data.table(input))
    sql_cols <- dbListFields(db$con, tablename)
    sql_cols <- sql_cols[!grepl('\\.\\.pg\\.dropped', sql_cols)]

    # Add ID column if missing
    if (isTRUE(add_id)) {
        last_id <- dbGetQuery(db$con, paste('SELECT', id_colname, 
                                         'FROM', tablename,
                                         'ORDER BY', id_colname,
                                         'DESC LIMIT 1'))
        if (nrow(last_id) == 0) {
            i <- 0
        } else {
            i <- unlist(last_id, use.names = FALSE)
        }
        input <- input[, (id_colname) := as.integer(i+(1:.N))]
    }
    tmp_dir <- '/tmp'
    tmp_fname <- tempfile(pattern = 'dt2sql_', tmpdir=tmp_dir, fileext = '.csv')
    oldscipen <- options(scipen=500)
    fwrite(input, tmp_fname, quote = TRUE)
    readLines(tmp_fname)
    cp <- system2('psql', c('-d', db$info$dbname, '-c', 
                            paste0('"', '\\copy ', tablename,
                                   '(', paste(colnames(input), collapse = ','), ')',
                                   ' FROM ', shQuote(tmp_fname), 
                                   ' WITH CSV HEADER', '"')), stdout = TRUE)
    file.remove(tmp_fname)

    # Update table serial sequence counter
    r <- dbSendQuery(db$con, paste0("SELECT pg_catalog.setval(pg_get_serial_sequence",
                                 "('", tablename,"', '", id_colname, "'),",
                                 "(SELECT MAX(", id_colname, ") FROM ",tablename,")+1);"))
    dbClearResult(r)
    return(cp)
}

