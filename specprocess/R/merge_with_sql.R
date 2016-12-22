#' Merge data table into PostgreSQL database
#'
#' @export
merge_with_sql <- function(input, tablename, 
                           dbname = 'leaf_spectra',
                           by = NULL) {
    db <- src_postgres('leaf_spectra')
    input2 <- copy_to(db, input,
                      name = 'input', 
                      temporary = TRUE)
    src_table <- tbl(db, tablename)
    columns <- colnames(src_table)
    newcolumns <- colnames(input)
    newcolumns <- newcolumns[newcolumns %in% columns]
    newinput <- anti_join(input2, src_table, by = by)
    newinput <- compute(newinput, name = 'temporary')
    drp <- dropifhas(db, 'input')
    n_added <- collect(count(newinput))$n
    if (n_added > 0) {
        insert <- sql(paste('INSERT INTO',
                            tablename,
                            escape(ident(newcolumns), 
                                   paren = TRUE),
                            'SELECT', 
                            escape(ident(newcolumns)),
                                   #paren = TRUE),
                            'from temporary'))
        send <- dbSendQuery(db$con, insert)
    }
    drp <- dropifhas(db, 'temporary')
    return(n_added)
}

dropifhas <- function(db, tablename) {
    if (isTRUE(db_has_table(db$con, tablename))) {
        db_drop_table(db$con, tablename)
    }
}
