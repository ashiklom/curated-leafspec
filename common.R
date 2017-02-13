specdb <- dplyr::src_sqlite('leaf_spectra.db')
DBI::dbGetQuery(specdb$con, 'PRAGMA foreign_keys = on')

write_spectradata <- function(values) {
    db_merge_into(db = specdb, table = 'spectra_data', values = values, 
                  by = 'spectraid', id_colname = 'spectradataid',
                  backend = 'sqlite_import', return = FALSE)
}
