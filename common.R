specdb <- dplyr::src_sqlite('leaf_spectra.db')
DBI::dbGetQuery(specdb$con, 'PRAGMA foreign_keys = on')

write_spectradata <- function(values) {
    db_merge_into(db = specdb, table = 'spectra_data', values = values, 
                  by = 'spectraid', backend = 'sqlite_import', return = FALSE)
}

write_project <- function(values) {
    db_merge_into(db = specdb, table = 'projects', values = values,
                  by = 'projectcode', id_colname = NULL, backend = 'insert',
                  return = TRUE)
}

write_sites <- function(values) {
    db_merge_into(db = specdb, table = 'sites', values = values,
                  by = c('projectcode', 'sitecode'), id_colname = NULL)
}

write_plots <- function(values) {
    db_merge_into(db = specdb, table = 'plots', values = values,
                  by = c('plotcode', 'sitecode'), id_colname = NULL)
}
