
#' Small function to check for values in data.table based on column.
#' Mainly used to make sure the sample_id is actually unique.
#' 
#' @export
check.unique <- function(dat, columns="sample_id"){
    dn <- dat[, .N, by=columns]
    dnu <- dn[N > 1]
    nnu <- nrow(dnu)
    if(nnu > 0) stop(sprintf("%d duplicates found", nnu))
}

