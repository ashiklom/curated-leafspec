#' Select only correct columns in the right order
#'
#' @export
subToCols <- function(dat) {
    cnames_dat <- colnames(dat)
    cnames <- all_cols[all_cols %in% cnames_dat]
    out <- dat[, cnames, with = FALSE]
    return(out)
}
