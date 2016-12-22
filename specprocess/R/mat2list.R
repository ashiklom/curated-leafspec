#' Convert multi-column matrix to named list of columns
#'
#' @export
mat2list <- function(mat) {
    cnames <- colnames(mat)
    mlist <- split(mat, rep(1:ncol(mat), each = nrow(mat))) %>%
        "names<-"(cnames)
    return(mlist)
}
