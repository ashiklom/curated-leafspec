#' Replace "-999" (and similar) with NA values
#'
#' @export
replace.na <- function(column, na.val = -999){
    if(is.numeric(column)){
        na.999 <- column <= na.val + 1
        column[na.999] <- NA
    }
    return(column)
}
