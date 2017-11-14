#' Specobs class
#' 
#' @export
specobs <- function(x) {
    if (!(inherits(x, "matrix"))) {
        stop("Object must inherit from matrix")
    }
    if (!("Wavelength" %in% colnames(x))) {
        stop("Object must have a 'Wavelength' column")
    }
    if (!is.numeric(x)) {
        stop("Object must be numeric")
    }
    wl_col <- which(colnames(x) == "Wavelength")
    other_cols <- which(colnames(x) != "Wavelength")
    x <- x[, c(wl_col, other_cols)]
    out <- structure(x, class = append("specobs", "matrix"))
    return(out)
}

#' @export
print.specobs <- function(x, ...) {
    print(head(x))
    cat("...", "\n")
    print(tail(x))
}


#' @export
"[[.specobs" <- function(x, rowvec, colvec = NA) {
    ind <- which(x[,"Wavelength"] %in% rowvec)
    out <- x[ind,]
    if (!is.na(colvec)) {
        out <- out[, colvec]
    }
    return(out)
}

#' @export
plot.specobs <- function(obj, ...) {
    x <- obj[, 1]
    y <- obj[, -1]
    plot(x, y, type='l', xlab = "Wavelength", ...)
}
