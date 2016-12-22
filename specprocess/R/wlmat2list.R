#' Convert multi-column matrix where wavelength is first column to named list
#'
#' @export
wlmat2list <- function(wlmat) {
    wl <- wlmat[,1]
    mat <- wlmat[,-1]
    out <- mat2list(mat) %>%
        lapply(function(x) cbind("Wavelength" = wl, x)) %>%
        lapply(specobs)
    return(out)
}
