#' Retrieve spectra from NetCDF database
#'
#' @param spectra_id Vector of integer IDs of spectra to retrieve
#' @param wavelengths Vector of wavelengths for retrieved spectra
#' @inheritParams add_spectra
#' @export
get_spectra <- function(spectra_id, filename, wavelengths = seq(400, 2500)) {
  nid <- length(spectra_id)
  nwl <- length(wavelengths)
  nc <- ncdf4::nc_open(filename)
  nc_waves <- nc$dim$wavelength$vals
  wl_min <- min(nc_waves)
  start_wl <- min(wavelengths) - wl_min
  result <- matrix(NA_real_, nwl, nid)
  dimnames(result) <- list(wavelengths, spectra_id)
  for (i in seq_along(spectra_id)) {
    nc_start <- c(start_wl, spectra_id[i])
    nc_count <- c(nwl, 1)
    result[, i] <- ncdf4::ncvar_get(nc, "spectra", nc_start, nc_count)
  }
  result
}
