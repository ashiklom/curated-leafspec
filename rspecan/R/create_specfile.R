#' Create the spectra NetCDF file
#'
#' @param filename Name of spectra database
#' @export
create_specfile <- function(filename) {
  dim_wl <- ncdf4::ncdim_def(
    "wavelength",
    units = "nm",
    vals = seq(300, 2500),
    unlim = TRUE
  )
  dim_id <- ncdf4::ncdim_def(
    "spectra_id",
    units = "",
    vals = 0L,
    unlim = TRUE
  )
  var_spec <- ncdf4::ncvar_def(
    "spectra",
    units = "0-1",
    dim = list(dim_wl, dim_id),
    missval = NA,
    prec = "double"
  )
  nc <- ncdf4::nc_create(filename, vars = var_spec)
  on.exit(ncdf4::nc_close(nc))
  invisible(filename)
}
