#' Add spectra matrix to NetCDF database
#'
#' @param mat Matrix of spectra. Wavelengths must be rows.
#' @param wavelengths Vector of wavelengths for input spectra.
#' @inheritParams create_specfile
#' @export
add_spectra <- function(mat, filename, wavelengths = seq(400, 2500), start_id = NULL) {
  if (!is.matrix(mat)) {
    warning("Converting mat to matrix")
    mat <- as.matrix(mat)
  }
  nwl <- length(wavelengths)
  nid <- ncol(mat)
  nr <- nrow(mat)
  if (nr != nwl) {
    stop("Number of rows in matrix (", nr,
         "does not equal number of wavelengths (", nwl, ").")
  }
  if (!all(diff(wavelengths) == 1) || !is.integer(wavelengths)) {
    stop("Wavelengths must be contiguous integer values")
  }

  ## Determine input
  nc <- ncdf4::nc_open(filename, write = TRUE)
  on.exit(ncdf4::nc_close(nc))
  nc_waves <- nc$dim$wavelength$vals
  wl_min <- min(nc_waves)
  wl_inds <- wavelengths - wl_min
  ids <- ncdf4::ncvar_get(nc, "spectra_id")
  idmax <- max(ids)
  if (is.null(start_id)) {
    start_id <- idmax + 1
  }
  count_id <- ncol(mat)
  end_id <- start_id + count_id - 1
  add_ids_all <- seq(start_id, end_id)
  add_ids <- add_ids_all[!add_ids_all %in% ids]
  ncdf4::ncvar_put(nc, "spectra_id", vals = add_ids,
                   start = start_id, count = length(add_ids))

  # TODO: Extend wavelengths if absent

  nc_start <- c(wl_inds[1], start_id)
  nc_count <- c(nwl, count_id)

  ## Write to NetCDF file
  ncdf4::ncvar_put(nc, "spectra", mat, start = nc_start, count = nc_count)

  ## Return IDs written to netCDF file
  add_ids_all
}
