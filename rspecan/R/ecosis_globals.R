#' @export
ecosis_colnames <- c(
  "Common Name" = "common_name",
  "Description" = "description",
  "Latin Genus" = "genus",
  "Latin Species" = "species",
  "USDA Symbol" = "USDA_code",
  "Spectra" = "spectra_id",
  "Latitude" = "latitude",
  "Longitude" = "longitude",
  "Sample Collection Date" = "collection_date"
)

#' @export
processed_dir <- "processed_data"
dir.create(processed_dir, showWarnings = FALSE)

#' @export
submit_dir <- "submit_scripts"
dir.create(submit_dir, showWarnings = FALSE)

#' @export
log_dir <- ".logs"
dir.create(log_dir, showWarnings = FALSE)

#' @export
ecosis_dir <- "raw_data/ecosis/data"

#' @export
prospect_wl <- seq(400, 2500)

