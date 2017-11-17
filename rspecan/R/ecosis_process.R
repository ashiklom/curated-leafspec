#' @export
col_rename <- function(oldnames, newnames = ecosis_colnames) {
  newnames[oldnames]
}

#' @export
make_json <- function(dat, newnames = ecosis_colnames) {
  dat_sub <- dat %>%
    select(-one_of(newnames))
  meta_list <- apply(dat_sub, 1, as.list)
  meta_json <- vapply(meta_list, jsonlite::toJSON, character(1), auto_unbox = TRUE)
  meta_json
}

#' @export
col_process <- function(dat, ecosis_colnames = ecosis_colnames) {
  nms <- ecosis_colnames[names(ecosis_colnames) %in% colnames(dat)]
  dat %>%
    rename_at(
      names(nms),
      col_rename,
      newnames = nms
    ) %>%
    mutate(metadata = make_json(., nms)) %>%
    select(unname(nms), metadata)
}

#' @export
dat2specmat <- function(dat_full, spectra_colname,
                        wave_rxp = "^[[:digit:]]+$") {
  spectra_id <- dat_full[[spectra_colname]]
  stopifnot(!any(duplicated(spectra_id)))
  dat_full %>%
    select(matches(wave_rxp)) %>%
    as.matrix() %>%
    t() %>%
    "colnames<-"(spectra_id)
}

#' @export
getwl <- function(spectra) as.numeric(rownames(spectra))

#' @importFrom here here
#' @export
here::here

#' @export
sethere <- function() setwd(here())
