library(rspecan)
sethere()

wave_rxp <- "^[[:digit:]]+$"
spectra_colname <- "Spectra"
data_name <- "ecosis_01"

ecosis_file <- dir(ecosis_dir, "ecosis1.csv", full.names = TRUE)
dat_full <- read_csv(ecosis_file)

dat <- dat_full %>%
  select(-matches(wave_rxp)) %>%
  col_process() %>%
  mutate(
    data_name = data_name,
    spectra_type = "reflectance"
  )

spectra <- dat2specmat(dat_full, spectra_colname, wave_rxp)
stopifnot(ncol(spectra) == nrow(dat))

wl <- getwl(spectra)
if (interactive()) {
  matplot(wl, spectra, type = "l")
}

wl_prospect <- wl >= 400 & wl <= 2500
wl_bad <- (wl > 1810 & wl < 1940) | (wl > 2450)
wl_keep <- wl_prospect & !wl_bad

data_wl_inds <- which(wl_keep)
wl_kept <- wl[wl_keep]
prospect_wl_inds <- which(prospect_wl %in% wl_kept)

stopifnot(length(data_wl_inds) == length(prospect_wl_inds))

sp_good <- spectra[data_wl_inds, ]
if (interactive()) {
  matplot(wl_kept, sp_good, type = "l")
}

store_path <- file.path(processed_dir, paste0(data_name, ".rds"))

datalist <- list(
  data_name = "ecosis_01",
  data_filename = ecosis_file,
  self_filename = store_path,
  metadata = dat,
  spectra = spectra,
  data_wl_inds = data_wl_inds,
  prospect_wl_inds = prospect_wl_inds
)

submit_df <- dat %>%
  filter(spectra_type == "reflectance") %>%
  select(data_name, spectra_id)

saveRDS(datalist, store_path)
write_submit_file(submit_df, data_name)
