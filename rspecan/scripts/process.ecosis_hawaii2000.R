library(rspecan)
sethere()

wave_rxp <- "^[[:digit:]]+$"
spectra_colname <- "Spectra"
data_name <- "ecosis_hawaii2000"
data_longname <- "Hawaii 2000 vegetation species spectra"
ecosis_file <- "raw_data/hawaii-2000-vegetation-species-spectra.csv"

dat_full <- read_csv(ecosis_file)

dat_sub <- dat_full %>%
  select(-matches(wave_rxp))

dat <- dat_sub %>%
  transmute(
    data_name = !!data_name,
    common_name = `Common Name`,
    description = `Description`,
    genus = case_when(
      !is.na(`Latin genus`) ~ `Latin genus`,
      !is.na(`Latin Genus`) ~ `Latin Genus`,
      TRUE ~ NA_character_
    ),
    species = case_when(
      !is.na(`Latin species`) ~ `Latin species`,
      !is.na(`Latin Species`) ~ `Latin Species`,
      TRUE ~ NA_character_
    ),
    latitude = Latitude,
    longitude = Longitude,
    collection_date = lubridate::mdy(`Sample Collection Date`),
    spectra_id = `Spectra`,
    spectra_type = "reflectance",
    target_type = `Surface Type`,
    USDA_code = `USDA Symbol`
  )

spectra <- dat2specmat(dat_full, spectra_colname, wave_rxp)
str(spectra)
stopifnot(ncol(spectra) == nrow(dat))

wl <- getwl(spectra)
if (FALSE) {
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
if (FALSE) {
  matplot(wl_kept, sp_good, type = "l")
}

store_path <- file.path(processed_dir, paste0(data_name, ".rds"))

datalist <- list(
  data_name = data_name,
  data_longname = data_longname,
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
