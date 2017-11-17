library(rspecan)
sethere()

data_name <- "ecosis_californiatraits"
data_longname <- "Fresh Leaf Spectra to Estimate Leaf Traits for California Ecosystems"
ecosis_file <- "raw_data/fresh-leaf-spectra-to-estimate-leaf-traits-for-california-ecosystems.csv"

dat_full <- read_csv(ecosis_file)

wave_rxp <- "^[[:digit:]]+$"
dat_sub <- dat_full %>%
  select(-matches(wave_rxp))

dat <- dat_sub %>%
  transmute(
    # metadata
    data_name = !!data_name,
    sample_id = `sample name`,
    spectra_id = spectra,
    spectra_type = recode(measurement, `REFL` = "reflectance"),
    replicate = `Replicate`,
    collection_date = 41365 + lubridate::as_date("1900-01-01"),
    latitude = Latitude,
    longitude = Longitude,
    instrument = `Instrument Model`,
    genus = `Latin Genus`,
    species = `Latin Species`,
    USDA_code = `species`,
    # traits
    cellulose = Cellulose,
    cellulose_unit = "%",
    LMA = `Leaf mass per area`,
    LMA_unit = "g m-2",
    Nmass = `Leaf nitrogen content per leaf dry mass`,
    Nmass_unit = "%",
    LWC_pct = `Leaf relative water content`,
    lignin = `Lignin`,
    lignin_unit = "%",
    target_type = `Target Type`,
    leaf_age = `age`
  )


spectra <- dat2specmat(dat_full, "spectra", wave_rxp)
str(spectra)
stopifnot(
  ncol(spectra) == nrow(dat),
  !any(duplicated(colnames(spectra))),
  !any(duplicated(dat$spectra_id))
)

wl <- getwl(spectra)
if (FALSE) {
  matplot(wl, spectra, type = "l")
}

wl_prospect <- wl >= 400 & wl <= 2500
wl_bad <- FALSE
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
