rm(list = ls())
library(rspecan)
sethere()

data_name <- "ecosis_crop_N_LMA"
data_longname <- "Fresh leaf spectra to estimate leaf N concentration and leaf mass per area in two vegetable crops"
ecosis_file <- "raw_data/fresh-leaf-spectra-to-estimate-leaf-n-concentration-and-leaf-mass-per-area-in-two-vegetable-crops.csv"

dat_full <- read_csv(ecosis_file)

wave_rxp <- "^[[:digit:]]+$"
alt_rxp <- "^B([[:digit:]]+$)"
spectra <- dat_full %>%
  rename_at(vars(matches(alt_rxp)), ~gsub(alt_rxp, "\\1", .)) %>%
  dat2specmat("Spectra", wave_rxp)
stopifnot(!any(duplicated(colnames(spectra))))
str(spectra)
wl <- getwl(spectra)
if (FALSE) {
  matplot(wl, spectra, pch = 20)
}

stop("Stop here...")

wl_prospect <- wl >= 400 & wl <= 2500
wl_bad <- wl > 2300
wl_keep <- wl_prospect & !wl_bad

data_wl_inds <- which(wl_keep)
wl_kept <- wl[wl_keep]
prospect_wl_inds <- which(prospect_wl %in% wl_kept)

stopifnot(length(data_wl_inds) == length(prospect_wl_inds))

sp_good <- spectra[data_wl_inds, ]
if (FALSE) {
  matplot(wl_kept, sp_good, type = "l")
}

dat_sub <- dat_full %>%
  select(-matches(wave_rxp))

dat <- dat_sub %>%
  transmute(
    data_name = !!data_name,
    spectra_id = Spectra,
    spectra_type = "reflectance",
    collection_date = lubridate::mdy(`Sample Collection Date`),
    latitude = `Latitude`,
    longitude = `Longitude`,
    USDA_code = `USDA Symbol`,
    genus = `Latin Genus`,
    species = `Latin Species`,
    common_name = `Common Name`,
    elevation = `Elevation (m)`,
    elevation_unit = "m",
    description = `Full Description`,
    ash = `% Ash`,
    ash_unit = "%",
    fat = `% Fat`,
    fat_unit = "%",
    neutral_detergent_fiber = `% Neutral detergent fiber`,
    neutral_detergent_fiber_unit = "%",
    NSC = `% Non-structural carbohydrate`,
    NSC_unit = "%",
    protein = `% Protein`,
    protein_unit = "%",
    LWC = `Relative water content` * 100,
    LWC_unit = "%"
  )
stopifnot(!any(duplicated(dat$spectra_id)))

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
