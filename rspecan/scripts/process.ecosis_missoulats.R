rm(list = ls())
library(rspecan)
sethere()

data_name <- "ecosis_missoulats"
data_longname <- "Missoula Montana lodgepole pine & big sagebrush time series"
ecosis_file <- "raw_data/missoula-montana-lodgepole-pine---big-sagebrush-time-series.csv"
dat_full <- read_csv(ecosis_file)

wave_rxp <- "^[[:digit:]]+$"
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

spectra <- dat2specmat(dat_full, "Spectra", wave_rxp)
str(spectra)
stopifnot(
  ncol(spectra) == nrow(dat),
  !any(duplicated(colnames(spectra)))
)

wl <- getwl(spectra)
if (FALSE) {
  matplot(wl, spectra, type = "l", xlim = c(2000, 2500), ylim = c(0, 1))
}

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
