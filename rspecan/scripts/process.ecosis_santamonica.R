rm(list = ls())
library(rspecan)
sethere()

data_name <- "ecosis_santamonica"
data_longname <- "Santa Monica Mountains vegetation species spectra"
ecosis_file <- "raw_data/santa-monica-mountains-vegetation-species-spectra.csv"

dat_full <- read_csv(ecosis_file)

wave_rxp <- "^[[:digit:]]+$"
dat_sub <- dat_full %>%
  select(-matches(wave_rxp))

dat <- dat_sub %>%
  transmute(
    data_name = !!data_name,
    spectra_id = Spectra,
    spectra_type = "reflectance",
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
    USDA_code = `USDA Symbol`,
    collection_date = lubridate::mdy(`Sample Collection Date`),
    target_type = `Surface Type`
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
  matplot(wl, spectra, pch = 20, xlim = c(2300, 2400))
}

wl_prospect <- wl >= 400 & wl <= 2500
wl_bad <- wl > 2320
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
