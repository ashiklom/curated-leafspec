source("common.R")

accp_path <- file.path("raw/ACCP/accp/")

# Load all chemistry data
traits_path <- file.path(accp_path, "leafchem")
traits_files <- list.files(traits_path, "chem.dat$")
traits_list <- list()
for (f in traits_files){
    print(f)
    fname <- file.path(traits_path, f)
    dat <- fread(fname, header=TRUE)
    traits_list[[f]] <- dat
}
traits_dat_raw <- rbindlist(traits_list, fill=TRUE)

# Fix names
colnames_dict <- c('sampleid' = 'SampleName',
                   'site_id' = 'Site',
                   'plot_id' = 'Plot',
                   'species' = 'species_code',
                   'cell' = 'leaf_cellulose_percent',
                   'lignin' = 'leaf_lignin_percent',
                   'carbon' = 'leaf_C_percent',
                   'hydrogen' = 'leaf_H_percent',
                   'nitrogen' = 'leaf_N_percent',
                   'water_th' = 'leaf_water_content',
                   'chloro_a' = 'leaf_chlorophyll_a',
                   'chloro_b' = 'leaf_chlorophyll_b')
setnames(traits_dat_raw, names(colnames_dict), colnames_dict)

# Fix values
traits_dat_raw <- traits_dat_raw[, lapply(.SD, replace.na)] %>%
    .[, SampleYear := colldate %>% as.character %>%
      as.Date(format = "%y%m%d") %>%
      strftime("%Y") %>%
      as.numeric()] %>%
  .[, Project := "ACCP"] %>%
  .[, FullName := paste(Project, SampleName, SampleYear,
                        sep = id_separator)]

species.info <- fread(file.path(traits_path, "LTER_species.dat"))
species.info[, author := NULL]
setkey(species.info, species_code)
setkey(traits_dat_raw, species_code)
traits_dat_sp <- species.info[traits_dat_raw] %>%
    setnames('latin_name', 'RawSpecies')

# Load all spectral data
spec_path <- file.path(accp_path, "leafspec")
spec_files <- list.files(spec_path, "sp.dat$")
refl_list <- list()
dry_list <- character()
PA_list <- character()
for (f in spec_files){
    dry <- "fresh"
    if (grepl("_d_sp.dat", f)) dry <- "dry"
    fname <- file.path(spec_path, f)
    rawdat <- fread(fname)
    wl <- rawdat[['wavelength']]
    dat <- rawdat %>% 
        select(-band, -wavelength) %>%
        as.matrix()
    ids <- colnames(dat)
    datlist <- dat %>%
        split(., rep(1:ncol(.), each = nrow(.))) %>%
        "names<-"(ids) %>%
        lapply(function(x) cbind("Wavelength" = wl, x)) %>%
        lapply(specobs)
    PA_list[ids] <- datlist %>%
        sapply(function(x) ifelse(any(x[,-1] > 0.9), "PA", NA))
    dry_list[ids] <- dry
    refl_list[ids] <- datlist
}

# Condense data down to only matching spectra-trait pairs
keep_names <- intersect(names(refl_list), traits_dat_sp[, SampleName])
traits_dat <- traits_dat_sp[SampleName %in% keep_names] %>%
    .[, Reflectance := refl_list[SampleName]] %>%
    .[, FreshDry := dry_list[SampleName]] %>%
    .[, SpecialSpec := PA_list[SampleName]] %>%
    subToCols()

saveRDS(traits_dat, file = "processed-spec-data/accp.rds")

