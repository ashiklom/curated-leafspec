source("common.R")
library(magrittr)

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
colnames_dict <- c('sampleid' = 'sample_name',
                   'site_id' = 'site',
                   'plot_id' = 'plot',
                   'species' = 'species_code',
                   'cell' = 'leaf_cellulose_percent',
                   'lignin' = 'leaf_lignin_percent',
                   'carbon' = 'leafC',
                   'hydrogen' = 'leafH',
                   'nitrogen' = 'leafN',
                   'water_th' = 'leaf_water_content',
                   'chloro_a' = 'leaf_chlorophyll_a',
                   'chloro_b' = 'leaf_chlorophyll_b')
setnames(traits_dat_raw, names(colnames_dict), colnames_dict)

# Fix values
traits_dat_raw <- traits_dat_raw[, lapply(.SD, replace.na)]
traits_dat_raw[, sample_year := colldate %>% as.character %>%
                                as.Date(format = "%y%m%d") %>%
                                strftime("%Y") %>%
                                as.numeric()]
traits_dat_raw[, project := "ACCP"]
traits_dat_raw[, sample_id := paste(project, sample_name, sample_year,
                                    sep = id_separator)]

species.info <- fread(file.path(traits_path, "LTER_species.dat"))
species.info[, author := NULL]
setkey(species.info, species_code)
setkey(traits_dat_raw, species_code)
traits_dat_sp <- species.info[traits_dat_raw]
names_dict <- c('latin_name' = 'species_scientific',
                'common_name' = 'species_common')
setnames(traits_dat_sp, names(names_dict), names_dict)
source("fix.species.R")
traits_dat_sp <- fix.species(traits_dat_sp)

# Remove values not in common
matchcols <- colnames(traits_dat_sp)[colnames(traits_dat_sp) %in% 
                                       columns.data]
traits_dat <- traits_dat_sp[,matchcols,with=F]

# Load all spectral data
spec_path <- file.path(accp_path, "leafspec")
# NOTE: Only fresh ("f") spectra are loaded
spec_files <- list.files(spec_path, "sp.dat$")
refl_list <- list()
dry_list <- character()
for (f in spec_files){
    dry <- "fresh"
    if (grepl("_d_sp.dat", f)) dry <- "dry"
    fname <- file.path(spec_path, f)
    dat <- fread(fname) %>% as.matrix() %>% t()
    colnames(dat) <- dat["wavelength",]
    ids <- rownames(dat)[-c(1:2)]
    dry_list[ids] <- dry
    refl_list[[f]] <- dat[-(1:2),]
}
refl_mat_full <- do.call(rbind, refl_list)

# Condense data down to only matching spectra-trait pairs
refl_names <- rownames(refl_mat_full)
refl_mat <- refl_mat_full[refl_names %in% traits_dat[, sample_name], ]
traits_dat <- traits_dat[sample_name %in% rownames(refl_mat),]

# Add dry/fresh designation
dry_list_sub <- dry_list[names(dry_list) %in% traits_dat[,sample_name]]
setkey(traits_dat, sample_name)
traits_dat[names(dry_list_sub), fresh_dry := dry_list_sub]

accp_list <- list(traits = traits_dat, reflectance = refl_mat)
saveRDS(accp_list, file = "processed-spec-data/accp.rds")

