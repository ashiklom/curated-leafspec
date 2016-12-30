library(specprocess)
specdb <- src_postgres('leaf_spectra')
data_path <- 'data/lopex'
projectcode <- 'lopex'

# Set site and plot tables
sites <- data.table(
    code = 'lopex.ispra',
    description = 'Joint Research Center, Ispra, Italy'
)
merge_with_sql(sites, 'sites')

plots <- data.table(
    code = 'lopex.ispra',
    description = 'Joint Research Center, Ispra, Italy',
    sitecode = 'lopex.ispra',
    latitude = 45.803,
    longitude = 8.630
)
merge_with_sql(plots, 'plots')

# specinfo_check <- lopex.uniqspec %>%
#     mutate(Instrument = "Perkin Elmer Lambda 19 double-beam spectrophotometer (BaSO4 integrating sphere)",
#            Apparatus = "Integrating sphere", 
#            Calibration = "Spectralon ratio", 
#            Comments = "See http://teledetection.ipgp.jussieu.fr/opticleaf/lopex.htm#spectral for more info") %>%
#     mergeWithSQL(db, "specInfo", ., 'SpectraName')

#' Set paths for LOPEX data
PATH.chem <- file.path(data_path, "LDB_lopex1993.csv")
PATH.spec <- file.path(data_path, "spec")

#' Load main data.
#species.info <- fread(PATH.speciesinfo, header=TRUE)
species.rxp <- "([[:alpha:]]{3})[[:alpha:]]* x? ?([[:alpha:]]{3})[[:alpha:]]* *.*"
lopex.chem <- fread(PATH.chem, header=TRUE) %>%
    rename(datacode = `Latin Name`) %>%
    select(-`Plant Type`, -`English Name`) %>%
    mutate_if(is.numeric, na_if, y=-999) %>%
    mutate(projectcode = projectcode,
           year = 1993,
           sitecode = sites$code,
           plotcode = plots$code,
           datacode = na_if(datacode, '')) %>%
    .[, datacode := datacode[1], by = cumsum(!is.na(datacode))] %>%
    .[, samplename := sprintf("%s_Leaf%0.2d", gsub(species.rxp, "\\1-\\2", datacode), 1:.N)] %>%
    .[grepl('Vitis vinifera.*Sylvestris', datacode),
      samplename := gsub('Vit-vin_', 'Vit-vin-syl_', samplename)] %>%
    .[, samplecode := paste(projectcode, samplename, year, sep = '|')] %>%
    setkey(samplecode)

#' Read in reflectance and transmittance data
refl_list <- list()
trans_list <- list()
for (ID in lopex.chem[, unique(samplecode)]) {
    refl_files <- lopex.chem[ID, Refl_file]
    refl_files_full <- file.path(PATH.spec, refl_files)
    refl_list[[ID]] <- fread(refl_files_full) %>%
        setnames(c('wavelength', 'value')) %>%
        mutate(samplecode = ID,
               fname = refl_files,
               type = 'reflectance')
    trans_files <- lopex.chem[ID, Trans_file] 
    trans_files_full <- file.path(PATH.spec, trans_files)
    trans_list[[ID]] <- fread(trans_files_full) %>%
        setnames(c('wavelength', 'value')) %>%
        mutate(samplecode = ID,
               fname = refl_files,
               type = 'transmittance')
}
specdat <- rbind(rbindlist(refl_list), rbindlist(trans_list))

names_dict <- c("C_C" = "leaf_C_pct_mass",
                "C_H" = "leaf_H_pct_mass",
                "C_O" = "leaf_O_pct_mass",
                "C_N" = "leaf_N_pct_mass")

lopex.traits <- lopex.chem %>%
    rename_(.dots = setNames(names(names_dict), names_dict)) %>%
    .[, leaf_chla_area := ud.convert(C_a, 'ug cm-2', 'kg m-2')] %>%
    .[, leaf_chlb_area := ud.convert(C_b, 'ug cm-2', 'kg m-2')] %>%
    .[, leaf_chltot_area := ud.convert(C_ab, 'ug cm-2', 'kg m-2')] %>%
    .[, leaf_cartot_area := ud.convert(C_car, 'ug cm-2', 'kg m-2')] %>%
    .[, leaf_anth_area := ud.convert(C_anth, 'ug cm-2', 'kg m-2')] %>%
    .[, leaf_mass_per_area := ud.convert(LMA, 'g cm-2', 'kg m-2')] %>%
    .[, leaf_water_thickness := ud.convert(EWT, 'g cm-2', 'kg m-2')] %>%
    .[, leaf_CN_ratio_mass := leaf_C_pct_mass/leaf_N_pct_mass] %>%
    .[, leaf_protein_pct_mass := 0.5*(C_prot1 + C_prot2)] %>%
    .[, leaf_cellulose_pct_mass := 0.5*(C_cell1 + C_cell2)] %>%
    .[, leaf_lignin_pct_mass := 0.5*(C_lign1 + C_lign2)] %>%
    .[!(is.na(C_star1) | is.na(C_star2)),
        leaf_starch_pct_mass := 0.5*(C_star1 + C_star2)] %>%
    .[(is.na(C_star1) & !is.na(C_star2)),
        leaf_starch_pct_mass := C_star2] %>%
    .[(is.na(C_star2) & !is.na(C_star1)),
        leaf_starch_pct_mass := C_star2]

spec_samples <- specdat %>% distinct(samplecode)

chem_samples <- lopex.traits %>%
    distinct(samplecode, projectcode, year, sitecode, plotcode, samplename, datacode) %>%
    left_join(tbl(specdb, 'species_dict') %>% 
              select(-id, -comment) %>% 
              collect %>% 
              setDT) %>%
    select(-datacode)

samples <- full_join(spec_samples, chem_samples) %>% rename(code = samplecode)
merge_with_sql(samples, 'samples')

spectra_info <- specdat %>% distinct(samplecode, type)
merge_with_sql(spectra_info, 'spectra_info', 'samplecode')

spectra_data <- specdat %>%
    left_join(tbl(specdb, 'spectra_info') %>%
              select(samplecode, spectraid = id) %>%
              collect %>% 
              setDT)
merge_with_sql(spectra_data, 'spectra_data', 'spectraid')

traits <- lopex.traits %>%
    select(samplecode, starts_with('leaf_')) %>% 
    melt(id.vars = 'samplecode', variable.name = 'trait', na.rm = TRUE)

trait_info <- traits %>%
    distinct(trait) %>%
    .[grepl('_pct', trait), unit := '%'] %>%
    .[grepl('_area|_thickness', trait), unit := 'kg m-2'] %>%
    .[grepl('ratio', trait), unit := 'unitless']

merge_with_sql(trait_info, 'trait_info', 'trait')
merge_with_sql(traits, 'trait_data', 'samplecode')

#saveRDS(lopex.traits, file = "processed-spec-data/lopex.rds")
