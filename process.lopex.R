library(specprocess)
source('common.R')
data_path <- 'data/lopex'
projectcode <- 'lopex'

# Set site and plot tables
site_plot <- tibble(
    sitecode = 'lopex.ispra',
    sitedescription = 'Joint Research Center, Ispra, Italy',
    latitude = 45.803,
    longitude = 8.630) %>%
    mutate(plotcode = sitecode,
           plotdescription = sitedescription) %>%
    db_merge_into(db = specdb, table = 'sites', values = ., 
                  by = 'sitecode', id_colname = 'siteid') %>%
    db_merge_into(db = specdb, table = 'plots', values = ., 
                  by = 'plotcode', id_colname = 'plotid')

specmethods <- tibble(
    instrumentname = "Perkin Elmer Lambda 19 double-beam spectrophotometer (BaSO4 integrating sphere)",
    apparatus = "Integrating sphere", 
    calibration = "Spectralon ratio", 
    specmethodcomment = "See http://teledetection.ipgp.jussieu.fr/opticleaf/lopex.htm#spectral for more info"
    ) %>%
    db_merge_into(db = specdb, table = 'instruments', values = ., 
                  by = 'instrumentname', id_colname = 'instrumentid') %>%
    db_merge_into(db = specdb, table = 'specmethods', values = .,
                  by = c('instrumentid', 'apparatus', 'calibration'), 
                  id_colname = 'specmethodid')

#' Set paths for LOPEX data
PATH.chem <- file.path(data_path, "LDB_lopex1993.csv")
PATH.spec <- file.path(data_path, "spec")

#' Load main data.
#species.info <- fread(PATH.speciesinfo, header=TRUE)
species.rxp <- "([[:alpha:]]{3})[[:alpha:]]* x? ?([[:alpha:]]{3})[[:alpha:]]* *.*"
lopex.chem <- fread(PATH.chem, header=TRUE) %>%
    rename(speciesdatacode = `Latin Name`) %>%
    select(-`Plant Type`, -`English Name`) %>%
    mutate_if(is.numeric, na_if, y=-999) %>%
    mutate(projectcode = projectcode,
           year = 1993,
           sitecode = site_plot$sitecode,
           plotcode = site_plot$plotcode,
           speciesdatacode = na_if(speciesdatacode, '')) %>%
    .[, speciesdatacode := speciesdatacode[1], by = cumsum(!is.na(speciesdatacode))] %>%
    .[, samplename := sprintf("%s_Leaf%0.2d", gsub(species.rxp, "\\1-\\2", speciesdatacode), 1:.N)] %>%
    .[grepl('Vitis vinifera.*Sylvestris', speciesdatacode),
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
        setnames(c('wavelength', 'spectravalue')) %>%
        mutate(samplecode = ID,
               fname = refl_files,
               spectratype = 'reflectance')
    trans_files <- lopex.chem[ID, Trans_file] 
    trans_files_full <- file.path(PATH.spec, trans_files)
    trans_list[[ID]] <- fread(trans_files_full) %>%
        setnames(c('wavelength', 'spectravalue')) %>%
        mutate(samplecode = ID,
               fname = refl_files,
               spectratype = 'transmittance')
}
specdat <- rbind(rbindlist(refl_list), rbindlist(trans_list))

names_dict <- c("C_C" = "leaf_C_pct_mass",
                "C_H" = "leaf_H_pct_mass",
                "C_O" = "leaf_O_pct_mass",
                "C_N" = "leaf_N_pct_mass")

lopex.traits <- lopex.chem %>%
    rename_(.dots = setNames(names(names_dict), names_dict)) %>%
    .[, leaf_chla_per_area := ud.convert(C_a, 'ug cm-2', 'kg m-2')] %>%
    .[, leaf_chlb_per_area := ud.convert(C_b, 'ug cm-2', 'kg m-2')] %>%
    .[, leaf_chltot_per_area := ud.convert(C_ab, 'ug cm-2', 'kg m-2')] %>%
    .[, leaf_cartot_per_area := ud.convert(C_car, 'ug cm-2', 'kg m-2')] %>%
    .[, leaf_anth_per_area := ud.convert(C_anth, 'ug cm-2', 'kg m-2')] %>%
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
    distinct(samplecode, projectcode, year, sitecode, plotcode, samplename, speciesdatacode) %>%
    left_join(tbl(specdb, 'species_dict') %>% 
              select(-speciesdictid, -speciesdictcomment) %>% 
              collect %>% 
              setDT) %>%
    select(-speciesdatacode)

samples <- full_join(spec_samples, chem_samples) %>% 
    db_merge_into(db = specdb, table = 'samples', values = .,
                  by = 'samplecode', id_colname = 'sampleid')

spectra_info <- specdat %>% 
    distinct(samplecode, spectratype) %>%
    mutate(specmethodid = specmethods$specmethodid) %>%
    db_merge_into(db = specdb, table = 'spectra_info', values = .,
                  by = c('samplecode', 'spectratype'), id_colname = 'spectraid')

spectra_data <- specdat %>%
    select(-fname) %>%
    left_join(spectra_info %>% select(samplecode, spectraid, spectratype)) %>%
    write_spectradata

traits <- lopex.traits %>%
    select(samplecode, starts_with('leaf_')) %>% 
    melt(id.vars = 'samplecode', variable.name = 'trait',
         value.name = 'traitvalue', na.rm = TRUE)

trait_info <- traits %>%
    distinct(trait) %>%
    .[grepl('_pct', trait), unit := '%'] %>%
    .[grepl('_area|_thickness', trait), unit := 'kg m-2'] %>%
    .[grepl('ratio', trait), unit := 'unitless'] %>%
    db_merge_into(db = specdb, table = 'trait_info', values = .,
                  by = 'trait', id_colname = 'traitid')

traits <- db_merge_into(db = specdb, table = 'trait_data', values = traits,
                        by = 'trait', id_colname = 'traitdataid')
