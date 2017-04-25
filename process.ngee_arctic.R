library(specprocess)
library(readxl)
library(lubridate)

source('common.R')

dbGetQuery(specdb$con, 'DELETE FROM projects WHERE projectcode = "ngee_arctic"')

projects <- tibble(
    projectcode = 'ngee_arctic',
    projectshortname = 'NGEE-Arctic',
    projectdescription = 'Next Generation Ecosystem Experiment (NGEE) - Arctic',
    pointofcontact = 'Serbin, Shawn',
    email = 'serbinsh@bnl.gov') %>% 
    write_project()

path_nga <- '~/Dropbox/NASA_TE_PEcAn-RTM_Project/Data/NGEE-Arctic/'

scale_wl <- function(., scale_factor = 0.01) {
    mutate_at(., vars(starts_with('Wave_')), funs(. * scale_factor))
}

# Load spectral data
speclist <- list()

spec_2013_file <- file.path(path_nga, "2013_data", 
                            "NGEE-Arctic_2013_Spectra_and_Trait_Data_QA_v2_forR.csv")
speclist[['2013']] <- fread(spec_2013_file, header = TRUE) %>%
    .[, SampleName := paste0('BNL', Sample_ID)] %>%
    mutate(year = 2013, sitecode = 'Barrow') %>%
    scale_wl()

spec_2014_file <- file.path(path_nga, "2014_Data", 
                            "NGEE-Arctic_Barrow_2014_Leaf_GasExchange_Spectra.xlsx")

speclist[['2014']] <- read_excel(spec_2014_file, sheet = 1) %>% 
    setDT() %>%
    setnames("Spectra", "SampleName") %>%
    mutate(year = 2014, sitecode = 'Barrow') %>%
    scale_wl()

spec_2015_file <- file.path(path_nga, "2015_Data",
                            "NGEE-Arctic_Barrow_2015_SVC_Leaf_Spectra.xlsx")
speclist[['2015']] <- read_excel(spec_2015_file, sheet = 1) %>%
    setDT() %>%
    setnames('Sample_Barcode', 'SampleName') %>%
    mutate(year = 2015, sitecode = 'Barrow') %>%
    scale_wl()

spec_2016b_file <- file.path(path_nga, "2016_Data",
                             "NGEE-Arctic_Barrow_2016_SVC_Leaf_Spectra.xlsx")
speclist[['2016b']] <- read_excel(spec_2016b_file, sheet = 1) %>%
    setDT() %>%
    setnames('Sample_Barcode', 'SampleName') %>%
    mutate(year = 2016, sitecode = 'Barrow') %>%
    scale_wl()

spec_2016s_file <- file.path(path_nga, "2016_Data",
                             "NGEE-Arctic_Seward_2016_HR1024i_Leaf_Spectral_Reflectance.xlsx")
speclist[['2016s']] <- read_excel(spec_2016s_file, sheet = 2) %>%
    setDT() %>%
    setnames('BNL_Barcode', 'SampleName') %>%
    mutate(year = 2016, sitecode = 'Seward_Kougarok') %>%
    scale_wl()

specdat_full <- rbindlist(speclist, fill = TRUE) %>%
    mutate(samplecode = paste(projects$projectcode, SampleName, year, sep = '|'))

samples_spec <- specdat_full %>%
    select(-starts_with('Wave_')) %>%
    .[!is.na(Spectrometer_GPS_Lat),
      plotcode := as.character(1:.N),
      .(Spectrometer_GPS_Lat, Spectrometer_GPS_Long)] %>%
    select(samplecode, SampleName, year, sitecode, plotcode,
           latitude = Spectrometer_GPS_Lat,
           longitude = Spectrometer_GPS_Long) %>%
    mutate(plotcode = paste(sitecode, plotcode, sep = '.'))

spectra_data <- specdat_full %>%
    select(samplecode, starts_with('Wave_')) %>%
    melt(id.vars = 'samplecode', variable.name = 'wavelength', 
         value.name = 'spectravalue') %>%
    mutate(wavelength = as.numeric(gsub('Wave_', '', wavelength)))

# Load main traits file
traits_main_file <- file.path(path_nga,
                              "NGEEArctic_BNL_leaf_C_N_LMA_2012-2015.xlsx")
traits_main <- read_excel(traits_main_file, sheet = 2) %>%
    .[, !is.na(colnames(.)) & colnames(.) != ''] %>%
    filter(!is.na(Sample_ID)) %>%
    mutate_if(is.numeric, na_if, y=-9999) %>%
    rename(SampleName = Sample_ID,
           leaf_C_pct_mass = Perc_C,
           leaf_N_pct_mass = Perc_N,
           leaf_CN_ratio_mass = CN_ratio,
           speciesdatacode = USDA_Species_Code) %>%
    mutate(collectiondate = as.Date(strptime(Measurement_Date, "%Y%m%d")),
           year = year(collectiondate),
           sitecode = 'Barrow',
           leaf_mass_per_area = ud.convert(LMA_gDW_m2, 'g m-2', 'kg m-2'),
           leaf_N_per_area = ud.convert(N_area_gDW_m2, 'g m-2', 'kg m-2'),
           leaf_C_per_area = ud.convert(C_area_gDW_m2, 'g m-2', 'kg m-2')) %>%
    select(SampleName, year, collectiondate, sitecode, speciesdatacode, 
           starts_with('leaf_')) %>%
    setDT()

# Load pigment data for 2015
pigments_file <- file.path(path_nga, "2015_Data",
                           "NGEE-Arctic_Barrow_2015_leaf_pigment_extractions.xlsx")
pigments <- read_excel(pigments_file, sheet = 2) %>%
    setnames('Barcode', 'SampleName') %>%
    mutate(leaf_chla_per_area = ud.convert(Chl_a_mg_m2, 'mg m-2', 'kg m-2'),
           leaf_chlb_per_area = ud.convert(Chl_b_mg_m2, 'mg m-2', 'kg m-2'),
           leaf_chltot_per_area = ud.convert(Chl_a_plus_b_mg_m2, 'mg m-2', 'kg m-2'),
           leaf_cartot_per_area = ud.convert(Tot_Car_mg_m2, 'mg m-2', 'kg m-2'),
           year = 2015, 
           sitecode = 'Barrow') %>%
    select(SampleName, year, sitecode, 
           leaf_area = Total_area_m2,
           starts_with('leaf')) %>%
    setDT()

# Get other trait data from 2013
traits2013 <- speclist[['2013']] %>%
    select(-starts_with('Wave')) %>%
    mutate(leaf_chla_per_area = ud.convert(Chl_a_g_m2, 'g m-2', 'kg m-2'),
           leaf_chlb_per_area = ud.convert(Chl_b_g_m2, 'g m-2', 'kg m-2'),
           leaf_chltot_per_area = leaf_chla_per_area + leaf_chlb_per_area,
           leaf_N_per_area = ud.convert(Narea_gN_m2, 'g m-2', 'kg m-2'), 
           leaf_mass_per_area = ud.convert(LMA_gDW_m2, 'g m-2', 'kg m-2')) %>%
    select(SampleName, year, speciesdatacode = USDA_Species,
           starts_with('leaf_', ignore.case = FALSE),
           leaf_area = Total_Leaf_Area_m2)

# Load other data from 2016
seward_2016_lma_file <- file.path(path_nga, "2016_Data", 
                                  "2016KGsamples_LMA-edited.csv")
seward_2016_lma <- fread(seward_2016_lma_file) %>%
    rename(sitecode = Site) %>% 
    mutate(SampleName = paste0("BNL", Sample_Barcode), 
           leaf_mass_per_area = ud.convert(LMA_gDW_m2, 'g m-2', 'kg m-2'),
           year = 2016,
           collectiondate = as.Date(strptime(Measurement_Date, "%Y%m%d"))) %>%
    select(SampleName, sitecode, year, speciesdatacode = USDA_Species_Code,
           collectiondate, leaf_mass_per_area)

barrow_2016_chn_file <- file.path(path_nga, "2016_Data", 
                                  "Barrow2016_CHN_analysis-edited.xlsx")
barrow_2016_chn <- read_excel(barrow_2016_chn_file, sheet = 2) %>%
    rename(SampleName = Sample_Barcode,
           speciesdatacode = USDA_Species_Code,
           leaf_C_pct_mass = Perc_C,
           leaf_N_pct_mass = Perc_N,
           leaf_CN_ratio_mass = CN_ratio) %>%
    mutate(year = 2016,
           sitecode = 'Barrow') %>%
    select(SampleName, speciesdatacode, sitecode, starts_with('leaf')) %>%
    setDT()

barrow_2016_lma_file <- file.path(path_nga, "2016_Data", 
                                  "Barrow2016_samples_LMA.csv")
barrow_2016_lma <- fread(barrow_2016_lma_file) %>%
    mutate(SampleName = paste0("BNL", Sample_Barcode),
           year = 2016, 
           collectiondate = as.Date(strptime(Measurement_Date, "%Y%m%d")),
           sitecode = 'Barrow',
           leaf_mass_per_area = ud.convert(LMA_gDW_m2, 'g m-2', 'kg m-2')) %>%
    select(SampleName, year, sitecode, collectiondate,
           speciesdatacode = Species, leaf_mass_per_area)

dat_2016 <- full_join(seward_2016_lma, barrow_2016_lma) %>%
    full_join(barrow_2016_chn)
dat_other <- full_join(traits_main, pigments) %>%
    filter(!is.na(year))
traits_full <- full_join(dat_2016, dat_other) %>%
    setkey(SampleName) %>%
    .[traits2013[, SampleName], 
      `:=`(leaf_chla_per_area = traits2013[, leaf_chla_per_area],
           leaf_chlb_per_area = traits2013[, leaf_chlb_per_area],
           leaf_chltot_per_area = traits2013[, leaf_chltot_per_area])]

samples_raw <- samples_spec %>%
    full_join(select(traits_full, -starts_with('leaf_'))) %>%
    mutate(projectcode = projects$projectcode,
           samplecode = paste(projectcode, SampleName, year, sep = '|'),
           plotcode = if_else(is.na(plotcode), 
                              paste(sitecode, plotcode, sep = '.'),
                              plotcode)) %>%
    left_join(read_csv('data/ngee_arctic/ngee_arctic_species_dict.csv') %>% setDT())

# Merge with SQL
sites <- samples_raw %>%
    distinct(projectcode, sitecode) %>%
    write_sites()

# TODO: Finer resolution for plot latitude and longitude...?
plots <- samples_raw %>%
    group_by(sitecode, plotcode) %>%
    summarize(latitude = mean(latitude),
              longitude = mean(longitude)) %>%
    mutate(latitude = if_else(is.na(latitude) & sitecode == 'Barrow', 
                              71.275764, latitude),
           longitude = if_else(is.na(longitude) & sitecode == 'Barrow',
                               -156.641386, longitude)) %>%
    write_plots()

samples <- db_merge_into(db = specdb, table = 'samples', values = samples_raw, by = 'samplecode')

#TODO: Add instrument, specmethod

spectra_info <- spectra_data %>%
    distinct(samplecode) %>%
    mutate(spectratype = 'reflectance') %>%
    db_merge_into(db = specdb, table = 'spectra_info', values = .,
                  by = c('samplecode', 'spectratype'), id_colname = 'spectraid')

spectra_data_in <- spectra_data %>%
    left_join(spectra_info) %>%
    write_spectradata

trait_data <- traits_full %>%
    left_join(select(samples_raw, samplecode, SampleName)) %>%
    select(samplecode, starts_with('leaf_')) %>%
    melt(id.vars = 'samplecode', variable.name = 'trait',
         value.name = 'traitvalue', na.rm = TRUE) %>% 
    group_by(samplecode, trait) %>% 
    summarize(traitvalue = mean(traitvalue)) %>% 
    ungroup()

trait_info <- trait_data %>%
    distinct(trait) %>%
    mutate(unit = case_when(grepl('_per_area', .$trait) ~ 'kg m-2',
                            grepl('_pct_mass', .$trait) ~ '%',
                            grepl('ratio', .$trait) ~ 'unitless',
                            .$trait == 'leaf_area' ~ 'm2')) %>%
    db_merge_into(db = specdb, table = 'trait_info', values = ., by = 'trait')

traits <- db_merge_into(db = specdb, table = 'trait_data', values = trait_data, by = 'samplecode')

## Sanity checks
#samples %>% group_by(sitecode) %>% count()
#samples %>% group_by(speciescode) %>% count()
#samples %>% filter(is.na(speciescode)) %>% 
    #group_by(sitecode, year) %>% count()
