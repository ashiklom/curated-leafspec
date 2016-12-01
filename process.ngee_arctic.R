source("common.R")
library(readxl)
library(magrittr)
library(lubridate)

projectcode <- "ngee_arctic"

path_nga <- "raw/NGEE-Arctic"

# Load spectral data
speclist <- list()

spec_2013_file <- file.path(path_nga, "2013_data", 
                            "NGEE-Arctic_2013_Spectra_and_Trait_Data_QA_v2_forR.csv")
speclist[['2013']] <- fread(spec_2013_file, header = TRUE) %>%
    .[, SampleName := paste0('BNL', Sample_ID)] %>%
    mutate(SampleYear = 2013, Site = 'Barrow')

getwlcols <- function(dat) {
    grep('Wave_', colnames(dat), value = TRUE)
}

scale_wl <- function(dat, scale_factor = 0.01) {
    dat[, getwlcols(dat) := lapply(.SD, '*', 0.01), 
        .SDcols = getwlcols(dat)]
}

spec_2014_file <- file.path(path_nga, "2014_Data", 
                            "NGEE-Arctic_Barrow_2014_Leaf_GasExchange_Spectra.xlsx")

speclist[['2014']] <- read_excel(spec_2014_file, sheet = 1) %>% 
    setDT() %>%
    setnames("Spectra", "SampleName") %>%
    mutate(SampleYear = 2014, Site = 'Barrow') %>%
    scale_wl()

spec_2015_file <- file.path(path_nga, "2015_Data",
                            "NGEE-Arctic_Barrow_2015_SVC_Leaf_Spectra.xlsx")
speclist[['2015']] <- read_excel(spec_2015_file, sheet = 1) %>%
    setDT() %>%
    setnames('Sample_Barcode', 'SampleName') %>%
    mutate(SampleYear = 2015, Site = 'Barrow') %>%
    scale_wl()

spec_2016b_file <- file.path(path_nga, "2016_Data",
                             "NGEE-Arctic_Barrow_2016_SVC_Leaf_Spectra.xlsx")
speclist[['2016b']] <- read_excel(spec_2016b_file, sheet = 1) %>%
    setDT() %>%
    setnames('Sample_Barcode', 'SampleName') %>%
    mutate(SampleYear = 2016, Site = 'Barrow') %>%
    scale_wl()

spec_2016s_file <- file.path(path_nga, "2016_Data",
                             "NGEE-Arctic_Seward_2016_HR1024i_Leaf_Spectral_Reflectance.xlsx")
speclist[['2016s']] <- read_excel(spec_2016s_file, sheet = 2) %>%
    setDT() %>%
    setnames('BNL_Barcode', 'SampleName') %>%
    mutate(SampleYear = 2016, Site = 'Seward_Kougarok') %>%
    scale_wl()

specdat_full <- rbindlist(speclist, fill = TRUE)
specmat <- specdat_full %>%
    select(contains("Wave_")) %>%
    as.matrix() %>%
    "colnames<-"(gsub('Wave_', '', colnames(.))) %>%
    "rownames<-"(specdat_full[, SampleName]) %>%
    rbind("Wavelength" = as.numeric(colnames(.)), .) %>%
    t() %>%
    wlmat2list

specdat <- specdat_full %>% 
    select(SampleName, SampleYear, Site) %>%
    .[, Reflectance := specmat[SampleName]]

# Load main traits file
traits_namesdict <- c('Sample_ID' = 'SampleName',
                      'Perc_C' = 'leaf_C_percent',
                      'Perc_N' = 'leaf_N_percent',
                      'LMA_gDW_m2' = 'leaf_mass_per_area',
                      'CN_ratio' = 'leaf_CN_ratio',
                      'USDA_Species_Code' = 'RawSpecies')

traits_main_file <- file.path(path_nga,
                              "NGEEArctic_BNL_leaf_C_N_LMA_2012-2015.xlsx")
traits_main <- read_excel(traits_main_file, sheet = 2) %>%
    .[, !is.na(colnames(.)) & colnames(.) != ''] %>%
    setnames(names(traits_namesdict), traits_namesdict) %>%
    setDT() %>%
    mutate(SampleYear = year(strptime(Measurement_Date, "%Y%m%d")),
           DOY = yday(strptime(Measurement_Date, "%Y%m%d")),
           Site = 'Barrow')

# Load pigment data for 2015
chl_convert <- 1000 / (100^2)
pigments_file <- file.path(path_nga, "2015_Data",
                           "NGEE-Arctic_Barrow_2015_leaf_pigment_extractions.xlsx")
pigments <- read_excel(pigments_file, sheet = 2) %>%
    setnames('Barcode', 'SampleName') %>%
    mutate(leaf_chlorophyll_a = Chl_a_mg_m2 * chl_convert,
           leaf_chlorophyll_b = Chl_b_mg_m2 * chl_convert,
           leaf_chlorophyll_total = Chl_a_plus_b_mg_m2 * chl_convert,
           leaf_carotenoid_total = Tot_Car_mg_m2 * chl_convert, 
           SampleYear = 2015, 
           Site = 'Barrow') %>%
    setDT()

# Load other data from 2016
seward_2016_lma_file <- file.path(path_nga, "2016_Data", 
                                  "2016KGsamples_LMA-edited.csv")
seward_2016_lma <- fread(seward_2016_lma_file) %>%
    mutate(SampleName = paste0("BNL", Sample_Barcode), 
           SampleYear = 2016,
           DOY = yday(strptime(Measurement_Date, "%Y%m%d"))) %>%
    rename(leaf_mass_per_area = LMA_gDW_m2,
           RawSpecies = USDA_Species_Code) %>%
    setDT()

barrow_2016_chn_file <- file.path(path_nga, "2016_Data", 
                                  "Barrow2016_CHN_analysis-edited.xlsx")
barrow_2016_chn <- read_excel(barrow_2016_chn_file, sheet = 2) %>%
    rename(SampleName = Sample_Barcode,
           RawSpecies = USDA_Species_Code,
           leaf_C_percent = Perc_C,
           leaf_N_percent = Perc_N,
           leaf_CN_ratio = CN_ratio) %>%
    mutate(SampleYear = 2016,
           Site = 'Barrow') %>%
    setDT()

barrow_2016_lma_file <- file.path(path_nga, "2016_Data", 
                                  "Barrow2016_samples_LMA.csv")
barrow_2016_lma <- fread(barrow_2016_lma_file) %>%
    rename(leaf_mass_per_area = LMA_gDW_m2,
           RawSpecies = Species) %>%
    mutate(SampleName = paste0("BNL", Sample_Barcode),
           SampleYear = 2016, 
           DOY = yday(strptime(Measurement_Date, "%Y%m%d")),
           Site = 'Barrow') %>%
    setDT()

dat_2016 <- full_join(seward_2016_lma, barrow_2016_lma) %>%
    full_join(barrow_2016_chn) %>%
    subToCols()
dat_other <- full_join(traits_main, pigments) %>%
    filter(!is.na(SampleYear))
traits_full <- full_join(dat_2016, dat_other)

nga_dat <- specdat %>%
    left_join(traits_full, by = c('SampleName', 'SampleYear', 'Site')) %>%
    mutate(Project = projectcode,
           FullName = paste(Project, SampleName, SampleYear, 
                            sep = id_separator))

# Sanity checks
nga_dat %>% group_by(Site) %>% count()
nga_dat %>% group_by(RawSpecies) %>% count()
nga_dat %>% filter(is.na(RawSpecies)) %>% 
    group_by(Site, SampleYear) %>% count()
 
saveRDS(nga_dat, file = rds_name(projectcode))
