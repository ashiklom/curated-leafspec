library(specprocess)
library(readxl)
library(lubridate)
source('common.R')

#' Projects table
project_code <- "yang_pheno"

#projectname <- "Seasonal variability of multiple leaf traits captured by leaf spectroscopy at two temperate deciduous forests"

#projectreference <- "Yang, X., Tang, J., Mustard, J.F., Wu, J., Zhao, K., Serbin, S., Lee, J.-E., 2016. Seasonal variability of multiple leaf traits captured by leaf spectroscopy at two temperate deciduous forests. Remote Sensing of Environment 179, 1–12."

#project_doi <- '10.1016/j.rse.2016.03.026'

# MV (Martha's Vineyard) -- 41.362, -70.578
# A,B,C indicate three individuals
# U,L mean upper and lower
# N,S mean leaves were on the north and south part of the canopy
# 1,2,3 mean different leaves
# All trees are Red Oak

##' Sites table
sites <- tribble(
    ~sitecode, ~description,
    'yang_pheno.MV', "Martha\'s Vineyard, MA, USA",
    'yang_pheno.HF', "Harvard Forest, Petersham, MA, USA") %>%
    db_merge_into(db = specdb, table = 'sites', values = .,
                  by = 'sitecode', id_colname = 'siteid')

plots <- tribble(
    ~sitecode, ~latitude, ~longitude,
    'yang_pheno.MV', 41.362, -70.578,
    'yang_pheno.HF', 42.531, -72.190) %>%
    left_join(sites) %>%
    mutate(plotcode = sitecode) %>%
    db_merge_into(db = specdb, table = 'plots', values = .,
                  by = 'plotcode', id_colname = 'plotid')

# HF
# RO -- Red Oak
# RM -- Red maple
# YB -- Yellow Birch
# U,L are upper and lower 
# First number indicates trees
# Second number indicates leaves

# Set up trait names
traits <- c('leaf_chla_per_area' = 'Chla',
            'leaf_chlb_per_area' = 'Chlb',
            'leaf_chltot_per_area' = 'TotChl',
            'leaf_mass_per_area' = 'LMA',
            'leaf_cartot_per_area' = 'Car',
            'leaf_C_pct_mass' = 'TotC',
            'leaf_N_pct_mass' = 'TotN')
ntraits <- length(traits)

readYang <- function(SampleYear, Site) {
    rootdir <- '~/Dropbox/NASA_TE_PEcAn-RTM_Project/Data/Yang_etal'
    fname <- sprintf("%s/%d_%s_leaftraits_forShawn.xlsx", 
                     rootdir, SampleYear, Site)
    dat_list <- list()
    SampleNames <- character()
    is_MV <- SampleYear == 2011 & Site == "MV"
    is_HF <- SampleYear == 2012 & Site == "HF"
    
    # Load trait data
    for (i in seq_along(traits)){
        dat <- read_excel(path = fname, sheet = traits[i]) %>% 
            .[colnames(.) != ""] %>%
            setDT
        SampleNames <- union(SampleNames, colnames(dat)[-1])
        setnames(dat, 1, "DOY")
        dat <- dat[, lapply(.SD, as.numeric)][!is.na(DOY)]
        dat[, trait := names(traits)[i]]
        dat_list[[i]] <- dat
    }

    dat_full <- rbindlist(dat_list)

    dat_trait <- dat_full %>% 
        melt(id.vars = c("trait", "DOY"),
             variable.name = 'barcode',
             value.name = 'traitvalue') %>%
        dcast(DOY + barcode ~ trait) %>%
        mutate(projectcode = project_code,
               year = SampleYear,
               sitecode = paste(projectcode, Site, sep = '.'),
               plotcode = sitecode,
               samplecode = paste(projectcode, 
                                  paste(barcode, DOY, sep = '_'),
                                  year, 
                                  sep = '|'))

    species_cols <- c('RawSpecies')
    species <- c('RO' = 'Quercus rubra',
                 'RM' = 'Acer rubrum',
                 'YB' = 'Betula alleghaniensis')
    #species_cols <- c('species_scientific', 'species_code', 'species_common')
    #species <- list(RO = list('Quercus rubra', 'QURU', 'Red oak'),
                    #RM = list('Acer rubrum', 'ACRU', 'Red maple'),
                    #YB = list('Betula alleghaniensis', 'BEAL2', 'Yellow birch'))

    if (is_MV){
        dat_trait <- mutate(dat_trait, speciesdatacode = 'Quercus rubra')
    } else if (is_HF) {
        dat_trait <- mutate(dat_trait, 
                            label = gsub('(RO|RM|YB).*', "\\1", barcode),
                            speciesdatacode = species[label]) %>% select(-label)
    }

    dat_trait <- dat_trait %>%
        mutate(sunshade = case_when(grepl('U', barcode) ~ 'sun',
                                    grepl('L', barcode) ~ 'shade'))

    samples_all <- dat_trait %>%
        distinct(samplecode, projectcode, sitecode, plotcode, speciesdatacode, DOY, year) %>%
        mutate(collectiondate = as.Date(paste(year, DOY, sep = '_'),
                                        '%Y_%j')) %>%
        left_join(tbl(specdb, 'species_dict') %>%
                  filter(projectcode == project_code) %>%
                  select(speciesdatacode, speciescode) %>%
                  collect() %>%
                  setDT()) %>%
        select(-speciesdatacode) %>%
        db_merge_into(db = specdb, table = 'samples', values = .,
                    by = 'samplecode', id_colname = 'sampleid')

    sample_condition <- dat_trait %>%
        distinct(samplecode, sunshade) %>%
        melt(id.vars = 'samplecode', variable.name = 'condition',
             value.name = 'conditionvalue')
    sample_condition_info <- distinct(sample_condition, condition) %>%
        db_merge_into(db = specdb, table = 'sample_condition_info', values = .,
                    by = 'condition', id_colname = 'conditionid')
    sample_condition <- db_merge_into(db = specdb, table = 'sample_condition', values = sample_condition,
                                      by = 'samplecode', id_colname = 'conditiondataid')

    trait_data <- dat_trait %>%
        select(samplecode, starts_with('leaf_')) %>%
        mutate_at(vars(matches('_(chl|car)')), ud.convert,
                  u1 = 'ug cm-2', u2 = 'kg m-2') %>%
        mutate(leaf_mass_per_area = ud.convert(leaf_mass_per_area,
                                               'g m-2', 'kg m-2')) %>%
        melt(id.vars = 'samplecode', variable.name = 'trait',
             value.name = 'traitvalue', na.rm = TRUE)

    trait_info <- trait_data %>%
        distinct(trait) %>%
        mutate(unit = case_when(grepl('_pct_mass', .$trait) ~ '%',
                                grepl('_per_area', .$trait) ~ 'kg m-2',
                                TRUE ~ NA_character_)) %>%
        db_merge_into(db = specdb, table = 'trait_info', values = .,
                    by = 'trait', id_colname = 'traitid')

    trait_data <- db_merge_into(db = specdb, table = 'trait_data', values = trait_data,
                                by = c('samplecode', 'trait'), id_colname = 'traitdataid')

    # Load spectral data
    getSpec <- function(fname) {
        spec <- fread(fname, header=FALSE) %>%
            setnames(c("wavelength", 
                       samples_all[DOY == doys[i], samplecode])) %>%
            filter(wavelength != 0) %>%
            melt(id.vars = 'wavelength', variable.name = 'samplecode', 
                 value.name = 'spectravalue', na.rm = TRUE)
        return(spec)
    }

    doys <- sort(samples_all[, unique(DOY)])
    if (is_MV) {
        reflpath <- file.path(rootdir, "2011-MV")
    } else if (is_HF) {
        reflpath <- file.path(rootdir, "2012_HF/ref")
        transpath <- file.path(rootdir, "2012_HF/tra")
    }

    refl_flist <- list.files(reflpath, ".csv", full.names = TRUE)
    stopifnot(length(refl_flist) == length(doys))
    if (is_HF) {
        trans_flist <- list.files(transpath, full.names = TRUE)
        stopifnot(length(trans_flist) == length(doys))
    }

    refl_list <- list()
    if (is_HF) trans_list <- list()

    for (i in seq_along(doys)) {
        refl_list[[i]] <- getSpec(refl_flist[i])
        if (is_HF) trans_list[[i]] <- getSpec(trans_flist[i])
    }
    
    refl_dat <- rbindlist(refl_list) %>%
        mutate(spectratype = 'reflectance')

    if (is_HF) {
        trans_dat <- rbindlist(trans_list) %>%
            mutate(spectratype = 'transmittance')
        spectra_raw <- rbind(refl_dat, trans_dat)
    } else {
        spectra_raw <- refl_dat
    }

    spectra_info <- spectra_raw %>%
        distinct(samplecode, spectratype) %>%
        db_merge_into(db = specdb, table = 'spectra_info', values = .,
                    by = c('samplecode', 'spectratype'), id_colname = 'spectraid')

    spectra_data <- spectra_raw %>%
        left_join(spectra_info) %>%
        write_spectradata
}

##options(error = recover)
yang_mv <- readYang(2011, "MV")
yang_hf <- readYang(2012, "HF")

