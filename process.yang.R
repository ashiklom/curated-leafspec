source("common.R")
library(readxl)
library(lubridate)

#' Projects table
projectcode <- "Yang_et_al"

projectname <- "Seasonal variability of multiple leaf traits captured by leaf spectroscopy at two temperate deciduous forests"

projectreference <- "Yang, X., Tang, J., Mustard, J.F., Wu, J., Zhao, K., Serbin, S., Lee, J.-E., 2016. Seasonal variability of multiple leaf traits captured by leaf spectroscopy at two temperate deciduous forests. Remote Sensing of Environment 179, 1–12."

project_doi <- '10.1016/j.rse.2016.03.026'

projectID <- tibble(ProjectCode = projectcode,
                         ProjectName = projectname) %>%
    mergeWithSQL(db, "projects", ., "ProjectName") %>%
    filter(ProjectName == projectname) %>%
    select(ProjectID) %>%
    .[[1]]

# MV (Martha's Vineyard) -- 41.362, -70.578
# A,B,C indicate three individuals
# U,L mean upper and lower
# N,S mean leaves were on the north and south part of the canopy
# 1,2,3 mean different leaves
# All trees are Red Oak

#' Sites table
mv_name <- "Martha's Vineyard"
mv_desc <- "Martha's Vineyard, MA, USA"
mv_lat <- 41.362
mv_lon <- -70.578
mv_site <- tibble(
    SiteName = mv_name,
    SiteDescription = mv_desc,
    SiteLatitude = mv_lat,
    SiteLongitude = mv_lon) %>%
    mergeWithSQL(db, "sites", ., "SiteName")
mv_siteID <- filter(mv_site, SiteName == sitename) %>% 
    select(SiteID) %>% .[[1]]

#' Species table
mv_speciesID <- 1170    # Quercus rubra

sample_year <- 2011
sample_name <- "MV"

fname <- sprintf("raw/Yang_etal/%d_%s_leaftraits_forShawn.xlsx", 
                 sample_year, sample_site)
dat_list <- list()
sample_names <- character()

# Load trait data
for (i in seq_along(traits)){
    dat <- read_excel(path = fname, sheet = traits[i]) %>% 
        .[colnames(.) != ""] %>%
        setDT
    sample_names <- union(sample_names, colnames(dat)[-1])
    setnames(dat, 1, "DOY")
    dat <- dat[, lapply(.SD, as.numeric)][!is.na(DOY)]
    dat[, trait := names(traits)[i]]
    dat_list[[i]] <- dat
}

# HF
# RO -- Red Oak
# RM -- Red maple
# YB -- Yellow Birch
# U,L are upper and lower 
# First number indicates trees
# Second number indicates leaves

# Set up trait names
traits <- c('leaf_chlorophyll_a' = 'Chla',
            'leaf_chlorophyll_b' = 'Chlb',
            'leaf_chlorophyll_total' = 'TotChl',
            'leaf_mass_per_area' = 'LMA',
            'leaf_carotenoid_total' = 'Car',
            'leaf_C_percent' = 'TotC',
            'leaf_N_percent' = 'TotN')
ntraits <- length(traits)

readYang <- function(sample_year, sample_site) {
    fname <- sprintf("raw/Yang_etal/%d_%s_leaftraits_forShawn.xlsx", 
                     sample_year, sample_site)
    dat_list <- list()
    sample_names <- character()
    is_MV <- sample_year == 2011 & sample_site == "MV"
    is_HF <- sample_year == 2012 & sample_site == "HF"
    
    # Load trait data
    for (i in seq_along(traits)){
        dat <- read_excel(path = fname, sheet = traits[i]) %>% 
            .[colnames(.) != ""] %>%
            setDT
        sample_names <- union(sample_names, colnames(dat)[-1])
        setnames(dat, 1, "DOY")
        dat <- dat[, lapply(.SD, as.numeric)][!is.na(DOY)]
        dat[, trait := names(traits)[i]]
        dat_list[[i]] <- dat
    }

    dat_full <- rbindlist(dat_list)
    dat_trait <- dat_full %>% melt(id.vars = c("trait", "DOY"),
                                  variable.name = "sample_name") %>%
        dcast(DOY + sample_name ~ trait) %>%
        setDT()

    projname <- paste0("Yang", sample_site)

    dat_trait[, c('project', 'sample_year', 'sample_site') := 
             list(projname, sample_year, sample_site)]

    dat_trait[, sample_name := paste(sample_name, DOY, sep="_")]
    setnames(dat_trait, 'DOY', 'sample_doy')

    species_cols <- c('species_scientific', 'species_code', 'species_common')
    species <- list(RO = list('Quercus rubra', 'QURU', 'Red oak'),
                    RM = list('Acer rubrum', 'ACRU', 'Red maple'),
                    YB = list('Betula alleghaniensis', 'BEAL2', 'Yellow birch'))

    if (is_MV){
        dat_trait[, (species_cols) := species[['RO']]]
    } else if (is_HF) {
        dat_trait[, label := gsub('(RO|RM|YB).*', "\\1", sample_name)]
        for (l in names(species)){
            dat_trait[label == l, (species_cols) := species[[l]]]
        }
        dat_trait[, label := NULL]
    }

    dat_trait[grep("U", sample_name), canopy_position := "top"]
    dat_trait[grep("L", sample_name), canopy_position := "bottom"]

    dat_trait[, sample_id := paste(project, sample_name, sample_year, 
                                  sep = id_separator)]

    # Load spectral data

    getSpec <- function(fname, doy){
        spec <- fread(fname, header=FALSE)
        setnames(spec, c("Wavelength", dat_trait[sample_doy == doys[i], sample_id]))
        spec <- spec[Wavelength != 0]
        spec_mat <- as.matrix(spec) %>% t()
        if (all(is.na(spec_mat))) return(NULL)
        colnames(spec_mat) <- spec_mat['Wavelength',]
        return(spec_mat[-1,])
    }

    doys <- dat_trait[, unique(sample_doy)] %>% sort
    if (is_MV) {
        reflpath <- "raw/Yang_etal/2011-MV/"
    } else if (is_HF) {
        reflpath <- "raw/Yang_etal/2012_HF/ref/"
        transpath <- "raw/Yang_etal/2012_HF/tra/"
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
    
    refl_list <- refl_list[!sapply(refl_list, is.null)]
    refl_full <- do.call(rbind, refl_list)
    if (is_HF) {
        trans_list <- trans_list[!sapply(trans_list, is.null)]
        trans_full <- do.call(rbind, trans_list)
    }

    out <- list(traits = dat_trait, 
                reflectance = refl_full)
    if (is_HF) out$transmittance <- trans_full
    return (out)
}

#options(error = recover)
yang_mv <- readYang(2011, "MV")
saveRDS(yang_mv, file = "processed-spec-data/yangmv.rds")
yang_hf <- readYang(2012, "HF")
saveRDS(yang_hf, file = "processed-spec-data/yanghf.rds")

