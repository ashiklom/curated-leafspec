library(specprocess)
source('common.R')

project_code <- "divittorio_conifer"
projpath <- "~/Dropbox/NASA_TE_PEcAn-RTM_Project/Data/DiVittorio_confier/needle_data_share"

# Get all spec file names
# Commented-out directories are redundant ones
include_dirs <- c('sgf_oz',
#                  'sgf_oz1',
#                  'sgf_oz2',
                  'sgf_gr',
#                  'sgf_gr1',
#                  'sgf_gr2',
                  'sgf_wf',
#                  'sgf_wf1',
#                  'sgf_wf2',
                  'sqa_t1',
                  'sqa_t2',
                  'sqa_t3',
                  'sqa_t4',
                  'sqa_gr',
                  'sqa_wf',
                  'sqa_scale',
                  'sqa_suck',
                  'qcy_gr',
                  'qcy_wf',
                  'qcy_t1',
                  'qcy_t2',
                  'qcy_t3',
                  'qcy_t4')

namesdict <- c("chla" = "leaf_chla_per_area",
               "chlb" = "leaf_chlb_per_area",
               "tchl" = "leaf_chltot_per_area",
               "carot" = "leaf_cartot_per_area")

site_spec <- c("qcy" = "Plumas National Forest",
               "sgf" = "Giant Forest",
               "sqa" = "Mineral King")

site_chem <- c("pnf" = "Plumas National Forest",
               "gf" = "Giant Forest",
               "mk" = "Mineral King")

cond <- c("oz" = "ozone",
          "oz1" = "ozone",
          "oz2" = "ozone",
          "gr" = "green",
          "wf" = "winter_fleck",
          "scale" = "scale_insect",
          "suck" = "sucking_insect",
          "rand" = "random",
          "t1" = "1", 
          "t2" = "2",
          "t3" = "3",
          "t4" = "4")

# NOTE: Only using the top spectra 
specfiles <- list.files(file.path(projpath, include_dirs), 
                        pattern = "_top_.*.txt", 
                        full.names = TRUE) %>%
    grep("_[12]d_", ., value = TRUE, invert = TRUE)

specregex <- '(^[[:alpha:]]+)_([[:alnum:]]+)_top_s([[:digit:]]+$)'

specdat <- data.table(projectcode = project_code,
                      specfile = specfiles,
                      year = 2009) %>%
    .[, rawstring := gsub('.txt', '', basename(specfile))] %>%
    .[, rawsite_spec := gsub(specregex, "\\1", rawstring)] %>%
    .[, rawcond_spec := gsub(specregex, "\\2", rawstring)] %>%
    .[, specnum := as.integer(gsub(specregex, "\\3", rawstring))] %>%
    .[, Site := site_spec[rawsite_spec]] %>%
    .[, OtherCondition := cond[rawcond_spec]] %>%
    .[, treenum := as.integer(OtherCondition)] %>%
    .[OtherCondition %in% c('1','2','3','4'), OtherCondition := 'random'] %>%
    .[Site == 'Giant Forest' & OtherCondition == 'ozone' & specnum <= 25,
      treenum := 1] %>%
    .[Site == 'Giant Forest' & OtherCondition == 'ozone' & specnum > 25,
      treenum := 2] %>%
    .[Site == 'Giant Forest' & OtherCondition == 'ozone',
      specnum := 1:.N, by = treenum]

chempath <- file.path(projpath, "all_pig.txt")

chemraw <- read.table(chempath, header = TRUE,
                      stringsAsFactors = FALSE)

head_inds <- as.numeric(chemraw[,1]) %>% 
    is.na %>% 
    which %>% 
    c(nrow(chemraw) + 1)

chemlist <- list()

for (i in seq_along(c(head_inds[-1]))) {
    h <- head_inds[i]
    a <- head_inds[i] + 1
    b <- head_inds[i+1] - 1
    info <- unique(unlist(chemraw[h,]))
    stopifnot(length(info) == 1)
    dat <- chemraw[a:b,] %>% 
        lapply(as.numeric) %>%
        as.data.table %>%
        .[, info := info]
    chemlist[[i]] <- dat
}

chemdat <- rbindlist(chemlist) %>%
    setnames(names(namesdict), namesdict) %>%
    .[, rawsite := gsub("(.*)-(.*)", "\\1", info)] %>%
    .[, rawcond := gsub("(.*)-(.*)", "\\2", info)] %>%
    .[, Site := site_chem[rawsite]] %>%
    .[, OtherCondition := cond[rawcond]] %>%
    .[rawcond == 'oz1', treenum := 1] %>%
    .[rawcond == 'oz2', treenum := 2] %>%
    .[OtherCondition == "random", treenum := rep(1:4, each = 32)] %>%
    .[, specnum := 1:.N, by = .(info, treenum)] %>%
    .[grep('oz(1|2)', rawcond), OtherCondition := 'ozone']

alldat <- full_join(specdat, chemdat) %>%
    mutate_at(vars(starts_with('leaf_')), ud.convert, 
              u1 = 'ug cm-2', u2 = 'kg m-2') %>%
    mutate(samplecode = paste(projectcode, 
                              paste(rawsite, rawcond, treenum, specnum,
                                    sep = '-'), 
                              year, sep = '|'),
           speciescode = case_when(Site %in% c('Plumas National Forest',
                                               'Mineral King') ~ 'PIPO',
                                   Site == 'Giant Forest' ~ 'PIJE'),
           sitecode = paste(projectcode, rawsite, sep = '.'),
           plotcode = sitecode) %>%
    select(samplecode, speciescode, sitecode, plotcode, projectcode, year,
           sitedescription = Site, dv_needle_condition = OtherCondition,
           specfile, starts_with('leaf_'))
glimpse(alldat)

sites <- alldat %>%
    distinct(sitecode, sitedescription) %>%
    db_merge_into(db = specdb, table = 'sites', values = .,
                  by = c('sitecode', 'sitedescription'), id_colname = 'siteid')

plots <- tribble(
    ~sitedescription, ~latitude, ~longitude,
    'Giant Forest', 36.58, -118.77,
    'Plumas National Forest', 39.78, -120.98,
    'Mineral King', 36.45, -118.59) %>% 
    left_join(sites) %>%
    mutate(plotcode = sitecode,
           plotdescription = sitedescription) %>%
    db_merge_into(db = specdb, table = 'plots', values = ., 
                  by = c('plotcode', 'plotdescription'), id_colname = 'plotid')

samples <- alldat %>%
    select(samplecode, speciescode, sitecode, plotcode,
           projectcode, year) %>%
    db_merge_into(db = specdb, table = 'samples', values = .,
                  by = 'samplecode', id_colname = 'sampleid')
    
sample_condition <- alldat %>%
    select(samplecode, dv_needle_condition) %>%
    melt(id.vars = 'samplecode', variable.name = 'condition',
         value.name = 'conditionvalue', na.rm = TRUE)

sample_condition_info <- sample_condition %>%
    distinct(condition) %>%
    mutate(conditiondescription = paste('Needle damage condition.',
                                        '"green" indicates healthy needles.', 
                                        'See "Di Vittorio 2009 J. Environ. Qual."',
                                        'for more info.')) %>%
    db_merge_into(db = specdb, table = 'sample_condition_info', values = .,
                  by = 'condition', id_colname = 'conditionid')

sample_condition <- db_merge_into(db = specdb, table = 'sample_condition', 
                                  values = sample_condition,
                                  by = 'samplecode', id_colname = 'conditiondataid')

trait_data <- alldat %>%
    select(samplecode, starts_with('leaf_')) %>%
    melt(id.vars = 'samplecode', variable.name = 'trait',
         value.name = 'traitvalue', na.rm = TRUE)

trait_info <- trait_data %>%
    distinct(trait) %>%
    mutate(unit = 'kg m-2') %>%
    db_merge_into(db = specdb, table = 'trait_info', values = .,
                  by = 'trait', id_colname = 'traitid')

trait_data <- db_merge_into(db = specdb, table = 'trait_data', 
                            values = trait_data, by = 'samplecode', 
                            id_colname = 'traitdataid')

# Load spectra as a list
spec_list <- list()
for (i in seq_len(nrow(alldat))) {
    code <- alldat[i, samplecode]
    specfile <- alldat[i, specfile]
    spec <- fread(specfile)
    setnames(spec, c('Wavelength', 'Reflectance', 'Transmittance'))
    # Downsample to PROSPECT resolution
    minx <- ceiling(min(spec[, Wavelength]))
    maxx <- floor(max(spec[, Wavelength]))
    newx <- minx:maxx
    new_refl <- select(spec, Wavelength, Reflectance) %>%
        spline(x = .[,Wavelength], y = .[,Reflectance], xout = newx) %>%
        as.data.table %>%
        rename(wavelength = x, spectravalue = y) %>%
        mutate(samplecode = code, spectratype = 'reflectance')
    new_trans <- select(spec, Wavelength, Transmittance) %>%
        spline(x = .[,Wavelength], y = .[,Transmittance], xout = newx) %>%
        as.data.table %>%
        rename(wavelength = x, spectravalue = y) %>%
        mutate(samplecode = code, spectratype = 'transmittance')
    spec_list[[i]] <- rbind(new_refl, new_trans)
}

spectra_raw <- rbindlist(spec_list)

spectra_info <- spectra_raw %>%
    distinct(samplecode, spectratype) %>%
    db_merge_into(db = specdb, table = 'spectra_info', values = .,
                  by = c('samplecode', 'spectratype'), id_colname = 'spectraid')

spectra_data <- spectra_raw %>%
    left_join(spectra_info %>% select(samplecode, spectratype, spectraid)) %>%
    select(spectraid, wavelength, spectravalue) %>%
    write_spectradata
    
