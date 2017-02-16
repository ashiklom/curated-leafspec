library(specprocess)
source('common.R')

#DBI::dbGetQuery(specdb$con,
#'DELETE FROM projects WHERE projectcode = \'foster_beetle\'')

datapath <- '~/Dropbox/NASA_TE_PEcAn-RTM_Project/Data/Foster_etal/SBMP2_contrem_all.csv'

projects <- tibble(
    projectcode = 'foster_beetle',
    projectdescription = 'Foster et al. 2017 (FEM) bark beetle spectra study',
    pointofcontact = 'Foster, Adrianna',
    email = 'acf7m@virginia.edu',
    doi = '10.1016/j.foreco.2016.11.004') %>%
    db_merge_into(db = specdb, table = 'projects', values = .,
                  by = 'projectcode', id_colname = 'projectid')

siteplot <- tibble(
    sitecode = 'foster_beetle.MP',
    sitedescription = paste('Monarch Pass,', 'Sawatch Range,', 
                            'Grand Mesa-Uncompahgre-Gunnison National Forests,',
                            'Salida, CO'),
    latitude = 38 + (30/60) + (10.08/3600),
    longitude = -106 - (20/60) - (8.1594/3600)) %>%
    mutate(plotcode = sitecode,
           plotdescription = sitedescription) %>%
    db_merge_into(db = specdb, table = 'sites', values = .,
                  by = 'sitecode', id_colname = 'siteid') %>%
    db_merge_into(db = specdb, table = 'plots', values = .,
                  by = 'plotcode', id_colname = 'plotid')

    
rawdat <- read_csv(datapath)

samples <- rawdat %>%
    select(TREEID, Height, Infested) %>%
    mutate(speciescode = 'PIEN',
           projectcode = projects$projectcode,
           plotcode = siteplot$plotcode,
           year = 2014,
           collectiondate = as.Date('2014-09-05'),
           spectra_name = paste(TREEID, Height, sep = '_'),
           samplecode = paste(projectcode, spectra_name, year, sep = '|')) %>%
    db_merge_into(db = specdb, table = 'samples', values = .,
                  by = 'samplecode', id_colname = 'sampleid')

sample_condition_info <- tibble(
    condition = 'bark_beetle_infested',
    conditiondescription = paste('Tree infested (yes/no) by spruce beetle', 
                                 '(Dendroctonus rufipennis (Kirby))'),
    conditioncomment = 'See Foster et al. (2017) FME') %>%
    db_merge_into(db = specdb, table = 'sample_condition_info', values = .,
                  by = 'condition', id_colname = 'conditionid')

sample_condition <- samples %>%
    mutate(bark_beetle_infested = recode(Infested, `Y` = 'yes', `N` = 'no'),
           CanopyPosition = recode(Height, `H` = 'M', `L` = 'B'),
           sunshade = 'shade') %>%
    select(samplecode, bark_beetle_infested, CanopyPosition, sunshade) %>%
    gather(condition, conditionvalue, -samplecode, na.rm = TRUE) %>%
    db_merge_into(db = specdb, table = 'sample_condition', values = .,
                  by = c('samplecode', 'condition'), id_colname = 'conditiondataid')

specmethods <- tibble(
    instrumentname = 'ASD FieldSpec Pro',
    apparatus = 'Leaf clip',
    specmethodcomment = 'Foster et al. (2017) FEM') %>%
    db_merge_into(db = specdb, table = 'instruments', values = .,
                  by = 'instrumentname', id_colname = 'instrumentid') %>%
    db_merge_into(db = specdb, table = 'specmethods', values = .,
                  by = c('instrumentid', 'apparatus', 'specmethodcomment'),
                  id_colname = 'specmethodid')

spectra_info <- samples %>%
    select(samplecode) %>%
    mutate(specmethodid = specmethods$specmethodid,
           spectratype = 'continuum-removed reflectance') %>%
    db_merge_into(db = specdb, table = 'spectra_info', values = .,
                  by = c('samplecode', 'spectratype'), id_colname = 'spectraid')

spectra_data <- rawdat %>%
    left_join(samples) %>%
    left_join(spectra_info) %>%
    select(spectraid, matches('^\\d+$')) %>%
    gather(wavelength, spectravalue, -spectraid) %>%
    mutate(wavelength = as.numeric(wavelength)) %>%
    write_spectradata

#testref <- PEcAnRTM::prospect(c(1.4, 30, 0.01, 0.01), 4, FALSE)[,1]
#tcr <- prospectr::continuumRemoval(testref, 400:2500, type = 'R')
#matplot(cbind(testref, tcr), type = 'l', col = c('red', 'blue'))

#library(ggplot2)
#ggplot(spectra_data) + 
    #aes(x = wavelength, y = spectravalue, color = spectraid) +
    #geom_line()
