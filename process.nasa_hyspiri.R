library(specprocess)
library(stringr)
source('common.R')

datapath <- '~/Projects/nasa_hyspiri'

projects <- tibble(projectcode = 'nasa_hyspiri',
                   projectdescription = 'NASA HyspIRI field campaign',
                   pointofcontact = 'Serbin, Shawn',
                   email = 'sserbin@bnl.gov') %>%
    db_merge_into(db = specdb, table = 'projects', values = .,
                  by = 'projectcode', id_colname = 'projectid')

file_list <- list.files(datapath, '^Averaged_Spectra.csv$', recursive = TRUE) %>%
    grep('canopy', ., ignore.case = TRUE, value = TRUE, invert = TRUE)

if (!exists('specdat_raw')) {
    specdat_raw <- lapply(file.path(datapath, file_list), read_csv) %>% 
        bind_rows(.id = 'file_number')
    specdat_raw %<>% 
        filter(!str_detect(Spectra, paste0('(?i)(canopy|bare|ground|',
                                           'dirt|road|litter|cwd|soil)'))) %>%
        mutate(specnum = row_number())
}

samples_raw <- specdat_raw %>% 
    select(-starts_with('Wave_')) %>%
    mutate(file_name = file_list[as.numeric(file_number)],
           file_name = str_replace(file_name, '/[Pp]rocessed.*$', ''),
           # Parse date from file_name
           Campaign = str_extract(file_name, '(?<=/)[[:alpha:]]+\\d{4}(?=/)'),
           colldate_raw = str_extract(file_name, '([[:digit:]]{4}201[[:digit:]])'),
           collectiondate = as.Date(colldate_raw, format = '%m%d%Y'),
           collectiondate = if_else(is.na(collectiondate) & 
                                    str_detect(file_name, '201[0-8][[:digit:]]{4}'), 
                                as.Date(str_extract(file_name, '201[0-8][[:digit:]]{4}'),
                                        format = '%Y%m%d'),
                                collectiondate),
           year = lubridate::year(collectiondate),
           year = if_else(is.na(year), 
                          as.numeric(str_extract(file_name, '201[0-8]')), 
                          year))

samples_raw %>% filter(is.na(year))

specstr <- samples_raw[['Spectra']]
speclist <- str_split(specstr, '_')
spec_length <- sapply(speclist, length)

table(spec_length)

sp5 <- do.call(rbind, speclist[spec_length == 5]) %>%
    as_data_frame %>%
    mutate(Spectra = specstr[spec_length == 5],
           site = V1,
           plot = str_extract(Spectra, 'Plot\\d+'),
           specnum = samples_raw$specnum[spec_length == 5],
           species = case_when(.$V2 %in% c('Grape', 'CADE27', 'QUDO') ~ .$V2,
                               .$V3 == 'Grape' ~ 'Grape',
                               TRUE ~ NA_character_),
           spectratype = case_when(str_detect(.$V5, '^[Rr]') ~ 'reflectance',
                                   str_detect(.$V5, '^[Tt]') ~ 'transmittance',
                                   TRUE ~ NA_character_),
           Sample_name = paste(V1, V2, V3, sep = '_'))

sp6 <- do.call(rbind, speclist[spec_length == 6]) %>%
    as_data_frame %>%
    mutate(Spectra = specstr[spec_length == 6],
           site = V1,
           specnum = samples_raw$specnum[spec_length == 6],
           species = case_when(.$V2 == 'Grape' ~ 'Grape',
                               !str_detect(.$V3, 'Bot|Top|Chem') ~ .$V3,
                               .$V2 == 'OrangeTree1' ~ 'OrangeTree',
                               TRUE ~ NA_character_),
           spectratype = case_when(str_detect(.$V6, '^[Rr]') ~ 'reflectance',
                                   str_detect(.$V6, '^[Tt]') ~ 'transmittance',
                                   TRUE ~ NA_character_),
           plot = V2,
           CanopyPosition = str_extract(V4, '(?<=^L\\d{1,2})(T|M|B)'),
           Sample_Name = paste(V1, V2, V3, V4, sep = '_'))

sp7 <- do.call(rbind, speclist[spec_length == 7]) %>%
    as_data_frame %>%
    mutate(Spectra = specstr[spec_length == 7],
           site = V1,
           plot = V2,
           species = V3,
           specnum = samples_raw$specnum[spec_length == 7],
           spectratype = case_when(str_detect(.$V6, '^[Rr]') ~ 'reflectance',
                                   str_detect(.$V6, '^[Tt]') ~ 'transmittance',
                                   TRUE ~ NA_character_),
           CanopyPosition = str_extract(V4, '(?<=^L\\d{1,2})(T|M|B)'),
           Sample_Name = paste(V1, V2, V3, V4, sep = '_'))

sp8 <- do.call(rbind, speclist[spec_length == 8]) %>%
    as_data_frame %>%
    mutate(Spectra = specstr[spec_length == 8],
           site = V1,
           plot = V2,
           species = V3,
           specnum = samples_raw$specnum[spec_length == 8],
           spectratype = case_when(str_detect(.$V7, '^[Rr]') ~ 'reflectance',
                                   str_detect(.$V7, '^[Tt]') ~ 'transmittance',
                                   TRUE ~ NA_character_),
           CanopyPosition = str_extract(V4, '(?<=^L\\d{1,2})(T|M|B)'),
           Sample_Name = paste(V1, V2, V3, V4, V5, sep = '_'))

spectra_meta <- bind_rows(sp5, sp6, sp7, sp8) %>%
    select(-matches('^V\\d+$')) %>%
    mutate(speciescode = recode(species,
                                `ImmatureOat` = 'AVENA',
                                `ImmatureOats` = 'AVENA',
                                `MatureOat` = 'AVENA',
                                `Almond` = 'PRDU',
                                `CAAN4sweet` = 'CAAN4',
                                `CAAN` = 'CAAN4',
                                `MandarinOrange` = 'CIRE3',
                                `OrangeTree` = 'CITRU2',
                                `Peach` = 'PRPE3',
                                `LemonTree` = 'CILI5',
                                `Grape` = 'VITIS',
                                `OakMistletoe` = 'PHLE14',
                                `NPV` = NA_character_),
           plot = case_when(is.na(.$plot) ~ NA_character_,
                            .$plot == 'na' ~ NA_character_,
                            .$plot == 'Grape' ~ NA_character_,
                            str_detect(.$plot, 'Chem\\d+') ~ NA_character_,
                            .$plot == 'TOWER' ~ 'Tower',
                            TRUE ~ .$plot)) %T>%
    glimpse

# TODO: Identify individual leaves
samples <- samples_raw %>% 
    inner_join(spectra_meta) %>%
    filter(!is.na(Sample_Name)) %>%
    mutate(projectcode = projects$projectcode,
           samplecode = paste(projectcode, paste(Sample_Name, Campaign, sep = '_'),
                              year, sep = '|'),
           sitecode = paste(projectcode, site, sep ='.'),
           plotcode = paste(sitecode, plot, sep = '.'),
           sunshade = recode(CanopyPosition,
                             `B` = 'shade',
                             `M` = 'shade',
                             `T` = 'sun',
                             .missing = NA_character_)) %>%
    group_by(samplecode) %>%
    mutate(collectiondate = mean(collectiondate, na.rm = TRUE)) %>%
    ungroup()

# TODO: Parse out exact coordinates
siteinfo <- readxl::read_excel(file.path(datapath, 'aaGPS_Locations', 
                                'NASA_HyspIRI_California_Project_GPS_Locations_v2.xlsx')) %>%
    group_by(Site) %>%
    summarize(latitude = mean(Latitude, na.rm = TRUE),
              longitude = mean(Longitude, na.rm = TRUE)) %>%
    mutate(sitecode = recode(Site,
                             `Kingsburg` = 'KING',
                             `Shafter` = 'SHAF',
                             `Russell Ranch` = 'RR'),
           sitecode = paste(projects$projectcode, sitecode, sep ='.'))

siteplot <- samples %>%
    distinct(sitecode, plotcode) %>%
    left_join(siteinfo) %T>%
    (. %>% distinct(sitecode) %>%
        db_merge_into(db = specdb, table = 'sites', values = .,
                      by = 'sitecode', id_colname = 'siteid')) %>%
    db_merge_into(db = specdb, table = 'plots', values = .,
                  by = c('sitecode', 'plotcode'), id_colname = 'plotid')

#mysp <- tbl(specdb, 'species') %>%
    #select(speciescode) %>%
    #collect()
#samples %>%
    #count(speciescode, sort = TRUE) %>%
    #anti_join(mysp)

samples %>%
    distinct(samplecode, projectcode, year, plotcode, speciescode, collectiondate) %>%
    db_merge_into(db = specdb, table = 'samples', values = .,
                  by = 'samplecode', id_colname = 'sampleid', return = FALSE)

## TODO: Fill in spec methods
#specmethods <- tribble(
    #~spectratype, ~apparatus, 
    #'reflectance', 'Leaf clip',
    #'transmittance', 'Integrating sphere') %>%
    #mutate(instrumentname = 

#DBI::dbGetQuery(specdb$con,
#'DELETE FROM spectra_info WHERE samplecode LIKE \'nasa_hyspiri%\'')

sample_condition <- samples %>%
    select(samplecode, CanopyPosition, sunshade) %>%
    gather(condition, conditionvalue, -samplecode, na.rm = TRUE) %>%
    db_merge_into(db = specdb, table = 'sample_condition', values = .,
                  by = c('samplecode', 'condition'), id_colname = 'conditiondataid')

spectra_info <- samples %>%
    select(samplecode, spectratype, specnum, Spectra) %>%
    mutate(spectracomment = paste(specnum, Spectra)) %>%
    db_merge_into(db = specdb, table = 'spectra_info', values = .,
                  by = c('samplecode', 'spectratype', 'spectracomment'),
                  id_colname = 'spectraid')

spectra_data <- specdat_raw %>%
    inner_join(spectra_info) %>%
    select(spectraid, starts_with('Wave_')) %>%
    gather(wave_raw, spectravalue, -spectraid) %>%
    mutate(wavelength = as.numeric(str_extract(wave_raw, '\\d+$'))) %>%
    write_spectradata

traits_raw <- readxl::read_excel(file.path(datapath, 'NASA_HyspIRI_CHN_Samples.xlsx')) %>%
    rename(year = Sample_Year)

trait_data <- traits_raw %>%
    inner_join(samples %>% distinct(Sample_Name, year, samplecode)) %>%
    rename(leaf_C_pct_mass = Perc_C,
           leaf_N_pct_mass = Perc_N,
           leaf_H_pct_mass = Perc_H) %>%
    mutate(leaf_CN_ratio_mass = leaf_C_pct_mass/leaf_N_pct_mass) %>%
    select(samplecode, starts_with('leaf_', ignore.case = FALSE)) %>%
    gather(trait, traitvalue, -samplecode, na.rm = TRUE) %>%
    db_merge_into(db = specdb, table = 'trait_data', values = .,
                  by = c('samplecode', 'trait'), id_colname = 'traitdataid')

