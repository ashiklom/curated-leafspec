library(specprocess)
library(stringr)
library(forcats)
source('common.R')

DBI::dbGetQuery(specdb$con, 'DELETE FROM projects WHERE projectcode = "nasa_hyspiri"')

datapath <- '~/Projects/nasa_hyspiri'

projects <- tibble(projectcode = 'nasa_hyspiri',
                   projectshortname = 'NASA HyspIRI',
                   projectdescription = 'NASA HyspIRI field campaign',
                   pointofcontact = 'Serbin, Shawn',
                   email = 'sserbin@bnl.gov') %>%
    write_project()

rdata_path <- '~/Dropbox/NASA_TE_PEcAn-RTM_Project/Data/NASA_HyspIRI/NASA_HyspIRI_Compiled_Field_Data.RData'
load(rdata_path)

raw_spectra <- as_data_frame(hyspiri_leaf_spectra) %>% 
    mutate(Spectra = as.character(Spectra),
           specnum = row_number())
raw_sla <- as_data_frame(SLA_and_Spectra) %>% 
    mutate(Spectra = as.character(Spectra),
           Sample_Name = as.character(Sample_Name))
raw_chn <- as_data_frame(chn.data) %>% 
    rename(speciescode = USDA_Species_Code) %>% 
    mutate(collectiondate = lubridate::mdy(Sample_Date))

# Match up spectra
sla_samples_raw <- raw_sla %>% 
    select(-starts_with('Wave')) %>% 
    mutate(collectiondate = lubridate::ymd(Measurement_Date),
           # Fix mislabeled instrument
           Instrument = if_else(str_detect(Spectra, 'SJJR_Tower_PICO3_ELKCP4') & Instrument == 'FS4',
                                'FS3', as.character(Instrument)),
           instrumentcode = fct_recode(Instrument, 
                                       `asd-fs3` = 'FS3',
                                       `asd-fs4` = 'FS4',
                                       `se-psm3500` = 'SE')) %>% 
    select(-Measurement_Date) %>% 
    # Average out duplicate measurements
    group_by(Sample_Name, Spectra, collectiondate, instrumentcode) %>% 
    summarize_if(is.numeric, mean) %>% 
    ungroup()

samples_raw <- raw_spectra %>% 
    select(-starts_with('Wave', ignore.case = FALSE)) %>% 
    mutate(collectiondate = lubridate::ymd(Sample_Date),
           year = lubridate::year(collectiondate),
           instrumentcode = fct_recode(Instrument,
                                       `asd-fs3` = 'ASD_FS3',
                                       `asd-fs4` = 'ASD_FS4',
                                       `se-psm3500` = 'SpecEvo_PSM3500')
           ) %>% 
    select(-Sample_Date, -Instrument)

# This should be empty. That indicates that there are no SLA samples not in spectra
#anti_join(sla_samples_raw, samples_raw)

specstr <- samples_raw[['Spectra']]
speclist <- str_split(specstr, '_')
spec_length <- sapply(speclist, length)

table(spec_length)

sp4 <- do.call(rbind, speclist[spec_length == 4]) %>% 
    as_data_frame %>% 
    mutate(Spectra = specstr[spec_length == 4],
           site = V1,
           plot = V2,
           species = case_when(str_detect(.$V2, "OakMistletoe") ~ 'PHLE14',
                               TRUE ~ NA_character_),
           spectratype = case_when(str_detect(.$V4, '^[Rr]') ~ 'reflectance',
                                   str_detect(.$V4, '^[Tt]') ~ 'transmittance',
                                   TRUE ~ NA_character_),
           specnum = samples_raw$specnum[spec_length == 4],
           Sample_Name = paste(V1, V2, sep = '_')
           )

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
           Sample_Name = paste(V1, V2, V3, sep = '_'))

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

spectra_meta <- bind_rows(sp4, sp5, sp6, sp7) %>% 
    select(-matches('^V\\d+$')) %>%
    # Remove non-plant spectra
    filter(!species %in% c('BareField', 'LitterNeedles', 'NPV', 'Soil',
                           'CWD1', 'CWD2', NA)) %>% 
    mutate(speciescode = recode(species,
                                `ImmatureOat` = 'AVENA',
                                `ImmatureOats` = 'AVENA',
                                `MatureOat` = 'AVENA',
                                `Almond` = 'PRDU',
                                `CAAN4sweet` = 'CAAN4',
                                `CAAN` = 'CAAN4',
                                `MandarinOrange` = 'CIRE3',
                                `Orange` = 'CITRU2',
                                `OrangeTree` = 'CITRU2',
                                `Peach` = 'PRPE3',
                                `LemonTree` = 'CILI5',
                                `LemonTrees` = 'CILI5',
                                `Grape` = 'VITIS',
                                `OakMistletoe` = 'PHLE14'),
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
    rename(sitecode = site) %>% 
    mutate(projectcode = projects[['projectcode']],
           samplecode = paste(projectcode, paste(Sample_Name, collectiondate, sep = '_'),
                              year, sep = '|'),
           plotcode = paste(sitecode, plot, sep = '.'),
           sunshade = recode(CanopyPosition,
                             `B` = 'shade',
                             `M` = 'shade',
                             `T` = 'sun',
                             .missing = NA_character_))
    
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
           projectcode = projects[['projectcode']])

siteplot <- samples %>%
    distinct(sitecode, plotcode) %>%
    left_join(siteinfo) %T>%
    (. %>% distinct(sitecode) %>% write_sites()) %>% 
    write_plots()

#mysp <- tbl(specdb, 'species') %>%
    #select(speciescode) %>%
    #collect()
#samples %>%
    #count(speciescode, sort = TRUE) %>%
    #anti_join(mysp)

samples %>%
    distinct(samplecode, projectcode, year, plotcode, speciescode, collectiondate) %>%
    db_merge_into(db = specdb, table = 'samples', values = .,
                  by = 'samplecode', return = FALSE)

## TODO: Fill in spec methods
#specmethods <- tribble(
#    ~spectratype, ~apparatus, 
#    'reflectance', 'Leaf clip',
#    'transmittance', 'Integrating sphere') %>%
#    mutate(instrumentname = 

# DBI::dbGetQuery(specdb$con,
# 'DELETE FROM spectra_info WHERE samplecode LIKE \'nasa_hyspiri%\'')

sample_condition <- samples %>%
    distinct(samplecode, CanopyPosition, sunshade) %>%
    rename(canopyposition = CanopyPosition) %>% 
    gather(condition, conditionvalue, -samplecode, na.rm = TRUE) %>%
    db_merge_into(db = specdb, table = 'sample_condition', values = ., by = c('samplecode', 'condition'))

spectra_info <- samples %>%
    select(samplecode, spectratype, specnum, Spectra) %>%
    mutate(spectracomment = paste(specnum, Spectra)) %>%
    db_merge_into(db = specdb, table = 'spectra_info', values = .,
                  by = c('samplecode', 'spectratype', 'spectracomment'),
                  id_colname = 'spectraid')

spectra_data <- raw_spectra %>%
    inner_join(spectra_info) %>%
    select(spectraid, starts_with('Wave_')) %>%
    gather(wave_raw, spectravalue, -spectraid) %>%
    mutate(wavelength = as.numeric(str_extract(wave_raw, '\\d+$'))) %>%
    write_spectradata

raw_chn %>% select(Sample_Name) %>% anti_join(samples)

samples %>% 
    filter(str_detect(Sample_Name, 'SJJR_Tower_QUCH2_T')) %>% 
    select(Sample_Name)

convert_unit <- function(trait, value) {
    case_when(trait == 'SLA_cm2_g_DW', udunits2::ud.convert(value, 'cm2 g-1', ''))
}

sla_averaged <- sla_samples_raw %>% 
    inner_join(samples %>% distinct(samplecode, Sample_Name, collectiondate)) %>% 
    group_by(samplecode) %>% 
    summarize(leaf_mass_per_area = mean(udunits2::ud.convert(LMA_gDW_m2, 'g m-2', 'kg m-2'))) %>% 
    ungroup() %>% 
    gather(trait, traitvalue, -samplecode, na.rm = TRUE)


trait_data <- raw_chn %>% 
    mutate(Sample_Name = str_replace(Sample_Name, '_LC(_RG)?$', ''),
           Sample_Name = if_else(Sample_Name == 'SCRec_Plot9_PEAM3_ELK6T6sun2',
                                 paste0(Sample_Name, 'time2'), Sample_Name),
           Sample_Name = str_replace(Sample_Name, 'TOWER', 'Tower')) %>% 
    select(Sample_Name, collectiondate, 
           leaf_C_pct_mass = Perc_C, 
           leaf_H_pct_mass = Perc_H,
           leaf_N_pct_mass = Perc_N) %>% 
    mutate(leaf_CN_ratio_mass = leaf_C_pct_mass/leaf_N_pct_mass) %>% 
    inner_join(samples %>% distinct(samplecode, Sample_Name, collectiondate)) %>% 
    # Keep only unambiguous matches
    group_by(Sample_Name, collectiondate) %>% 
    mutate(n = n()) %>% 
    ungroup() %>% 
    filter(n == 1) %>% 
    select(samplecode, starts_with('leaf_', ignore.case = FALSE)) %>%
    gather(trait, traitvalue, -samplecode, na.rm = TRUE) %>% 
    bind_rows(sla_averaged)

#trait_data %>% count(samplecode)

trait_info <- trait_data %>% 
    distinct(trait) %>% 
    mutate(unit = case_when(str_detect(.$trait, 'pct_mass') ~ '%',
                            .$trait == 'leaf_mass_per_area' ~ 'kg m-2',
                            TRUE ~ NA_character_)) %>% 
    db_merge_into(db = specdb, table = 'trait_info', values = ., by = 'trait')

db_merge_into(db = specdb, table = 'trait_data', values = trait_data, 
              by = c('samplecode', 'trait'), return = FALSE)

