library(specprocess)
source('common.R')

datapath <- '~/Dropbox/NASA_TE_PEcAn-RTM_Project/Data/Jin_Wu_Brazil_Data'

DBI::dbGetQuery(specdb$con, 'DELETE FROM projects WHERE projectcode == "wu_brazil"')

projects <- tibble(projectcode = 'wu_brazil',
                   projectshortname = 'Wu et al. 2016 New Phyt.',
                   projectdescription = 'Wu et al. 2016 New Phytologist Brazil canopy traits study',
                   pointofcontact = 'Wu, Jin',
                   email = 'jinwu@bnl.gov',
                   doi = '10.1111/nph.14051') %>%
    write_project()

specdata <- read_csv(file.path(datapath, 'Brazil_ASD_Leaf_Spectra_filter_v1.csv')) %>%
    mutate(wavelength = as.numeric(gsub(' nm', '', Wavelength))) %>%
    select(-Wavelength) %>%
    gather(key = Leaf_Number, value = spectravalue, -wavelength)

species_data <- readxl::read_excel(file.path(datapath, 'Brazil_Species_Name.xlsx')) %>% 
    mutate(Leaf_Number = paste0('L', row_number())) %>% 
    left_join(read_csv('data/species_dict/wu_brazil_species_dict.csv'))

traitdata <- read_csv(file.path(datapath, 'Brazil_Trait_Data_filter_v1.csv')) %>%
    left_join(species_data) %>% 
    mutate(collectiondate = lubridate::mdy(Day),
           year = lubridate::year(collectiondate),
           leaf_mass_per_area = 1/SLA_m2_kg,
           leaf_water_thickness = Water_Perc * leaf_mass_per_area,
           CanopyPosition = recode(Light_Environment,
                                   `3` = 'T',
                                   `2` = 'M',
                                   `1` = 'B'),
           CompleteLeaf = recode(Complete_Leaf, 
                                 `1` = 'complete',
                                 `2` = 'non-complete'),
           LeafAge = recode(Leaf_Age,
                            `1` = 'young',
                            `2` = 'mature',
                            `3` = 'old'),
           sunshade = if_else(CanopyPosition == 'T', 'sun', 'shade'),
           projectcode = projects[['projectcode']],
           samplecode = paste(projectcode, Leaf_Number, year, sep = '|')
           ) %>% 
    select(samplecode, speciescode, Leaf_Number, leaf_mass_per_area, leaf_water_thickness,
           CanopyPosition, CompleteLeaf, LeafAge, sunshade)


siteplot <- tribble(
    ~sitecode, ~latitude, ~longitude,
    'Brazil', (-2 - 51/60), (-54 - 58/60)) %>%
    mutate(projectcode = projects$projectcode, plotcode = sitecode) %>%
    write_sites() %>% 
    write_plots()

# TODO: Fill in missing species

samples <- traitdata %>%
    distinct(samplecode, speciescode) %>%
    mutate(projectcode = projects$projectcode,
           plotcode = siteplot$plotcode) %>%
    db_merge_into(db = specdb, table = 'samples', values = ., by = 'samplecode')

specmethods <- tibble(
    specmethodcode = 'chavana-bryant-2016',
    instrumentcode = 'asd-fspro',
    calibration = 'Spectralon ratio',
    specmethodcomment = 'Chavana-Bryant et al. 2016; doi:10.1111/nph.13853') %>%
    db_merge_into(db = specdb, table = 'specmethods', values = ., by = 'specmethodcode')

spectra_info <- specdata %>%
    left_join(traitdata %>% distinct(Leaf_Number, samplecode)) %>% 
    distinct(samplecode) %>%
    mutate(specmethodcode = specmethods[['specmethodcode']],
           spectratype = 'reflectance') %>%
    db_merge_into(db = specdb, table = 'spectra_info', values = .,
                  by = c('samplecode', 'spectratype'), id_colname = 'spectraid')

spectra_data <- specdata %>%
    left_join(traitdata %>% distinct(Leaf_Number, samplecode)) %>% 
    left_join(spectra_info) %>%
    write_spectradata
    
sample_condition <- traitdata %>%
    select(samplecode, CompleteLeaf, LeafAge, sunshade) %>%
    gather(condition, conditionvalue, -samplecode)

sample_condition_info <- sample_condition %>%
    distinct(condition) %>%
    db_merge_into(db = specdb, table = 'sample_condition_info', values = .,
                  by = 'condition')

sample_condition <- db_merge_into(db = specdb, table = 'sample_condition',
                                  values = sample_condition, 
                                  by = c('samplecode', 'condition'))

trait_data <- traitdata %>%
    select(samplecode, starts_with('leaf_', ignore.case = FALSE)) %>%
    gather(trait, traitvalue, -samplecode) %T>%
    (. %>% distinct(trait) %>%
        db_merge_into(db = specdb, table = 'trait_info', values = ., by = 'trait')) %>%
    db_merge_into(db = specdb, table = 'trait_data', values = .,
                  by = c('samplecode', 'trait'))
