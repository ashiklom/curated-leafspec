library(specprocess)
library(readxl)
source('common.R')

dbGetQuery(specdb$con, 'DELETE FROM projects WHERE projectcode = "ngee_tropics"')

projects <- tibble(
    projectcode = 'ngee_tropics',
    projectshortname = 'NGEE-Tropics',
    projectdescription = 'Next Generation Ecosystem Experiments (NGEE) - Tropics',
    pointofcontact = 'Serbin, Shawn',
    email = 'serbinsh@bnl.gov') %>% 
    write_project()

path_ngt <- '~/Dropbox/NASA_TE_PEcAn-RTM_Project/Data/NGEE-Tropics'

namesdict <- c("LWP_bar" = "leaf_water_potential",
               "Species" = "RawSpecies", 
               "Location" = "Site")

Location <- 'Panama'
SampleYear <- 2016

path_year <- file.path(path_ngt, paste(Location, SampleYear, sep = "_"))
chemfile <- list.files(path_year, "Leaf_Samples", full.names = TRUE)

chemdat <- read_excel(chemfile, sheet = 4) %>%
    setDT() %>%
    mutate(LWP_bar = as.numeric(LWP_bar)) %>% 
    mutate_if(is.numeric, na_if, y = -9999) %>%
    rename(speciesdatacode = Species, 
           SampleName = Barcode,
           canopyposition = Canopy_position) %>%
    mutate(leaf_water_potential = ud.convert(LWP_bar, 'bar', 'Pa')) %>%
    select(SampleName, speciesdatacode, canopyposition,
           starts_with('leaf_', ignore.case = FALSE))

specfile <- list.files(path_year, "leaf_spectra",
                        full.names = TRUE)

spectra_raw <- read_excel(specfile) %>%
    setDT() %>%
    filter(!(Spectra == "BNL11815" & Site == "PNM")) %>%
    mutate(collectiondate = as.Date(strptime(Date, '%Y%m%d')),
            sitecode = paste(projects[['projectcode']], Site, sep = '.')) %>%
    select(SampleName = Spectra, sitecode, collectiondate,
            Instrument, starts_with('Wave_'))

#spectra_raw %>%
    #select(-starts_with('Wave_')) %>%
    #glimpse()

# NOTE: 20 data points have no species codes associated with them

samples <- chemdat %>%
    select(-canopyposition, -starts_with('leaf_')) %>%
    full_join(select(spectra_raw, -starts_with('Wave_'), -Instrument)) %>%
    mutate(year = SampleYear,
           projectcode = projects$projectcode,
           samplecode = paste(projectcode, SampleName, year, sep = '|'),
           plotcode = sitecode) %>%
    left_join(read_csv('data/species_dict/ngee_tropics_species_dict.csv') %>% setDT())

#samples %>%
    #filter(is.na(speciescode), !is.na(speciesdatacode)) %>%
    #distinct(speciesdatacode)

sitelatlon <- tribble(
    ~sitecode, ~latitude, ~longitude,
    'NA', NA, NA,
    # NOTE: Approximate coordinates based on Google Maps location of San 
    # Lorenzo Protected Forest;  need more precise ones from Shawn
    'PNM', 9.25, -79.99,
    'SanLorenzo', 9.25, -79.99) %>%
    setDT()

sites <- samples %>%
    distinct(projectcode, sitecode) %>%
    write_sites()

plots <- samples %>%
    distinct(sitecode, plotcode) %>%
    left_join(sitelatlon) %>%
    write_plots()

samples <- db_merge_into(db = specdb, table = 'samples', values = samples, by = 'samplecode')

sample_condition_info <- tibble(
    condition = 'sunshade',
    conditiondescription = 'Whether leaf is sunlit (sun; same as canopyposition "top") or shaded (shade; same as canopyposition "mid" or "bot")') %>%
    db_merge_into(db = specdb, table = 'sample_condition_info', values = ., by = 'condition')

sample_condition <- chemdat %>%
    distinct(SampleName, canopyposition) %>%
    mutate(sunshade = recode(canopyposition, 
                             sunlit = 'sun',
                             understory = 'shade')) %>%
    select(-canopyposition) %>%
    melt(id.vars = 'SampleName', variable.name = 'condition',
         value = 'conditionvalue') %>%
    left_join(select(samples, SampleName, samplecode)) %>%
    select(-SampleName) %>%
    db_merge_into(db = specdb, table = 'sample_condition', values = .,
                  by = c('samplecode', 'condition'))

trait_data <- chemdat %>%
    left_join(select(samples, samplecode, SampleName)) %>%
    select(samplecode, starts_with('leaf_', ignore.case = FALSE)) %>%
    melt(id.vars = 'samplecode', variable.name = 'trait',
         value.name = 'traitvalue', na.rm = TRUE)

trait_info <- trait_data %>%
    distinct(trait) %>%
    mutate(unit = case_when(trait == 'leaf_water_potential' ~ 'Pa', 
                            TRUE ~ NA_character_)) %>%
    db_merge_into(db = specdb, table = 'trait_info', values = ., by = 'trait')

trait_data <- db_merge_into(db = specdb, table = 'trait_data', values = trait_data,
                            by = c('samplecode', 'trait'))

# TODO: Add more methods information
specmethods <- tibble(instrumentcode = 'svc-hr-1024i',
                      specmethodcode = 'ngee_tropics-method') %>% 
    db_merge_into(db = specdb, table = 'specmethods', values = ., by = 'specmethodcode')

spectra_info <- spectra_raw %>%
    select(SampleName) %>% 
    mutate(instrumentcode = specmethods[['instrumentcode']]) %>%
    left_join(select(samples, samplecode, SampleName)) %>%
    left_join(specmethods %>% setDT()) %>%
    mutate(spectratype = 'reflectance') %>%
    db_merge_into(db = specdb, table = 'spectra_info', values = .,
                  by = c('samplecode', 'spectratype'), id_colname = 'spectraid')

spectra_data <- spectra_raw %>%
    select(SampleName, starts_with('Wave_')) %>%
    left_join(select(samples, samplecode, SampleName)) %>%
    left_join(spectra_info) %>%
    select(spectraid, starts_with('Wave_')) %>%
    melt(id.vars = 'spectraid', variable.name = 'wavelength', 
         value.name = 'spectravalue') %>%
    mutate(wavelength = as.numeric(gsub('Wave_', '', wavelength))) %>%
    write_spectradata

# Useful species list from Smithsonian Tropical Research Institute:
# http://www.stri.si.edu/sites/esp/tesp/plant_species.htm
