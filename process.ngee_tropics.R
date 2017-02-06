library(specprocess)
library(readxl)
specdb <- src_postgres('leaf_spectra')

project_code <- "ngee_tropics"

path_ngt <- "data/ngee_tropics/"

namesdict <- c("LWP_bar" = "leaf_water_potential",
               "Species" = "RawSpecies", 
               "Location" = "Site")

Location <- 'Panama'
SampleYear <- 2016

path_year <- file.path(path_ngt, paste(Location, SampleYear, sep = "_"))
chemfile <- list.files(path_year, "Leaf_Samples", full.names = TRUE)

chemdat <- read_excel(chemfile, sheet = 4) %>%
    setDT() %>%
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
            sitecode = paste(project_code, Site, sep = '.')) %>%
    select(SampleName = Spectra, sitecode, collectiondate,
            Instrument, starts_with('Wave_'))

#spectra_raw %>%
    #select(-starts_with('Wave_')) %>%
    #glimpse()

samples <- chemdat %>%
    select(-canopyposition, -starts_with('leaf_')) %>%
    full_join(select(spectra_raw, -starts_with('Wave_'), -Instrument)) %>%
    mutate(year = SampleYear,
           projectcode = project_code,
           samplecode = paste(projectcode, SampleName, year, sep = '|'),
           sitecode = if_else(is.na(sitecode),
                              paste(projectcode, sitecode, sep = '.'),
                              sitecode),
           plotcode = sitecode) %>%
    left_join(tbl(specdb, 'species_dict') %>% 
              filter(projectcode == project_code) %>% 
              select(speciesdatacode, speciescode) %>%
              collect() %>%
              setDT()) %>%
    select(-speciesdatacode)

sites <- samples %>%
    distinct(sitecode) %>%
    db_merge_into(db = specdb, table = 'sites', values = .,
                  by = 'sitecode', id_colname = 'siteid')

plots <- samples %>%
    distinct(sitecode, plotcode) %>%
    db_merge_into(db = specdb, table = 'plots', values = .,
                  by = 'plotcode', id_colname = 'plotid')

samples <- db_merge_into(db = specdb, table = 'samples', values = samples,
                            by = 'samplecode', id_colname = 'sampleid')

sample_condition_info <- tibble(
    condition = 'sunshade',
    conditiondescription = 'Whether leaf is sunlit (sun; same as canopyposition "top") or shaded (shade; same as canopyposition "mid" or "bot")') %>%
    db_merge_into(db = specdb, table = 'sample_condition_info', values = .,
                  by = c('condition', 'conditiondescription'), id_colname = 'conditionid')

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
                  by = c('samplecode', 'condition'), id_colname = 'conditiondataid')

trait_data <- chemdat %>%
    left_join(select(samples, samplecode, SampleName)) %>%
    select(samplecode, starts_with('leaf_', ignore.case = FALSE)) %>%
    melt(id.vars = 'samplecode', variable.name = 'trait',
         value.name = 'traitvalue', na.rm = TRUE)

trait_info <- trait_data %>%
    distinct(trait) %>%
    mutate(unit = case_when(trait == 'leaf_water_potential' ~ 'Pa', 
                            TRUE ~ NA_character_)) %>%
    db_merge_into(db = specdb, table = 'trait_info', values = .,
                  by = 'trait', id_colname = 'traitid')

trait_data <- db_merge_into(db = specdb, table = 'trait_data', values = trait_data,
                            by = c('samplecode', 'trait'), id_colname = 'traitdataid')

instruments <- spectra_raw %>%
    distinct(Instrument) %>%
    rename(instrumentname = Instrument) %>%
    db_merge_into(db = specdb, table = 'instruments', values = .,
                  by = 'instrumentname', id_colname = 'instrumentid')

# TODO: Add more methods information
specmethods <- instruments %>%
    mutate(specmethodcomment = 'NGEE Tropics') %>%
    db_merge_into(db = specdb, table = 'specmethods', values = .,
                  by = c('instrumentid', 'specmethodcomment'),
                  id_colname = 'specmethodid')

spectra_info <- spectra_raw %>%
    select(SampleName, instrumentname = Instrument) %>%
    left_join(select(samples, samplecode, SampleName)) %>%
    left_join(specmethods) %>%
    mutate(spectratype = 'reflectance') %>%
    db_merge_into(db = specdb, table = 'spectra_info', values = .,
                  by = 'samplecode', id_colname = 'spectraid')

spectra_data <- spectra_raw %>%
    select(SampleName, starts_with('Wave_')) %>%
    left_join(select(samples, samplecode, SampleName)) %>%
    left_join(spectra_info %>% select(samplecode, spectraid)) %>%
    select(spectraid, starts_with('Wave_')) %>%
    melt(id.vars = 'spectraid', variable.name = 'wavelength', 
         value.name = 'spectravalue') %>%
    mutate(wavelength = as.numeric(gsub('Wave_', '', wavelength)))

mrg <- db_merge_into(db = specdb, table = 'spectra_data', values = spectra_data, 
                     by = 'spectraid', id_colname = 'spectradataid',
                     return = FALSE, backend = 'psql_copy')

