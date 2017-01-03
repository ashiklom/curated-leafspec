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
    rename(datacode = Species, 
           SampleName = Barcode,
           canopyposition = Canopy_position) %>%
    mutate(leaf_water_potential = ud.convert(LWP_bar, 'bar', 'Pa')) %>%
    select(SampleName, datacode, canopyposition,
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

spectra_raw %>%
    select(-starts_with('Wave_')) %>%
    glimpse()

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
              select(datacode, speciescode) %>%
              collect() %>%
              setDT()) %>%
    select(-datacode)

sites <- samples %>%
    distinct(sitecode) %>%
    rename(code = sitecode)
merge_with_sql(sites, 'sites', 'code')

plots <- samples %>%
    distinct(sitecode, plotcode) %>%
    rename(code = plotcode)
merge_with_sql(plots, 'plots', 'code')

samples %>% 
    rename(code = samplecode) %>%
    merge_with_sql('samples', 'code')

sample_condition <- chemdat %>%
    distinct(SampleName, canopyposition) %>%
    mutate(sunshade = recode(canopyposition, 
                             sunlit = 'sun',
                             understory = 'shade')) %>%
    select(-canopyposition) %>%
    melt(id.vars = 'SampleName', variable.name = 'condition') %>%
    left_join(select(samples, SampleName, samplecode)) %>%
    select(-SampleName)

sample_condition_info <- distinct(sample_condition, condition)

merge_with_sql(sample_condition_info, 'sample_condition_info', 'condition')
merge_with_sql(sample_condition, 'sample_condition', 'samplecode')

trait_data <- chemdat %>%
    left_join(select(samples, samplecode, SampleName)) %>%
    select(samplecode, starts_with('leaf_', ignore.case = FALSE)) %>%
    melt(id.vars = 'samplecode', variable.name = 'trait', na.rm = TRUE)

trait_info <- trait_data %>%
    distinct(trait) %>%
    mutate(unit = case_when(trait == 'leaf_water_potential' ~ 'Pa', 
                            TRUE ~ NA_character_))

merge_with_sql(trait_info, 'trait_info', 'trait')
merge_with_sql(trait_data, 'trait_data', 'samplecode')

instruments <- spectra_raw %>%
    distinct(Instrument) %>%
    rename(name = Instrument)
merge_with_sql(instruments, 'instruments', 'name')

# TODO: Add more methods information
specmethods <- tbl(specdb, 'instruments') %>%
    filter(name == instruments$name) %>%
    select(instrumentid = id) %>%
    collect() %>%
    setDT()
merge_with_sql(specmethods, 'specmethods', 'instrumentid')

spectra_info <- spectra_raw %>%
    select(SampleName, Instrument) %>%
    left_join(select(samples, samplecode, SampleName)) %>%
    left_join(tbl(specdb, 'instruments') %>%
              select(instrumentid = id, Instrument = name) %>%
              left_join(tbl(specdb, 'specmethods') %>%
                        select(instrumentid, specmethodid = id)) %>%
              select(Instrument, specmethodid) %>%
              collect() %>%
              setDT()) %>%
    mutate(type = 'reflectance') %>%
    select(-Instrument, -SampleName)
merge_with_sql(spectra_info, 'spectra_info', 'samplecode')

spectra_data <- spectra_raw %>%
    select(SampleName, starts_with('Wave_')) %>%
    left_join(select(samples, samplecode, SampleName)) %>%
    left_join(tbl(specdb, 'samples') %>% 
              filter(projectcode == project_code) %>%
              select(samplecode = code) %>%
              inner_join(tbl(specdb, 'spectra_info') %>% 
                         select(spectraid = id, samplecode)) %>%
              collect() %>%
              setDT()) %>%
    select(spectraid, starts_with('Wave_')) %>%
    melt(id.vars = 'spectraid', variable.name = 'wavelength') %>%
    mutate(wavelength = as.numeric(gsub('Wave_', '', wavelength)))
merge_with_sql(spectra_data, 'spectra_data', 'spectraid')

