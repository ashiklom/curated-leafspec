#!/usr/bin/Rscript
library(specprocess)
library(stringr)
source('common.R')

usda_path <- '~/Projects/common-data/usdadb.sqlite3'
try_path <- '~/Projects/try/try-sqlite/try.sqlite'

myspecies <- tbl(specdb, 'species') %>%
    inner_join(tbl(specdb, 'samples') %>% count(speciescode)) %>%
    collect()

usda_full <- src_sqlite(usda_path) %>% tbl('usda')
usda_sub <- usda_full %>%
    filter(Symbol %in% myspecies[['speciescode']]) %>%
    select(-Genus, -Species, -Family, -Scientific_Name_x) %>%
    collect() %>%
    rename(speciescode = Symbol)

species <- myspecies %>% 
    left_join(usda_sub)

try_all <- src_sqlite(try_path)

try_data <- try_all %>% tbl('orig_data')
try_datanames <- try_all %>% tbl('orig_datanames')

# Set growth form
message("Growth form")
species <- species %>%
    mutate(growth_form = 
           case_when(str_detect(.$Growth_Habit, '^Tree') ~ 'tree',
                     str_detect(.$Growth_Habit, '^(Subshrub|Shrub)') ~ 'shrub',
                     str_detect(.$Growth_Habit, '^Graminoid') ~ 'graminoid',
                     str_detect(.$Growth_Habit, '^Vine') ~ 'vine', 
                     str_detect(.$Growth_Habit, '^Forb') ~ 'herb',
                     TRUE ~ NA_character_))

gf_missing <- species %>% 
    filter(is.na(growth_form)) %>%
    distinct(tryspeciesid, scientificname, n) %>%
    arrange(desc(n)) %T>%
    print

combinevals <- . %>%
    collect %>%
    group_by(AccSpeciesID) %>%
    summarize(rawval = paste(unique(tolower(OrigValueStr)), 
                             collapse = ' ')) %>%
    ungroup %>%
    setDT %>%
    setkey(AccSpeciesID)

setDT(species)

gf_try <- try_data %>%
    filter(DataID == 47,
           AccSpeciesID %in% gf_missing[['tryspeciesid']]) %>%
    distinct(AccSpeciesID, OrigValueStr) %>%
    combinevals %>%
    mutate(growth_form = case_when(str_detect(.$rawval, 'tree') ~ 'tree',
                                   str_detect(.$rawval, 'shrub|woody plant') ~ 'shrub',
                                   str_detect(.$rawval, 'forb|herb|b h$') ~ 'herb',
                                   str_detect(.$rawval, 'grass|sedge') ~ 'herb',
                                   str_detect(.$rawval, 'palm') ~ 'tree',
                                   str_detect(.$rawval, 'lianna|vina') ~ 'vine',
                                   TRUE ~ NA_character_))

species <- species %>%
    .[tryspeciesid %in% gf_try[['AccSpeciesID']],
      growth_form := gf_try[J(tryspeciesid), growth_form]] %>%
    .[grepl('Pittoniotis trichantha', scientificname),
      c('growth_form', 'leaf_type', 'phenology') := 
          list('tree', 'broad', 'evergreen')] %>%
    .[grepl('Eucalyptus gunnii', scientificname),
      c('growth_form', 'leaf_type', 'phenology') := 
          list('tree', 'broad', 'evergreen')] %>%
    .[grepl('Callicarpa bodinieri', scientificname),
      c('growth_form', 'leaf_type') := list('shrub', 'broad')] %>%
    .[grepl('Rhododendron calophytum', scientificname),
      c('growth_form', 'leaf_type') := list('shrub', 'broad')] %>%
    .[grepl('Ensete ventricosum', scientificname),
      c('growth_form', 'leaf_type') := list('herb', 'broad')] %>%
    .[grepl('Wyethia Nutt.', scientificname), 
      c('growth_form', 'leaf_type') := list('herb', 'broad')]

species %>% filter(is.na(growth_form)) %>%
    select(speciescode, scientificname)

species <- species %>%
    mutate(woody = recode(growth_form,
                          `tree` = TRUE,
                          `shrub` = TRUE,
                          `subshrub` = TRUE,
                          `palm` = TRUE,
                          `herb` = FALSE,
                          `graminoid` = FALSE,
                          `vine` = FALSE))

## Photosynthesis type
message("PS type")
ps_try <- try_data %>%
    filter(DataID == 25,
           AccSpeciesID %in% species[['tryspeciesid']]) %>%
    combinevals %>%
    .[grepl('c4', rawval), ps_type := 'C4'] %>%
    .[is.na(ps_type) & grepl('c3', rawval), ps_type := 'C3']

species <- species %>%
    .[tryspeciesid %in% ps_try[['AccSpeciesID']],
      ps_type := ps_try[J(tryspeciesid), ps_type]]

## Phenology
message("Phenology")
species <- species %>%
    .[Leaf_Retention == 'Yes', phenology := 'evergreen'] %>%
    .[Leaf_Retention == 'No', phenology := 'deciduous']

pheno_missing <- species %>%
    filter(is.na(phenology)) %>%
    select(tryspeciesid, scientificname, n)

pheno_try <- try_data %>%
    filter(DataID == 42,
           AccSpeciesID %in% pheno_missing[['tryspeciesid']]) %>%
    combinevals %>%
    .[grepl('ev(ergreen)?|\\<e\\>', rawval) & !grepl('deciduous', rawval),
      phenology := 'evergreen'] %>%
    .[is.na(phenology) & grepl('deciduous|\\<d\\>', rawval),
      phenology := 'deciduous']

other_deciduous <- c('Petasites frigidus',
                     'Arctophila fulva',
                     'Alnus alnobetula',
                     'Saxifraga punctata',
                     'Quercus prinus',
                     'Dupontia fisheri',
                     'Bambusa arundinacea',
                     'Ludwigia',
                     'Callicarpa bodinieri',
                     'Carex L.',
                     'Geranium|Iris|Rhexia|Solanum|Wyethia')

species <- species %>%
    .[tryspeciesid %in% pheno_try[['AccSpeciesID']],
      phenology := pheno_try[J(tryspeciesid), phenology]] %>%
    .[grepl(paste(other_deciduous, collapse = '|'), scientificname),
      phenology := FALSE] %>%
    .[grepl('Ensete ventricosum', scientificname), 
      phenology := TRUE]

# Leaf type
message("Leaf type")
leaftype_missing <- species %>%
    filter(is.na(leaf_type)) %>%
    select(tryspeciesid, scientificname, n) %>%
    arrange(desc(n))

leaftype_try <- try_data %>%
    filter(DataID == 48,
           AccSpeciesID %in% species[['tryspeciesid']]) %>%
    combinevals %>%
    .[grepl('broad|\\<b\\>', rawval), leaf_type := 'broad'] %>%
    .[is.na(leaf_type) & grepl('needle|\\<n\\>', rawval), 
      leaf_type := 'needle']

other_broadleaved <- c("Petasites frigidus (L.) Fr.",
                       "Carex aquatilis Wahlenb.",
                       "Arctophila fulva (Trin.) Andersson",
                       "Alnus alnobetula subsp. fruticosa (Rupr.) Raus",
                       "Arctagrostis latifolia (R.Br.) Griseb.",
                       "Saxifraga punctata L.", 
                       "Quercus prinus L.",
                       "Dupontia fisheri R.Br.",
                       "Euonymus fortunei (Turcz.) Hand.-Maz.",
                       "Cornus alba L.",
                       "Cecropia schreberiana Miq.",
                       "Corylus maxima Mill.", 
                       "Typha angustifolia L.",
                       "Andropogon gerardii Vitman",
                       "Bambusa arundinacea (Retz.) Willd.", 
                       "Cercis siliquastrum L.",
                       "Glycine max (L.) Merr.",
                       "Ludwigia L.", 
                       "Medicago sativa L.",
                       "Oryza sativa L.",
                       "Osmanthus heterophyllus (G. Don) P.S. Green",
                       "Parthenocissus tricuspidata (Siebold & Zucc.) Planch.",
                       "Tilia tomentosa Moench",
                       "Viburnum rhytidophyllum Hemsl.",
                       "Brassica oleracea L.",
                       "Carex L.",
                       "Geranium L.",
                       "Iris germanica L.",
                       "Lactuca sativa L.",
                       "Lonicera L.",
                       "Phalaris arundinacea L.",
                       "Rhexia mariana L.",
                       "Salvia officinalis L.",
                       "Sorghum halepense (L.) Pers.",
                       "Solanum lycopersicum L.",
                       "Solanum tuberosum L.",
                       "Spartina pectinata Bosc ex Link")

other_needle <- c("Pinus pungens Lamb.")

species <- species %>%
    .[tryspeciesid %in% leaftype_try[['AccSpeciesID']],
      leaf_type := leaftype_try[J(tryspeciesid), leaf_type]] %>%
    .[scientificname %in% other_broadleaved, leaf_type := 'broad'] %>%
    .[scientificname %in% other_needle, leaf_type := 'needle']

## Nitrogen fixation
message("Nitrogen fixation")
species <- species %>%
    .[Nitrogen_Fixation %in% c('Low', 'Medium'), 
      nitrogen_fixer := TRUE]

nf_missing <- species %>% 
    filter(is.na(nitrogen_fixer)) %>%
    select(tryspeciesid, scientificname, n) %>%
    arrange(desc(n))

nf_try <- try_data %>%
    filter(DataID == 9,
           AccSpeciesID %in% nf_missing[['tryspeciesid']]) %>%
    distinct(AccSpeciesID, OrigValueStr) %>%
    combinevals %>%
    .[, nitrogen_fixer := FALSE] %>%
    .[grepl('\\<y\\>|\\<yes\\>', rawval) & !grepl('\\<not?\\>', rawval),
      nitrogen_fixer := TRUE]

no_nitrogen_fix <- c("Alnus alnobetula subsp. fruticosa (Rupr.) Raus",
                     "Tocoyena pittieri (Standl.) Standl.",
                     "Virola multiflora (Standl.) A.C.Sm.",
                     "Albizia guachapele (Kunth) Dugand",
                     "Pittoniotis trichantha Griseb.",
                     "Saxifraga punctata L.",
                     "Quercus prinus L.",
                     "Dupontia fisheri R.Br.",
                     "Salix lanata subsp. richardsonii (Hook.) A.K.Skvortsov",
                     "Acer saccharinum L.",
                     "Euonymus fortunei (Turcz.) Hand.-Maz.",
                     "Schefflera arboricola (Hayata) Merr.",
                     "Acer spicatum Lam.",
                     "Corylus maxima Mill.",
                     "Magnolia acuminata (L.) L.",
                     "Typha angustifolia L.",
                     "Bambusa arundinacea (Retz.) Willd.",
                     "Cercis siliquastrum L.",
                     "Eucalyptus gunnii Hook. f.",
                     "Hydrangea macrophylla (Thunb.) Ser.",
                     "Ludwigia L.",
                     "Osmanthus heterophyllus (G. Don) P.S. Green",
                     "Parthenocissus tricuspidata (Siebold & Zucc.) Planch.",
                     "Pinus pungens Lamb.",
                     "Prunus armeniaca L.",
                     "Tilia platyphyllos Scop.",
                     "Tilia tomentosa Moench",
                     "Viburnum plicatum Thunb.",
                     "Viburnum rhytidophyllum Hemsl.",
                     "Weigela florida (Bunge) A. DC.",
                     "Callicarpa bodinieri H.Lév.",
                     "Rhododendron calophytum Franch.",
                     "Cotinus coggygria Scop.",
                     "Ensete ventricosum (Welw.) Cheeseman",
                     "Hamamelis virginiana L.",
                     "Iris germanica L.",
                     "Morus nigra L.",
                     "Populus ×canadensis Moench (pro sp.) [deltoides × nigra]",
                     "Rhexia mariana L.",
                     "Salvia officinalis L.",
                     "Solanum lycopersicum L.",
                     "Spartina pectinata Bosc ex Link",
                     "Wyethia Nutt.")

species <- species %>%
    .[tryspeciesid %in% nf_try[['AccSpeciesID']],
      nitrogen_fixer := nf_try[J(tryspeciesid), nitrogen_fixer]] %>%
    .[scientificname %in% no_nitrogen_fix, nitrogen_fixer := FALSE]

## Shade tolerance
message("Shade")
species <- species %>% mutate(shade_tolerance = Shade_Tolerance)

stnum_try <- try_data %>%
    filter(DataID == 208, DatasetID == 49,
           AccSpeciesID %in% species[['tryspeciesid']]) %>%
    distinct(AccSpeciesID, OrigValueStr) %>%
    group_by(AccSpeciesID) %>%
    summarize(shade_tolerance_numeric = mean(as.numeric(OrigValueStr))) %>%
    collect %>%
    setDT %>%
    setkey(AccSpeciesID)

species <- species %>%
    .[, shade_tolerance_numeric := 
      stnum_try[J(tryspeciesid), shade_tolerance_numeric]]


## Mycorrhizal association
message("Mycorhhizae")
myco_dat <- fread("~/Projects/prospect-traits/analysis/mycorrhizal_SPCD_data.csv")
myco_dat <- myco_dat %>%
    select(speciescode = SPECIES_SYMBOL, myco_asso = MYCO_ASSO)

species <- species %>% left_join(myco_dat)

species_final <- species %>%
    select(speciescode, scientificname, family,
           growth_form, leaf_type, phenology, ps_type,
           nitrogen_fixer, shade_tolerance, shade_tolerance_numeric,
           myco_asso)

species_final %>% 
    summarize_all((function(x) sum(is.na(x)))) %>%
    glimpse()

species_missing <- species_final %>%
    filter((is.na(growth_form) |
           is.na(leaf_type) | 
           is.na(phenology) | 
           is.na(ps_type) |
           is.na(nitrogen_fixer) |
           is.na(shade_tolerance) |
           is.na(myco_asso)))

species_complete <- species_final %>%
    filter(!(is.na(growth_form) |
             is.na(leaf_type) | 
             is.na(phenology) | 
             is.na(ps_type) |
             is.na(nitrogen_fixer) |
             is.na(shade_tolerance) |
             is.na(myco_asso)))

write_csv(species_missing, 'data/common/missing_attributes.csv', na = '')
write_csv(species_complete, 'data/common/complete_attributes.csv', na = '')
write_csv(species_final, 'data/common/all_attributes.csv', na = '')

processed <- readxl::read_excel('data/common/species_attributes_processed.xlsx') %>%
    select(-scientificname) %>%
    left_join(species_final, by = 'speciescode') %>%
    select(speciescode, scientificname, family, add_family,
           matches('.')) %>%
    mutate(family = if_else(is.na(family), add_family, family))

write_csv(processed, 'data/common/attr_processed_v2.csv', na = '')

#processed %>%
    #filter(is.na(family))

#species %>% filter(is.na(nitrogen_fixer)) %>% select(scientificname, N) %>% arrange(desc(N)) %>% .[['scientificname']]

#try_datanames %>%
    #filter(TraitName %like% '%shade%') %>%
    #glimpse

#try_data %>%
    #filter(DataID == 208,
           #DatasetID %in% c(49, 68)) %>%
    #group_by(DatasetID) %>%
    #summarize(N = n(), 
              #mu = mean(as.numeric(OrigValueStr)),
              #low = min(as.numeric(OrigValueStr)),
              #hi = max(as.numeric(OrigValueStr)))


    #filter(OrigValueStr %like% '%C4%') %>%
    #select(DataID, TraitID, 
