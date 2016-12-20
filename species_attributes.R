#!/usr/bin/Rscript
library(dtplyr)
library(data.table)
library(dplyr)

species <- readRDS('species_merged.rds')

usda_full <- src_sqlite('usdadb_new.sqlite3') %>% tbl('usda')
usda_sub <- usda_full %>%
    filter(Symbol %in% species[['SpeciesCode']]) %>%
    select(-Genus, -Species, -Family, -Scientific_Name_x) %>%
    collect() %>%
    rename(SpeciesCode = Symbol)

species <- species %>% 
    left_join(usda_sub, by = 'SpeciesCode', copy = TRUE)

try_all <- src_sqlite('~/Projects/try/try-sqlite/try.sqlite')

try_data <- try_all %>% tbl('orig_data')
try_datanames <- try_all %>% tbl('orig_datanames')

# Set growth form
message("Growth form")
species <- species %>%
    .[grepl('^Tree', Growth_Habit), growth_form := 'tree'] %>%
    .[grepl('^(Subshrub|Shrub)', Growth_Habit), 
      growth_form := 'shrub'] %>%
    .[grepl('^Graminoid', Growth_Habit), growth_form := 'graminoid'] %>%
    .[grepl('^Vine', Growth_Habit), growth_form := 'vine'] %>%
    .[grepl('^Forb', Growth_Habit), growth_form := 'herb']

gf_missing <- species %>% 
    filter(is.na(growth_form)) %>%
    select(TRY_Species_ID, ScientificName, N) %>%
    arrange(desc(N))

combinevals <- . %>%
    collect %>%
    group_by(AccSpeciesID) %>%
    summarize(rawval = paste(unique(tolower(OrigValueStr)), 
                             collapse = ' ')) %>%
    ungroup %>%
    setDT

gf_try <- try_data %>%
    filter(DataID == 47,
           AccSpeciesID %in% gf_missing[['TRY_Species_ID']]) %>%
    distinct(AccSpeciesID, OrigValueStr) %>%
    combinevals %>%
    .[grepl('tree', rawval), growth_form := 'tree'] %>%
    .[is.na(growth_form) & grepl('shrub|woody plant', rawval),
      growth_form := 'shrub'] %>%
    .[is.na(growth_form) & grepl('forb|herb|b h$', rawval), 
      growth_form := 'herb'] %>%
    setkey(AccSpeciesID)

species <- species %>%
    .[TRY_Species_ID %in% gf_try[['AccSpeciesID']],
      growth_form := gf_try[J(TRY_Species_ID), growth_form]] %>%
    .[grepl('Pittoniotis trichantha', ScientificName),
      c('growth_form', 'leaf_type', 'evergreen') := 
          list('tree', 'broad', TRUE)] %>%
    .[grepl('Eucalyptus gunnii', ScientificName),
      c('growth_form', 'leaf_type', 'evergreen') := 
          list('tree', 'broad', TRUE)] %>%
    .[grepl('Callicarpa bodinieri', ScientificName),
      c('growth_form', 'leaf_type') := list('shrub', 'broad')] %>%
    .[grepl('Rhododendron calophytum', ScientificName),
      c('growth_form', 'leaf_type') := list('shrub', 'broad')] %>%
    .[grepl('Ensete ventricosum', ScientificName),
      c('growth_form', 'leaf_type') := list('herb', 'broad')] %>%
    .[grepl('Wyethia Nutt.', ScientificName), 
      c('growth_form', 'leaf_type') := list('herb', 'broad')]

species <- species %>%
    .[grepl('tree|shrub', growth_form), woody := TRUE] %>%
    .[grepl('herb|graminoid|vine', growth_form), woody := FALSE]

## Photosynthesis type
message("PS type")
ps_try <- try_data %>%
    filter(DataID == 25,
           AccSpeciesID %in% species[['TRY_Species_ID']]) %>%
    combinevals %>%
    .[grepl('c4', rawval), ps_type := 'C4'] %>%
    .[is.na(ps_type) & grepl('c3', rawval), ps_type := 'C3'] %>%
    setkey(AccSpeciesID)

species <- species %>%
    .[TRY_Species_ID %in% ps_try[['AccSpeciesID']],
      ps_type := ps_try[J(TRY_Species_ID), ps_type]]

## Phenology
message("Phenology")
species <- species %>%
    .[Leaf_Retention == 'Yes', evergreen := TRUE] %>%
    .[Leaf_Retention == 'No', evergreen := FALSE]

pheno_missing <- species %>%
    filter(is.na(evergreen)) %>%
    select(TRY_Species_ID, ScientificName, N)

pheno_try <- try_data %>%
    filter(DataID == 42,
           AccSpeciesID %in% pheno_missing[['TRY_Species_ID']]) %>%
    combinevals %>%
    .[grepl('ev(ergreen)?|\\<e\\>', rawval) & !grepl('deciduous', rawval),
      evergreen := TRUE] %>%
    .[is.na(evergreen) & grepl('deciduous|\\<d\\>', rawval),
      evergreen := FALSE] %>%
    setkey(AccSpeciesID)

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
    .[TRY_Species_ID %in% pheno_try[['AccSpeciesID']],
      evergreen := pheno_try[J(TRY_Species_ID), evergreen]] %>%
    .[grepl(paste(other_deciduous, collapse = '|'), ScientificName),
      evergreen := FALSE] %>%
    .[grepl('Ensete ventricosum', ScientificName), 
      evergreen := TRUE]

## Leaf type
message("Leaf type")
leaftype_missing <- species %>%
    filter(is.na(leaf_type)) %>%
    select(TRY_Species_ID, ScientificName, N) %>%
    arrange(desc(N))

leaftype_try <- try_data %>%
    filter(DataID == 48,
           AccSpeciesID %in% leaftype_missing[['TRY_Species_ID']]) %>%
    combinevals %>%
    .[grepl('broad|\\<b\\>', rawval), leaf_type := 'broad'] %>%
    .[is.na(leaf_type) & grepl('needle|\\<n\\>', rawval), 
      leaf_type := 'needle'] %>%
    setkey(AccSpeciesID)

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
    .[TRY_Species_ID %in% leaftype_try[['AccSpeciesID']],
      leaf_type := leaftype_try[J(TRY_Species_ID), leaf_type]] %>%
    .[ScientificName %in% other_broadleaved, leaf_type := 'broad'] %>%
    .[ScientificName %in% other_needle, leaf_type := 'needle']

## Nitrogen fixation
message("Nitrogen fixation")
species <- species %>%
    .[Nitrogen_Fixation %in% c('Low', 'Medium'), 
      nitrogen_fixer := TRUE]

nf_missing <- species %>% 
    filter(is.na(nitrogen_fixer)) %>%
    select(TRY_Species_ID, ScientificName, N) %>%
    arrange(desc(N))

nf_try <- try_data %>%
    filter(DataID == 9,
           AccSpeciesID %in% nf_missing[['TRY_Species_ID']]) %>%
    distinct(AccSpeciesID, OrigValueStr) %>%
    combinevals %>%
    .[, nitrogen_fixer := FALSE] %>%
    .[grepl('\\<y\\>|\\<yes\\>', rawval) & !grepl('\\<not?\\>', rawval),
      nitrogen_fixer := TRUE] %>%
    setkey(AccSpeciesID)

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
    .[TRY_Species_ID %in% nf_try[['AccSpeciesID']],
      nitrogen_fixer := nf_try[J(TRY_Species_ID), nitrogen_fixer]] %>%
    .[ScientificName %in% no_nitrogen_fix, nitrogen_fixer := FALSE]

## Shade tolerance
message("Shade")
species <- species %>%
    .[Shade_Tolerance %in% c('Tolerant', 'Intermediate'),
      shade_tolerant := TRUE] %>%
    .[Shade_Tolerance == 'Intolerant', shade_tolerant := FALSE]

stnum_try <- try_data %>%
    filter(DataID == 208, DatasetID == 49,
           AccSpeciesID %in% species[['TRY_Species_ID']]) %>%
    distinct(AccSpeciesID, OrigValueStr) %>%
    group_by(AccSpeciesID) %>%
    summarize(shade_tolerance_numeric = mean(as.numeric(OrigValueStr))) %>%
    collect %>%
    setDT %>%
    setkey(AccSpeciesID)

species <- species %>%
    .[, shade_tolerance_numeric := 
      stnum_try[J(TRY_Species_ID), shade_tolerance_numeric]]


## Mycorrhizal association
message("Mycorhhizae")
myco_dat <- fread("~/Projects/prospect-traits/analysis/mycorrhizal_SPCD_data.csv")
myco_dat <- myco_dat %>%
    setnames("SPECIES_SYMBOL", "SpeciesCode") %>%
    select(SpeciesCode, MYCO_ASSO)

species <- species %>%
    left_join(myco_dat, by = 'SpeciesCode')

species_final <- species %>%
    select(SpeciesCode, ScientificName, Genus, Species, Family,
           xOrder, SubClass, Class, Division,
           growth_form, leaf_type, evergreen, ps_type,
           nitrogen_fixer, shade_tolerant, shade_tolerance_numeric,
           MYCO_ASSO)

saveRDS(species_final, 'species_final.rds')



#species %>% filter(is.na(nitrogen_fixer)) %>% select(ScientificName, N) %>% arrange(desc(N)) %>% .[['ScientificName']]

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
