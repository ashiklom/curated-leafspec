CREATE TABLE samples(
    SampleID INTEGER PRIMARY KEY,
    ProjectID INTEGER,
    SpeciesID INTEGER,
    FullName TEXT,
    Name TEXT,
    Year INTEGER,
    Date TEXT,
    CanopyPosition TEXT,
    NeedleAge INTEGER,
    OldNew TEXT,
    SiteName TEXT,
    PlotName TEXT,
    Latitude REAL,
    Longitude REAL,
    Comments TEXT);

CREATE TABLE traitInfo(
    TraitID INTEGER PRIMARY KEY,
    Trait TEXT,
    Description TEXT,
    Unit TEXT,
    Comments TEXT);

CREATE TABLE traits(
    ObservationID INTEGER PRIMARY KEY,
    SampleID INTEGER,
    TraitID INTEGER,
    Value REAL,
    Comments TEXT);

CREATE TABLE specInfo(
    SpectraID INTEGER PRIMARY KEY,
    SampleID INTEGER,
    SpectraType TEXT,
    InstrumentID INTEGER,
    Comments TEXT);

CREATE TABLE spectra(
    SpecObsID INTEGER PRIMARY KEY,
    SampleID INTEGER,
    Wavelength REAL,
    Value REAL);

CREATE TABLE species(
    SpeciesID INTEGER PRIMARY KEY,
    ScientificName TEXT,
    CommonName TEXT,
    USDA_Code TEXT,
    Family TEXT,
    Phenology TEXT,
    LeafType TEXT,
    GrowthForm TEXT,
    Cotyledon TEXT,
    Succession TEXT,
    Comments TEXT);

CREATE TABLE results(
    SpectraID INTEGER PRIMARY KEY,
    TraitID INTEGER,
    Value REAL,
    Comments TEXT);

