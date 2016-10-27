CREATE TABLE samples(
    SampleID INTEGER PRIMARY KEY,
    ProjectID INTEGER,
    SpeciesID INTEGER,
    FullName TEXT,
    SampleName TEXT,
    SampleYear INTEGER,
    SampleDate TEXT,
    CanopyPosition TEXT,
    NeedleAge INTEGER,
    NeedleOldNew TEXT,
    SiteID INTEGER,
    PlotID INTEGER,
    Comments TEXT);

CREATE TABLE sites(
    SiteID INTEGER PRIMARY KEY,
    SiteName TEXT,
    SiteDescription TEXT,
    SiteLatitude REAL,
    SiteLongitude REAL,
    Comments);

CREATE TABLE plots(
    PlotID INTEGER PRIMARY KEY,
    SiteID INTEGER,
    PlotName TEXT,
    PlotDescription TEXT,
    PlotLatitude REAL,
    PlotLongitude REAL,
    Comments);

CREATE TABLE traits(
    ObservationID INTEGER PRIMARY KEY,
    SampleID INTEGER,
    TraitID INTEGER,
    Value REAL,
    Comments TEXT);

CREATE TABLE specInfo(
    SpectraID INTEGER PRIMARY KEY,
    SpectraName TEXT,
    SpectraType TEXT,
    SampleID INTEGER,
    InstrumentID TEXT,
    Calibration TEXT,
    Apparatus TEXT,
    Comments TEXT);

CREATE TABLE spectra(
    SpecObsID INTEGER PRIMARY KEY,
    SpectraID INTEGER,
    Wavelength REAL,
    Value REAL);

CREATE TABLE results(
    SpectraID INTEGER PRIMARY KEY,
    TraitID INTEGER,
    Value REAL,
    Comments TEXT);

