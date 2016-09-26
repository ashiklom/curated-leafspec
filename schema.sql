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
    Comments);

CREATE TABLE plots(
    PlotID INTEGER PRIMARY KEY,
    SiteID INTEGER,
    PlotName TEXT,
    PlotDescription TEXT,
    Latitude REAL,
    Longitude REAL,
    Comments);

CREATE TABLE projects(
    ProjectID INTEGER PRIMARY KEY,
    ProjectName TEXT,
    ProjectCode TEXT,
    Affiliation TEXT,
    PointOfContact TEXT,
    Comments TEXT);

CREATE TABLE traitInfo(
    TraitID INTEGER PRIMARY KEY,
    TraitName TEXT,
    TraitDescription TEXT,
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
    Instrument TEXT,
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

