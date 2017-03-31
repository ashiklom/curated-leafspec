PRAGMA foreign_keys = ON;

/* Metadata TABLEs */
CREATE TABLE projects(
    projectcode TEXT UNIQUE,
    projectshortname TEXT, 
    projectdescription TEXT,
    pointofcontact TEXT,
    email TEXT,
    doi TEXT,
    projectcomment TEXT,
    PRIMARY KEY (projectcode)
);

CREATE TABLE sites(
    sitecode TEXT UNIQUE, 
    projectcode TEXT REFERENCES projects (projectcode),
    sitedescription TEXT,
    sitecomment TEXT,
    PRIMARY KEY (projectcode, sitecode)
);

CREATE TABLE plots(
    sitecode TEXT REFERENCES sites (sitecode) ON DELETE CASCADE,
    plotcode TEXT UNIQUE,
    plotdescription TEXT,
    latitude NUMERIC,
    longitude NUMERIC,
    plotcomment TEXT,
    PRIMARY KEY (sitecode, plotcode)
);

CREATE TABLE species(
    speciescode TEXT UNIQUE,
    speciescodetype TEXT,
    scientificname TEXT,
    genus TEXT,
    species TEXT,
    subspecies TEXT,
    variety TEXT,
    subvariety TEXT,
    forma TEXT,
    family TEXT,
    authority TEXT,
    tryspeciesid INTEGER,
    speciescomment TEXT,
    PRIMARY KEY (speciescode)
);

CREATE TABLE samples(
    samplecode TEXT UNIQUE,
    projectcode TEXT REFERENCES projects (projectcode) ON DELETE CASCADE,
    year INTEGER,
    collectiondate TEXT,
    plotcode TEXT REFERENCES plots (plotcode) ON DELETE CASCADE,
    speciescode TEXT REFERENCES species (speciescode) ON DELETE CASCADE,
    PRIMARY KEY (samplecode)
);

CREATE TABLE sample_condition_info(
    condition TEXT UNIQUE,
    conditiondescription TEXT,
    conditioncomment TEXT,
    PRIMARY KEY (condition)
);

CREATE TABLE sample_condition(
    samplecode TEXT REFERENCES samples (samplecode) ON DELETE CASCADE,
    condition TEXT REFERENCES sample_condition_info (condition) ON DELETE CASCADE,
    conditionvalue TEXT,
    conditionvaluecomment TEXT,
    PRIMARY KEY (samplecode, condition)
);

CREATE TABLE instruments(
    instrumentcode TEXT UNIQUE,
    instrumentname TEXT,
    minwavelength real,
    maxwavelength real,
    spectralresolution real,
    instrumentcomment TEXT,
    PRIMARY KEY (instrumentcode)
);

CREATE TABLE specmethods(
    specmethodcode TEXT UNIQUE,
    instrumentcode TEXT REFERENCES instruments (instrumentcode) ON DELETE CASCADE,
    apparatus TEXT,
    calibration TEXT,
    specmethodcomment TEXT,
    PRIMARY KEY (specmethodcode)
);

CREATE TABLE spectra_info(
    spectraid INTEGER PRIMARY KEY,
    samplecode TEXT NOT NULL REFERENCES samples (samplecode) ON DELETE CASCADE,
    spectratype TEXT,
    specmethodcode INTEGER REFERENCES specmethods (specmethodcode) ON DELETE CASCADE,
    sampleprep TEXT,
    spectracomment TEXT
);

CREATE TABLE spectra_data(
    spectraid INTEGER NOT NULL REFERENCES spectra_info (spectraid) ON DELETE CASCADE,
    wavelength NUMERIC 
        CHECK (wavelength > 0),
    spectravalue NUMERIC,
    CONSTRAINT unique_spectrum UNIQUE(spectraid, wavelength),
    PRIMARY KEY (spectraid, wavelength)
);

CREATE TABLE trait_info(
    trait TEXT UNIQUE,
    traitdescription TEXT,
    unit TEXT,
    traitinfocomment TEXT,
    PRIMARY KEY (trait)
);

CREATE TABLE trait_data(
    samplecode TEXT NOT NULL REFERENCES samples (samplecode) ON DELETE CASCADE, 
    trait TEXT REFERENCES trait_info (trait) ON DELETE CASCADE,
    traitvalue NUMERIC,
    traitdatacomment TEXT,
    PRIMARY KEY (samplecode, trait)
);
