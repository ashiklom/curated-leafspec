PRAGMA foreign_keys = ON;

/* Metadata TABLEs */
CREATE TABLE projects(
    projectid INTEGER PRIMARY KEY,
    projectcode TEXT UNIQUE,
    projectdescription TEXT,
    pointofcontact TEXT,
    email TEXT,
    doi TEXT,
    projectcomment TEXT
);

CREATE TABLE sites(
    siteid INTEGER PRIMARY KEY,
    sitecode TEXT UNIQUE, 
    sitedescription TEXT,
    sitecomment TEXT
);

CREATE TABLE plots(
    plotid INTEGER PRIMARY KEY,
    sitecode TEXT REFERENCES sites (sitecode) ON DELETE CASCADE,
    plotcode TEXT UNIQUE,
    plotdescription TEXT,
    latitude NUMERIC,
    longitude NUMERIC,
    plotcomment TEXT
);

CREATE TABLE species(
    speciesid INTEGER PRIMARY KEY,
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
    speciescomment TEXT
);

CREATE TABLE species_dict(
    speciesdictid INTEGER PRIMARY KEY,
    speciesdatacode TEXT,
    projectcode TEXT REFERENCES projects (projectcode) ON DELETE CASCADE,
    speciescode TEXT REFERENCES species (speciescode) ON DELETE CASCADE,
    speciesdictcomment TEXT,
    UNIQUE (speciesdatacode, projectcode)
);

CREATE TABLE samples(
    sampleid INTEGER PRIMARY KEY,
    samplecode TEXT UNIQUE,
    projectcode TEXT REFERENCES projects (projectcode) ON DELETE CASCADE,
    year INTEGER,
    collectiondate TEXT,
    plotcode TEXT REFERENCES plots (plotcode) ON DELETE CASCADE,
    speciescode TEXT REFERENCES species (speciescode) ON DELETE CASCADE
    /*samplecomment TEXT*/
);

CREATE TABLE sample_condition_info(
    conditionid INTEGER PRIMARY KEY,
    condition TEXT UNIQUE,
    conditiondescription TEXT,
    conditioncomment TEXT
);

CREATE TABLE sample_condition(
    conditiondataid INTEGER PRIMARY KEY,
    samplecode TEXT REFERENCES samples (samplecode) ON DELETE CASCADE,
    condition TEXT REFERENCES sample_condition_info (condition) ON DELETE CASCADE,
    conditionvalue TEXT,
    conditionvaluecomment TEXT
);

CREATE TABLE instruments(
    instrumentid INTEGER PRIMARY KEY,
    instrumentname TEXT UNIQUE,
    minwavelength real,
    maxwavelength real,
    spectralresolution real,
    instrumentcomment TEXT
);

CREATE TABLE specmethods(
    specmethodid INTEGER PRIMARY KEY,
    instrumentid INTEGER REFERENCES instruments (instrumentid) ON DELETE CASCADE,
    apparatus TEXT,
    calibration TEXT,
    specmethodcomment TEXT
);

CREATE TABLE spectra_info(
    spectraid INTEGER PRIMARY KEY,
    samplecode TEXT NOT NULL REFERENCES samples (samplecode) ON DELETE CASCADE,
    spectratype TEXT,
    specmethodid INTEGER REFERENCES specmethods (specmethodid) ON DELETE CASCADE,
    sampleprep TEXT,
    spectracomment TEXT
);

CREATE TABLE spectra_data(
    spectradataid INTEGER PRIMARY KEY,
    spectraid INTEGER NOT NULL REFERENCES spectra_info (spectraid) ON DELETE CASCADE,
    wavelength NUMERIC 
        CHECK (wavelength > 0),
    spectravalue NUMERIC,
    CONSTRAINT unique_spectrum UNIQUE(spectraid, wavelength)
);

/* data TABLEs */
CREATE TABLE trait_info(
    traitid INTEGER PRIMARY KEY,
    trait TEXT UNIQUE,
    traitdescription TEXT,
    unit TEXT,
    traitinfocomment TEXT
);

CREATE TABLE trait_data(
    traitdataid INTEGER PRIMARY KEY,
    samplecode TEXT NOT NULL REFERENCES samples (samplecode) ON DELETE CASCADE, 
    trait TEXT REFERENCES trait_info (trait) ON DELETE CASCADE,
    traitvalue NUMERIC,
    traitdatacomment TEXT
);
