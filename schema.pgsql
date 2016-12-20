/* Create database if it doesn't exist */
/* createdb leaf_spectra */

/* Delete old tables */
DROP TABLE IF EXISTS projects CASCADE;
DROP TABLE IF EXISTS sites CASCADE;
DROP TABLE IF EXISTS plots CASCADE;
DROP TABLE IF EXISTS species CASCADE;
DROP TABLE IF EXISTS samples CASCADE;
DROP TABLE IF EXISTS trait_info CASCADE;
DROP TABLE IF EXISTS instruments CASCADE;
DROP TABLE IF EXISTS specmethods CASCADE;
DROP TABLE IF EXISTS spectra_info CASCADE;
DROP TABLE IF EXISTS trait_data CASCADE;
DROP TABLE IF EXISTS spectra_data CASCADE;

/* Metadata tables */
CREATE TABLE projects(
    ID bigserial PRIMARY KEY,
    Code text,
    Description text,
    Affiliation text,
    PointOfContact text,
    Email text,
    Comment text
);

CREATE TABLE sites(
    ID bigserial PRIMARY KEY,
    Code text,
    Name text,
    Comment text
);

CREATE TABLE plots(
    ID bigserial PRIMARY KEY,
    SiteID bigint REFERENCES sites (ID),
    Code text,
    Name text,
    Latitude numeric,
    Longitude numeric,
    Comment text
);

CREATE TABLE species(
    ID bigserial PRIMARY KEY,
    Code text,
    CodeType text,
    ScientificName text UNIQUE,
    Genus text,
    Species text,
    Subspecies text,
    Family text,
    Authority text,
    Variety text,
    TryID bigint,
    Comment text
);

CREATE TABLE samples(
    ID bigserial PRIMARY KEY,
    FullName text UNIQUE,
    ProjectID bigint REFERENCES projects (ID),
    Name text,
    Year integer,
    CollectionDate date,
    PlotID bigint REFERENCES plots (ID),
    SpeciesID bigint REFERENCES species (ID),
    Comment text,
    UNIQUE (Name, ProjectID)
);

CREATE TABLE trait_info(
    ID bigserial PRIMARY KEY,
    Trait text UNIQUE,
    Description text,
    Unit text,
    Comment text
);

CREATE TABLE instruments(
    ID bigserial PRIMARY KEY,
    Name text,
    MinWavelength real,
    MaxWavelength real,
    SpectralResolution real,
    Comment text
);

CREATE TABLE specmethods(
    ID bigserial PRIMARY KEY,
    InstrumentID bigint REFERENCES instruments (ID),
    Apparatus text,
    Calibration text,
    Comment text
);

CREATE TABLE spectra_info(
    ID bigserial PRIMARY KEY,
    SampleID bigint REFERENCES samples (ID),
    Type text 
        CONSTRAINT legal_spectra_type 
        CHECK (Type = 'reflectance' OR Type = 'transmittance' OR Type = 'pseudo-absorbance'),
    SpecMethodID bigint REFERENCES specmethods (ID),
    Comment text
);

/* Data tables */
CREATE TABLE trait_data(
    ID bigserial PRIMARY KEY,
    SampleID bigint REFERENCES samples (ID),
    TraitID bigint REFERENCES trait_info (ID),
    Value numeric,
    Comment text
);

CREATE TABLE spectra_data(
    ID bigserial PRIMARY KEY,
    SpectraID bigint REFERENCES spectra_info,
    Wavelength numeric CONSTRAINT legal_wavelength CHECK (Wavelength > 0),
    Value numeric
);
    
