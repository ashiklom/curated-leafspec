/* Create database if it doesn't exist */
/* createdb leaf_spectra */

/* Delete old tables */
DROP TABLE IF EXISTS projects CASCADE;
DROP TABLE IF EXISTS sites CASCADE;
DROP TABLE IF EXISTS plots CASCADE;
DROP TABLE IF EXISTS species CASCADE;
DROP TABLE IF EXISTS species_dict CASCADE;
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
    Code text UNIQUE,
    Description text,
    Affiliation text,
    PointOfContact text,
    Email text,
    Comment text
);

CREATE TABLE sites(
    ID bigserial PRIMARY KEY,
    Code text UNIQUE, 
    Description text,
    Comment text
);

CREATE TABLE plots(
    ID bigserial PRIMARY KEY,
    SiteCode text REFERENCES sites (Code),
    Code text UNIQUE,
    Description text,
    Latitude numeric,
    Longitude numeric,
    Comment text
);

CREATE TABLE species(
    ID bigserial PRIMARY KEY,
    Code text UNIQUE,
    CodeType text,
    ScientificName text,
    Genus text,
    Species text,
    Subspecies text,
    Variety text,
    SubVariety text,
    Forma text,
    Family text,
    Authority text,
    TryID bigint,
    Comment text
);

CREATE TABLE species_dict(
    ID bigserial PRIMARY KEY,
    DataCode text,
    ProjectCode text REFERENCES projects (Code),
    SpeciesCode text REFERENCES species (Code),
    Comment text,
    CONSTRAINT unique_datacode_project 
        UNIQUE (DataCode, ProjectCode)
);

CREATE TABLE samples(
    ID bigserial PRIMARY KEY,
    Code text UNIQUE,
    ProjectCode text REFERENCES projects (Code),
    Year integer,
    CollectionDate date,
    PlotCode text REFERENCES plots (Code),
    SpeciesCode text REFERENCES species (Code),
    CanopyPosition text,
    NeedleOldNew text,
    NeedleAge text,
    OtherCondition text,
    Comment text
);

CREATE TABLE sample_condition(
    ID bigserial PRIMARY KEY,
    SampleCode text REFERENCES samples (Code),
    Condition text REFERENCES sample_condition_info (Condition),
    Value text,
    Comment text
);

CREATE TABLE sample_condition_info(
    ID bigserial PRIMARY KEY,
    Condition text,
    Description text,
    Comment text
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
    SampleCode text NOT NULL REFERENCES samples (Code),
    Type text 
        CONSTRAINT legal_spectra_type 
        CHECK (Type = 'reflectance' OR 
                Type = 'transmittance' OR 
                Type = 'pseudo-absorbance'),
    SpecMethodID bigint REFERENCES specmethods (ID),
    SamplePrep text,
    Comment text
);

/* Data tables */
CREATE TABLE trait_data(
    ID bigserial PRIMARY KEY,
    SampleCode text NOT NULL REFERENCES samples (Code),
    Trait text REFERENCES trait_info (Trait),
    Value numeric,
    Comment text
);

CREATE TABLE spectra_data(
    ID bigserial PRIMARY KEY,
    SpectraID bigint NOT NULL REFERENCES spectra_info,
    Wavelength numeric CONSTRAINT legal_wavelength CHECK (Wavelength > 0),
    Value numeric
);
