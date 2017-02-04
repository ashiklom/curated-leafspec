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
DROP TABLE IF EXISTS sample_condition CASCADE;
DROP TABLE IF EXISTS sample_condition_info CASCADE;

/* Metadata tables */
CREATE TABLE projects(
    ProjectID bigserial PRIMARY KEY,
    ProjectCode text UNIQUE,
    ProjectDescription text,
    PointOfContact text,
    Email text,
    DOI text,
    ProjectComment text
);

CREATE TABLE sites(
    SiteID bigserial PRIMARY KEY,
    SiteCode text UNIQUE, 
    SiteDescription text,
    SiteComment text
);

CREATE TABLE plots(
    PlotID bigserial PRIMARY KEY,
    SiteCode text REFERENCES sites (SiteCode) ON DELETE CASCADE,
    PlotCode text UNIQUE,
    PlotDescription text,
    Latitude numeric,
    Longitude numeric,
    PlotComment text
);

CREATE TABLE species(
    SpeciesID bigserial PRIMARY KEY,
    SpeciesCode text UNIQUE,
    SpeciesCodeType text,
    ScientificName text,
    Genus text,
    Species text,
    Subspecies text,
    Variety text,
    SubVariety text,
    Forma text,
    Family text,
    Authority text,
    TrySpeciesID bigint,
    SpeciesComment text
);

CREATE TABLE species_dict(
    SpeciesDictID bigserial PRIMARY KEY,
    SpeciesDataCode text,
    ProjectCode text REFERENCES projects (ProjectCode) ON DELETE CASCADE,
    SpeciesCode text REFERENCES species (SpeciesCode) ON DELETE CASCADE,
    SpeciesDictComment text,
    CONSTRAINT unique_datacode_project 
        UNIQUE (SpeciesDataCode, ProjectCode)
);

CREATE TABLE samples(
    SampleID bigserial PRIMARY KEY,
    SampleCode text UNIQUE,
    ProjectCode text REFERENCES projects (ProjectCode) ON DELETE CASCADE,
    Year integer,
    CollectionDate date,
    PlotCode text REFERENCES plots (PlotCode) ON DELETE CASCADE,
    SpeciesCode text REFERENCES species (SpeciesCode) ON DELETE CASCADE,
    CanopyPosition text,
    NeedleOldNew text,
    NeedleAge text,
    OtherCondition text,
    SampleComment text
);

CREATE TABLE sample_condition_info(
    ConditionID bigserial PRIMARY KEY,
    Condition text UNIQUE,
    ConditionDescription text,
    ConditionComment text
);

CREATE TABLE sample_condition(
    ConditionDataID bigserial PRIMARY KEY,
    SampleCode text REFERENCES samples (SampleCode) ON DELETE CASCADE,
    Condition text REFERENCES sample_condition_info (Condition) ON DELETE CASCADE,
    ConditionValue text,
    ConditionValueComment text
);

CREATE TABLE instruments(
    InstrumentID bigserial PRIMARY KEY,
    InstrumentName text UNIQUE,
    MinWavelength real,
    MaxWavelength real,
    SpectralResolution real,
    InstrumentComment text
);

CREATE TABLE specmethods(
    SpecMethodID bigserial PRIMARY KEY,
    InstrumentID bigint REFERENCES instruments (InstrumentID) ON DELETE CASCADE,
    Apparatus text,
    Calibration text,
    SpecMethodComment text
);

CREATE TABLE spectra_info(
    SpectraID bigserial PRIMARY KEY,
    SampleCode text NOT NULL REFERENCES samples (SampleCode) ON DELETE CASCADE,
    SpectraType text 
        CONSTRAINT legal_spectra_type 
        CHECK (SpectraType = 'reflectance' OR 
                SpectraType = 'transmittance' OR 
                SpectraType = 'pseudo-absorbance'),
    SpecMethodID bigint REFERENCES specmethods (SpecMethodID) ON DELETE CASCADE,
    SamplePrep text,
    Comment text
);

CREATE TABLE spectra_data(
    SpectraDataID bigserial PRIMARY KEY,
    SpectraID bigint NOT NULL REFERENCES spectra_info (SpectraID) ON DELETE CASCADE,
    Wavelength numeric CONSTRAINT legal_wavelength CHECK (Wavelength > 0),
    SpectraValue numeric
);

/* Data tables */
CREATE TABLE trait_info(
    TraitID bigserial PRIMARY KEY,
    Trait text UNIQUE,
    TraitDescription text,
    Unit text,
    TraitInfoComment text
);

CREATE TABLE trait_data(
    TraitDataID bigserial PRIMARY KEY,
    SampleCode text NOT NULL REFERENCES samples (SampleCode) ON DELETE CASCADE, 
    Trait text REFERENCES trait_info (Trait) ON DELETE CASCADE,
    TraitValue numeric,
    TraitDataComment text
);
