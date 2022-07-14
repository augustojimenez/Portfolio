USE REPOSITORIO_DATOS;
GO

IF OBJECT_ID('dbo.DATA_TABLE', 'U') IS NOT NULL
	DROP TABLE dbo.DATA_TABLE;
GO

IF OBJECT_ID('dbo.COUNTRY', 'U') IS NOT NULL
	DROP TABLE dbo.COUNTRY;
GO

IF OBJECT_ID('dbo.KPI', 'U') IS NOT NULL
	DROP TABLE dbo.KPI;
GO

IF OBJECT_ID('dbo.LANGUAGES', 'U') IS NOT NULL
	DROP TABLE dbo.LANGUAGES;
GO

CREATE TABLE LANGUAGES (
	ID			INT NOT NULL PRIMARY KEY IDENTITY(0, 1),
    LANGUAGE	CHAR(2) NOT NULL);
GO

INSERT INTO LANGUAGES (LANGUAGE)
VALUES
	('en'),
	('es');

CREATE TABLE COUNTRY (
	COUNTRY_ISO3	CHAR(3) NOT NULL,
    COUNTRY_ISO2	CHAR(2),
    NAME			VARCHAR(100),
    REGION			VARCHAR(100),
    ADMIN_REGION	VARCHAR(100),
    INCOME_LEVEL	VARCHAR(100),
    LENDING_TYPE	VARCHAR(100),
    CAPITAL_CITY	VARCHAR(100),
    LONGITUDE		FLOAT(10),
    LATITUDE		FLOAT(10),
    LANGUAGE		INT NOT NULL,
	PRIMARY KEY (COUNTRY_ISO3, LANGUAGE),
	CONSTRAINT FK_LANGUAGE_COUNTRY FOREIGN KEY (LANGUAGE) REFERENCES LANGUAGES(ID));
GO

CREATE TABLE KPI (
	INDICATOR		VARCHAR(50) NOT NULL,
    NAME			TEXT,
    SOURCE			TEXT,
    SOURCE_NOTES	TEXT,
    SUBJECT_NOTES 	TEXT,
    LANGUAGE 		INT NOT NULL,
    UNITS			VARCHAR(50),
    SCALE			VARCHAR(8),
	PRIMARY KEY (INDICATOR, LANGUAGE),
	CONSTRAINT FK_LANGUAGE_KPI FOREIGN KEY (LANGUAGE) REFERENCES LANGUAGES(ID));
GO

CREATE TABLE DATA_TABLE (
	COUNTRY_ISO3	CHAR(3) NOT NULL,
    DATE			INTEGER NOT NULL,
    VALUE			FLOAT,
    INDICATOR		VARCHAR(50) NOT NULL,
	LANGUAGE		INT NOT NULL,
    PRIMARY KEY (COUNTRY_ISO3, INDICATOR, LANGUAGE, DATE),
	CONSTRAINT FK_INDICATOR FOREIGN KEY (INDICATOR, LANGUAGE) REFERENCES KPI(INDICATOR, LANGUAGE),
	CONSTRAINT FK_COUNTRY FOREIGN KEY (COUNTRY_ISO3, LANGUAGE) REFERENCES COUNTRY(COUNTRY_ISO3, LANGUAGE));
GO