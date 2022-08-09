USE REPOSITORIO_DATOS;
GO

SELECT
	C.COUNTRY_ISO3,
	C.COUNTRY_ISO2,
	C.NAME AS COUNTRY,
	C.REGION,
	C.INCOME_LEVEL,
	C.LENDING_TYPE,
	C.CAPITAL_CITY,
	K.INDICATOR AS INDICATOR_CODE,
	CASE										/* Reason: The IMF names their KPIs the same when it only changes the units of measurement.*/
		WHEN K.UNITS IS NULL THEN K.NAME		/* Hence, KPIs code is unique, but it's not named uniquely. But the combination of name */
		ELSE CONCAT(K.NAME, ' (', K.UNITS, ')')	/* and units of measurement is indeed unique. */
	END AS INDICATOR,
	K.SOURCE,
	K.SOURCE_NOTES,
	K.SUBJECT_NOTES,
	L.LANGUAGE,
	DT.DATE AS YEAR,
	DT.VALUE
FROM DATA_TABLE AS DT
JOIN COUNTRY AS C ON
	DT.COUNTRY_ISO3 = C.COUNTRY_ISO3
JOIN KPI AS K ON
	DT.INDICATOR = K.INDICATOR
JOIN LANGUAGES AS L ON
	K.LANGUAGE = L.ID AND
	K.LANGUAGE = C.LANGUAGE
WHERE
	DT.VALUE IS NOT NULL AND
	C.NAME IS NOT NULL AND
	K.NAME IS NOT NULL;