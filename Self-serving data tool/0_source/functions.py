import os
import pyodbc
import numpy as np
import pandas as pd
import xml.etree.ElementTree as ET

#### Funciones relativas a la actualizacion de BD de MySQL ####
def delete_indicator(indicator, connection, lan = 0):    
    delete_query = """
        DELETE FROM DATA_TABLE
        WHERE
            INDICATOR = ? AND
            LANGUAGE = ?"""
        
    cursor = connection.cursor()
    cursor.execute(delete_query, (indicator, lan))
    connection.commit()

def insert_into_table(df, connection):
    cursor = connection.cursor()
    insert_query = """
        INSERT INTO DATA_TABLE
            (country_iso3, date, value, indicator, language)
        VALUES
            (?, ?, ?, ?, ?) """

    for index, row in df.iterrows():
        record = (row['country_iso3'], row['date'], row['value'], row['indicator'], row['language'])
        cursor.execute(insert_query, record)
        connection.commit()

def populate_kpi_table(df, source, lan, connection):    
    delete_query = """
        DELETE FROM KPI
        WHERE
            SOURCE LIKE ? AND
            LANGUAGE = ? """

    # Delete prevous data from COUNTRY table where language matches
    cursor = connection.cursor()
    cursor.execute(delete_query, (source, lan))
    
    connection.commit()

    insert_query = """INSERT INTO KPI
                    (INDICATOR, NAME, SOURCE, SOURCE_NOTES, SUBJECT_NOTES, LANGUAGE, UNITS, SCALE)
                    VALUES
                    (?, ?, ?, ?, ?, ?, ?, ?) """
    
    for index, row in df.iterrows():
        record = (row['indicator'], row['name'], row['source'], row['source_notes'],
                    row['subject_notes'], row['language'], row['units'], row['scale'])

        cursor.execute(insert_query, record)
        connection.commit()

def populate_country_table(df, connection, lan = 0):
    truncate_query = """ DELETE FROM COUNTRY  WHERE LANGUAGE = ?"""

    # Delete prevous data from COUNTRY table where language matches
    cursor = connection.cursor()
    cursor.execute(truncate_query, (lan,))
    connection.commit()

    insert_query = """INSERT INTO COUNTRY
                    (COUNTRY_ISO3, COUNTRY_ISO2, NAME, REGION, ADMIN_REGION, INCOME_LEVEL, LENDING_TYPE, CAPITAL_CITY, LONGITUDE, LATITUDE, LANGUAGE)
                    VALUES
                    (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) """
        
    for index, row in df.iterrows():
        record = (row['country_iso3'],
                    row['country_iso2'],
                    row['name'],
                    row['region'],
                    row['admin_region'],
                    row['income_level'],
                    row['lending_type'],
                    row['capital_city'],
                    row['longitude'],
                    row['latitude'],
                    row['language'])
        
        cursor.execute(insert_query, record)
        connection.commit()

##### Funciones relativas al Banco Mundial ####
def wb_get_kpis(http, lan = 0):
    if(lan == 0):
        lan_desc = 'en'
    elif(lan == 1):
        lan_desc = 'es'

    indicators_wp = f'http://api.worldbank.org/V2/{lan_desc}/indicator?page='

    indicators = http.get(indicators_wp + str(1)).content.decode('utf-8')

    root = ET.XML(indicators)

    pages = int(root.attrib['pages'])

    indicator = []
    name = []
    source = []
    source_note = []
    source_organization = []

    for page in range(pages):
        print('\tWorking on page: ' + str(page + 1))
        
        indicators = http.get(indicators_wp + str(page + 1)).text[3:]
        
        root = ET.XML(indicators)

        for child in root:
            indicator.append(child.attrib['id'])
            name.append(child[0].text)
            source.append(child[2].text)
            source_note.append(child[3].text)
            source_organization.append(child[4].text)
        

    df = pd.DataFrame(data = [indicator, name, source, source_note, source_organization]).T
    
    df.columns = ['indicator', 'name', 'source', 'source_notes', 'subject_notes']
    df['source'] = 'world_bank'
    df['language'] = lan
    df['units'] = None
    df['scale'] = None

    return df

def wb_get_countries(http, lan = 0):
    if(lan == 0):
        lan_desc = 'en'
    elif(lan == 1):
        lan_desc = 'es'

    countries_wp = f'http://api.worldbank.org/V2/{lan_desc}/country?page='
    
    countries = http.get(countries_wp + str(1)).content.decode('utf-8')

    root = ET.XML(countries)

    pages = int(root.attrib['pages'])

    country_iso3 = []
    country_iso2 = []
    name = []
    region = []
    admin_region = []
    income_level = []
    lending_type = []
    capital_city = []
    longitude = []
    latitude = []

    for page in range(pages):
        print('\tWorking on page: ' + str(page + 1))
        countries = http.get(countries_wp + str(page + 1)).content.decode('utf-8')

        root = ET.XML(countries)

        for child in root:
            country_iso3.append(child.attrib['id'])
            country_iso2.append(child[0].text)
            name.append(child[1].text)
            region.append(child[2].text)
            admin_region.append(child[3].text)
            income_level.append(child[4].text)
            lending_type.append(child[5].text)
            capital_city.append(child[6].text)
            longitude.append(child[7].text)
            latitude.append(child[8].text)
    
    df = pd.DataFrame(data = [country_iso3, country_iso2, name, region, admin_region, income_level, lending_type, capital_city, longitude, latitude]).T
    df.columns = ['country_iso3', 'country_iso2', 'name', 'region', 'admin_region', 'income_level', 'lending_type', 'capital_city', 'longitude', 'latitude']
    df['language'] = lan

    return df

def wb_get_header(indicator, http, lan = 0):
    if(lan == 0):
        lan_desc = 'en'
    elif(lan == 1):
        lan_desc = 'es'

    data_wp = ['https://api.worldbank.org/V2/' + lan_desc + '/country/all/indicator/', '?page=']

    URL = data_wp[0] + indicator + data_wp[1] + str(1)

    wp = http.get(URL).content.decode('utf-8')
    
    root = ET.XML(wp)

    header = root.attrib

    return header

def is_updated(indicator, new_date):
    file_path = './1_data/BM/' + indicator + '.csv'
    
    if os.path.isfile(file_path):
        last_updated = pd.read_csv(file_path)['last_updated'].max()
        is_updated = last_updated == new_date
    else:
        is_updated = False
    
    return is_updated

def wb_get_data(indicator, http, lan, pages, last_updated):
    file_path = f'../1_data/BM/{indicator}.csv'

    if is_updated(indicator, last_updated):
        return None
    
    data_wp = ['https://api.worldbank.org/V2/', '/country/all/indicator/', '?page=']

    #variable = []
    #country = []
    country_iso3 = []
    date = []
    value = []

    if(lan == 0):
        lan_desc = 'en'
    elif(lan == 1):
        lan_desc = 'es'

    for page in range(pages):
        print('\tWorking on page: ' + str(page + 1))

        URL = data_wp[0] + lan_desc + data_wp[1] + indicator + data_wp[2] + str(page + 1)
        
        wp = http.get(URL).content.decode('utf-8')

        root = ET.XML(wp)
        
        for child in root:
            #variable.append(child[0].text)
            #country.append(child[1].text)
            country_iso3.append(child[2].text)
            date.append(child[3].text)
            value.append(child[4].text)
    
    df = pd.DataFrame([country_iso3, date, value]).T
    df.columns = ['country_iso3', 'date', 'value']
    df['indicator'] = indicator
    df['last_updated'] = last_updated
    df['language'] = lan

    if os.path.isfile(file_path):
        old_df = pd.read_csv('../1_data/BM/' + indicator + '.csv', index_col = [0])
        df = pd.concat([df, old_df])
    
    return df

# initializes country and kpi tables
def init_ctry_kpi_tbl(http, connection, lan = 0):
    if(lan == 0):
        lan_desc = 'en'
    elif(lan == 1):
        lan_desc = 'es'

    ### Updates countries list ###
    print('Initializing countries list')
    country = wb_get_countries(http, lan)
    country.to_csv(f'../1_data/BM/countries_{lan_desc}.csv', encoding = 'latin1')
    populate_country_table(country, connection, lan)

    ### Updates indicators list ###
    print('Initializing indicators list')
    wb_get_kpis(http, lan).to_csv(f'../1_data/BM/indicators_{lan_desc}.csv', encoding = 'ISO-8859-1')
    # to be improved
    kpi = pd.read_csv(f'../1_data/BM/indicators_{lan_desc}.csv', encoding = 'utf-8')
    kpi.drop_duplicates(subset = 'indicator', inplace = True)
    kpi.replace(np.nan, None, inplace = True)
    populate_kpi_table(kpi, 'world_bank', lan, connection)

def wb_update(http, connection, lan = 0):
    if(lan == 0):
        lan_desc = 'en'
    elif(lan == 1):
        lan_desc = 'es'

    ### List of KPIs to update ### Reemplazar por query al servidor
    kpi_to_update = pd.read_excel("../1_data/KPIs Institucionales.xlsx", "KPIs")

    ### Updates for World Bank ###
    kpi_to_update = kpi_to_update.loc[(kpi_to_update['Fuente de datos'] == 'Banco Mundial (BM)') & (kpi_to_update['API'] == 1), 'ID']

    ### Updates data for given indicator ###
    for indicator in kpi_to_update:
        indicator = indicator.strip()
        print('Working on ' + indicator)

        header = wb_get_header(indicator, http, lan)
        pages = int(header['pages'])
        new_date = header['lastupdated']

        df = wb_get_data(indicator, http, lan, pages, new_date)
        df.dropna(subset = ['country_iso3'], inplace = True)

        if df is None:
            print('No new update available.')
        else:
            delete_indicator(indicator, connection, lan)
            print('Old records have been deleted.')
            
            print('Inserting new records...')
            insert_into_table(df, connection)
            
            df.to_csv(f'../1_data/BM/{indicator}_{lan_desc}.csv')
            print('Updates saved.')

##### Funciones relativas al FMI ####
def get_imf_url(database = "WEO_Apr_22"):
    db_list = "../1_data/FMI/imf_db_list.xlsx"

    df = pd.read_excel(db_list)
    df = df.loc[df.database == database]
    
    name = df.iat[0, 0]
    url = df.iat[0, 1]

    return name, url

def get_imf_db(http, url, name):
    data = http.get(url)
    open(f'../1_data/FMI/{name}.xls', 'wb').write(data.content)

def imf_update(http, connection, database = "WEO_Apr_22"):
    name = get_imf_url(database)[0]
    url = get_imf_url(database)[1]
    
    get_imf_db(http, url, name)
    
    df = pd.read_excel(f'../1_data/FMI/{database}.xlsx', skipfooter = 2)
    df.replace(np.nan, None, inplace = True)
    df.replace('--', None, inplace = True)
    
    kpi = df[['WEO Subject Code', 'Subject Descriptor', 'Subject Notes', 'Units', 'Scale']].drop_duplicates().copy()
    kpi.columns = ['indicator', 'name', 'subject_notes', 'units', 'scale']
    kpi['source'] = database
    kpi['source_notes'] = None
    kpi['language'] = 0

    # Buscar forma automatica de actualizar rango de paises disponibles
    df = pd.melt(df, id_vars = ['ISO', 'WEO Subject Code'], value_vars = list(range(1980, 2028)))[['ISO', 'variable', 'value', 'WEO Subject Code']]
    df.columns = ['country_iso3', 'date', 'value', 'indicator']

    # Manually change Kosovo country code (ISO3), from UVK to XKX
    df['country_iso3'] = np.where(df['country_iso3'] == 'UVK', 'XKX', df['country_iso3'])
    # Manually change West Bank and Gaza country code (ISO3), from WBG to PSE
    df['country_iso3'] = np.where(df['country_iso3'] == 'WBG', 'PSE', df['country_iso3'])            
    
    df['language'] = 0
    
    populate_kpi_table(kpi, database, 0, connection)

    for indicator in df['indicator'].unique():
        delete_indicator(indicator, connection)

    insert_into_table(df, connection)

    print("International Monetary Fund database has been updated.")