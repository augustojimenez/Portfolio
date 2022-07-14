import os
import sys
import pyodbc
import requests
import xml.etree.ElementTree as ET

from requests.adapters import HTTPAdapter
from requests.packages.urllib3.util.retry import Retry

sys.path.append(os.path.abspath('./0_source/'))
from functions import *

os.chdir('./0_source')

retry_strategy = Retry(
        total = 3,
        status_forcelist = [408, 429, 500, 502, 503, 504, 404, 400],
        allowed_methods = ["HEAD", "GET", "OPTIONS"])
    
adapter = HTTPAdapter(max_retries = retry_strategy)

http = requests.Session()
http.mount("https://", adapter)
http.mount("http://", adapter)

end = False

try:
        connection = pyodbc.connect('Trusted_Connection=yes;',
                                Driver = '{SQL Server}',
                                Server = 'data',
                                Database = 'repositorio_datos')
        
        while end == False:
                print('Elija la fuente de datos que desea actualizar:')
                print('\t1 - Banco Mundial.')
                print('\t2 - Fondo Monetario Internacional (World Economic Outlook).')
                print('\t3 - Banco Central de la República Dominicana.')
                print('\t4 - Todos los anteriores.')
                print('\t5 - Inicializar tablas de países e indicadores.')
                print('\t6 - Salir.')
                
                option = int(input())

                if option == 6:
                        end = True
                elif option == 1:
                        # Updates World Bank database
                        # Language
                        # 'en' = 0
                        # 'es' = 1
                        wb_update(http, connection, lan = 0)
                elif option == 2:
                        # Updates International Monetary Fund database
                        print('Updating IMF data.')
                        imf_update(http, connection, database = "WEO_Apr_22")
                elif option == 3:
                        print('Work in progress.')
                elif option == 4:
                        # Updates World Bank database
                        wb_update(http, connection, lan = 0)
                        # Updates International Monetary Fund database
                        imf_update(http, connection, database = "WEO_Apr_22")
                elif option == 5:
                        init_ctry_kpi_tbl(http, connection, lan = 0)
                        init_ctry_kpi_tbl(http, connection, lan = 1)
except pyodbc.Error as e:
        print(f'Failed to insert record into COUNTRY table: {e}')
finally:
        connection.close()
        print('MySQL connection is closed')