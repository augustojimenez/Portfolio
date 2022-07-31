# Collecting Residential Prices Using R

A construction company, specialized in building condominiums, wants to expand into other territories. Specifically, it is planning to build more premium condominiums in an unfamiliar neighborhood (one with higher income levels). Thus, it is directed towards a more demanding clientele.

This company’s general manager is uncertain on how to price each apartment, and asked me to help her out. In the Dominican Republic, there are no official records available on residential apartment prices. Hence, for this analysis, new data needed to be collected. This is done via web scraping supercasas.com using R.

supercasas.com is an e-commerce company that provides a platform that facilitates homeowner-to-homebuyer sales in the Dominican Republic. It is the most popular platform providing such service in this market.

# How to use
1) Uses R 4.1.1.
2) Install required libraries:
- `dplyr`
- `readr`
- `rvest`
- `stringi`

# Project structure

This project is structured using a scheme similar to that explained in this article: https://chrisvoncsefalvay.com/2018/08/09/structuring-r-projects/:

- 0_source: contains the codes used in the project, such as the one to extract the house prices, extract the neighborhoods, etc.
- 1_data: contains the raw (1_data/0_raw) and processed (1_data/1_processed) data used in the project.
