# Using OCR to collect data COVID-19 statistics

In the Dominican Republic, public institutions do not usually share data with one another. This was particularly true during the first months of COVID-19 pandemic. Thus, data is mainly limited to the one published through official mediums. Additionally, given the importance of social media, said data is often published firstly via social apps, such as Instagram or Facebook.

At the Ministry of Economy, Planning, and Development, we wanted to make publicly available any and all data regarding the COVID-19 pandemic. For this purpose, we designed a dashboard containing relevant statistics shown in an appealing manner.

The code and data available in this repository are part of such dashboard. Here, I put on display one of the my many contributions to this dashboard. I used Optical Character Recognition (OCR) to convert images (i.e. Instagram posts published by the Dominican Republic’s Ministry of Health) into .csv files.

You can access to the aforementioned dashboard here: https://mepyd.gob.do/datos.

# Skills at display

- Data processing
- Data cleaning

# Requirements

1) Implemented using R 4.1.1.
2) Using the following libraries:
- `tesseract`
- `magick`
- `dplyr`
- `purr`
- `reticulate`
- `stringi`

# Project structure

This project is structured using a scheme similar to that explained in this article: https://chrisvoncsefalvay.com/2018/08/09/structuring-r-projects/:

- 0_source: contains the code used in the project.
- 1_data: contains the raw (1_data/0_raw) and processed (1_data/1_processed) data used in the project.
