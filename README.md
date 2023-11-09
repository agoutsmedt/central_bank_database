# The Central Bank Database

This repository gathers scripts to collect documents published by central banks and to clean this data to create a database of central banks' documents.

## Scraping scripts

You will find here all the scripts to scrap the documents published by central banks.

### scraping_bis.R

This script allows you to scrap the speeches published on the [Bank of International Settlements website](https://www.bis.org/cbspeeches/index.htm):
  
  - It extracts the metadata of the speeches (Title, date, author, etc...)
  - It cleans this metadata to identify more clearly the speaker, the central bank of the speaker, etc...
  - It downloads the corresponding pdf version of the scraped speeches
  - It extracts the text of these pdf versions.
  
## Cleaning scripts

Here are all the scripts to clean the data scraped and to merge them to build one big database of central banks communication.