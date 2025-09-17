# Tables ----

library(data.table)
library(dplyr)
library(jsonlite)

base_url <- "http://localhost:4949/"

tables_url <- paste0(base_url, "tables")
tables_raw_file <- "data-raw/ots_tables.csv"
tables_tidy_file <- "data/ots_tables.rda"

if (!file.exists(tables_raw_file)) {
  download.file(tables_url, tables_raw_file)
}

if (!file.exists(tables_tidy_file)) {
  ots_tables <- fread(tables_raw_file) %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")})
  save(ots_tables, file = tables_tidy_file, version = 2)
}

# Country codes ----

countries_url <- paste0(base_url, "countries")
countries_raw_file <- "data-raw/ots_countries.json"
countries_tidy_file <- "data/ots_countries.rda"

if (!file.exists(countries_raw_file)) {
  download.file(countries_url, countries_raw_file)
}

if (!file.exists(countries_tidy_file)) {
  ots_countries <- fromJSON(countries_raw_file) %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")}) %>% 
    mutate_if(is.numeric, as.integer) %>% 
    as.data.table()
  
  save(ots_countries, file = countries_tidy_file, version = 2)
}

# Commodity codes ----

commodities_url <- paste0(base_url, "commodities")
commodities_raw_file <- "data-raw/ots_commodities.json"
commodities_tidy_file <- "data/ots_commodities.rda"

if (!file.exists(commodities_raw_file)) {
  download.file(commodities_url, commodities_raw_file)
}

if (!file.exists(commodities_tidy_file)) {
  ots_commodities <- fromJSON(commodities_raw_file) %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")}) %>% 
    as.data.table()
  
  save(ots_commodities, file = commodities_tidy_file, version = 2, compress = "xz")
}

# Shorter commodity codes ----

commodities_short_url <- paste0(base_url, "commodities_short")
commodities_short_raw_file <- "data-raw/ots_commodities_short.json"
commodities_short_tidy_file <- "data/ots_commodities_short.rda"

if (!file.exists(commodities_short_raw_file)) {
  download.file(commodities_short_url, commodities_short_raw_file)
}

if (!file.exists(commodities_short_tidy_file)) {
  ots_commodities_short <- fromJSON(commodities_short_raw_file) %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")}) %>% 
    as.data.table()
  
  save(ots_commodities_short, file = commodities_short_tidy_file, version = 2, compress = "xz")
}

# GDP deflator ----

# Source
# https://data.worldbank.org/indicator/NY.GDP.DEFL.KD.ZG

gdp_deflator_url <- paste0(base_url, "gdp_deflator")
gdp_deflator_raw_file <- "data-raw/ots_gdp_deflator.json"
gdp_deflator_tidy_file <- "data/ots_gdp_deflator.rda"

if (!file.exists(gdp_deflator_raw_file)) {
  download.file(gdp_deflator_url, gdp_deflator_raw_file)
}

if (!file.exists(gdp_deflator_tidy_file)) {
  ots_gdp_deflator <- fromJSON(gdp_deflator_raw_file) %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")}) %>% 
    mutate(
      year_from = as.integer(year_from),
      year_to = as.integer(year_to),
      gdp_deflator = as.numeric(gdp_deflator)
    ) %>%
    as.data.table()
  
  save(ots_gdp_deflator, file = gdp_deflator_tidy_file, version = 2)
}
