# Tables ----

library(data.table)
library(jsonlite)

base_url <- "http://localhost:5000/"

tables_url <- paste0(base_url, "tables")
tables_raw_file <- "data-raw/ots_tables.json"
tables_tidy_file <- "data/ots_tables.rda"

if (!file.exists(tables_raw_file)) {
  download.file(tables_url, tables_raw_file)
}

if (!file.exists(tables_tidy_file)) {
  ots_tables <- as.data.table(fromJSON(tables_raw_file))
  save(ots_tables, file = tables_tidy_file, version = 2)
}

# Country codes ----

countries_url <- paste0(base_url, "countries")
countries_raw_file <- "data-raw/ots_countries.json"
countries_tidy_file <- "data/ots_countries.rda"

countries_colours_url <- paste0(base_url, "countries_colours")
countries_colours_raw_file <- "data-raw/ots_countries_colours.json"
countries_colours_tidy_file <- "data/ots_colours_countries.rda"

if (!file.exists(countries_raw_file)) {
  download.file(countries_url, countries_raw_file)
}

if (!file.exists(countries_colours_raw_file)) {
  download.file(countries_colours_url, countries_colours_raw_file)
}

if (!file.exists(countries_tidy_file)) {
  ots_countries <- as.data.table(fromJSON(countries_raw_file))
  ots_countries_colours <- as.data.table(fromJSON(countries_colours_raw_file))

  setnames(ots_countries_colours, "iso3_dynamic", "dynamic_code")

  ots_countries <- merge(ots_countries, ots_countries_colours)

  ots_countries[, continent := fcase(
    region_id == 1, "Not applicable",
    region_id == 2, "Africa",
    region_id %in% c(3L, 4L, 10L, 12L), "Americas",
    region_id %in% c(5L, 6L, 7L, 9L, 13L, 14L), "Asia",
    region_id == 8L, "Europe",
    region_id == 11L, "Oceania",
    default = "Not applicable"
  )]

  ots_countries[, region_id := NULL]

  setnames(ots_countries, "region_colour", "colour")
  setcolorder(ots_countries, "colour", after = "continent")
  setcolorder(ots_countries, "iso3", before = "dynamic_code")

  save(ots_countries, file = countries_tidy_file, version = 2)
}

# Sector codes ----

sectors_url <- paste0(base_url, "sectors")
sectors_raw_file <- "data-raw/ots_sectors.json"
sectors_tidy_file <- "data/ots_sectors.rda"

sectors_colours_url <- paste0(base_url, "sectors_colours")
sectors_colours_raw_file <- "data-raw/ots_sectors_colours.json"
sectors_colours_tidy_file <- "data/ots_sectors_colours.rda"

if (!file.exists(sectors_raw_file)) {
  download.file(sectors_url, sectors_raw_file)
}

if (!file.exists(sectors_colours_raw_file)) {
  download.file(sectors_colours_url, sectors_colours_raw_file)
}

if (!file.exists(sectors_tidy_file)) {
  ots_sectors <- as.data.table(fromJSON(sectors_raw_file))
  ots_sectors_colours <- as.data.table(fromJSON(sectors_colours_raw_file))

  ots_sectors <- merge(ots_sectors, ots_sectors_colours)

  setcolorder(ots_sectors, "broad_sector", before = "broad_sector_id")

  save(ots_sectors, file = sectors_tidy_file, version = 2)
}

# Industry codes ----

industries_url <- paste0(base_url, "industries")
industries_raw_file <- "data-raw/ots_industries.json"
industries_tidy_file <- "data/ots_industries.rda"

if (!file.exists(industries_raw_file)) {
  download.file(industries_url, industries_raw_file)
}

if (!file.exists(industries_tidy_file)) {
  ots_industries <- as.data.table(fromJSON(industries_raw_file))
  save(ots_industries, file = industries_tidy_file, version = 2)
}
