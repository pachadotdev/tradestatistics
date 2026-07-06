#' String matching of official country names and ISO-3 codes according to
#' the United Nations nomenclature
#' @description Takes a text string and searches within the
#' package data for a country code in the context of valid API country codes.
#' @param countryname A text string such as "Chile", "CHILE" or "CHL".
#' @return A single character if there is a exact match (e.g.
#' \code{ots_country_code("Chile")}) or a tibble in case of multiple matches
#' (e.g. \code{ots_country_code("Germany")})
#' @export
#' @examples
#' ots_country_code("Chile ")
#' ots_country_code("america")
#' ots_country_code("UNITED  STATES")
#' ots_country_code(" united_")
#' @keywords functions
ots_country_code <- function(countryname = NULL) {
  if (is.null(countryname)) {
    stop("'countryname' is NULL.")
  } else {
    stopifnot(is.character(countryname))
    
    countryname <- iconv(countryname, to = "ASCII//TRANSLIT", sub = " ")
    countryname <- gsub("[^[:alpha:]]", "", countryname)
    countryname <- toupper(countryname)
  }

  if (countryname == "") {
    stop("The input results in an empty string after removing multiple spaces and special symbols. Please check the spelling or explore the countries table provided within this package.")
  } else if (countryname == "all" | countryname == "ALL") {
    countrycode <- tradestatistics::ots_countries
  } else {
    countrycode <- tradestatistics::ots_countries[grepl(countryname, toupper(country))]
  }
  
  return(countrycode)
}

#' String matching of broad sector names and IDs
#' @description Takes a text string and searches within the
#' package data for all matching sector IDs in the context of valid API
#' sector codes.
#' @param sector A text string such as "Agriculture", "MINING" or "serv".
#' @return A data frame with all possible matches (no uppercase distinction)
#' showing the sector name and sector ID.
#' @export
#' @examples
#' ots_sector_code("Agriculture")
#' ots_sector_code("mining")
#' @keywords functions
ots_sector_code <- function(sector = NULL) {
  if (is.null(sector)) {
    stop("'sector' is NULL.")
  }
  stopifnot(is.character(sector))

  sector <- tolower(iconv(sector, to = "ASCII//TRANSLIT", sub = ""))
  sector <- gsub("[^[:alpha:]]", "", sector)

  if (sector == "") {
    stop("The input results in an empty string after removing multiple spaces and special symbols. Please check the spelling or explore the ots_sectors table provided within this package.")
  }

  d <- tradestatistics::ots_sectors[
    grepl(sector, tolower(tradestatistics::ots_sectors$broad_sector)), ]

  return(d)
}

#' String matching of industry names and IDs
#' @description Takes a text string and searches within the
#' package data for all matching industry IDs in the context of valid API
#' industry codes.
#' @param industry A text string such as "Wheat", "CEREAL" or "oilseed".
#' @return A data frame with all possible matches (no uppercase distinction)
#' showing the industry name and industry ID.
#' @export
#' @examples
#' ots_industry_code("Wheat")
#' ots_industry_code("cereal")
#' @keywords functions
ots_industry_code <- function(industry = NULL) {
  if (is.null(industry)) {
    stop("'industry' is NULL.")
  }
  stopifnot(is.character(industry))

  industry <- tolower(iconv(industry, to = "ASCII//TRANSLIT", sub = ""))
  industry <- gsub("[^[:alpha:]]", "", industry)

  if (industry == "") {
    stop("The input results in an empty string after removing multiple spaces and special symbols. Please check the spelling or explore the ots_industries table provided within this package.")
  }

  d <- tradestatistics::ots_industries[
    grepl(industry, tolower(tradestatistics::ots_industries$industry_descr)), ]

  return(d)
}
