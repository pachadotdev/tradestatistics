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
    countryname <- tolower(countryname)
  }

  countryname <- switch(
    countryname,
    "us" = "usa",
    "america" = "usa",
    "united states" = "usa",
    "united states of america" = "usa",
    "uk" = "united kingdom",
    "england" = "united kingdom",
    "scotland" = "united kingdom",
    "holland" = "netherlands",
    "myanmar" = "burma",
    "persia" = "iran",
    "siam" = "thailand",
    "indochina" = "vietnam",
    "rhodesia" = "zimbabwe",
    "british honduras" = "belice",
    "bengal" = "bangladesh",
    "east pakistan" = "bangladesh",
    "zaire" = "democratic republic of the congo",
    countryname
  )

  if (countryname == "") {
    stop("The input results in an empty string after removing multiple spaces and special symbols. Please check the spelling or explore the countries table provided within this package.")
  } else if (countryname == "all" | countryname == "ALL") {
    countrycode <- tradestatistics::ots_countries
  } else {
    countrycode <- tradestatistics::ots_countries[grepl(countryname, tolower(country_name))]
  }
  
  return(countrycode)
}

#' String matching of official commodity/section names and Harmonized System (HS) codes
#' according to the United Nations nomenclature
#' @description Takes a text string and searches within the
#' package data for all matching commodity codes in the context of valid API
#' commodity codes.
#' @param commodity A text string such as "Animals", "COPPER" or "fruits".
#' @param section A text string such as "meat", "FISH" or "Dairy".
#' @param chapter A text string such as "animal", "VEGETABLES" or "Wood".
#' @return A tibble with all possible matches (no uppercase distinction)
#' showing the commodity name and commodity code
#' @export
#' @examples
#' ots_commodity_code(commodity = "ANIMALS ")
#' ots_commodity_code(section = "  fish")
#' ots_commodity_code(commodity = "Milk", section = "Dairy")
#' @keywords functions
ots_commodity_code <- function(commodity = NULL, section = NULL, chapter = NULL) {
  # For data.table variable binding warnings
  .SD <- NULL; commodity_name <- NULL; section_name <- NULL; chapter_name <- NULL
  if (is.null(commodity) && is.null(section) && is.null(chapter)) {
    stop("'commodity', 'section', and 'chapter' are all NULL.")
  }

  # Match by commodity name only
  if (!is.null(commodity) && is.null(section) && is.null(chapter)) {
    stopifnot(is.character(commodity))
    commodity <- tolower(iconv(commodity, to = "ASCII//TRANSLIT", sub = ""))
    commodity <- gsub("[^[:alpha:]]", "", commodity)
    if (commodity == "") {
      stop("The input results in an empty string after removing multiple spaces and special symbols. Please check the spelling or explore the commodities table provided within this package.")
    } else {
      d <- tradestatistics::ots_commodities[grepl(commodity, tolower(commodity_name)), .SD, .SDcols = c("commodity_code", "commodity_name", "chapter_code", "chapter_name", "section_code", "section_name")]
    }
  }

  # Match by section name only
  if (is.null(commodity) && !is.null(section) && is.null(chapter)) {
    stopifnot(is.character(section))
    section <- tolower(iconv(section, to = "ASCII//TRANSLIT", sub = ""))
    section <- gsub("[^[:alpha:]]", "", section)
    if (section == "") {
      stop("The input results in an empty string after removing multiple spaces and special symbols. Please check the spelling or explore the commodities table provided within this package.")
    } else {
      d <- tradestatistics::ots_commodities[grepl(section, tolower(section_name)), .SD, .SDcols = c("commodity_code", "commodity_name", "chapter_code", "chapter_name", "section_code", "section_name")]
    }
  }

  # Match by both commodity and section name
  if (!is.null(commodity) && !is.null(section) && is.null(chapter)) {
    stopifnot(is.character(commodity))
    stopifnot(is.character(section))
    commodity <- tolower(iconv(commodity, to = "ASCII//TRANSLIT", sub = ""))
    commodity <- gsub("[^[:alpha:]]", "", commodity)
    section <- tolower(iconv(section, to = "ASCII//TRANSLIT", sub = ""))
    section <- gsub("[^[:alpha:]]", "", section)
    if (commodity == "" || section == "") {
      stop("The input results in an empty string after removing multiple spaces and special symbols. Please check the spelling or explore the commodities table provided within this package.")
    } else {
      d <- tradestatistics::ots_commodities[
        grepl(commodity, tolower(commodity_name)) &
        grepl(section, tolower(section_name)),
        .SD, .SDcols = c("commodity_code", "commodity_name", "chapter_code", "chapter_name", "section_code", "section_name")
      ]
    }
  }

  # Match by chapter name only
  if (is.null(commodity) && is.null(section) && !is.null(chapter)) {
    stopifnot(is.character(chapter))
    chapter <- tolower(iconv(chapter, to = "ASCII//TRANSLIT", sub = ""))
    chapter <- gsub("[^[:alpha:]]", "", chapter)
    if (chapter == "") {
      stop("The input results in an empty string after removing multiple spaces and special symbols. Please check the spelling or explore the commodities table provided within this package.")
    } else {
      d <- tradestatistics::ots_commodities[grepl(chapter, tolower(chapter_name)), .SD, .SDcols = c("commodity_code", "commodity_name", "chapter_code", "chapter_name", "section_code", "section_name")]
    }
  }

  # Match by commodity and chapter
  if (!is.null(commodity) && is.null(section) && !is.null(chapter)) {
    stopifnot(is.character(commodity))
    stopifnot(is.character(chapter))
    commodity <- tolower(iconv(commodity, to = "ASCII//TRANSLIT", sub = ""))
    commodity <- gsub("[^[:alpha:]]", "", commodity)
    chapter <- tolower(iconv(chapter, to = "ASCII//TRANSLIT", sub = ""))
    chapter <- gsub("[^[:alpha:]]", "", chapter)
    if (commodity == "" || chapter == "") {
      stop("The input results in an empty string after removing multiple spaces and special symbols. Please check the spelling or explore the commodities table provided within this package.")
    } else {
      d <- tradestatistics::ots_commodities[
        grepl(commodity, tolower(commodity_name)) &
        grepl(chapter, tolower(chapter_name)),
        .SD, .SDcols = c("commodity_code", "commodity_name", "chapter_code", "chapter_name", "section_code", "section_name")
      ]
    }
  }

  # Match by section and chapter
  if (is.null(commodity) && !is.null(section) && !is.null(chapter)) {
    stopifnot(is.character(section))
    stopifnot(is.character(chapter))
    section <- tolower(iconv(section, to = "ASCII//TRANSLIT", sub = ""))
    section <- gsub("[^[:alpha:]]", "", section)
    chapter <- tolower(iconv(chapter, to = "ASCII//TRANSLIT", sub = ""))
    chapter <- gsub("[^[:alpha:]]", "", chapter)
    if (section == "" || chapter == "") {
      stop("The input results in an empty string after removing multiple spaces and special symbols. Please check the spelling or explore the commodities table provided within this package.")
    } else {
      d <- tradestatistics::ots_commodities[
        grepl(section, tolower(section_name)) &
        grepl(chapter, tolower(chapter_name)),
        .SD, .SDcols = c("commodity_code", "commodity_name", "chapter_code", "chapter_name", "section_code", "section_name")
      ]
    }
  }

  # Match by commodity, section, and chapter
  if (!is.null(commodity) && !is.null(section) && !is.null(chapter)) {
    stopifnot(is.character(commodity))
    stopifnot(is.character(section))
    stopifnot(is.character(chapter))
    commodity <- tolower(iconv(commodity, to = "ASCII//TRANSLIT", sub = ""))
    commodity <- gsub("[^[:alpha:]]", "", commodity)
    section <- tolower(iconv(section, to = "ASCII//TRANSLIT", sub = ""))
    section <- gsub("[^[:alpha:]]", "", section)
    chapter <- tolower(iconv(chapter, to = "ASCII//TRANSLIT", sub = ""))
    chapter <- gsub("[^[:alpha:]]", "", chapter)
    if (commodity == "" || section == "" || chapter == "") {
      stop("The input results in an empty string after removing multiple spaces and special symbols. Please check the spelling or explore the commodities table provided within this package.")
    } else {
      d <- tradestatistics::ots_commodities[
        grepl(commodity, tolower(commodity_name)) &
        grepl(section, tolower(section_name)) &
        grepl(chapter, tolower(chapter_name)),
        .SD, .SDcols = c("commodity_code", "commodity_name", "chapter_code", "chapter_name", "section_code", "section_name")
      ]
    }
  }

  return(d)
}
