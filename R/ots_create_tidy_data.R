#' Downloads and processes the data from the API to return a human-readable tibble
#' @description Accesses \code{api.tradestatistics.io} and
#' performs different API calls to transform and return tidy data.
#' @param years Year contained within the years specified in
#' api.tradestatistics.io/year_range (e.g., \code{2002} or \code{seq(2002, 2010, 2)}).
#' Default set to \code{2019}.
#' @param importers Importers (e.g. \code{"chl"}, \code{"Chile"} or
#' \code{c("chl", "Peru")}). Default set to \code{"all"}.
#' @param exporters Exporters (e.g. \code{"chl"}, \code{"Chile"} or
#' \code{c("chl", "Peru")}). Default set to \code{"all"}.
#' @param sectors Sectors (e.g. \code{"Agriculture"}, \code{1} or search matches for \code{"agri"})
#' to filter sectors. Default set to \code{"all"}.
#' @param industries Industries (e.g. \code{"Cereal products"}, \code{5} or search matches for \code{"cereal"})
#' to filter industries. Default set to \code{"all"}.
#' @param table Character string to select the table to obtain the data.
#' Default set to \code{itpde_imp} (aggregate trade by importer-year).
#' Run \code{ots_tables} in case of doubt.
#' @param max_attempts How many times to try to download data in case the
#' API or the internet connection fails when obtaining data. Default set
#' to \code{5}.
#' @param use_cache Logical to save and load from cache. If \code{TRUE}, the results will be cached in memory
#' if \code{file} is \code{NULL} or on disk if `file` is not \code{NULL}. Default set to \code{FALSE}.
#' @param file Optional character with the full file path to save the data. Default set to \code{NULL}.
#' @return A tibble that describes bilateral trade metrics (imports,
#' exports, trade balance and relevant metrics
#' such as exports growth w/r to last year) between a \code{reporter}
#' and \code{partner} country.
#' @importFrom data.table `:=` rbindlist setnames
#' @importFrom crul HttpClient
#' @export
#' @examples
#' \dontrun{
#' # The next examples can take more than 5 seconds to compute,
#' # so these are just shown without evaluation according to CRAN rules
#'
#' # Run `ots_countries` to display the full table of countries
#' # Run `ots_sectors` to display the full table of sectors
#'
#' # Agricultural trade (sector)
#' ots_create_tidy_data(years = 2020, sectors = "Agriculture",
#'  table = "itpde_sec")
#' 
#' # Cereal products trade (industry-importer)
#' ots_create_tidy_data(years = 2020, importers = "chl",
#'  industries = "Cereal products", table = "itpde_imp_ind")
#'
#' # Aggregate trade (bilateral)
#' ots_create_tidy_data(years = 2020, importers = "chl",
#'  exporters = c("arg", "bra"), table = "itpde_imp_exp")
#' }
#' @keywords functions
ots_create_tidy_data <- function(years = 2020,
                                 importers = "all",
                                 exporters = "all",
                                 sectors = "all",
                                 industries = "all",
                                 table = "itpde_imp",
                                 max_attempts = 5,
                                 use_cache = FALSE,
                                 file = NULL) {
  if (!is.logical(use_cache)) {
    stop("use_cache must be logical.")
  }
  
  if (!any(c(is.null(file), is.character(file)))) {
    stop("file must be NULL or character.")
  }

  # convert reporter/partner to uppercase
  importers <- toupper(importers)
  exporters <- toupper(exporters)
  
  ots_cache(
    use_cache = use_cache,
    file = file,
    years = years,
    importers = importers,
    exporters = exporters,
    sectors = sectors,
    industries = industries,
    table = table,
    max_attempts = max_attempts
  )
}

#' Downloads and processes the data from the API to return a human-readable tibble (unmemoised, internal)
#' @description A separation of \code{ots_create_tidy_data()} for making caching optional.
#' @importFrom jsonlite fromJSON
#' @keywords internal
ots_create_tidy_data_unmemoised <- function(years = 2018,
                                            importers = "usa",
                                            exporters = "all",
                                            sectors = "all",
                                            industries = "all",
                                            table = "itpde_imp",
                                            max_attempts = 5) {
  # silence data.table no visible binding notes
  country <- NULL; dynamic_code <- NULL; importer_name <- NULL; exporter_name <- NULL

  # Check tables ----
  if (!table %in% tradestatistics::ots_tables$table) {
    stop("The requested table does not exist. Please check the spelling or explore the 'ots_table' table provided within this package.")
  }

  # Check years ----
  year_depending_queries <- grep("^importers|^exporters|^itpd",
    tradestatistics::ots_tables$table,
    value = TRUE
  )

  # year_range <- try(fromJSON("http://127.0.0.1:5000/years"))
  year_range <- try(fromJSON("https://api.tradestatistics.io/years"))
  year_range <- try(as.numeric(year_range$year))

  if (all(years %in% min(year_range):max(year_range)) != TRUE &&
    table %in% year_depending_queries) {
    stop("Provided that the table you requested contains a 'year' field, please verify that you are requesting data contained within the years from api.tradestatistics.io/year_range.")
  }

  # Check importers and exporters ----
  importer_depending_queries <- grep("_imp|^itpde$|^itpds$",
    tradestatistics::ots_tables$table,
    value = TRUE
  )

  exporter_depending_queries <- grep("_exp|^itpde$|^itpds$",
    tradestatistics::ots_tables$table,
    value = TRUE
  )

  if (!is.null(importers)) {
    if (!(length(importers) == 1 && identical(importers, "ALL")) &&
        !all(importers %in% tradestatistics::ots_countries$dynamic_code) == TRUE && table %in% importer_depending_queries) {
      importers_iso <- importers[importers %in% tradestatistics::ots_countries$dynamic_code]
      importers_no_iso <- importers[!importers %in% tradestatistics::ots_countries$dynamic_code]

      importers_no_iso <- vapply(
        seq_along(importers_no_iso),
        function(x) {
          y <- tradestatistics::ots_country_code(importers_no_iso[x])

          if (nrow(y) == 0) {
            stop("It was not possible to find ISO codes for any of the importers you requested. Please check ots_countries.")
          } else {
            y <- y[, .(dynamic_code)]
            y <- as.vector(unlist(y))
          }

          if (length(y) > 1) {
            # print("----")
            # print(y)
            stop("There are multiple matches for the importers you requested. Please check ots_countries.")
          } else {
            return(y)
          }
        },
        character(1)
      )

      importers <- unique(c(importers_iso, importers_no_iso))
    }
  }

  if (!is.null(exporters)) {
    if (
      !(length(exporters) == 1 && identical(exporters, "ALL")) &&
      isTRUE(!all(exporters %in% tradestatistics::ots_countries$dynamic_code)) &&
      table %in% exporter_depending_queries
    ) {
      exporters_iso <- exporters[exporters %in% tradestatistics::ots_countries$dynamic_code]
      exporters_no_iso <- exporters[!exporters %in% tradestatistics::ots_countries$dynamic_code]

      exporters_no_iso <- sapply(
        seq_along(exporters_no_iso),
        function(x) {
          y <- tradestatistics::ots_country_code(exporters_no_iso[x])

          if (nrow(y) == 0) {
            stop("There are multiple matches for the exporters you requested. Please check ots_countries.")
          } else {
            y <- y[, .(dynamic_code)]
            y <- as.vector(unlist(y))
          }

          if (length(y) > 1) {
            stop("There are multiple matches for the exporters you requested. Please check ots_countries.")
          } else {
            return(y)
          }
        }
      )

      exporters <- unique(c(exporters_iso, exporters_no_iso))
    }
  }

  # Check sectors ----
  # ots_sectors has: broad_sector (name, e.g. "Agriculture") and broad_sector_id (integer 1-4)
  sectors_depending_queries <- grep("_sec|^itpde$|^itpds$",
    tradestatistics::ots_tables$table,
    value = TRUE
  )

  if (!(length(sectors) == 1 && identical(sectors, "all"))) {
    sectors <- as.character(sectors)
    sector_ids <- character(0)
    for (s in sectors) {
      if (grepl('^[0-9]+$', s)) {
        matched <- tradestatistics::ots_sectors[
          tradestatistics::ots_sectors$broad_sector_id == as.integer(s), ]
        if (nrow(matched) == 0) {
          stop(sprintf("Sector ID '%s' not found. Please check ots_sectors.", s))
        }
        sector_ids <- c(sector_ids, as.character(as.integer(s)))
      } else {
        s_clean <- tolower(iconv(s, to = "ASCII//TRANSLIT", sub = ""))
        matched <- tradestatistics::ots_sectors[
          grepl(s_clean, tolower(tradestatistics::ots_sectors$broad_sector)), ]
        if (nrow(matched) == 0) {
          stop(sprintf("No sector found matching '%s'. Please check ots_sectors.", s))
        }
        sector_ids <- c(sector_ids, as.character(matched$broad_sector_id))
      }
    }
    sectors <- unique(sector_ids)
    if (length(sectors) == 0) {
      stop("The requested sectors do not exist. Please check ots_sectors.")
    }
    if (!table %in% sectors_depending_queries) {
      sectors <- "all"
      warning("The sectors argument will be ignored for tables without a sector field.")
    }
  }

  # Check industries ----
  # ots_industries has: industry_descr (name, e.g. "Wheat") and industry_id (integer)
  industries_depending_queries <- grep("_ind|^itpde$|^itpds$",
    tradestatistics::ots_tables$table,
    value = TRUE
  )

  if (!(length(industries) == 1 && identical(industries, "all"))) {
    industries <- as.character(industries)
    industry_ids <- character(0)
    for (ind in industries) {
      if (grepl('^[0-9]+$', ind)) {
        matched <- tradestatistics::ots_industries[
          tradestatistics::ots_industries$industry_id == as.integer(ind), ]
        if (nrow(matched) == 0) {
          stop(sprintf("Industry ID '%s' not found. Please check ots_industries.", ind))
        }
        industry_ids <- c(industry_ids, as.character(as.integer(ind)))
      } else {
        ind_clean <- tolower(iconv(ind, to = "ASCII//TRANSLIT", sub = ""))
        matched <- tradestatistics::ots_industries[
          grepl(ind_clean, tolower(tradestatistics::ots_industries$industry_descr)), ]
        if (nrow(matched) == 0) {
          stop(sprintf("No industry found matching '%s'. Please check ots_industries.", ind))
        }
        industry_ids <- c(industry_ids, as.character(matched$industry_id))
      }
    }
    industries <- unique(industry_ids)
    if (length(industries) == 0) {
      stop("The requested industries do not exist. Please check ots_industries.")
    }
    if (!table %in% industries_depending_queries) {
      industries <- "all"
      warning("The industries argument will be ignored for tables without an industry field.")
    }
  }

  # Check optional parameters ----
  if (!is.numeric(max_attempts) || max_attempts <= 0) {
    stop("max_attempts must be a positive integer.")
  }

  if (is.null(importers)) {
    importers <- "all"
    warning("No importer was specified, therefore all available importers will be returned.")
  }

  if (is.null(exporters)) {
    exporters <- "all"
    warning("No exporter was specified, therefore all available exporters will be returned.")
  }

  # Build API parameter grid ----
  condensed_parameters <- expand.grid(
    year = years,
    reporter = importers,
    partner = exporters,
    sector = if (length(sectors) == 1 && identical(sectors, "all")) "all" else sectors,
    industry = if (length(industries) == 1 && identical(industries, "all")) "all" else industries,
    stringsAsFactors = FALSE
  )

  n_params <- nrow(condensed_parameters)
  if (is.null(n_params) || length(n_params) != 1L || is.na(n_params) || n_params <= 0L) {
    stop("No valid API calls could be constructed. Please check your parameters.")
  }

  data <- lapply(
    seq_len(n_params),
    function(x) {
      ots_read_from_api(
        table = table,
        max_attempts = max_attempts,
        year = condensed_parameters$year[x],
        importer = condensed_parameters$reporter[x],
        exporter = condensed_parameters$partner[x],
        sector = condensed_parameters$sector[x],
        industry = condensed_parameters$industry[x]
      )
    }
  )
  data <- rbindlist(data, fill = TRUE)

  # If the API returned no trade columns treat this as no data
  if (!any(grepl('^trade', names(data)))) {
    warning("The parameters you specified resulted in API calls returning 0 rows.")
    return(data)
  }

  # no data in API message
  if ("observation" %in% names(data)) {
    non_obs_cols <- setdiff(names(data), "observation")
    if (length(non_obs_cols) == 0L) {
      warning("The parameters you specified resulted in API calls returning 0 rows.")
      return(data)
    }
    all_na_non_obs <- TRUE
    for (col in non_obs_cols) {
      if (any(!is.na(data[[col]]))) {
        all_na_non_obs <- FALSE
        break
      }
    }
    if (all_na_non_obs) {
      warning("The parameters you specified resulted in API calls returning 0 rows.")
      return(data)
    }
    if (all(is.na(data$observation))) {
      data[, observation := NULL]
    }
  }

  # Add country names ----
  if (table %in% importer_depending_queries) {
    data <- merge(data, tradestatistics::ots_countries[, .(dynamic_code, country)],
                  all.x = TRUE, all.y = FALSE,
                  by.x = "importer_iso3_dynamic", by.y = "dynamic_code",
                  allow.cartesian = TRUE)
    data <- setnames(data, "country", "importer_name")
  }

  if (table %in% exporter_depending_queries) {
    data <- merge(data, tradestatistics::ots_countries[, .(dynamic_code, country)],
                  all.x = TRUE, all.y = FALSE,
                  by.x = "exporter_iso3_dynamic", by.y = "dynamic_code",
                  allow.cartesian = TRUE)
    data <- setnames(data, "country", "exporter_name")
  }

  columns_order <- c("year",
                     grep("^importer_", colnames(data), value = TRUE),
                     grep("^exporter_", colnames(data), value = TRUE),
                     grep("^broad_sector", colnames(data), value = TRUE),
                     grep("^industry", colnames(data), value = TRUE),
                     grep("^trade", colnames(data), value = TRUE),
                     grep("^country|^rta", colnames(data), value = TRUE),
                     grep("source", colnames(data), value = TRUE)
  )

  data <- data[, ..columns_order]

  return(data)
}

#' Downloads and processes the data from the API to return a human-readable tibble (memoised, internal)
#' @description A composition of \code{ots_create_tidy_data_unmemoised()} and \code{memoise()} for caching the output
#' @importFrom memoise memoise
#' @keywords internal
ots_create_tidy_data_memoised <- memoise::memoise(ots_create_tidy_data_unmemoised)
