#' Downloads and processes the data from the API to return a human-readable tibble
#' @description Accesses \code{api.tradestatistics.io} and
#' performs different API calls to transform and return tidy data.
#' @param years Year contained within the years specified in
#' api.tradestatistics.io/year_range (e.g. \code{c(2002,2004)}, \code{c(2002:2004)} or \code{2002}).
#' Default set to \code{2019}.
#' @param reporters ISO code for reporter country (e.g. \code{"chl"}, \code{"Chile"} or
#' \code{c("chl", "Peru")}). Default set to \code{"all"}.
#' @param partners ISO code for partner country (e.g. \code{"chl"}, \code{"Chile"} or
#' \code{c("chl", "Peru")}). Default set to \code{"all"}.
#' @param commodities HS commodity codes (e.g. \code{"0101"}, \code{"01"} or search
#' matches for \code{"apple"})
#' to filter commodities. Default set to \code{"all"}.
#' @param chapters HS chapter codes (e.g. \code{"01"}). Default set to \code{"all"}.
#' @param sections HS section codes (e.g. \code{"01"}). Default set to \code{"all"}.
#' @param table Character string to select the table to obtain the data.
#' Default set to \code{yr} (Year - Reporter).
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
#' # Run `ots_commodities` to display the full table of commodities
#'
#' # What does Chile export to China? (2002)
#' ots_create_tidy_data(years = 2002, reporters = "chl", partners = "chn")
#'
#' # What can we say about Horses export in Chile and the World? (2002)
#' ots_create_tidy_data(years = 2002, commodities = "010110", table = "yc")
#' ots_create_tidy_data(years = 2002, reporters = "chl", commodities = "010110", table = "yrc")
#'
#' # What can we say about the different types of apples exported by Chile? (2002)
#' ots_create_tidy_data(years = 2002, reporters = "chl", commodities = "apple", table = "yrc")
#' }
#' @keywords functions
ots_create_tidy_data <- function(years = 2020,
                                 reporters = "all",
                                 partners = "all",
                                 commodities = "all",
                                 chapters = "all",
                                 sections = "all",
                                 table = "yr",
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
  reporters <- toupper(reporters)
  partners <- toupper(partners)
  
  ots_cache(
    use_cache = use_cache,
    file = file,
    years = years,
    reporters = reporters,
    partners = partners,
  commodities = commodities,
  chapters = chapters,
  sections = sections,
    table = table,
    max_attempts = max_attempts
  )
}

#' Downloads and processes the data from the API to return a human-readable tibble (unmemoised, internal)
#' @description A separation of \code{ots_create_tidy_data()} for making caching optional.
#' @importFrom utils read.csv
#' @keywords internal
ots_create_tidy_data_unmemoised <- function(years = 2018,
                                            reporters = "usa",
                                            partners = "all",
                                            commodities = "all",
                                            chapters = "all",
                                            sections = "all",
                                            table = "yr",
                                            max_attempts = 5) {
  # silence data.table no visible binding notes
  country_name <- NULL; country_iso <- NULL; reporter_name <- NULL; partner_name <- NULL
  commodity_code <- NULL; commodity_name <- NULL; section_name <- NULL; section_code <- NULL
  chapter_name <- NULL; chapter_code <- NULL; section_color <- NULL

  # Check tables ----
  if (!table %in% tradestatistics::ots_tables$table) {
    stop("The requested table does not exist. Please check the spelling or explore the 'ots_table' table provided within this package.")
  }

  # Check years ----
  year_depending_queries <- grep("^reporters|^y|^rtas",
    tradestatistics::ots_tables$table,
    value = TRUE
  )

  # year_range <- try(read.csv("http://127.0.0.1:4949/year_range"))
  year_range <- try(read.csv("https://api.tradestatistics.io/year_range"))
  year_range <- try(as.numeric(year_range$year))

  if (all(years %in% min(year_range):max(year_range)) != TRUE &&
    table %in% year_depending_queries) {
    stop("Provided that the table you requested contains a 'year' field, please verify that you are requesting data contained within the years from api.tradestatistics.io/year_range.")
  }

  # Check reporters and partners ----
  reporter_depending_queries <- grep("^yr",
    tradestatistics::ots_tables$table,
    value = TRUE
  )
  
  partner_depending_queries <- grep("^yrp",
    tradestatistics::ots_tables$table,
    value = TRUE
  )

  if (!is.null(reporters)) {
  if (!all(reporters %in% tradestatistics::ots_countries$country_iso) == TRUE && table %in% reporter_depending_queries) {
      reporters_iso <- reporters[reporters %in% tradestatistics::ots_countries$country_iso]
      reporters_no_iso <- reporters[!reporters %in% tradestatistics::ots_countries$country_iso]
      
      reporters_no_iso <- sapply(
        seq_along(reporters_no_iso),
        function(x) {
          y <- tradestatistics::ots_country_code(reporters_no_iso[x])
          
          if (nrow(y) == 0) {
            stop("It was not possible to find ISO codes for any of the reporters you requested. Please check ots_countries.")
          } else {
            y <- y[, .(country_iso)]
            y <- as.vector(unlist(y))
          }
          
          if (length(y) > 1) {
            stop("There are multiple matches for the reporters you requested. Please check ots_countries.")
          } else {
            return(y)
          }
        }
      )
      
      reporters <- unique(c(reporters_iso, reporters_no_iso))
    }
  }

  if (!is.null(partners)) {
    if (
      isTRUE(!all(partners %in% tradestatistics::ots_countries$country_iso)) &&
      table %in% partner_depending_queries
    ) {
      partners_iso <- partners[partners %in% tradestatistics::ots_countries$country_iso]
      partners_no_iso <- partners[!partners %in% tradestatistics::ots_countries$country_iso]

      partners_no_iso <- sapply(
        seq_along(partners_no_iso),
        function(x) {
          y <- tradestatistics::ots_country_code(partners_no_iso[x])
          
          if (nrow(y) == 0) {
            stop("There are multiple matches for the partners you requested. Please check ots_countries.")
          } else {
            y <- y[, .(country_iso)]
            y <- as.vector(unlist(y))
          }
          
          if (length(y) > 1) {
            stop("There are multiple matches for the partners you requested. Please check ots_countries.")
          } else {
            return(y)
          }
        }
      )
      
      partners <- unique(c(partners_iso, partners_no_iso))
    }
  }

  # Check commodity codes ----
  commodities_depending_queries <- grep("c$",
    tradestatistics::ots_tables$table,
    value = TRUE
  )

  # If commodities == "all" we don't filter by commodity
  if (!(length(commodities) == 1 && identical(commodities, "all"))) {

    # normalize
    commodities <- as.character(commodities)

    # Expand numeric HS prefixes of any length (e.g. "03", "0301") into full commodity codes
    is_numeric_token <- grepl('^[0-9]+$', commodities)
    if (any(is_numeric_token)) {
      prefixes <- unique(commodities[is_numeric_token])
      expanded <- unlist(lapply(prefixes, function(p) {
        tradestatistics::ots_commodities$commodity_code[startsWith(tradestatistics::ots_commodities$commodity_code, p)]
      }))
      # remove the numeric tokens and add expanded codes (keep tokens that didn't match anything for later error handling)
      commodities <- unique(c(commodities[!is_numeric_token], expanded))
    }

    if (
      isTRUE(!all(as.character(commodities) %in% tradestatistics::ots_commodities$commodity_code)) &&
      table %in% commodities_depending_queries
    ) {

      # commodities without match (wm) - these are likely textual searches (names, chapters, sections)
      commodities_wm <- commodities[!commodities %in%
        tradestatistics::ots_commodities$commodity_code]

      # commodity name match (pmm)
      pnm <- lapply(
        seq_along(commodities_wm),
        function(x) { tradestatistics::ots_commodity_code(commodity = commodities_wm[x]) }
      )
      pnm <- rbindlist(pnm)

      # chapter name match (cnm)
      cnm <- lapply(
        seq_along(commodities_wm),
        function(x) { tradestatistics::ots_commodity_code(chapter = commodities_wm[x]) }
      )
      cnm <- rbindlist(cnm)

      # section name match (snm)
      snm <- lapply(
        seq_along(commodities_wm),
        function(x) { tradestatistics::ots_commodity_code(section = commodities_wm[x]) }
      )
      snm <- rbindlist(snm)

      commodities_wm <- rbind(pnm, cnm, snm, fill = TRUE)
      commodities_wm <- unique(commodities_wm[nchar(commodity_code) == 4, .(commodity_code)])
      commodities_wm <- as.vector(unlist(commodities_wm))

      commodities <- c(commodities[commodities %in%
        tradestatistics::ots_commodities$commodity_code], commodities_wm)

      if(length(commodities) == 0) {
        commodities <- NA
      }
    }

    if (!all(as.character(commodities) %in%
      tradestatistics::ots_commodities$commodity_code) &&
      table %in% commodities_depending_queries) {
      stop("The requested commodities do not exist. Please check ots_commodities.")
    }
  }
  # If chapters or sections provided, prefer fetching the full commodity set and
  # filtering locally when the user did not explicitly provide commodity codes.
  # This avoids issuing one API call per commodity (very slow). If the user
  # explicitly provided commodity codes, keep the original behaviour and expand
  # chapters/sections into commodity codes.
  chapter_filter <- NULL
  section_filter <- NULL

  # chapters: accept either codes (e.g. "03") or names
  if (!(length(chapters) == 1 && identical(chapters, "all"))) {
    chapters <- as.character(chapters)
    # if user did not provide explicit commodity codes, record chapter filter
    # and avoid expanding into many commodity-specific API calls
    if (length(commodities) == 1 && identical(commodities, "all")) {
      chapter_filter <- chapters
    } else {
      ch_codes <- c()
      for (ch in chapters) {
        if (grepl('^[0-9]+$', ch)) {
          # numeric chapter code
          ch_codes <- c(ch_codes, tradestatistics::ots_commodities$commodity_code[substr(tradestatistics::ots_commodities$chapter_code,1,nchar(ch)) == ch])
        } else {
          # name match
          tmp <- tradestatistics::ots_commodities[grepl(tolower(ch), tolower(chapter_name))]
          ch_codes <- c(ch_codes, tmp$commodity_code)
        }
      }
      if (length(ch_codes) > 0) commodities <- unique(c(as.character(commodities), ch_codes))
    }
  }

  if (!(length(sections) == 1 && identical(sections, "all"))) {
    sections <- as.character(sections)
    if (length(commodities) == 1 && identical(commodities, "all")) {
      section_filter <- sections
    } else {
      sec_codes <- c()
      for (s in sections) {
        if (grepl('^[0-9]+$', s)) {
          # numeric section code
          sec_codes <- c(sec_codes, tradestatistics::ots_commodities$commodity_code[substr(tradestatistics::ots_commodities$section_code,1,nchar(s)) == s])
        } else {
          # name match
          tmp <- tradestatistics::ots_commodities[grepl(tolower(s), tolower(section_name))]
          sec_codes <- c(sec_codes, tmp$commodity_code)
        }
      }
      if (length(sec_codes) > 0) commodities <- unique(c(as.character(commodities), sec_codes))
    }
  }
  
  # Check section codes -----------------------------------------------------
  sections <- sort(as.character(sections))
  if (!all(sections %in% c(unique(tradestatistics::ots_commodities$section_code), "all") == TRUE) &&
      table %in% commodities_depending_queries) {
    for (i in seq_along(sections)) {
      if (sections[i] != "all") {
        sections[i] <- as.integer(substr(sections, 1, 3))
      }
  if (nchar(sections[i]) != 2 && sections[i] != "999") {
        sections[i] <- paste0("0", sections[i])
      }
    }
    for (i in seq_along(sections)) {
      sections[i] <- if (!sections[i] %in% tradestatistics::ots_commodities$section_code) {
        NA
      } else {
        sections[i]
      }
    }
    sections <- sections[!is.na(sections)]
    if(length(sections) == 0) {
      sections <- NA
    }
  }
  if (!all(sections %in% c(unique(tradestatistics::ots_commodities$section_code), "all") == TRUE) &&
    table %in% commodities_depending_queries) {
    stop("The requested sections do not exist. Please check section_code in ots_commodities.")
  }
  
  # Check optional parameters ----
  if (!is.numeric(max_attempts) || max_attempts <= 0) {
    stop("max_attempts must be a positive integer.")
  }

  # Read from API ----
  if (!table %in% commodities_depending_queries && any(commodities != "all") == TRUE) {
    commodities <- "all"
    warning("The commodities argument will be ignored provided that you requested a table without commodity_code field.")
  }
  
  if (is.null(reporters)) {
    reporters <- "all"
    warning("No reporter was specified, therefore all available reporters will be returned.")
  }

  if (is.null(partners)) {
    partners <- "all"
    warning("No partner was specified, therefore all available partners will be returned.")
  }

  # Build API parameter grid. When a specific commodity code is requested,
  # include the commodity's section_code (from ots_commodities) in the API call
  base_grid <- expand.grid(
    year = years,
    reporter = reporters,
    partner = partners,
    stringsAsFactors = FALSE
  )

  if (length(commodities) == 1 && identical(commodities, "all")) {
    # Request all commodities from the API for the specified year/reporter/partner
    # and apply chapter/section filters locally. This avoids issuing many
    # per-commodity API calls and ensures consistent filtering.
    condensed_parameters <- cbind(base_grid, commodity = "all", section = "all", stringsAsFactors = FALSE)
  } else {
    # ensure commodities vector
    commodities <- as.character(commodities)
    # map each commodity to its section code using ots_commodities
    commodity_sections <- sapply(commodities, function(cc) {
      idx <- which(tradestatistics::ots_commodities$commodity_code == cc)
      if (length(idx) >= 1) {
        tradestatistics::ots_commodities$section_code[idx[1]]
      } else {
        # fallback to 'all' if we can't find a section (shouldn't happen after validation)
        "all"
      }
    }, USE.NAMES = FALSE)

    # create one block per commodity with its corresponding section
    blocks <- lapply(seq_along(commodities), function(i) {
      cbind(base_grid, commodity = commodities[i], section = commodity_sections[i], stringsAsFactors = FALSE)
    })
    condensed_parameters <- do.call(rbind, blocks)
  }

  # Safely determine number of parameter blocks. If this is NA/0 or not a
  # positive integer it means the requested commodities/expansions produced
  # no valid API calls (e.g. non-existing commodity codes). In that case
  # surface a clear error instead of calling seq_len on an invalid value and
  # producing a spurious warning that pollutes tests.
  n_params <- nrow(condensed_parameters)
  if (is.null(n_params) || length(n_params) != 1L || is.na(n_params) || n_params <= 0L) {
    stop("The requested commodities do not exist. Please check ots_commodities.")
  }

  data <- lapply(
    seq_len(n_params),
    function(x) {
      ots_read_from_api(
        table = table,
        max_attempts = max_attempts,
        year = condensed_parameters$year[x],
        reporter_iso = condensed_parameters$reporter[x],
        partner_iso = condensed_parameters$partner[x],
        commodity_code = condensed_parameters$commodity[x],
        section_code = condensed_parameters$section[x]
      )
    }
  )
  data <- rbindlist(data, fill = TRUE)
  
  # If the API returned no trade columns (e.g. no rows for the requested
  # reporter/partner/year) treat this as no data and warn the user. Many API
  # endpoints that return zero results do not include the usual
  # 'trade_value_*' columns.
  if (!any(grepl('^trade_value_', names(data)))) {
    warning("The parameters you specified resulted in API calls returning 0 rows.")
    return(data)
  }

  # no data in API message
  if ("observation" %in% names(data)) {
    # if observation is the only column returned, treat as no data
    non_obs_cols <- setdiff(names(data), "observation")
    if (length(non_obs_cols) == 0L) {
      warning("The parameters you specified resulted in API calls returning 0 rows.")
      return(data)
    }
    # if all non-observation columns are NA, treat as no data
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
    # otherwise drop observation if it's all NA and continue
    if (all(is.na(data$observation))) {
      data[, observation := NULL]
    }
  }

  # Add attributes based on codes, etc (and join years, if applicable) ------

  # include countries data
  if (table %in% reporter_depending_queries) {
    data <- merge(data, tradestatistics::ots_countries[, .(country_iso, country_name)],
                  all.x = TRUE, all.y = FALSE,
                  by.x = "reporter_iso", by.y = "country_iso",
                  allow.cartesian = TRUE)
    data <- setnames(data, "country_name", "reporter_name")
  }

  if (table %in% partner_depending_queries) {
    data <- merge(data, tradestatistics::ots_countries[, .(country_iso, country_name)],
                  all.x = TRUE, all.y = FALSE,
                  by.x = "partner_iso", by.y = "country_iso",
                  allow.cartesian = TRUE)
    data <- setnames(data, "country_name", "partner_name")
  }
  
  # If commodity_code is present, ensure descriptive fields are populated.
  # Prefer values returned by the API; otherwise fill from the package table
  # using a straight match to avoid .x/.y merge complexities.
  if ("commodity_code" %in% names(data)) {
    # create an index that maps each row's commodity_code to ots_commodities
    idx <- match(as.character(data$commodity_code), tradestatistics::ots_commodities$commodity_code)

    # helper to fill a column from ots_commodities if missing or all NA in data
    fill_if_missing <- function(colname) {
      if (!(colname %in% names(data))) {
        data[[colname]] <<- tradestatistics::ots_commodities[[colname]][idx]
      } else {
        # if column exists but has NA in some rows, fill those rows
        nas <- is.na(data[[colname]]) | data[[colname]] == ""
        if (any(nas)) {
          data[[colname]][nas] <<- tradestatistics::ots_commodities[[colname]][idx][nas]
        }
      }
    }

    # fill descriptive and code columns
    fill_if_missing('commodity_name')
    fill_if_missing('chapter_code')
    fill_if_missing('chapter_name')
    fill_if_missing('section_code')
    fill_if_missing('section_name')
    fill_if_missing('section_color')
  }
  
    # If we recorded chapter/section filters to avoid expanding into many API
    # calls, apply those filters now on the assembled `data` table.
    if (exists("chapter_filter") && !is.null(chapter_filter)) {
      # accept numeric codes or names; normalize to two-digit chapter codes
      chs <- as.character(unlist(chapter_filter))
      ch_codes <- c()
      for (ch in chs) {
        if (grepl('^[0-9]+$', ch)) {
          ch_codes <- c(ch_codes, sprintf("%02d", as.integer(ch)))
        } else {
          matches <- tradestatistics::ots_commodities[grepl(tolower(ch), tolower(chapter_name)), unique(chapter_code)]
          ch_codes <- c(ch_codes, matches)
        }
      }
      ch_codes <- unique(ch_codes)
      # filter rows where commodity_code prefix matches chapter codes
      data <- data[substr(as.character(commodity_code), 1, 2) %in% ch_codes]
    }

    if (exists("section_filter") && !is.null(section_filter)) {
      secs <- as.character(unlist(section_filter))
      sec_codes <- c()
      for (s in secs) {
        if (grepl('^[0-9]+$', s)) {
          sec_codes <- c(sec_codes, sprintf("%02d", as.integer(s)))
        } else {
          matches <- tradestatistics::ots_commodities[grepl(tolower(s), tolower(section_name)), unique(section_code)]
          sec_codes <- c(sec_codes, matches)
        }
      }
      sec_codes <- unique(sec_codes)
      # filter rows where commodity_code's section (first 2 or 3 digits depending) matches
      # section_code in ots_commodities; we'll compare by matching the section_code column
      idx <- match(as.character(data$commodity_code), tradestatistics::ots_commodities$commodity_code)
      data <- data[tradestatistics::ots_commodities$section_code[idx] %in% sec_codes]
    }

  columns_order <- c("year",
                     grep("^reporter_", colnames(data), value = TRUE),
                     grep("^partner_", colnames(data), value = TRUE),
                     grep("^commodity_", colnames(data), value = TRUE),
                     grep("^chapter_", colnames(data), value = TRUE),
                     grep("^section_", colnames(data), value = TRUE),
                     grep("^trade_", colnames(data), value = TRUE),
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
