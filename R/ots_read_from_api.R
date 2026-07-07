#' Reads data from the API (internal function)
#' @description Accesses \code{api.tradestatistics.io} and
#' performs different API calls to return \code{data.frames} by reading 
#' \code{JSON} data. The parameters here are passed from 
#' \code{ots_create_tidy_data}.
#' @importFrom crul HttpClient
#' @importFrom jsonlite fromJSON
#' @keywords internal
ots_read_from_api <- function(year = NULL,
                              importer = NULL,
                              exporter = NULL,
                              sector = "all",
                              industry = "all",
                              table = "itpde_imp",
                              max_attempts = 5) {
  stopifnot(max_attempts > 0)

  if (any(table %in% c("countries", "industries", "sectors", "tables"))) {
    message("The requested table is included within the package.")
    return(TRUE)
  }
  
  imp <- tolower(importer)
  exp <- tolower(exporter)
  sec <- sector
  ind <- industry

  url <- switch(
    table,
    "importers" = sprintf("importers?year=%s", year),
    "exporters" = sprintf("exporters?year=%s", year),

    "itpde_sec" = sprintf("itpde_sec?year=%s&sector=%s", year, sec),
    "itpds_sec" = sprintf("itpds_sec?year=%s&sector=%s", year, sec),

    "itpde_ind" = sprintf("itpde_ind?year=%s&industry=%s", year, ind),
    "itpds_ind" = sprintf("itpds_ind?year=%s&industry=%s", year, ind),

    "itpde_imp" = sprintf("itpde_imp?year=%s&importer=%s", year, imp),
    "itpds_imp" = sprintf("itpds_imp?year=%s&importer=%s", year, imp),

    "itpde_exp" = sprintf("itpde_exp?year=%s&exporter=%s", year, exp),
    "itpds_exp" = sprintf("itpds_exp?year=%s&exporter=%s", year, exp),

    "itpde_imp_sec" = sprintf("itpde_imp_sec?year=%s&importer=%s&sector=%s", year, imp, sec),
    "itpds_imp_sec" = sprintf("itpds_imp_sec?year=%s&importer=%s&sector=%s", year, imp, sec),

    "itpde_exp_sec" = sprintf("itpde_exp_sec?year=%s&exporter=%s&sector=%s", year, exp, sec),
    "itpds_exp_sec" = sprintf("itpds_exp_sec?year=%s&exporter=%s&sector=%s", year, exp, sec),

    "itpde_imp_ind" = sprintf("itpde_imp_ind?year=%s&importer=%s&industry=%s", year, imp, ind),
    "itpds_imp_ind" = sprintf("itpds_imp_ind?year=%s&importer=%s&industry=%s", year, imp, ind),

    "itpde_exp_ind" = sprintf("itpde_exp_ind?year=%s&exporter=%s&industry=%s", year, exp, ind),
    "itpds_exp_ind" = sprintf("itpds_exp_ind?year=%s&exporter=%s&industry=%s", year, exp, ind),

    "itpde_imp_exp" = sprintf("itpde_imp_exp?year=%s&importer=%s&exporter=%s", year, imp, exp),
    "itpds_imp_exp" = sprintf("itpds_imp_exp?year=%s&importer=%s&exporter=%s", year, imp, exp),

    "itpde_imp_exp_sec" = sprintf("itpde_imp_exp_sec?year=%s&importer=%s&exporter=%s&sector=%s", year, imp, exp, sec),
    "itpds_imp_exp_sec" = sprintf("itpds_imp_exp_sec?year=%s&importer=%s&exporter=%s&sector=%s", year, imp, exp, sec),

    "itpde" = sprintf("itpde?year=%s&importer=%s&exporter=%s&sector=%s&industry=%s", year, imp, exp, sec, ind),
    "itpds" = sprintf("itpds?year=%s&importer=%s&exporter=%s&sector=%s&industry=%s", year, imp, exp, sec, ind)
  )

  # base_url <- "http://127.0.0.1:5000/"
  base_url <- "https://api.tradestatistics.io/"
  
  resp <- HttpClient$new(url = base_url)
  resp <- resp$get(url)

  # on a successful GET, return the response
  if (resp$status_code == 200) {
    combination <- paste(year, importer, exporter, sep = ", ")
    
    if (sector != "all") {
      combination <- paste(combination, sector, sep = ", ")
    }
    
    message(sprintf("Downloading data for the combination %s...", combination))
      
    data <- try(fromJSON(resp$parse(encoding = "UTF-8")))

    if (!is.data.frame(data)) {
      stop("It wasn't possible to obtain data. Provided this function tests your internet connection\nyou misspelled a reporter, partner or table, or there was a server problem. Please check and try again.")
    }
    
    return(data)
  } else if (max_attempts <= 0) {
    # when attempts run out, stop with an error
    stop("Cannot connect to the API. Either the server is down or there is a connection problem.")
  } else {
    # otherwise, sleep five seconds and try again
    Sys.sleep(5)
    ots_read_from_api(
      year = year, importer = importer, exporter = exporter,
      sector = sector, industry = industry, table = table,
      max_attempts = max_attempts - 1
    )
  }
}
