#' @keywords internal
"_PACKAGE"

utils::globalVariables(c(
  "year", "country", "observation", "..columns_order", "."
  ))

#' OTS Tables
#'
#' Existing API tables with both description and source.
#'
#' @docType data
#' @keywords datasets
#' @name ots_tables
#' @usage ots_tables
#' @source Derived from USITC
#' @format A data frame with 30 rows and 3 variables
#' \describe{
#'   \item{\code{table}}{Table name}
#'   \item{\code{description}}{Description of table contents}
#'   \item{\code{source}}{Source for the data}
#' }
NULL

#' OTS Countries
#'
#' Official country names, ISO-3 codes, continent and EU membership.
#'
#' @docType data
#' @keywords datasets
#' @name ots_countries
#' @usage ots_countries
#' @source Derived from USITC
#' @format A data frame with 308 observations on the following 5 variables
#' \describe{
#'   \item{\code{country}}{Official name (e.g. United Kingdom)}
#'   \item{\code{iso3}}{ISO-3 code (e.g. "GBR")}
#'   \item{\code{dynamic_code}}{Deambiguated ISO-3 code (e.g. "DEU.X" for East Germany)}
#'   \item{\code{continent}}{Corresponding continent (e.g., Europe)}
#'   \item{\code{colour}}{Assigned colour by continen (e.g., '#d1a1bc')}
#' }
NULL

#' OTS Sectors
#'
#' Broad sector IDs.
#'
#' @docType data
#' @keywords datasets
#' @name ots_sectors
#' @usage ots_sectors
#' @source Derived from USITC
#' @format A data frame with 4 observations on the following 2 variables
#' \describe{
#'   \item{\code{broad_sector}}{Sector name (e.g. 'Agriculture')}
#'   \item{\code{broad_sector_id}}{Sector code (e.g. '1')}
#'   \item{\code{colour}}{Sector colour (e.g., '#74c0e2')}
#' }
NULL

#' OTS Industries
#'
#' Industry IDs.
#'
#' @docType data
#' @keywords datasets
#' @name ots_industries
#' @usage ots_industries
#' @source Derived from USITC
#' @format A data frame with 170 observations on the following 2 variables
#' \describe{
#'   \item{\code{industry_descr}}{Industry name (e.g. 'Wheat')}
#'   \item{\code{industry_id}}{Industry code (e.g. '1')}
#' }
NULL
