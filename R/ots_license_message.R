.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "
    Welcome to tradestatistics package. Visit tradestatistics.io to check the
    code of conduct and full-detail tables available in direct download.

    The data comes from USITC and it is distributed under Creative Commons 
    Attribution 4.0 International License.
    "
  )
}
