.onLoad <- function(libname, pkgname) {
  # set scientific notation options
  options(scipen = 999)

  # set global timeout limit
  options(timeout = 3600)

  # memoise functions
  locate_download_link <<- memoise::memoise(locate_download_link)


}
