# Suppress R CMD check notes
.datatable.aware <- TRUE

if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".SD", ":="
  ))
}

.onLoad <- function(libname, pkgname) {
  # set scientific notation options
  options(scipen = 999)

  # set global timeout limit
  options(timeout = 3600)

  # memoise functions
  locate_download_link <<- memoise::memoise(locate_download_link)




  FCIP_FORCE_NUMERIC_KEYS <<- c(
    "commodity_year",
    FCIP_INSURANCE_POOL,
    "record_category_code",
    "insurance_plan_code",
    "coverage_level_percent"
  )


}
