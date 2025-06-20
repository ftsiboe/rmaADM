.onLoad <- function(libname, pkgname) {
  # set scientific notation options
  options(scipen = 999)

  # set global timeout limit
  options(timeout = 3600)

  # memoise functions
  locate_download_link <<- memoise::memoise(locate_download_link)

  # create some global variables
  # Define constants used by the function
  FCIP_INSURANCE_POOL <<- c(
    "state_code",
    "county_code",
    "commodity_code",
    "type_code",
    "practice_code"
  )

  FCIP_INSURANCE_ELECTION <<- c(
    "unit_structure_code",
    "insurance_plan_code",
    "coverage_type_code",
    "coverage_level_percent"
  )

  FCIP_FORCE_NUMERIC_KEYS <<- c(
    "commodity_year",
    FCIP_INSURANCE_POOL,
    "record_category_code",
    "insurance_plan_code",
    "coverage_level_percent"
  )


}
