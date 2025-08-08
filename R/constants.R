

#' Insurance pool identifier fields
#'
#' A character vector of column names that together define a unique insurance
#' pool in the Federal Crop Insurance Program (FCIP).
#'
#' @format A \code{character} vector of field names.
#' @return A \code{character} vector specifying the columns used to define
#'   each FCIP insurance pool.
#' @examples
#' \dontrun{
#' # Default insurance pool fields
#' FCIP_INSURANCE_POOL
#'}
#' @export
FCIP_INSURANCE_POOL <- c(
  "state_code",
  "county_code",
  "commodity_code",
  "type_code",
  "practice_code"
)



#' Insurance election identifier fields
#'
#' A character vector of column names that define an insurance election within
#' the Federal Crop Insurance Program (FCIP). Each field corresponds to an
#' attribute of a policy election.
#'
#' @format A \code{character} vector of field names:
#' @return A \code{character} vector of field names used to specify insurance elections.
#' @examples
#' \dontrun{
#' # Default election fields
#' FCIP_INSURANCE_ELECTION
#'}
#' @export
FCIP_INSURANCE_ELECTION <- c(
  "unit_structure_code",
  "insurance_plan_code",
  "coverage_type_code",
  "coverage_level_percent"
)





#' Insurance election identifier fields (recoded)
#'
#' A character vector of recoded column names that specify an insurance
#' election within the Federal Crop Insurance Program (FCIP). These fields
#' correspond to recoded versions of the original election attributes.
#'
#' @format A \code{character} vector of field names:
#' @examples
#' \dontrun{
#' # View the default recoded election fields
#' FCIP_INSURANCE_ELECTION_RCODED
#'
#'}
#' @export
FCIP_INSURANCE_ELECTION_RCODED <- c(
  "unit_structure_recode",
  "insurance_plan_recode",
  "coverage_type_code",
  "coverage_level_percent"
)


#' Column names to coerce to numeric
#'
#' A character vector of column names that should be converted from character
#' to numeric during data ingestion and cleaning.
#' @examples
#' \dontrun{
#' # View default keys
#' FCIP_FORCE_NUMERIC_KEYS
#' }
#'
#' @export
FCIP_FORCE_NUMERIC_KEYS <- c(
  "commodity_year",
  FCIP_INSURANCE_POOL,
  "record_category_code",
  "insurance_plan_code",
  "coverage_level_percent"
)

