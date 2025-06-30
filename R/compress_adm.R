
#' Compress ADM files by maintaining only the necessary level of granularity
#'
#' @param table_code An adm record code to identify the target record.
#' @param df a data frame corresponding to the data represented by the table_code
#' @param dir Path to where downloaded adm files are stored.
#' @return A data frame of aggregated parameter values.
#' @import data.table
#' @importFrom readr read_delim
#' @export
compress_adm <- function(table_code, df, dir) {


  # Define constants used by the function
  FCIP_INSURANCE_POOL <- c(
    "state_code",
    "county_code",
    "commodity_code",
    "type_code",
    "practice_code"
  )

  FCIP_INSURANCE_ELECTION <- c(
    "unit_structure_code",
    "insurance_plan_code",
    "coverage_type_code",
    "coverage_level_percent"
  )

  FCIP_FORCE_NUMERIC_KEYS <- c(
    "commodity_year",
    FCIP_INSURANCE_POOL,
    "record_category_code",
    "insurance_plan_code",
    "coverage_level_percent"
  )

  ## Determine parameter list and aggregation keys

  # Base Rate
  if (table_code == "A01010") {
    parameter_list <- c(
      "reference_amount", "reference_rate", "exponent_value",
      "fixed_rate", "prior_year_reference_amount", "prior_year_reference_rate",
      "prior_year_exponent_value", "prior_year_fixed_rate",
      "base_rate", "prior_year_base_rate"
    )
    aggregation_point <- FCIP_INSURANCE_POOL
  }

  # Coverage Level Differential
  if (table_code == "A01040") {
    parameter_list <- c(
      "rate_differential_factor", "unit_residual_factor",
      "enterprise_unit_residual_factor", "whole_farm_unit_residual_factor",
      "prior_year_rate_differential_factor", "prior_year_unit_residual_factor",
      "prior_year_enterprise_unit_residual_factor", "prior_year_whole_farm_unit_residual_factor",
      "cat_residual_factor", "prior_cat_residual_factor"
    )
    aggregation_point <- c(FCIP_INSURANCE_POOL, FCIP_INSURANCE_ELECTION[!FCIP_INSURANCE_ELECTION %in% "unit_structure_code"])
  }

  # Beta ID
  if (table_code == "A00030") {
    parameter_list <- "beta_id"
    aggregation_point <- c(FCIP_INSURANCE_POOL, "insurance_plan_code", "unit_structure_code")
  }

  # Combo Revenue Factor
  if (table_code == "A01030") {
    parameter_list <- c("mean_quantity", "standard_deviation_quantity")
    aggregation_point <- c("commodity_code", "state_code", "lookup_rate")
  }

  # Historical Revenue Capping
  if (table_code == "A01110") {
    parameter_list <- c(
      "capping_reference_yield", "capping_reference_rate", "capping_exponent_value", "capping_fixed_rate",
      "prior_capping_reference_yield", "prior_capping_reference_rate", "prior_capping_exponent_value", "prior_capping_fixed_rate",
      paste0("beta_", 0:14, "_factor")
    )
    aggregation_point <- c(FCIP_INSURANCE_POOL, "capping_year")
  }

  # Premium Subsidy Percent
  if (table_code == "A00070") {
    parameter_list <- c("subsidy_percent")
    aggregation_point <- c(FCIP_INSURANCE_ELECTION, "commodity_year")
  }

  # Price
  if (table_code == "A00810") {
    parameter_list <- c("established_price", "projected_price", "harvest_price")
    aggregation_point <- c(FCIP_INSURANCE_POOL, "insurance_plan_code")
  }

  # Price Volatility Factor
  # if (table_code == "A00810_PVF") {
  #   parameter_list <- c("price_volatility_factor")
  #   aggregation_point <- c(FCIP_INSURANCE_POOL, "insurance_plan_code")
  # }

  ## Filter on reference/record codes if present

  # Keep where reference_amount_code is available and equals "Y" (Yield)
  if ("reference_amount_code" %in% names(df) && length(unique(df$reference_amount_code)) > 1) {
    df <- df[reference_amount_code == "Y"]
  }

  # Keep where record_category_code is available and equals 1 (Base Rate)
  if ("record_category_code" %in% names(df) && length(unique(df$record_category_code)) > 1) {
    df <- df[record_category_code == 1]
  }

  ## Special-case reshaping

  if (table_code == "A00030") {
    # Define unit structure flags
    flags <- names(df)[grepl("_unit_", names(df)) & grepl("_allowed_flag", names(df))]
    keepers <- intersect(c(aggregation_point, parameter_list), names(df))

    # Filter out NA beta_id, select only keepers+flags, then unique
    df <- unique(df[!is.na(beta_id), c(keepers, flags), with = FALSE])

    # Melt to long, keep only the "Y" rows
    df <- melt(
      df,
      id.vars = keepers,
      measure.vars = flags,
      variable.name = "nme",
      value.name = "unit_structure_code"
    )[unit_structure_code == "Y"]

    # Map flag-names to two-letter codes
    code_map <- c(
      optional_unit_allowed_flag = "OU",
      basic_unit_allowed_flag = "BU",
      enterprise_unit_allowed_flag = "EU",
      whole_farm_unit_allowed_flag = "WU",
      enterprise_unit_by_practice_allowed_flag = "EU"
    )

    df[, unit_structure_code := code_map[nme]]
    df[, nme := NULL]  # Drop the helper column
  }

  if (table_code == "A01030") {
    df <- df[, lookup_rate := base_rate
    ][, unique(.SD), .SDcols = intersect(
      c("commodity_year", aggregation_point, parameter_list), names(df)
    )]
  }

  if (table_code == "A01110") {
    df <- df[, unique(.SD), .SDcols = intersect(
      c("commodity_year", aggregation_point, parameter_list), names(df)
    )]
  }

  # aggregate if an aggregation point and parameter list are defined
  if( !is.null(aggregation_point) & !is.null(parameter_list)){
  # Aggregate parameters by taking the mean
  df <- df[, lapply(.SD, function(x) mean(x, na.rm = TRUE)),
           by = c(names(df)[names(df) %in% c("commodity_year",
                                             aggregation_point,
                                             "insurance_plan_recode",
                                             "unit_structure_recode")]),
           .SDcols = parameter_list]
  }

  return(df)
}
