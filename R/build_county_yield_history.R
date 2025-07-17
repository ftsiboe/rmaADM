# Global variable declarations for R CMD check
globalVariables(c("age", "age_min", "commodity_year", "historical_yield_trend_id", "yield_year"))

#' Load and join insurance offer and yield trend tables for a year
#'
#' Loads the insurance offer table and historical yield trend table for a
#' specific year, filters out missing trend IDs, and joins them on common columns.
#'
#' @param year [numeric] Year to load data for
#'
#' @return \link[data.table]{data.table} containing joined insurance offer and yield trend data
load_and_join_tables <- function(year) {

  # load insurance offer data set
  ofer <- get_adm_data(year = year, dataset = "insuranceoffer")
  ofer <- ofer[!historical_yield_trend_id %in% NA]
  ofer <- unique(ofer[, .SD, .SDcols =
                        c(intersect(names(ofer),
                                    c("commodity_year",
                                      FCIP_INSURANCE_POOL,
                                      "historical_yield_trend_id")))])

  # load historical yield trend data set
  hist <- get_adm_data(year = year, "historical_yield_trend")

  # join them together
  hist <- dplyr::inner_join(hist,ofer,by=names(ofer)[names(ofer) %in% names(hist)])

  return(hist)
}

#' Process data for a single year
#'
#' Loads and processes insurance offer and yield trend data for a single year,
#' aggregating yield data by mean within FCIP insurance pools, commodity years, and yield years.
#'
#' @param year [numeric] Year to process data for
#'
#' @return \link[data.table]{data.table} with aggregated yield data, or NULL if processing fails
process_single_archive <- function(year) {
  tryCatch({
    hist <- load_and_join_tables(year)

    hist <- hist[, lapply(.SD, function(x) mean(x, na.rm = TRUE)),
                 by = c(names(hist)[names(hist) %in%
                                      c( FCIP_INSURANCE_POOL,
                                        "commodity_year",
                                        "yield_year")]),
                 .SDcols = c(names(hist)[names(hist) %in%
                                           c("yield_amount",
                                             "trended_yield_amount",
                                             "detrended_yield_amount")])]
    return(hist)
  }, error = function(e){return(NULL)})
}

#' Filter to earliest available trends per insurance pool
#'
#' Computes the age of each yield record (yield_year - commodity_year) and
#' filters to keep only the earliest available trend data for each FCIP
#' insurance pool.
#'
#' @param rma_yield [\link[data.table]{data.table}] Yield data with yield_year and commodity_year columns
#'
#' @return \link[data.table]{data.table} filtered to earliest trends per insurance pool
filter_earliest_trends <- function(rma_yield) {
  rma_yield[, age := yield_year - commodity_year]
  rma_yield[, age_min := min(age, na.rm = TRUE), by = FCIP_INSURANCE_POOL]
  rma_yield <- rma_yield[age == age_min]
  return(rma_yield)
}

#' Reconcile yield history across all years
#'
#' For each unique yield year, filters to the earliest available trends
#' per insurance pool and computes mean yield amounts across pools.
#'
#' @param df [\link[data.table]{data.table}] Combined yield data from all archives
#'
#' @return \link[data.table]{data.table} with reconciled yield history, one row per insurance pool per year
reconcile_yield_history <- function(df) {
  data.table::rbindlist(
    lapply(
      unique(df$yield_year),
      function(year){
        rma_yield <- df[yield_year %in% year]

        rma_yield <- filter_earliest_trends(rma_yield)

        rma_yield <- rma_yield[
          , lapply(.SD, mean, na.rm = TRUE),
          by     =  FCIP_INSURANCE_POOL,
          .SDcols = c("yield_amount", "trended_yield_amount", "detrended_yield_amount")];gc()

        rma_yield[, commodity_year := year]

        return(rma_yield)
      }), fill = TRUE)
}

#' Build county yield history from ADM data
#'
#' Processes insurance offer and historical yield trend data for multiple years,
#' merges them on the historical trend IDs, aggregates by mean within each year,
#' then across all years selects the earliest available trend per FCIP insurance
#' pool and computes average yields. Saves the result as a parquet file with
#' automatic type conversion and optimization.
#'
#' @param years [numeric] Vector of years to process
#' @param export_dir [character] Directory path where the output parquet file will be saved.
#'   Defaults to "./data-raw"
#' @param by_year [logical] Currently unused parameter (kept for compatibility)
#'
#' @return
#' A \link[data.table]{data.table} with one row per FCIP insurance pool per year, containing
#' the mean `yield_amount`, `trended_yield_amount`, and
#' `detrended_yield_amount`, along with the corresponding
#' `commodity_year` and `yield_year`.
#'
#' @details
#' - Each year, the RMA rebuilds the historical yield trend table, so
#'   yields may differ across different releases of that table.
#' - This function uses information across all available tables to
#'   construct a single, reconciled county yield history.
#' - The output is automatically saved as a parquet file with automatic type 
#'   conversion and optimization in the specified directory.
#' - Applies the same type conversion logic as `download_adm2` for consistency.
#'
#' @importFrom data.table rbindlist
#' @importFrom dplyr inner_join
#' @export
#'
build_county_yield_history <- function(years, export_dir = "./data-raw", by_year = TRUE) {

  df <- data.table::rbindlist(
    lapply(years, process_single_archive),
    fill = TRUE
  )

  df <- reconcile_yield_history(df)

  # Generate metadata key for factor metadata
  metadata_key <- paste0("county_yield_history_", paste(range(years), collapse = "_"))

  # Save as parquet file using compress_adm2 function (handles type conversion automatically)
  file_name <- paste0(export_dir, "/county_yield_history.parquet")
  compress_adm2(df, file_name, metadata_key)

}
