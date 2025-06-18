#' #'
#' #' Reads insurance offer and historical yield trend tables from one or more
#' #' ADM year-to-date ZIP archives, merges them on the historical trend IDs,
#' #' aggregates by mean within each archive, then across all years selects the
#' #' earliest available trend per FCIP insurance pool and computes average yields.
#' #'
#' #' @param adm_ytd_archive_list [character]
#' #'   Vector of file paths to ADM YTD ZIP archives. Each archive must contain
#' #'   the tables `a00030_insurance_offer` and `a01115_historical_yield_trend`.
#' #'
#' #' @return
#' #' A data.table with one row per FCIP insurance pool per year, containing
#' #' the mean `yield_amount`, `trended_yield_amount`, and
#' #' `detrended_yield_amount`, along with the corresponding
#' #' `commodity_year` and `yield_year`.
#' #'
#' #' @details
#' #' - Each year, the RMA rebuilds the historical yield trend table (`a01115`), so
#' #'   yields may differ across different releases of that table.
#' #' - This function uses information across all available `a01115` tables to
#' #'   construct a single, reconciled county yield history.
#' #' - The `a01115` table is available for crop years 2016 and later.
#' #'
#' #' @importFrom data.table rbindlist
#' #' @importFrom dplyr inner_join
#' #' @export
#' #'
#' #' @examples
#' #' \dontrun{
#' #' archives <- c("ADM_2016.zip", "ADM_2017.zip", "ADM_2018.zip")
#' #' dt_history <- build_county_yield_history(archives)
#' #' head(dt_history)
#' #' }
#' build_county_yield_history <- function(adm_ytd_archive_list){
#'
#'   df <- data.table::rbindlist(
#'     lapply(
#'       1:length(adm_ytd_archive_list),
#'       function(i) {
#'         tryCatch({
#'           #i <- 1
#'
#'           ofer <- adm_ytd_reader(file_name="a00030_insurance_offer", adm_ytd_archive = adm_ytd_archive_list[[i]])
#'           ofer <- ofer[!historical_yield_trend_id %in% NA]
#'           ofer <- unique(ofer[, .SD, .SDcols =
#'                                 c(intersect(names(ofer),
#'                                             c("commodity_year",FCIP_INSURANCE_POOL,"historical_yield_trend_id")))])
#'
#'           hist <- adm_ytd_reader(file_name="a01115_historical_yield_trend",adm_ytd_archive = adm_ytd_archive_list[[i]])
#'
#'           hist <- dplyr::inner_join(hist,ofer,by=names(ofer)[names(ofer) %in% names(hist)])
#'           rm(ofer);gc()
#'
#'           # aggregate parameters by taking the mean
#'           hist <- hist[, lapply(.SD, function(x) mean(x, na.rm = TRUE)),
#'                        by = c(names(hist)[names(hist) %in%
#'                                             c(FCIP_INSURANCE_POOL,"commodity_year","yield_year")]),
#'                        .SDcols = c(names(hist)[names(hist) %in%
#'                                                  c("yield_amount","trended_yield_amount","detrended_yield_amount")])];gc()
#'           return(hist)
#'         }, error = function(e){return(NULL)})
#'       }), fill = TRUE)
#'
#'   df <- data.table::rbindlist(
#'     lapply(
#'       unique(df$yield_year),
#'       function(year){
#'         # year <- 2010
#'
#'         rma_yield <- df[yield_year %in% year]
#'
#'         # compute age
#'         rma_yield[, age := yield_year - commodity_year]
#'
#'         # compute the minimum age within each group
#'         rma_yield[, age_min := min(age, na.rm = TRUE), by = FCIP_INSURANCE_POOL]
#'
#'         # keep only the rows where age == age_min
#'         # (this implicitly drops the temporary age_min later if you want)
#'         rma_yield <- rma_yield[age == age_min]
#'
#'         # summarise the three yield-columns by group
#'         rma_yield <- rma_yield[
#'           , lapply(.SD, mean, na.rm = TRUE),
#'           by     = FCIP_INSURANCE_POOL,
#'           .SDcols = c("yield_amount", "trended_yield_amount", "detrended_yield_amount")];gc()
#'
#'         rma_yield[, commodity_year := year]
#'
#'         return(rma_yield)
#'       }), fill = TRUE);gc()
#'
#'   return(df)
#' }
