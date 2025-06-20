
#' Get USDA RMA Actuarial Data Master (ADM) data
#'
#' Retrieves processed ADM data for a specified year and dataset type. The function
#' automatically locates the appropriate data asset and loads it from cache or downloads
#' it from GitHub releases if not available locally.
#'
#' @param year Numeric. The year of ADM data to retrieve. Defaults to 2012.
#' @param dataset Character. The dataset type to retrieve (e.g., "baserate", "premium",
#'   "subsidy"). Defaults to "baserate". Dataset names are case-insensitive and
#'   underscores are automatically handled.
#'
#' @return A data.frame containing the requested ADM data.
#'
#' @details
#' This function serves as the main interface for accessing processed ADM data. It:
#' \itemize{
#'   \item Locates the appropriate data file based on year and dataset
#'   \item Loads data from local cache or downloads from GitHub releases if needed
#'   \item Handles case-insensitive dataset matching
#'   \item Automatically manages data download and caching
#' }
#'
#' The function expects that data has been previously processed and uploaded to GitHub
#' releases using the \code{\link{download_adm2}} workflow.
#'
#' @examples
#' \dontrun{
#' # Get premium data for 2020
#' price_2020 <- get_adm_data(year = 2020, dataset = "price")
#'
#' # Get subsidy data for 2018
#' combo_2018 <- get_adm_data(year = 2018, dataset = "comborevenuefactor")
#' }
#'
#' @seealso
#' \code{\link{download_adm2}} for processing raw ADM files,
#' \code{\link{list_data_assets}} for listing all available data assets
#'
#' @export
get_adm_data <- function(year = NULL, dataset = "baserate"){

  # get list of all assets
  file  <- locate_data_asset(year, dataset)

  # load the file from cache or github
  data <- get_cached_rds(file)

  # return the data
  return(data)
}


