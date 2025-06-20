
# Global variable bindings to avoid R CMD check NOTEs
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "reference_amount_code", "record_category_code", "beta_id",
    "unit_structure_code", "nme", "lookup_rate", "base_rate"
  ))
}

#' @importFrom magrittr %>%

#' Locate data asset files by year and dataset
#'
#' Internal helper function that searches for available data asset files that match the
#' specified year(s) and dataset. The function performs case-insensitive matching and
#' removes underscores from dataset names for flexible matching.
#'
#' @param year Numeric vector. The year(s) to search for (e.g., 2020 or c(2020, 2021)).
#' @param dataset Character. The dataset name to search for. Underscores are automatically
#'   removed and matching is case-insensitive.
#'
#' @return Character vector of matching file names (in lowercase with underscores removed).
#'
#' @details
#' The function normalizes both the search criteria and available files by:
#' \itemize{
#'   \item Converting to lowercase
#'   \item Removing underscores
#' }
#' This allows flexible matching regardless of case or underscore usage.
#'
#' @seealso \code{\link{list_data_assets}} for listing all available assets
#' @keywords internal
locate_data_asset <- function(year, dataset){

  # convert dataset to lower case and gsub _
  dataset <- tolower(gsub("_", "", dataset))

  # get all files stored
  all_files <- list_data_assets()

  # filter on year
  files <- all_files[grepl(paste0(year, collapse = "|"), all_files)]

  # filter on dataset
  files <- files[grepl(dataset, tolower(gsub("_", "", files)))]

  # if length of files is zero, issue error message and a list of all files
  if(length(files) == 0){
    stop(
      paste0(
        paste0("No files found for year ", year, " and dataset ", dataset, ".\n"),
        " Available datasets are:\n",
        paste0(paste(unique(all_files)), collapse = "\n ")
      )
    )
  }

  return(files)

}



#' @title Download a data file from GitHub Releases via piggyback
#' @param name   The basename of the .rds file, e.g. "foo.rds"
#' @param tag    Which release tag to download from (default: latest)
#' @return       The local path to the downloaded file
#' @keywords internal
#' @noRd
#' @import piggyback
get_cached_rds <- function(name,
                           repo = "dylan-turner25/rmaADM",
                           tag  = NULL) {
  dest_dir <- tools::R_user_dir("rmaADM", which = "cache")
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  dest_file <- file.path(dest_dir, name)
  if (!file.exists(dest_file)) {
    # download from the Release
    piggyback::pb_download(
      file     = name,
      repo     = repo,
      tag      = tag,
      dest = dest_dir
    )
  }
  readRDS(dest_file)
}

#' Clear the package cache of downloaded RDS files
#'
#' Deletes the entire cache directory used by the **rmaADM** package to store
#' downloaded \*.rds files. Useful if you need to force re-download of data,
#' or free up disk space.
#'
#' @return Invisibly returns `NULL`. A message is printed indicating which
#'   directory was cleared.
#' @export
#'
#' @examples
#' \dontrun{
#' # Remove all cached RDS files so they will be re-downloaded on next use
#' clear_rmaADM_cache()
#' }
clear_rmaADM_cache <- function(){
  dest_dir <- tools::R_user_dir("rmaADM", which = "cache")
  if (dir.exists(dest_dir)) {
    unlink(dest_dir, recursive = TRUE, force = TRUE)
  }
  message("Cleared cached files in ", dest_dir)
  invisible(NULL)
}


#' List asset names from the latest GitHub release
#'
#' Retrieves the metadata for the most recent release of the **rmaADM** repository
#' on GitHub and extracts the names of all attached release assets.
#'
#' @return A character vector of file names (assets) in the latest release.
#' @keywords internal
#' @examples
#' \dontrun{
#' files = list_data_assets()
#' }
#' @importFrom gh gh
list_data_assets <- function(){
  # 1. Fetch the release metadata (by tag, or "latest")
  release <- gh::gh(
    "/repos/{owner}/{repo}/releases/latest",
    owner = "dylan-turner25",
    repo  = "rmaADM"
  )

  # 2. Extract the assets list
  assets <- release$assets

  # 3. Pull out the bits you care about
  df <- data.frame(
    name = vapply(assets, `[[`, "", "name"),
    url  = vapply(assets, `[[`, "", "browser_download_url"),
    size = vapply(assets, `[[`, 0,  "size"),
    stringsAsFactors = FALSE
  )

  return(df$name)
}

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
  aggregation_point <- NULL
  parameter_list <- NULL

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
    aggregation_point <- FCIP_INSURANCE_ELECTION
  }

  # Price
  if (table_code == "A00810") {
    parameter_list <- c("established_price", "projected_price", "harvest_price")
    aggregation_point <- c(FCIP_INSURANCE_POOL, "insurance_plan_code")
  }

  # Dates
  if(table_code == "A00200"){
    parameter_list <-  names(df)[grepl("_date", names(df))]
    aggregation_point <- c(FCIP_INSURANCE_POOL, "insurance_plan_code")
  }

  # Price Volatility Factor
  # if (table_code == "A00810_PVF") {
  #   parameter_list <- c("price_volatility_factor")
  #   aggregation_point <- c(FCIP_INSURANCE_POOL, "insurance_plan_code")
  # }

  ## Read RDS and coerce to numeric
  setDT(df)

  df[, c(intersect(FCIP_FORCE_NUMERIC_KEYS, names(df))) := lapply(
    .SD, function(x) as.numeric(as.character(x))
  ), .SDcols = intersect(FCIP_FORCE_NUMERIC_KEYS, names(df))]

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

  # if (table_code == "A00030") {
  #   # Define unit structure flags
  #   flags <- names(df)[grepl("_unit_", names(df)) & grepl("_allowed_flag", names(df))]
  #   keepers <- intersect(c(aggregation_point, parameter_list), names(df))
  #
  #   # Filter out NA beta_id, select only keepers+flags, then unique
  #   df <- unique(df[!is.na(beta_id), c(keepers, flags), with = FALSE])
  #
  #   # Melt to long, keep only the "Y" rows
  #   df <- melt(
  #     df,
  #     id.vars = keepers,
  #     measure.vars = flags,
  #     variable.name = "nme",
  #     value.name = "unit_structure_code"
  #   )[unit_structure_code == "Y"]
  #
  #   # Map flag-names to two-letter codes
  #   code_map <- c(
  #     optional_unit_allowed_flag = "OU",
  #     basic_unit_allowed_flag = "BU",
  #     enterprise_unit_allowed_flag = "EU",
  #     whole_farm_unit_allowed_flag = "WU",
  #     enterprise_unit_by_practice_allowed_flag = "EU"
  #   )
  #
  #   df[, unit_structure_code := code_map[nme]]
  #   df[, nme := NULL]  # Drop the helper column
  # }

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


#' Locate the download link for the actuarial data master
#'
#' @param year the year of the actuarial data master to download
#' @param adm_url the url where the ADM FTP site is
#' @param ice_url the url where the ICE (insurance control elements) FTP site is
#' @param data_source either "adm" or "ice". Defaults to "adm".

#'
#' @returns a list of the data and layout file urls with the time the file was last updated on RMA's server
#'
#' @importFrom stringr str_match_all str_extract
#' @importFrom dplyr mutate filter
#'
#' @examples \dontrun{locate_download_link(year = 2012)}
locate_download_link <- function(year = 2012,
                                 adm_url = "https://pubfs-rma.fpac.usda.gov/pub/References/actuarial_data_master/",
                                 ice_url = "https://pubfs-rma.fpac.usda.gov/pub/References/insurance_control_elements/PASS/",
                                 data_source = c("adm","ice")){

  if(data_source == "ice"){
    url <- ice_url
  }

  if(data_source == "adm"){
    url <- adm_url
  }

  # read in the webpage
  html <- suppressWarnings(paste0(readLines(url), collapse = "\n"))

  # locate all the links
  links <- as.character(data.frame(stringr::str_match_all(html,
                                                          "href=\"(.*?)\""))[, 1])

  # get the link with the matching year
  link <- links[grepl(year,links)]
  link <- link[!grepl("test",link)] # for ICE links


  # apply some cleaning opperations
  link <- gsub("href=\"", "", link)
  link <- gsub("\"", "", link)
  link <- gsub("\\./", "", link)
  link <- paste0(url, link)

  # navigate the the cleaned link to get the correct sublink
  html <- suppressWarnings(paste0(readLines(link), collapse = "\n"))

  # Extract the <pre> block where the file info resides
  pre_block <- str_extract(html, "<pre>.*?</pre>")

  # Extract date, time, size, and filename from each line using regex
  matches <- str_match_all(
    pre_block,
    "(\\d{2}/\\d{2}/\\d{4})\\s+(\\d{2}:\\d{2}\\s+[AP]M)\\s+(\\d+)\\s+<a href=\"\\.\\/(.*?)\">"
  )[[1]]

  # Convert to data frame
  file_info <- data.frame(
    date = matches[, 2],
    time = matches[, 3],
    size_bytes = as.numeric(matches[, 4]),
    filename = matches[, 5],
    stringsAsFactors = FALSE
  )


  # combine date and time into a single POSIXct column
  file_info <- file_info %>%
    mutate(
      datetime = as.POSIXct(paste(date, time), format = "%m/%d/%Y %I:%M %p", tz = "EST")
    )

  # filter the file info to only include the data and layout files
  file_info <- file_info %>%
    filter(grepl("ytd|layout", tolower(filename)))


  # add the base url and year to the file names
  file_info <- file_info %>%
    mutate(filename = paste0(url,year,"/",filename))

  # convert the links to a list and name them
  links <- as.list(file_info$filename)

  # name the link that contains "YTD" as "data"
  names(links)[which(grepl("YTD|ytd",links))] <- "data"

  # name the link that contains "Layout" as "layout"
  names(links)[which(grepl("Layout|layout",links))] <- "layout"

  # add the update date to the links
  links$update_date <- file_info$datetime[grepl("YTD",file_info$filename)]

  # unlist layout and data links
  links$data <- unlist(links$data)
  links$layout <- unlist(links$layout)

  # return the links
  return(links)

}



#' Download the adm files for a given year
#'
#' This function downloads the raw adm files, applies some minor cleaning operations,
#' and converts them to .rds format for better compression.
#'
#' @param years the years of the actuarial data master to download
#' @param adm_url the url where the adm FTP site is
#' @param dir the directory to save the files to
#' @param helpers_only if TRUE, only keeps the helper files (i.e. files smaller than 1 mb)
#' @param helpers_size_threshold If `helpers_only` is `TRUE`, `helpers_size_threshold` indicates the size (in mb) above which data sets are kept. I.e. anything below `helpers_size_threshold` is assumed to be a helper dataset.
#' @param keep_source_files if TRUE, keeps the original zip files in the year directory. If FALSE, they will be deleted.
#' @returns the data and layout files for the given year
#' @importFrom utils download.file unzip
#' @importFrom readr read_delim
#' @import cli
#'
#' @examples \dontrun{download_adm(year = 2012)}
download_adm <- function(years = 2012,
                         adm_url = "https://pubfs-rma.fpac.usda.gov/pub/References/actuarial_data_master/",
                         dir = "./data-raw",
                         helpers_only = TRUE,
                         helpers_size_threshold = 5,
                         keep_source_files = FALSE){
  # if dir directory doesn't exist, create it
  if(!dir.exists(dir)) {
    dir.create(dir)
  }


  # loop over each year
  for(year in years){

    # create a year directory if it doesn't already exist
    if(!dir.exists(paste0(dir,"/",year))) {
      dir.create(paste0(dir,"/",year))
    }


    # check if there is already a file in the dir
    if (dir.exists(paste0(dir,"/",year))) {
      # get the list of files in the directory
      files <- list.files(paste0(dir,"/",year), full.names = TRUE)

      # if there are no files, set last_modified to NULL
      if (length(files) == 0) {
        last_modified <- NULL
      } else {
        # get the most recent file in the directory
        most_recent_file <- files[which.max(file.info(files)$mtime)]

        # get the time when the most recent file was last modified
        last_modified <- file.info(most_recent_file)$mtime
      }
    }

    # locate the urls for the data and adm
    urls <- locate_download_link(year = year, adm_url = adm_url)

    # check if the update date is greater than the last modified date
    skip = F
    if (!is.null(last_modified) && urls$update_date < last_modified) {
      cli::cli_alert_info(paste0("The data for ",year," is already up to date. Skipping download."))
      skip = T
    }

    # if skip = F, proceed with downloading the data
    if(skip == F){

    # download the data
    utils::download.file(urls[['data']],
                  destfile=paste0(dir,"/",year,"/adm_ytd_",year,".zip"),
                  mode="wb")

    # extract the data zip files
    utils::unzip(paste0(dir,"/",year,"/adm_ytd_",year,".zip"),
          exdir = paste0(dir,"/",year))

    # check if the layout url exists (typically doesn't prior to 2011)
    if("layout" %in% names(urls)){
      # download the layout file
      utils::download.file(urls[['layout']],
                    destfile=paste0(dir,"/",year,"/layout_",year,".zip"),
                    mode="wb")

      # extract the layout zip files
      utils::unzip(paste0(dir,"/",year,"/layout_",year,".zip"),
            exdir = paste0(dir,"/",year))
    }

    # get file paths for all txt files
    files <- list.files(paste0(dir,"/",year), full.names = TRUE, pattern = "\\.txt")

    # if helpers_only is TRUE, remove any files larger than 1mb
    if(helpers_only){
      # get the file sizes
      file_sizes <- file.info(files)$size

      # for any files larger than 1 mb, delete the file
      to_delete <- files[file_sizes > (1024* helpers_size_threshold)^2]

      # delete the files
      if(length(to_delete) > 0){
        file.remove(to_delete)
        cli::cli_alert_info(paste0("Deleted ", length(to_delete), paste0(" files larger than ",helpers_size_threshold," mb. To keep these files and clean them, set `helper_only = TRUE`." )))
      }

      # get updated list of file paths
      files <- list.files(paste0(dir,"/",year), full.names = TRUE, pattern = "\\.txt")

    }

    # set up a progress bar
    cli::cli_progress_bar("Converting .txt files to .rds", total = length(files))

    # loop over each txt file and convert it to .rds
    for(f in files){

      cli::cli_progress_update(status = paste0("cleaning ",f))

      # get the file name without the extension
      file_name <- clean_file_name(f, file_type_out = "rds")

      # suppress read_delim console output
      data <- readr::read_delim(f, delim = "|", col_names = TRUE, show_col_types = FALSE)

      # clean the data
      data <- clean_data(data)

      # save the file as an .rds
      saveRDS(data, file = file_name)

      # delete the original .txt file
      file.remove(f)
    }


    # remove the original zip files
    if(keep_source_files == FALSE){
      file.remove(list.files(paste0(dir,"/",year), full.names = TRUE, pattern = "\\.zip"))
    }

    }

    # close the progress bar
    cli::cli_progress_done()

  }

}


#' Check file availability and determine download/conversion strategy
#'
#' @param year_dir Character. Directory path for the specific year
#' @param dataset_codes Character vector. Dataset codes to check for
#' @param update_date POSIXct. Remote update date to compare against
#' @param overwrite Logical. Whether to force overwrite
#'
#' @return List with elements: skip_download (logical), convert_txt_to_rds (logical), rds_to_delete (character vector)
#' @keywords internal
check_file_status <- function(year_dir, dataset_codes, update_date, overwrite = FALSE) {
  skip_download <- FALSE
  convert_txt_to_rds <- FALSE
  rds_to_delete <- character(0)

  if (!overwrite && !is.null(dataset_codes)) {
    # Get all RDS and TXT files
    rds_files <- list.files(year_dir, pattern = "\\.rds$", full.names = TRUE)
    txt_files <- list.files(year_dir, pattern = "\\.txt$", full.names = TRUE)

    # Check which codes are represented by RDS and TXT files
    rds_codes_present <- sapply(dataset_codes, function(code) {
      any(grepl(code, rds_files))
    })

    txt_codes_present <- sapply(dataset_codes, function(code) {
      any(grepl(code, txt_files))
    })

    # Check if all dataset codes are represented by either RDS or TXT files
    all_codes_represented <- all(rds_codes_present | txt_codes_present)

    if (all_codes_represented) {
      # Check last modified date of all relevant files
      relevant_files <- c()
      if (length(rds_files) > 0) relevant_files <- c(relevant_files, rds_files)
      if (length(txt_files) > 0) relevant_files <- c(relevant_files, txt_files)

      if (length(relevant_files) > 0) {
        most_recent <- relevant_files[which.max(file.info(relevant_files)$mtime)]
        last_modified <- file.info(most_recent)$mtime

        if (update_date < last_modified) {
          skip_download <- TRUE

          # Identify RDS files to delete (where both RDS and TXT exist for same code)
          for (i in seq_along(dataset_codes)) {
            code <- dataset_codes[i]
            if (rds_codes_present[i] && txt_codes_present[i]) {
              # Both exist, mark RDS for deletion
              rds_to_delete <- c(rds_to_delete, rds_files[grepl(code, rds_files)])
            }
          }

          # Check if any codes need TXT to RDS conversion
          if (any(txt_codes_present)) {
            convert_txt_to_rds <- TRUE
          }
        }
      }
    }
  }

  return(list(
    skip_download = skip_download,
    convert_txt_to_rds = convert_txt_to_rds,
    rds_to_delete = rds_to_delete
  ))
}


#' Download and process USDA RMA Actuarial Data Master (ADM) files
#'
#' Downloads ADM data files for the specified years from the USDA RMA actuarial data master repository,
#' extracts them, converts each `.txt` file to a memory-efficient `.rds` format using `data.table::fread()`,
#' and processes the data in chunks to minimize RAM usage. Helper files can optionally be retained or filtered
#' based on matching codes. Source archives can also be deleted after extraction.
#'
#' @param years Integer vector. The years of ADM data to download (e.g., `c(2012, 2013)`).
#' @param adm_url Character. Base URL for the ADM repository. Defaults to the public USDA RMA FTP URL.
#' @param dir Character. Local directory where files will be downloaded and processed. Created if it does not exist.
#' @param dataset_codes Character vector. File name patterns to retain, if null, keeps all files.
#' @param keep_source_files Logical. If `FALSE`, removes downloaded `.zip` archives after extraction.
#' @param overwrite Logical. If `TRUE`, re-downloads and re-processes files even if the existing data appears up to date.
#' @param compress Logical. If `TRUE`, applies data compression using `compress_adm()` function during processing. Defaults to `TRUE`.
#'
#' @return Invisibly returns `NULL`. Processed `.rds` files are written to disk in the specified directory.
#'
#' @details
#' The function reads each `.txt` file in chunks (default 1 million rows at a time),
#' applies a `clean_data()` function to each chunk (assumed to be defined elsewhere),
#' and writes the results to compressed `.rds` files using `xz` compression.
#' Files are only re-downloaded if the remote data is newer than the latest local file,
#' unless `overwrite = TRUE`.
#'
#' @note Files are read using `data.table::fread()` with all columns as character to reduce type inference overhead.
#' Chunked processing is used to reduce peak memory usage during conversion.
#'
#' @importFrom data.table fread rbindlist
#' @importFrom cli cli_alert_info cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom utils download.file unzip
#' @export
download_adm2 <- function(
    years = 2012,
    adm_url = "https://pubfs-rma.fpac.usda.gov/pub/References/actuarial_data_master/",
    dir = "./data-raw",
    dataset_codes = c("A01090","A00070"),
    keep_source_files = FALSE,
    overwrite = FALSE,
    compress = TRUE
) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  for (year in years) {
    year_dir <- file.path(dir, as.character(year))
    if (!dir.exists(year_dir)) {
      dir.create(year_dir)
    }

    # locate download URLs
    urls <- locate_download_link(year = year, adm_url = adm_url)

    # check file status and determine strategy
    file_status <- check_file_status(year_dir, dataset_codes, urls$update_date, overwrite)

    if (file_status$skip_download) {
      # Delete RDS files where both RDS and TXT exist for same code
      if (length(file_status$rds_to_delete) > 0) {
        cli::cli_alert_info("Removing duplicate RDS files for {year}.")
        file.remove(file_status$rds_to_delete)
      }

      if (file_status$convert_txt_to_rds) {
        cli::cli_alert_info("Converting TXT files to RDS for {year}.")
      } else {
        cli::cli_alert_info("Data for {year} is up to date; skipping.")
        next
      }
    }

    if (!file_status$skip_download) {

    # check for existing zip file before downloading
    data_zip <- file.path(year_dir, sprintf("adm_ytd_%s.zip", year))

    if (file.exists(data_zip)) {
      cli::cli_alert_info("Found existing zip file for {year}; using cached download.")
    } else {
      # download data
      utils::download.file(urls$data, data_zip, mode = "wb")
    }

    # unzip data
    utils::unzip(data_zip, exdir = year_dir)
    if (!keep_source_files) file.remove(data_zip)

    # optionally download & unzip layout
    if ("layout" %in% names(urls)) {
      layout_zip <- file.path(year_dir, sprintf("layout_%s.zip", year))

      if (file.exists(layout_zip)) {
        cli::cli_alert_info("Found existing layout zip file for {year}; using cached download.")
      } else {
        # download layout
        utils::download.file(urls$layout, layout_zip, mode = "wb")
      }

      # unzip layout
      utils::unzip(layout_zip, exdir = year_dir)
      if (!keep_source_files) file.remove(layout_zip)
    }
    }

    # list .txt files
    txt_files <- list.files(year_dir, pattern = "\\.txt$", full.names = TRUE)

    # remove large helpers if requested
    if (!is.null(dataset_codes) && length(txt_files)) {
      #sizes <- file.info(txt_files)$size
      to_del <- txt_files[!grepl(paste(dataset_codes, collapse = "|"), txt_files)]
      if (length(to_del)) {
        file.remove(to_del)
      }
      txt_files <- setdiff(txt_files, to_del)
    }

    # convert each .txt to .rds
    for (f in txt_files) {

      chunk_size <- 1000000
      out_rds <- sub("\\.txt$", ".rds", f)
      temp_dir <- file.path(dirname(out_rds), "temp_chunks")
      dir.create(temp_dir, showWarnings = FALSE)

      chunk_count <- 0
      dt_names <- NULL
      rows_read <- 0

      # Process in chunks, saving each to temporary RDS
      repeat {
        dt <- tryCatch({
          data.table::fread(
            input = f,
            sep = "|",
            colClasses = "character",
            showProgress = FALSE,
            skip = rows_read,
            nrows = chunk_size
          )
        }, error = function(e) {
          # If we hit EOF error, return empty data.table to break loop
          data.table::data.table()
        })

        # Break if no rows read (natural EOF or error)
        if (nrow(dt) == 0) break

        rows_read <- rows_read + nrow(dt)

        chunk_count <- chunk_count + 1
        dt <- clean_data(dt)

        if (is.null(dt_names)) {
          dt_names <- names(dt)
        } else {
          colnames(dt) <- dt_names
        }

        if (compress) {
          dt <- compress_adm(table_code = substr(basename(f), 6, 11), df = dt)
        }

        # Save chunk to temporary file
        temp_file <- file.path(temp_dir, paste0("chunk_", chunk_count, ".rds"))
        saveRDS(dt, temp_file, compress = "xz")
        rm(dt)
        gc() # Force garbage collection after each chunk
      }

      # Combine chunks using streaming approach (read one at a time)
      if (chunk_count > 0) {
        temp_files <- file.path(temp_dir, paste0("chunk_", seq_len(chunk_count), ".rds"))

        # Initialize with first chunk
        final_dt <- readRDS(temp_files[1])
        file.remove(temp_files[1])

        # Stream remaining chunks
        for (i in 2:length(temp_files)) {
          chunk_dt <- readRDS(temp_files[i])
          final_dt <- rbind(final_dt, chunk_dt)
          rm(chunk_dt)
          file.remove(temp_files[i])
          gc()
        }

        saveRDS(final_dt, out_rds, compress = "xz")
        rm(final_dt)
      }

      # Cleanup
      unlink(temp_dir, recursive = TRUE)
      file.remove(f)
      gc()

    }
  }

  invisible(NULL)
}


#' Clean the file name
#'
#' @param file_name the file path of the file name to clean
#' @param file_type_out the type of file (currently only supports rds)
#'
#' @returns a version of the file name that is cleaned (i.e. snake case, .rds suffix, no extraneous information)
#'
#' @examples \dontrun{clean_file_name("./data-raw/2012_A01100_YieldAndTyield_YTD.txt")}
clean_file_name <- function(file_name, file_type_out = "rds"){

  # split the file path into the directory and file name
  parts <- strsplit(file_name, "/")[[1]]

  # file suffix
  suffix <- parts[length(parts)]

  # remove any instance of alphabetic character followed by 5 numbers
  #suffix <- gsub("[A-Za-z]\\d{5}", "", suffix)

  # remove the first 4 digits from the suffix if the first 4 digits are numeric digits
  suffix <- gsub("^[0-9]{4}", "", suffix)

  # remove "YTD"
  suffix <- gsub("YTD", "", suffix)

  # remove "_" unless "_" follows a number
  suffix <- gsub("(?<![0-9])_(?![0-9])", "", suffix, perl = TRUE)

  # convert from camel case to snake case
  suffix <- gsub("([a-z])([A-Z])", "\\1_\\2", suffix)
  suffix <- tolower(suffix)

  # split the suffix by the period
  suffix_parts <- strsplit(suffix, "\\.")[[1]]

  # replace the last part with the desired file type
  suffix_parts[length(suffix_parts)] <- file_type_out

  # paste the suffix parts back together
  suffix <- paste(suffix_parts, collapse = ".")

  # paste the directory and file name back together
  file_name <- paste(parts[-length(parts)], collapse = "/")

  # paste the file name and suffix together
  file_name <- paste0(file_name, "/", suffix)

  # return the file name
  return(file_name)

}


#' Apply standardized data cleaning opperations
#'
#' @param df a data frame to clean
#'
#' @returns a cleaned data frame
#' @importFrom janitor clean_names
#' @importFrom readr type_convert
#'
#' @examples \dontrun{clean_data(df)}
clean_data <- function(df){

  # clean column names
  df <- janitor::clean_names(df)

  # enforce data types
  df <- suppressMessages(readr::type_convert(df))

  # identify date
  date_cols <- grep("date", names(df), value = TRUE)

  # try to parse dates
  for(col in date_cols){
    input = as.character(df[[col]]) # convert to character
    input <- as.character(gsub("[^0-9]", "", input)) # remove non-numeric characters
    try({
      converted_dates <- readr::parse_date(input, format = "%Y%m%d", na = c("", "NA"))
      # if converted dates are not all NA
      if(!all(is.na(converted_dates))) {
        df[[col]] <- converted_dates
      }
    })
  }

  # return the df
  return(df)

}


#' Get File Information from a Directory
#'
#' Scans a specified directory for files with a given suffix and returns a data frame
#' containing their file paths, sizes in bytes, and sizes in megabytes.
#'
#' @param directory A character string specifying the path to the directory to scan.
#'   Defaults to \code{"./data-raw"}.
#' @param file_suffix A character string specifying the file suffix to match.
#'   Defaults to \code{".rds"}.
#'
#' @return A data frame with columns:
#'   \item{file_path}{Full file path}
#'   \item{size_bytes}{File size in bytes}
#'   \item{size_mb}{File size in megabytes}
#'
#' @examples
#' \dontrun{
#' get_file_info()
#' get_file_info(directory = "./my-data", file_suffix = ".csv")
#' }
#'
get_file_info <- function(directory = "./data-raw", file_suffix = ".rds") {
  # Get list of all files recursively
  files <- list.files(path = directory, recursive = TRUE, full.names = TRUE, pattern = file_suffix)

  # Filter only actual files (not directories)
  files <- files[file.info(files)$isdir == FALSE]

  # Get file sizes
  sizes <- file.info(files)$size

  # Create data frame
  df <- data.frame(
    file_path = files,
    size_bytes = sizes,
    size_mb = sizes / (1024 * 1024), # Convert to MB
    stringsAsFactors = FALSE
  )

  return(df)
}



#' Build Helper Datasets from Raw RDS Files
#'
#' Processes raw `.rds` files from a specified directory, filters them based on year and size,
#' combines matching files into unified datasets, saves them as `.rda` files, and generates
#' corresponding documentation entries in `./R/helper_data.R`.
#'
#' @param years A numeric vector of years used to filter the input `.rds` files by year in their file paths.
#' @param helper_codes A vector of ADM codes to identify the relevant helpers (ex: c("A01090","A00070")). If NULL, defaults to using all files in the year directory.
#' @param dir A character string specifying the directory containing the raw `.rds` files.
#'   Defaults to \code{"./data-raw"}.
#'
#' @details This function:
#' \itemize{
#'   \item Filters `.rds` files in the target directory to those matching specified years.
#'   \item Excludes files larger than 1MB and those containing "rate" in the filename.
#'   \item Binds rows of files with matching names across years into a single dataset.
#'   \item Saves each dataset to the \code{./data/} folder as `.rda` files.
#'   \item Writes or appends roxygen-style documentation entries to \code{./R/helper_data.R}.
#'   \item Renames the existing \code{helper_data.R} file with the current date before overwriting.
#' }
#'
#' Each resulting dataset will be available for use with \code{data()} if the package is rebuilt.
#'
#' @return No return value.
#' @importFrom dplyr filter mutate bind_rows .data
#' @importFrom purrr map
#' @examples \dontrun{
#' build_helper_datasets(years = 2020:2022)
#' }
build_helper_datasets <- function(years,dir = "./data-raw",  helper_codes = c("A01090","A00070") ){

  # id "./data" doesn't exist, create it
  if(!dir.exists("./data")) {
    dir.create("./data")
  }


  file_info <- get_file_info(directory = dir,
                              file_suffix = ".rds")

  # Keep only file info for the years specified
  file_info <- file_info[grepl(paste(years, collapse = "|"), file_info$file_path), ]

  # For each year, extract the year from the file path and convert to numeric
  file_info$year <- as.numeric(gsub(".*?/(\\d{4})/.*", "\\1", file_info$file_path))

  # Add a column with the file name without any of the parent folders
  file_info$file_name <- gsub(paste0(dir, "/"), "", file_info$file_path)
  file_info$file_name <- gsub("[0-9]{4}/", "", file_info$file_name)
  file_info$file_name <- gsub(".rds", "", file_info$file_name)

  # extract the actuarial code from each file path (e.x. A00010)
  file_info$adm_code <- substr(basename(file_info$file_path),6,11)

  # keep only files that are in the helper codes
  if(!is.null(helper_codes)){
    file_info <- file_info %>%
      filter(.data$adm_code %in% helper_codes)
  }

  # if "./R/data.R" already exists, rename the file name with the data appended
  if(file.exists("./R/helper_data.R")){
    file.rename("./R/helper_data.R", paste0("./R/helper_data_", Sys.Date(), ".R"))
  }

  # create a new file with the header for the data.R file
  write("#' @title Actuarial Data Master Helper Datasets\n",
        file = "./R/helper_data.R", append = FALSE)

  # for each unique value in the file_name column,
  # load all the datasets corresponding to that file name as
  # specified by the file_path and then row bind them together
  # and save them as a .rds file with the name of the file_name
  for(f in unique(file_info$adm_code)){

    # get the file paths for the current file name
    file_paths <- file_info[file_info$adm_code == f, "file_path"]

    # load the datasets
    data <- file_paths %>%
      purrr::map(~ {
        df <- readRDS(.x) # read in the data frame
        df[] <- lapply(df, as.character)  # Convert all columns to character
        df # return the data frame
      }) %>%
      dplyr::bind_rows() # bind rows

    # convert the columns back to their most approriate data type
    data <- suppressMessages(readr::type_convert(data))

    # Remove both prefix and suffix to get file name to export
    file_out <- unique(gsub("^\\d{4}_[A-Za-z]\\d{5}_|_YTD|\\.rds", "",
                     basename(file_paths)))


    # Dynamically assign the name of the data to the value in f
    assign(file_out, data)

    # Save the named object to an .rda file
    save(list = file_out, file = paste0("./data/", file_out, ".rda"), compress = "xz")


    # add a documentation entry in ./data/data.R for the dataset
    doc_entry <- paste0("#' @name ", file_out, "\n",
                        "#' @title ", file_out, "\n",
                        "#' @description A combined dataset for ", file_out, "\n",
                        "#' @format A data frame with ", nrow(data), " rows and ", ncol(data), " columns covering ",min(data$reinsurance_year),"-",max(data$reinsurance_year),".\n",
                        "#' @source Actuarial Data Master\n",
                        "#' @usage data(",file_out,")", "\n",
                        paste0('"',file_out,'"'))
    # append the doc entry to the data.R file
    write(doc_entry, file = "./R/helper_data.R", append = TRUE)
  }




}

