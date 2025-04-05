
#' Locate the download link for the actuarial data master
#'
#' @param year the year of the actuarial data master to download
#' @param adm_url the url where the adm FTP site is
#'
#' @returns a list of the data and layout file urls with the time the file was last updated on RMA's server
#'
#' @importFrom stringr str_match_all
#' @imports dplyr
#'
#' @examples \dontrun{locate_download_link(year = 2012)}
locate_download_link <- function(year = 2012,
                                 adm_url = "https://pubfs-rma.fpac.usda.gov/pub/References/actuarial_data_master/"){

  # read in the webpage
  html <- suppressWarnings(paste0(readLines(adm_url), collapse = "\n"))

  # locate all the links
  links <- as.character(data.frame(stringr::str_match_all(html,
                                                          "href=\"(.*?)\""))[, 1])

  # get the link with the matching year
  link <- links[grepl(year,links)]

  # apply some cleaning opperations
  link <- gsub("href=\"", "", link)
  link <- gsub("\"", "", link)
  link <- gsub("\\./", "", link)
  link <- paste0(adm_url, link)

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
    filter(grepl("YTD|Layout", filename))


  # add the base url and year to the file names
  file_info <- file_info %>%
    mutate(filename = paste0(adm_url,year,"/",filename))

  # convert the links to a list and name them
  links <- as.list(file_info$filename)

  # name the link that contains "YTD" as "data"
  links$data <- links[grepl("YTD",links)]

  # name the link that contains "Layout" as "layout"
  links$layout <- links[grepl("Layout",links)]

  # add the update date to the links
  links$update_date <- file_info$datetime[grepl("YTD",file_info$filename)]

  # return the links
  return(links)

}



#' Download the adm files for a given year
#'
#' @param year the year of the actuarial data master to download
#' @param adm_url the url where the adm FTP site is
#' @param dir the directory to save the files to
#'
#' @returns the data and layout files for the given year
#' @importFrom utils download.file unzip
#' @imports cli

#'
#' @examples \dontrun{download_adm(year = 2012)}
download_adm <- function(year = 2012,
                         adm_url = "https://pubfs-rma.fpac.usda.gov/pub/References/actuarial_data_master/",
                         dir = "./data-raw"){

  # create a year directory if it doesn't already exist
  if(!dir.exists(paste0(dir,"/",year))) {
    dir.create(paste0(dir,"/",year))
  }

  # check if there is already a file in the dir
  if (dir.exists(paste0(dir,"/",year))) {
    # get the time when the file was last modified
    last_modified <- file.info(paste0(dir,"/",year))$mtime
  }

  # locate the urls for the data and adm
  urls <- locate_download_link(year = year, adm_url = adm_url)

  # check if the update date is greater than the last modified date
  if (exists("last_modified") && urls$update_date < last_modified) {
    cli::cli_alert_info(paste0("The data for ",year," is already up to date. Skipping download."))
    return(invisible(NULL))
  }

  # download the data
  utils::download.file(urls[['data']],
                destfile=paste0(dir,"/",year,"/adm_ytd_",year,".zip"),
                mode="wb")

  # extract the data zip files
  utils::unzip(paste0(dir,"/",year,"/adm_ytd_",year,".zip"),
        exdir = paste0(dir,"/",year))


  # download the layout file
  utils::download.file(urls[['layout']],
                destfile=paste0(dir,"/",year,"/layout_",year,".zip"),
                mode="wb")

  # extract the layout zip files
  utils::unzip(paste0(dir,"/",year,"/layout_",year,".zip"),
        exdir = paste0(dir,"/",year))

  # get file paths for all txt files
  files <- list.files(paste0(dir,"/",year), full.names = TRUE, pattern = "\\.txt")

  # set up a progress bar
  cli::cli_progress_bar("Converting .txt files to .rds", total = length(files))

  # loop over each txt file and convert it to .rds
  for(f in files){

    cli::cli_progress_update()

    # get the file name without the extension
    file_name <- gsub("\\.txt", "", basename(f))

    # suppress read_delim console output
    data <- readr::read_delim(f, delim = "|", col_names = TRUE, show_col_types = FALSE)

    # save the file as an .rds
    saveRDS(data, paste0(dir,"/",year,"/",file_name,".rds"))

    # delete the original .txt file
    file.remove(f)

  }

  # close the progress bar
  cli::cli_progress_done()

}


