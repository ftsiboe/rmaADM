
#' Locate the download link for the actuarial data master
#'
#' @param year the year of the actuarial data master to download
#' @param adm_url the url where the adm FTP site is
#'
#' @returns a list of the data and layout file urls
#'
#' @importFrom stringr str_match_all
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

  # locate all the links
  links <- as.character(data.frame(stringr::str_match_all(html,
                                                          "href=\"(.*?)\""))[, 1])

  # filter the links to only include the data and layout files
  links <- links[grepl("YTD|Layout",links)]

  # apply some cleaning opperations
  links <- gsub("href=\"", "", links)
  links <- gsub("\"", "", links)
  links <- gsub("\\./", "", links)

  # add the base url and year to the links
  links <- paste0(adm_url,year,"/",links)

  # convert the links to a list and name them
  links <- as.list(links)
  names(links) <- c("data","layout")

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

#'
#' @examples \dontrun{download_adm(year = 2012)}
download_adm <- function(year = 2012,
                         adm_url = "https://pubfs-rma.fpac.usda.gov/pub/References/actuarial_data_master/",
                         dir = "./data-raw"){



  # create a year directory if it doesn't already exist
  dir.create(paste0(dir,"/",year))

  # locate the urls for the data and adm
  urls <- locate_download_link(year = year, adm_url = adm_url)

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
}

