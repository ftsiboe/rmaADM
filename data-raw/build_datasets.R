# load package
devtools::load_all()

# download the ADM files to data-raw
download_adm2(
  years = 2011:2014,
  helpers_only = TRUE,
  helper_codes = c("A01090","A00070")
)


# build the helper data sets
build_helper_datasets(years = 2011:2025,
                      helper_codes = c("A01090","A00070"))


# build year crop files (i.e. the non helper data sets that need to be split and uploaded as a release)

# create directories for split files
if (!dir.exists("data-raw/crop_year_files")) {
  dir.create("data-raw/crop_year_files", recursive = TRUE)
}
if (!dir.exists("data-raw/crop_plan_files")) {
  dir.create("data-raw/crop_plan_files", recursive = TRUE)
}

# function to get file size in MB
get_file_size_mb <- function(file_path) {
  size_bytes <- file.info(file_path)$size
  return(size_bytes / (1024 * 1024))
}

# function to split large files
split_large_files <- function(file_size_threshold_mb = 25) {
  # get all RDS files in year directories
  year_dirs <- list.dirs("data-raw", recursive = FALSE, full.names = TRUE)
  year_dirs <- year_dirs[grepl("^data-raw/\\d{4}$", year_dirs)]

  for (year_dir in year_dirs) {
    year <- basename(year_dir)
    rds_files <- list.files(year_dir, pattern = "\\.rds$", full.names = TRUE)

    for (file_path in rds_files) {
      file_size_mb <- get_file_size_mb(file_path)

      if (file_size_mb > file_size_threshold_mb) {
        cat(sprintf("Processing large file (%0.1f MB): %s\n", file_size_mb, basename(file_path)))

        # load the data
        data <- readRDS(file_path)

        # check for commodity_code column first
        if ("commodity_code" %in% names(data)) {
          # split by commodity_code
          unique_commodities <- unique(data$commodity_code)
          unique_commodities <- unique_commodities[!is.na(unique_commodities)]

          for (commodity in unique_commodities) {
            subset_data <- data[data$commodity_code == commodity, ]

            # create output filename
            base_name <- gsub("\\.rds$", "", basename(file_path))
            output_file <- sprintf("data-raw/crop_year_files/%s_commodity_%s.rds", base_name, commodity)

            # save split file
            saveRDS(subset_data, output_file)
            cat(sprintf("  Created: %s (%d rows)\n", basename(output_file), nrow(subset_data)))
          }

        } else if ("insurance_plan_code" %in% names(data)) {
          # fallback to insurance_plan_code
          unique_plans <- unique(data$insurance_plan_code)
          unique_plans <- unique_plans[!is.na(unique_plans)]

          for (plan in unique_plans) {
            subset_data <- data[data$insurance_plan_code == plan, ]

            # create output filename
            base_name <- gsub("\\.rds$", "", basename(file_path))
            output_file <- sprintf("data-raw/crop_plan_files/%s_plan_%s.rds", base_name, plan)

            # save split file
            saveRDS(subset_data, output_file)
            cat(sprintf("  Created: %s (%d rows)\n", basename(output_file), nrow(subset_data)))
          }

        } else {
          # error: neither column found
          stop(sprintf("Error: File %s does not contain 'commodity_code' or 'insurance_plan_code' columns for splitting", basename(file_path)))
        }
      }
    }
  }
}

# run the file splitting process
split_large_files(file_size_threshold_mb = 25)




# # upload cleaned files as a data release
# piggyback::pb_new_release(
#   repo = "dylan-turner25/rfsa",
#   tag  = "v0.1.0",
#   name = "First data release",
#   body = "This release contains all individual year-program files."
# )
#
# #upload your .rds files into that release
# rds_files <- list.files("data-raw/fsaFarmPayments/output_data", "\\.rds$",
#                         full.names = TRUE, recursive = TRUE)
#
#
# piggyback::pb_upload(
#   rds_files,
#   repo = "dylan-turner25/rfsa",
#   tag  = "v0.1.0",
#   overwrite = F
# )





