# load package
devtools::load_all()


# download the ADM files to data-raw
download_adm2(
  years = 2011:2025,
  dataset_codes = c("A01010",
                    "A01040",
                    "A00030",
                    "A01020",
                    "A01030",
                    "A01110",
                    "A00200",
                    "A01115", # historical yield trend
                    "A01130", # AreaCoverageLevel
                    "A01135", # Area Rate"
                    "A01005"  # AreaRiskRate
  ),
  compress = TRUE,
  keep_source = TRUE,
  overwrite = TRUE
)

# download subsidy and price datasets without any compression
download_adm2(
  years = 2011:2025,
  dataset_codes = c(
                    "A00070", # subsidy percent
                    "A00810" # price
                   ),
  compress = FALSE,
  keep_source = TRUE,
  overwrite = TRUE
  )



# upload cleaned files as a data release
piggyback::pb_new_release(
  repo = "dylan-turner25/rmaADM",
  tag  = "v0.1.0",
  name = "First data release",
  body = "This release contains cleaned files from the ADM."
)

#upload  .rds files into that release
rds_files <- list.files("data-raw", "\\.rds$",
                        full.names = TRUE, recursive = TRUE)


piggyback::pb_upload(
  rds_files,
  repo = "dylan-turner25/rmaADM",
  tag  = "v0.1.0",
  overwrite = T
)



# build county yield history
build_county_yield_history(years = 2016:2025)

#upload  .rds files into that release
rds_files <- list.files("data-raw", "county_yield_history.rds$",
                        full.names = TRUE, recursive = TRUE)


piggyback::pb_upload(
  rds_files,
  repo = "dylan-turner25/rmaADM",
  tag  = "v0.1.0",
  overwrite = T
)


