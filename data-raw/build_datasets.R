# load package
devtools::load_all()


# download the ADM files to data-raw
download_adm2(
  years = 2015:2025,
  dataset_codes = c("A01010", # base rate
                    "A01040", # coverage level differential
                    "A00030", # insurance offer
                    "A01020", # beta
                    "A01030", # combo revenue factor
                    "A01110", # historical revenue capping
                    "A00200", # dates
                    "A01115", # historical yield trend
                    "A01130", # AreaCoverageLevel
                    "A01135", # Area Rate"
                    "A01005", # AreaRiskRate
                    "A00070", # subsidy percent
                    "A00810"  # price
  ),
  overwrite = TRUE
)


# upload cleaned files as a data release
piggyback::pb_new_release(
  repo = "dylan-turner25/rmaADM",
  tag  = "v0.2.0",
  name = "First data release",
  body = "This release contains cleaned files from the ADM."
)

#upload  .rds files into that release
files <- list.files("data-raw", "\\.parquet$",
                        full.names = TRUE, recursive = TRUE)


piggyback::pb_upload(
  files,
  repo = "dylan-turner25/rmaADM",
  tag  = "v0.2.0",
  overwrite = F
)



# build county yield history
build_county_yield_history(years = 2011:2025)

#upload  .rds files into that release
rds_files <- list.files("data-raw", "county_yield_history.rds$",
                        full.names = TRUE, recursive = TRUE)


piggyback::pb_upload(
  rds_files,
  repo = "dylan-turner25/rmaADM",
  tag  = "v0.2.0",
  overwrite = T
)


