# load package
devtools::load_all()

# download the ADM files to data-raw
download_adm(years = 2011:2025,
             helpers_only = F,
             helpers_size_threshold = 10)

# build the helper data sets
build_helper_datasets(years = 2011,
                      size_threshold = 10)
