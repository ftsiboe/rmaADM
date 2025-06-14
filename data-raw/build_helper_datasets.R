# load package
devtools::load_all()

# download the ADM files to data-raw
download_adm2(years = 2011:2025,
             helpers_only = T,
             helpers_size_threshold = 100,
             specific_helpers = c("A00200"))

# build the helper data sets
build_helper_datasets(years = 2011:2025,
                      size_threshold = 1)


