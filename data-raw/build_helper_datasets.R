# load package
devtools::load_all()

# download the adm files to data-raw
download_adm(years = 2011:2025, helpers_only = F)

# build the helper datasets
build_helper_datasets(years = 2011, size_threshold = 10)
