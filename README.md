rmaADM
================

- [rmaADM](#rmaadm)
  - [Installation](#installation)
  - [Usage](#usage)
  - [Available Datasets](#available-datasets)
  - [Caching Behavior](#caching-behavior)
    - [How Caching Works](#how-caching-works)
    - [Cache Management](#cache-management)
  - [License](#license)
  - [Author](#author)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# rmaADM

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/dylan-turner25/rmaADM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dylan-turner25/rmaADM/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/dylan-turner25/rmaADM/branch/main/graph/badge.svg)](https://app.codecov.io/gh/dylan-turner25/rmaADM?branch=main)
<!-- badges: end -->

The `rmaADM` package provides easy access to USDA Risk Management Agency
(RMA) Actuarial Data Master (ADM) datasets. This package downloads and
provides cleaned, processed versions of RMA actuarial data for crop
insurance analysis across multiple years (2011-2025).

## Installation

You can install the development version of rmaADM from GitHub:

``` r
# Install from GitHub
remotes::install_github("dylan-turner25/rmaADM")
```

## Usage

The primary exported function of rmaADM package is `get_adm_data`. This
function retrieves precompiled data sets that are stored in the
package’s GitHub repository as a release. Below is an example of how to
use this function:

``` r

# install the rmaADM package from GitHub
remotes::install_github("dylan-turner25/rmaADM", force = T)

# load the rmaADM package
library(rmaADM)

# load the 2012 base rate data set. If this is the first 
# time downloading, you'll see a message about the dataset 
# being downloade from github.
df <- get_adm_data(year = 2012, dataset = "baserate")

# the data is cached in the R library folder for the rmaADM package
# if you run the same command again, it will not download the data again
# and will load much faster.
df <- get_adm_data(year = 2012, dataset = "baserate")

# if you want to clear the cache and force the data to be downloaded again,
# you can use the clear_rmaADM_cache function.
clear_rmaADM_cache()

# running the same command again will now download the data again
df <- get_adm_data(year = 2012, dataset = "baserate")


# the get_adm_data function is case-insensitive for dataset names
# and can use the name or record code. All of the following return the 
# same data for the year 2012.
get_adm_data(year = 2012, dataset = "BaseRate")
get_adm_data(year = 2012, dataset = "base_RaTe")
get_adm_data(year = 2012, dataset = "A01010")
get_adm_data(year = 2012, dataset = "a01010")


# to see all the available datasets you can run the helper function list_data_assets
all_data <- rmaADM:::list_data_assets()
```

## Available Datasets

The package provides access to multiple ADM datasets across years
2011-2025:

- **BaseRate (A01010)**: Base rates for crop insurance
- **Price (A00810)**: Commodity prices  
- **SubsidyPercent (A00070)**: Premium subsidy percentages
- **InsuranceOffer (A00030)**: Insurance offer information
- **Beta (A01020)**: Beta values for rate calculations
- **ComboRevenueFactor (A01030)**: Combined revenue factors
- **CoverageLevelDifferential (A01040)**: Coverage level differentials
- **HistoricalRevenueCapping (A01110)**: Historical revenue capping data
- **HistoricalYieldTrend (A01115)**: Historical yield trend data (2016+)
- **AreaCoverageLevel (A01130)**: Area coverage levels (2017+)
- **AreaRate (A01135)**: Area rates (2017+)
- **Date (A00200)**: Date information
- **county_yield_history**: A dataset combining HistoricalYieldTrend and
  InsuranceOffer

## Caching Behavior

The `get_adm_data()` function implements a caching system to minimize
download times and reduce bandwidth usage. The cached files are stored
in the same location that the package is installed. To view the cache
folder location, run `tools::R_user_dir("rmaADM", which = "cache")`.

### How Caching Works

1.  **First Request**: When you call `get_adm_data()` for a dataset, the
    function:
    - Checks if the file exists in the local cache
    - If not found, downloads the RDS file from GitHub releases
    - Stores the file in the cache directory
    - Returns the data
2.  **Subsequent Requests**: For the same dataset:
    - Loads directly from the cached file (much faster)
    - No network request is made

### Cache Management

To clear the cache, run `clear_rmaADM_cache()`, after which
`get_adm_data()` will redownload the relevant files the first time they
are requested after the cache is cleared.

## License

MIT License

## Author

Dylan Turner (<dylan.turner3@outlook.com>)
