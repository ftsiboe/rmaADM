test_that("get_adm_data validates input parameters", {
  # Test invalid year (should still work but may not find data)
  expect_error({
    get_adm_data(year = 1900, dataset = "baserate")
  })

  # Test completely invalid dataset
  expect_error({
    get_adm_data(year = 2012, dataset = "nonexistent_dataset_xyz")
  })
})

test_that("get_adm_data handles default parameters", {
  skip_on_cran()
  skip_if_offline()


})

test_that("subsidy percent data includes commodity_year column", {
  skip_on_cran()
  skip_if_offline()
  
  # Test that subsidy percent data has commodity_year column
  subsidy_data <- get_adm_data(year = 2024, dataset = "subsidypercent")
  
  expect_true("commodity_year" %in% colnames(subsidy_data),
              info = "SubsidyPercent data should include commodity_year column")
  
  # Test that commodity_year matches the requested year
  if ("commodity_year" %in% colnames(subsidy_data)) {
    expect_true(all(subsidy_data$commodity_year == 2024),
                info = "All commodity_year values should match the requested year (2024)")
  }
  
  # Test with another year
  subsidy_data_2023 <- get_adm_data(year = 2023, dataset = "subsidypercent")
  expect_true("commodity_year" %in% colnames(subsidy_data_2023),
              info = "SubsidyPercent data should include commodity_year column for 2023")
  
  if ("commodity_year" %in% colnames(subsidy_data_2023)) {
    expect_true(all(subsidy_data_2023$commodity_year == 2023),
                info = "All commodity_year values should match the requested year (2023)")
  }
})

test_that("datasets have continuous year availability", {
  skip_on_cran()
  skip_if_offline()
  
  # Get all available data assets
  all_assets <- rmaADM:::list_data_assets()
  
  # Extract year and dataset info from asset names
  # Format: "YYYY_AXXXXX_DatasetName_YTD.rds"
  asset_info <- data.frame(
    asset_name = all_assets,
    year = as.numeric(substr(all_assets, 1, 4)),
    dataset_code = substr(all_assets, 6, 11),
    dataset_name = gsub("^\\d{4}_[A-Za-z]\\d{5}_|_YTD\\.rds$", "", all_assets),
    stringsAsFactors = FALSE
  )
  
  # Get unique dataset types
  unique_datasets <- unique(asset_info$dataset_code)
  
  # Check continuity for each dataset type
  for (dataset_code in unique_datasets) {
    dataset_assets <- asset_info[asset_info$dataset_code == dataset_code, ]
    available_years <- sort(unique(dataset_assets$year))
    
    # Skip datasets with only one year (no continuity to check)
    if (length(available_years) <= 1) {
      next
    }
    
    # Check for gaps in year sequence
    year_range <- min(available_years):max(available_years)
    missing_years <- setdiff(year_range, available_years)
    
    expect_equal(length(missing_years), 0,
                 info = paste0("Dataset ", dataset_code, " (", 
                              unique(dataset_assets$dataset_name)[1], 
                              ") has non-continuous availability. Missing years: ", 
                              paste(missing_years, collapse = ", "),
                              ". Available years: ", 
                              paste(available_years, collapse = ", ")))
  }
  
  # Also test specific important datasets explicitly
  core_datasets <- c("A01010", "A00810", "A00070", "A00030")
  
  for (core_dataset in core_datasets) {
    if (core_dataset %in% unique_datasets) {
      core_assets <- asset_info[asset_info$dataset_code == core_dataset, ]
      core_years <- sort(unique(core_assets$year))
      
      # For core datasets, expect a reasonable range (2011-2025)
      expected_min_year <- 2011
      expected_max_year <- 2025
      
      # Check if dataset spans expected range
      if (length(core_years) > 1) {
        actual_range <- min(core_years):max(core_years)
        missing_in_range <- setdiff(actual_range, core_years)
        
        expect_equal(length(missing_in_range), 0,
                     info = paste0("Core dataset ", core_dataset, 
                                  " has gaps in year coverage. Missing years: ",
                                  paste(missing_in_range, collapse = ", "),
                                  ". Available years: ",
                                  paste(core_years, collapse = ", ")))
      }
    }
  }
})
