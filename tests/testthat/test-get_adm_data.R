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
