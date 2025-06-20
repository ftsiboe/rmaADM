test_that("locate_data_asset finds existing files", {
  skip_on_cran()
  
  # Mock list_data_assets to return known files
  local_mocked_bindings(
    list_data_assets = function() {
      c("2012_A01010_BaseRate_YTD.rds", 
        "2012_A00810_Price_YTD.rds",
        "2013_A01010_BaseRate_YTD.rds",
        "2020_A00070_SubsidyPercent_YTD.rds")
    }
  )
  
  # Test finding files by year and dataset
  result <- locate_data_asset(year = 2012, dataset = "baserate")
  expect_length(result, 1)
  expect_true(grepl("2012", result))
  expect_true(grepl("baserate", result, ignore.case = TRUE))
})

test_that("locate_data_asset handles multiple years", {
  skip_on_cran()
  
  local_mocked_bindings(
    list_data_assets = function() {
      c("2012_A01010_BaseRate_YTD.rds", 
        "2013_A01010_BaseRate_YTD.rds",
        "2014_A01010_BaseRate_YTD.rds")
    }
  )
  
  # Test multiple years
  result <- locate_data_asset(year = c(2012, 2013), dataset = "baserate")
  expect_length(result, 2)
  expect_true(all(grepl("2012|2013", result)))
})

test_that("locate_data_asset normalizes dataset names", {
  skip_on_cran()
  
  local_mocked_bindings(
    list_data_assets = function() {
      c("2012_A01010_BaseRate_YTD.rds")
    }
  )
  
  # Test various forms of dataset names
  expect_no_error(locate_data_asset(2012, "base_rate"))
  expect_no_error(locate_data_asset(2012, "BASE_RATE"))
  expect_no_error(locate_data_asset(2012, "baserate"))
  expect_no_error(locate_data_asset(2012, "BaseRate"))
})

test_that("locate_data_asset throws error for missing files", {
  skip_on_cran()
  
  local_mocked_bindings(
    list_data_assets = function() {
      c("2012_A01010_BaseRate_YTD.rds")
    }
  )
  
  # Test error for non-existent year/dataset combination
  expect_error({
    locate_data_asset(year = 1999, dataset = "baserate")
  }, "No files found")
  
  expect_error({
    locate_data_asset(year = 2012, dataset = "nonexistent")
  }, "No files found")
})