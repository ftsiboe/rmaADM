test_that("compress_adm handles Price table (A00810)", {
  test_data <- data.frame(
    commodity_year = 2012,
    state_code = "01",
    crop_code = "0041",
    insurance_plan_code = "02",
    established_price = 5.50,
    projected_price = 6.00,
    harvest_price = 5.75,
    price_volatility_factor = 0.12,
    stringsAsFactors = FALSE
  )
  
  result <- compress_adm("A00810", test_data)
  
  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)
  expect_true(all(c("established_price", "projected_price", "harvest_price", "price_volatility_factor") %in% names(result)))
})

test_that("compress_adm handles Subsidy table (A00070)", {
  test_data <- data.frame(
    commodity_year = 2012,
    state_code = "01",
    county_code = "001",
    crop_code = "0041",
    insurance_plan_code = "02",
    coverage_level_percent = 65,
    subsidy_percent = 59,
    stringsAsFactors = FALSE
  )
  
  result <- compress_adm("A00070", test_data)
  
  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)
})

test_that("compress_adm handles unknown table codes gracefully", {
  test_data <- data.frame(
    commodity_year = 2012,
    state_code = "01",
    some_value = 123,
    stringsAsFactors = FALSE
  )
  
  # Should not error on unknown table code
  expect_no_error({
    result <- compress_adm("UNKNOWN", test_data)
  })
  
  result <- compress_adm("UNKNOWN", test_data)
  expect_true(is.data.frame(result))
})