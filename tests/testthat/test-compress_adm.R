

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
