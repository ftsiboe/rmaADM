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
