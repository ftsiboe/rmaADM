test_that("download_adm2 validates input parameters", {
  expect_error({
    download_adm2(years = "invalid")
  })
  
  expect_no_error({
    download_adm2(years = 2012, dir = tempfile())
  })
})

test_that("download_adm2 creates directory structure without downloading", {
  # Test only directory creation logic without actual downloads
  temp_dir <- tempfile()
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # This will fail to locate URLs but should create the directory
  expect_error({
    download_adm2(
      years = 1999,  # Use invalid year to avoid downloads
      dir = temp_dir,
      dataset_codes = c("A01010"),
      adm_url = "http://invalid.url"
    )
  })
  
  # Directory should still be created
  expect_true(dir.exists(temp_dir))
})