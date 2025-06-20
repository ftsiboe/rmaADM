test_that("clean_file_name processes file names correctly", {
  skip_if_not_installed("rmaADM")
  
  # Test the function doesn't error
  expect_no_error({
    result <- clean_file_name("2020_SomeFile_YTD.txt")
  })
  
  result <- clean_file_name("2020_SomeFile_YTD.txt")
  expect_true(is.character(result))
})

test_that("get_file_info processes directory correctly", {
  skip_if_not_installed("rmaADM")
  
  # Create temporary directory with test files
  temp_dir <- tempfile()
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # Create test RDS files
  test_file1 <- file.path(temp_dir, "test1.rds")
  test_file2 <- file.path(temp_dir, "test2.rds")
  
  saveRDS(data.frame(x = 1), test_file1)
  saveRDS(data.frame(y = 2), test_file2)
  
  result <- get_file_info(temp_dir, ".rds")
  
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_true(all(c("file_path", "size_bytes", "size_mb") %in% names(result)))
  expect_true(all(result$size_bytes > 0))
})

test_that("clear_rmaADM_cache works without error", {
  skip_on_cran()
  
  # Should not error even if cache doesn't exist
  expect_no_error({
    clear_rmaADM_cache()
  })
})