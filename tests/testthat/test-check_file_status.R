test_that("check_file_status correctly identifies skip scenarios", {
  # Create temporary directory for testing
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  dataset_codes <- c("A01010", "A00810")
  future_date <- Sys.time() + 86400  # tomorrow
  past_date <- Sys.time() - 86400    # yesterday
  
  # Test 1: All RDS files present and up-to-date
  rds_files <- file.path(temp_dir, paste0("2012_", dataset_codes, "_test.rds"))
  for (f in rds_files) {
    saveRDS(data.frame(x = 1), f)
  }
  
  result <- check_file_status(temp_dir, dataset_codes, past_date, overwrite = FALSE)
  expect_true(result$skip_download)
  expect_false(result$convert_txt_to_rds)
  expect_length(result$rds_to_delete, 0)
})

test_that("check_file_status handles TXT files correctly", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  dataset_codes <- c("A01010", "A00810")
  past_date <- Sys.time() - 86400
  
  # Test: Only TXT files present and up-to-date
  txt_files <- file.path(temp_dir, paste0("2012_", dataset_codes, "_test.txt"))
  for (f in txt_files) {
    writeLines("test data", f)
  }
  
  result <- check_file_status(temp_dir, dataset_codes, past_date, overwrite = FALSE)
  expect_true(result$skip_download)
  expect_true(result$convert_txt_to_rds)
  expect_length(result$rds_to_delete, 0)
})

test_that("check_file_status identifies duplicate files for deletion", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  dataset_codes <- c("A01010")
  past_date <- Sys.time() - 86400
  
  # Create both RDS and TXT files for same dataset
  rds_file <- file.path(temp_dir, "2012_A01010_test.rds")
  txt_file <- file.path(temp_dir, "2012_A01010_test.txt")
  
  saveRDS(data.frame(x = 1), rds_file)
  writeLines("test data", txt_file)
  
  result <- check_file_status(temp_dir, dataset_codes, past_date, overwrite = FALSE)
  expect_true(result$skip_download)
  expect_true(result$convert_txt_to_rds)
  expect_length(result$rds_to_delete, 1)
  expect_true(grepl("A01010", result$rds_to_delete))
})

test_that("check_file_status respects overwrite parameter", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  dataset_codes <- c("A01010")
  past_date <- Sys.time() - 86400
  
  # Create RDS file
  rds_file <- file.path(temp_dir, "2012_A01010_test.rds")
  saveRDS(data.frame(x = 1), rds_file)
  
  # With overwrite = FALSE, should skip
  result1 <- check_file_status(temp_dir, dataset_codes, past_date, overwrite = FALSE)
  expect_true(result1$skip_download)
  
  # With overwrite = TRUE, should not skip
  result2 <- check_file_status(temp_dir, dataset_codes, past_date, overwrite = TRUE)
  expect_false(result2$skip_download)
})

test_that("check_file_status handles missing dataset codes", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  past_date <- Sys.time() - 86400
  
  # Test with NULL dataset_codes
  result1 <- check_file_status(temp_dir, NULL, past_date, overwrite = FALSE)
  expect_false(result1$skip_download)
  expect_false(result1$convert_txt_to_rds)
  
  # Test with missing files for dataset codes
  result2 <- check_file_status(temp_dir, c("A01010"), past_date, overwrite = FALSE)
  expect_false(result2$skip_download)
  expect_false(result2$convert_txt_to_rds)
})