test_that("data-raw files under 1MB have correct numeric columns", {
  skip_on_cran()
  
  # Get the force numeric keys from global variable
  force_numeric_keys <- FCIP_FORCE_NUMERIC_KEYS
  
  # Find all RDS files under 1MB in data-raw directory
  data_raw_dir <- system.file("../../../data-raw", package = "rmaADM")
  if (data_raw_dir == "" || !dir.exists(data_raw_dir)) {
    data_raw_dir <- file.path(getwd(), "data-raw")
  }
  
  skip_if_not(dir.exists(data_raw_dir), "data-raw directory not found")
  
  # Get all RDS files recursively
  rds_files <- list.files(data_raw_dir, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
  
  # Filter files under 1MB (1,048,576 bytes)
  small_files <- rds_files[file.size(rds_files) < 1048576 & !is.na(file.size(rds_files))]
  
  skip_if(length(small_files) == 0, "No RDS files under 1MB found in data-raw")
  
  # Test each small file
  for (file_path in small_files) {
    test_that(paste("File", basename(file_path), "has correct numeric columns"), {
      
      # Read the RDS file
      data <- readRDS(file_path)
      
      # Skip if not a data frame
      skip_if(!is.data.frame(data), paste("File", basename(file_path), "is not a data frame"))
      
      # Get columns that should be numeric and exist in the data
      columns_to_check <- intersect(force_numeric_keys, names(data))
      
      # Skip if no columns to check
      skip_if(length(columns_to_check) == 0, 
              paste("File", basename(file_path), "has no columns matching force numeric keys"))
      
      # Check each column that should be numeric
      for (col in columns_to_check) {
        expect_true(is.numeric(data[[col]]), 
                   info = paste("Column", col, "in file", basename(file_path), "should be numeric but is", class(data[[col]])[1]))
      }
    })
  }
})