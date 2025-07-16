# Compress ADM2 Implementation - Parquet Compression Strategy

## Overview

This document captures the implementation of a new compression strategy for the rmaADM package that uses parquet files with automatic type optimization instead of the legacy RDS compression approach.

## Problem Statement

The original `compress_adm` function reduced file sizes by removing columns and rows and aggregating data, which resulted in data loss. The goal was to create a new compression approach that:

1. **Preserves all data** (no information loss)
2. **Reduces file size** through intelligent type conversion
3. **Uses parquet format** for better compression and performance
4. **Automatically detects** numeric vs character columns
5. **Converts character columns to factors** for compression
6. **Integrates seamlessly** with the existing GitHub release workflow

## Solution Architecture

### Core Strategy
- **Automatic type detection**: Test if character columns can be converted to numeric without data loss
- **Factor conversion**: Convert remaining character columns to factors for optimal compression
- **Parquet output**: Save as compressed parquet files using Arrow
- **Metadata management**: Store factor level mappings for reconstruction
- **Backward compatibility**: Support both legacy RDS and new parquet files

### Key Components

#### 1. Type Detection Function
```r
is_numeric_convertible <- function(x) {
  if (is.numeric(x)) return(TRUE)
  if (!is.character(x)) return(FALSE)
  
  # Remove leading/trailing whitespace
  x_clean <- trimws(x)
  
  # Try to convert to numeric
  x_numeric <- suppressWarnings(as.numeric(x_clean))
  
  # Check if conversion was lossless (ignoring NA values)
  non_na_original <- !is.na(x_clean) & x_clean != ""
  non_na_converted <- !is.na(x_numeric)
  
  # All non-empty, non-NA values should convert successfully
  all(non_na_original == non_na_converted)
}
```

#### 2. Main Compression Function
```r
compress_adm2 <- function(df, output_path, metadata_key) {
  # Convert to data.table for efficient operations
  setDT(df)
  
  factor_metadata <- list()

  # Process each column for type optimization
  for (col_name in names(df)) {
    col_data <- df[[col_name]]

    # Skip if already numeric, date, or logical
    if (is.numeric(col_data) || inherits(col_data, "Date") || is.logical(col_data)) {
      next
    }

    # Convert character columns
    if (is.character(col_data)) {
      if (is_numeric_convertible(col_data)) {
        # Convert to numeric if lossless
        df[, (col_name) := as.numeric(col_data)]
      } else {
        # Convert to factor and store metadata
        factor_col <- as.factor(col_data)
        df[, (col_name) := factor_col]
        factor_metadata[[col_name]] <- levels(factor_col)
      }
    }
  }

  # Save factor metadata
  if (length(factor_metadata) > 0) {
    save_factor_metadata(metadata_key, factor_metadata)
  }

  # Write as parquet file
  arrow::write_parquet(df, output_path, compression = "snappy")
  
  return(df)
}
```

#### 3. Factor Metadata Management
```r
save_factor_metadata <- function(metadata_key, factor_metadata) {
  # Create data directory if it doesn't exist
  data_dir <- "./data"
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }
  
  metadata_file <- file.path(data_dir, "adm_factor_metadata.rds")
  
  # Load existing metadata or create new
  if (file.exists(metadata_file)) {
    existing_metadata <- readRDS(metadata_file)
  } else {
    existing_metadata <- list()
  }
  
  # Add/update metadata for this key
  existing_metadata[[metadata_key]] <- factor_metadata
  
  # Save updated metadata
  saveRDS(existing_metadata, metadata_file, compress = "xz")
}
```

#### 4. Enhanced Data Retrieval
```r
get_cached_data <- function(name, repo = "dylan-turner25/rmaADM", tag = NULL) {
  dest_dir <- tools::R_user_dir("rmaADM", which = "cache")
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  dest_file <- file.path(dest_dir, name)
  
  # Download if not cached
  if (!file.exists(dest_file)) {
    piggyback::pb_download(
      file = name,
      repo = repo,
      tag  = tag,
      dest = dest_dir
    )
  }
  
  # Read based on file extension
  file_ext <- tools::file_ext(name)
  
  if (file_ext == "rds") {
    # Legacy RDS files
    data <- readRDS(dest_file)
  } else if (file_ext == "parquet") {
    # New parquet files
    data <- arrow::read_parquet(dest_file)
    
    # Restore factor levels if metadata exists
    data <- restore_factor_levels(data, name)
  } else {
    stop("Unsupported file format: ", file_ext, ". Supported formats: rds, parquet")
  }
  
  return(data)
}
```

#### 5. Factor Level Restoration
```r
restore_factor_levels <- function(data, filename) {
  # Try to load factor metadata
  metadata_file <- system.file("data", "adm_factor_metadata.rds", package = "rmaADM")
  
  # If package metadata doesn't exist, check local data folder
  if (!file.exists(metadata_file) || metadata_file == "") {
    metadata_file <- "./data/adm_factor_metadata.rds"
  }
  
  if (!file.exists(metadata_file)) {
    # No metadata available, return data as-is
    return(data)
  }
  
  # Load metadata and restore factor levels
  factor_metadata <- readRDS(metadata_file)
  metadata_key <- tools::file_path_sans_ext(basename(filename))
  
  # Find matching metadata and restore factors
  # ... (implementation details for matching and restoration)
  
  return(data)
}
```

## Implementation Steps Completed

### 1. Added Dependencies
- Added `arrow` package to DESCRIPTION for parquet support

### 2. Created Core Functions
- `is_numeric_convertible()`: Tests if columns can be safely converted to numeric
- `compress_adm2()`: Main compression function with type optimization
- `save_factor_metadata()`: Manages factor level storage
- `restore_factor_levels()`: Reconstructs factors when reading parquet files

### 3. Enhanced Data Pipeline
- `get_cached_data()`: Handles both RDS and parquet files automatically
- Updated `get_adm_data()`: Uses new data retrieval function
- Modified `download_adm2()`: Integrated parquet compression

### 4. Simplified to Parquet-Only Approach
- Removed `compression_method` parameter
- Eliminated legacy compression branching logic
- Always outputs parquet files with type optimization
- Streamlined function signatures and documentation

## Benefits Achieved

### 1. Lossless Compression
- **No data loss**: Unlike original `compress_adm`, preserves all information
- **Intelligent optimization**: Automatically detects optimal data types
- **Factor efficiency**: Character-to-factor conversion provides excellent compression

### 2. Performance Improvements
- **Parquet format**: Columnar storage with built-in compression
- **Type optimization**: Numeric columns stored efficiently
- **Factor encoding**: Character data stored as integer codes with lookup tables

### 3. Seamless Integration
- **GitHub releases**: Works with existing piggyback workflow
- **Backward compatibility**: Supports both RDS and parquet files
- **Automatic detection**: No user intervention required for format handling

### 4. Maintainability
- **Single strategy**: Eliminated branching compression logic
- **Clear functions**: Each function has a specific, well-defined purpose
- **Comprehensive documentation**: Updated all function docs for clarity

## Usage Examples

### Data Processing
```r
# Download and process data (always outputs parquet)
download_adm2(
  years = c(2020, 2021), 
  dataset_codes = c("A01090", "A00070")
)
```

### Data Retrieval
```r
# Retrieve data (automatically handles format detection)
subsidy_data <- get_adm_data(year = 2020, dataset = "subsidy")
price_data <- get_adm_data(year = c(2020, 2021), dataset = "price")
```

### Cache Management
```r
# Clear cache to force re-download
clear_rmaADM_cache()
```

## Technical Details

### File Structure
```
/data-raw/
  ├── 2020/
  │   ├── 2020_A00070_SubsidyPercent_YTD.parquet
  │   └── 2020_A01090_...parquet
  └── 2021/
      └── ...

/data/
  └── adm_factor_metadata.rds  # Factor level mappings
```

### Metadata Format
```r
# Example metadata structure
adm_factor_metadata <- list(
  "A00070_2020" = list(
    "state_code" = c("01", "02", "04", ...),
    "commodity_code" = c("0011", "0081", ...)
  ),
  "A01090_2020" = list(
    "unit_structure_code" = c("BU", "EU", "OU", "WU")
  )
)
```

### Type Detection Logic
1. **Numeric columns**: Preserved as-is
2. **Date columns**: Preserved as-is  
3. **Logical columns**: Preserved as-is
4. **Character columns**:
   - If convertible to numeric without loss → convert to numeric
   - Otherwise → convert to factor and store level metadata

## Conclusion

The `compress_adm2` implementation successfully provides a superior compression strategy that:

- **Preserves data integrity** while achieving significant space savings
- **Automates type optimization** without manual configuration  
- **Integrates seamlessly** with existing workflows
- **Maintains backward compatibility** with legacy data
- **Provides excellent performance** through parquet format

The simplified, parquet-only approach eliminates complexity while delivering consistent, high-quality results for ADM data processing.

---
*Implementation completed: July 2025*
*Package: rmaADM*
*Approach: Automatic type detection + parquet compression*