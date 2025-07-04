# Description ------------------------------------------------------------------
# R script to process USAID Flood Response Post Intervention Survey data
# 
# This script transforms raw survey data from water points in Mulanje (2019-2020)
# into a clean, analysis-ready dataset. The data includes water quality measurements
# and operational assessments from various water points affected by flooding.
#
# Author: openwashdata
# Date: 2025-07-04
# License: CC BY 4.0
#
# Load packages ----------------------------------------------------------------
# Required packages for data processing
library(here)      # For consistent file paths
library(readr)     # For reading CSV files
library(dplyr)     # For data manipulation
library(readxl)    # For reading Excel files (if needed)
library(openxlsx)  # For writing Excel files

# Load Data --------------------------------------------------------------------
# Load raw survey data from CSV file
# Note: This file contains water quality measurements from 308+ water points
data_in <- readr::read_csv(here::here("data-raw", "USAID Flood Response - Post Intervention Survey.csv"))

# Initial data inspection -----------------------------------------------------
message("Raw data loaded: ", nrow(data_in), " rows, ", ncol(data_in), " columns")

# Tidy data --------------------------------------------------------------------
# Remove rows with missing coordinates (required for mapping water points)
initial_rows <- nrow(data_in)
data_in <- data_in %>%
  filter(!is.na(latitude), !is.na(longitude))

rows_removed <- initial_rows - nrow(data_in)
if (rows_removed > 0) {
  message("Removed ", rows_removed, " rows with missing coordinates")
}

# Replace unusually low pH values (0.73 and 0.68) with NA
# These values are outliers and likely measurement errors
data_in <- data_in %>%
  mutate(ph = ifelse(ph %in% c(0.68, 0.73), NA, ph))


# Character encoding cleanup --------------------------------------------------
# The raw data may contain special characters (e.g., μ for micro) that need 
# proper UTF-8 encoding. This is common when data is exported from various 
# survey platforms or entered on different systems.

# Function to check for non-UTF-8 characters in character columns
check_utf8 <- function(df) {
  invalid_cols <- sapply(df, function(column) {
    if (!is.character(column)) return(FALSE)
    any(sapply(column, function(x) {
      if (is.na(x)) return(FALSE)
      !identical(iconv(x, from = "UTF-8", to = "UTF-8"), x)
    }))
  })
  
  bad_cols <- names(df)[invalid_cols]
  
  if (length(bad_cols) > 0) {
    message("Non-UTF-8 characters detected in columns: ",
            paste(bad_cols, collapse = ", "))
  } else {
    message("All character data is properly UTF-8 encoded")
  }
  
  return(invisible(bad_cols))
}

# Check initial encoding
message("\nChecking character encoding before conversion...")
problematic_cols <- check_utf8(data_in)

# Convert from Latin1 to UTF-8 if needed
# Note: mWater platform sometimes exports with Latin1 encoding
if (length(problematic_cols) > 0) {
  message("Converting character data from Latin1 to UTF-8...")
  data_in[] <- lapply(data_in, function(x) {
    if (is.character(x)) {
      iconv(x, from = "latin1", to = "UTF-8", sub = "")
    } else {
      x
    }
  })
  
  # Verify conversion
  message("Checking character encoding after conversion...")
  check_utf8(data_in)
}

# Fix specific data quality issues ---------------------------------------------
message("\nApplying data quality fixes...")

# Fix encoding issue in electrical_conductivity_units
# The micro symbol (μ) is often incorrectly encoded as "?"
data_in <- data_in %>%
  mutate(electrical_conductivity_units = gsub("\\?S / cm", "μS / cm", electrical_conductivity_units))

# Data validation checks -------------------------------------------------------
message("\nPerforming data validation checks...")

# 1. Coordinate validation
invalid_coords <- data_in %>%
  filter(latitude < -90 | latitude > 90 | longitude < -180 | longitude > 180)

if (nrow(invalid_coords) > 0) {
  warning("Found ", nrow(invalid_coords), " rows with invalid coordinates")
  message("Invalid coordinate rows: ", paste(which(data_in$latitude < -90 | data_in$latitude > 90 | 
                                                    data_in$longitude < -180 | data_in$longitude > 180), 
                                              collapse = ", "))
}

# 2. pH validation (should be between 0-14)
if (any(!is.na(data_in$ph))) {
  ph_range <- range(data_in$ph, na.rm = TRUE)
  message("pH range: ", round(ph_range[1], 2), " - ", round(ph_range[2], 2))
  
  # Flag potentially erroneous pH values
  unusual_ph <- which(data_in$ph < 4 | data_in$ph > 10)
  if (length(unusual_ph) > 0) {
    warning("Unusual pH values found in rows: ", paste(unusual_ph, collapse = ", "))
    message("  Low pH (<4): ", paste(which(data_in$ph < 4), collapse = ", "))
    message("  High pH (>10): ", paste(which(data_in$ph > 10), collapse = ", "))
  }
}

# 3. Temperature validation (reasonable range for water)
if (any(!is.na(data_in$temperature_magnitude))) {
  temp_range <- range(data_in$temperature_magnitude, na.rm = TRUE)
  message("Temperature range: ", round(temp_range[1], 1), " - ", round(temp_range[2], 1), " °C")
  
  # Flag extreme temperatures
  extreme_temp <- which(data_in$temperature_magnitude < 0 | data_in$temperature_magnitude > 45)
  if (length(extreme_temp) > 0) {
    warning("Extreme temperature values in rows: ", paste(extreme_temp, collapse = ", "))
  }
}

# 4. E.coli validation (should be non-negative)
negative_ecoli <- which(data_in$ecoli_mpn_per_100ml < 0)
if (length(negative_ecoli) > 0) {
  warning("Negative E.coli counts in rows: ", paste(negative_ecoli, collapse = ", "))
}

# 5. Date format standardization
message("\nStandardizing date formats...")

# Convert date columns to consistent format
data_in <- data_in %>%
  mutate(
    submitted_on = as.character(submitted_on),  # Keep as character for consistency
    date_of_sample = as.character(date_of_sample)
  )

# Note about operational_feel_of_pump
# This field contains comma-separated multiple values in some entries
# Preserving as-is to maintain data integrity - users can parse as needed

# Summary statistics -----------------------------------------------------------
message("\nData summary after cleaning:")
message("  Total rows: ", nrow(data_in))
message("  Total columns: ", ncol(data_in))
message("  Water points represented: ", n_distinct(data_in$water_point_name, na.rm = TRUE))
message("  Date range: ", min(data_in$submitted_on, na.rm = TRUE), " to ", 
        max(data_in$submitted_on, na.rm = TRUE))

# Create final dataset
postfloodintervention <- data_in

# Export Data ------------------------------------------------------------------
message("\nExporting processed data...")

# Save as R data file
save(postfloodintervention, file = here::here("data", "postfloodintervention.rda"), 
     compress = "bzip2")
message("  ✓ Saved .rda file")

# Create directory for additional exports if it doesn't exist
extdata_dir <- here::here("inst", "extdata")
if (!dir.exists(extdata_dir)) {
  dir.create(extdata_dir, recursive = TRUE)
}

# Export as CSV
readr::write_csv(postfloodintervention,
                 here::here("inst", "extdata", "postfloodintervention.csv"))
message("  ✓ Exported CSV file")

# Export as Excel
openxlsx::write.xlsx(postfloodintervention,
                     here::here("inst", "extdata", "postfloodintervention.xlsx"))
message("  ✓ Exported Excel file")

message("\nData processing complete!")
