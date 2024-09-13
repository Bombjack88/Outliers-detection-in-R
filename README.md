# Automated Outlier Detection for ECB Interest Rate Data: Processing and Reporting

## Overview

This project provides an automated system for detecting outliers in interest rate data reported by the European Central Bank (ECB). The data is processed in several stages, resulting in a final report containing both statistical analysis of outliers and exported CSV files for further review.

## Project Structure

### Part 1: Data Processing

1. **Importing Required Packages**:  
   The program ensures that all necessary libraries are installed and loaded, such as `tsoutliers`, `seasonal`, `data.table`, and `tidyverse`.

2. **Importing Data**:  
   - Files are imported from a directory.
   - Data is processed to ensure the proper structure for analysis, including renaming columns and converting data types as needed.

3. **File Handling**:  
   - Merges new data with the previous full dataset.
   - Removes duplicate entries based on `series_code` and `periodo`.

4. **Data Cleaning**:  
   - Ensures that all values are numeric and in the correct format for analysis.

5. **Exporting Processed Data**:  
   - Final datasets are saved as CSV files for both the new data and the combined dataset.

### Part 2: Outlier Detection

The outlier detection is performed using two methods:

1. **TERROR Function**:  
   Uses the TRAMO/SEATS methodology to detect outliers in time series data, including Additive Outliers (AO) and Level Shifts (LS).
   
2. **Relative Variation**:  
   Compares quarterly or monthly data variations and flags outliers based on deviations from calculated quantiles (IQR).

### CSV Output

- **Terror Outliers**: Severe outliers detected by the TRAMO methodology are exported to `terror.csv`.
- **Relative Variation Outliers**: Detected by comparing data variations, exported to `var.csv`.


## How to Run

1. **Run Part 1**:  
   Execute the code up to line 685 (Part 1) to process and prepare the data.
   
2. **Run Part 2**:  
   After completing Part 1, run the `OutlierDetection` function with the appropriate input and output directories.

```r
OutlierDetection(dir_in, dir_out, 'BCE_Tax_juro_MIR', 2021)
