# Load required packages
if (!require("ipumsr")) install.packages("ipumsr", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("readr")) install.packages("readr", dependencies = TRUE)

library(dplyr)
library(ipumsr)
library(readr)

# Load dataset
ddi <- read_ipums_ddi("path/to/your/ipumsi_00014.xml")  # Modify path as needed
rawData <- read_ipums_micro(ddi)

# Create output folder
output_folder <- "Disability_Analysis_Results"
if (!dir.exists(output_folder)) dir.create(output_folder)

# Function to analyze a specific country and year
analyze_disability_data <- function(countryID, yearID) {
  
  cat("\nProcessing Country:", countryID, "Year:", yearID, "\n")
  
  # Filter data for the selected country and year
  country <- subset(rawData, COUNTRY == countryID & YEAR == yearID)
  
  # If no data, return
  if (nrow(country) == 0) {
    cat("\nNo data for Country:", countryID, "Year:", yearID, "\n")
    return(NULL)
  }
  
  # Clean missing values for URBAN (if <3%, remove)
  urban_9_count <- sum(country$URBAN == 9, na.rm = TRUE)
  if ((urban_9_count / nrow(country)) * 100 < 3) {
    country <- country %>% filter(URBAN != 9)
  }
  
  # Remove AGE < 10
  country <- subset(country, AGE > 9)
  
  # Remove missing AGE (999), SEX (9), EDATTAIN (0 & 9), EMPSTAT (0 & 9)
  country <- country %>% filter(AGE != 999, SEX != 9, !EDATTAIN %in% c(0, 9), !EMPSTAT %in% c(0, 9))
  
  # Convert disability categories (1 & 2 → 0, 3 & 4 → 1)
  disability_cols <- c("WGCARE", "WGCOGN", "WGCOMM", "WGHEAR", "WGMOBIL", "WGVISION")
  country <- country %>%
    mutate(across(all_of(disability_cols), ~ case_when(. %in% c(1, 2) ~ 0, . %in% c(3, 4) ~ 1, TRUE ~ .)))
  
  # Remove rows where all six disability columns are NA
  country <- country[rowSums(!is.na(country[disability_cols])) > 0, ]
  
  # Generate summary tables for different factors

  # Disability summary by category
  countryTotal <- country %>%
    group_by(COUNTRY, YEAR) %>%
    summarise(across(all_of(disability_cols), ~ sum(PERWT[. == 1], na.rm = TRUE), .names = "{.col}_disabled"),
              .groups = "drop") %>%
    mutate(TOTAL_DISABLED = rowSums(select(., ends_with("_disabled")), na.rm = TRUE))
  
  # Save temporary files
  write_csv(countryTotal, file.path(output_folder, paste0("Disability_Summary_", countryID, "_", yearID, ".csv")))
  
  # Clear memory after processing each country
  rm(country, countryTotal)
  gc()
  
  cat("\nCompleted Country:", countryID, "Year:", yearID, "\n")
}

# Get unique country-year combinations
unique_countries_years <- rawData %>%
  select(COUNTRY, YEAR) %>%
  distinct()

# Loop through all available countries and years
for (i in 1:nrow(unique_countries_years)) {
  analyze_disability_data(unique_countries_years$COUNTRY[i], unique_countries_years$YEAR[i])
}

# Combine all temporary CSV files into one
file_list <- list.files(output_folder, pattern = "Disability_Summary_.*\\.csv", full.names = TRUE)

if (length(file_list) > 0) {
  combined_data <- do.call(rbind, lapply(file_list, read_csv))
  write_csv(combined_data, file.path(output_folder, "Final_Disability_Summary.csv"))
  
  # Remove individual temporary files
  file.remove(file_list)
}

cat("\nAll analyses completed! Final CSV saved as 'Final_Disability_Summary.csv'.\n")
