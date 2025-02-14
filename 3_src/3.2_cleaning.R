library(dplyr)

# Load raw data
rawdata <- readRDS("1_data/1.1_raw/rawdata.RDS")

# Select relevant columns
cleaned_data <- rawdata %>%
  select(COUNTRY, YEAR, URBAN, PERWT, AGE, AGE2, SEX, EDATTAIN, EMPSTAT,
         WGCARE, WGCOGN, WGCOMM, WGHEAR, WGMOBIL, WGVISION)

# Filter by country (example: Brazil in 2000)
cleaned_data <- cleaned_data %>% filter(COUNTRY == 76 & YEAR == 2000)

# Remove rows where AGE < 10 (missing data)
cleaned_data <- cleaned_data %>% filter(AGE > 9)

# Save cleaned data
saveRDS(cleaned_data, "1_data/1.2_cleaned/cleaned_data.RDS")
