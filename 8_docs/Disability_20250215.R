# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).
# Calculate the totals by category(disability) and subcategory(1~4, NA)

# ERICK BAUTISTA

# --------------------------------------------------------------------------------------------------------------------
# 1. DATA COLLECTION
# --------------------------------------------------------------------------------------------------------------------
# 1.1 Install and load packages
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
install.packages('ipumsr')

# Load necessary libraries
library(dplyr)
library(ipumsr)  # Only if you're using IPUMS data

# --------------------------------------------------------------------------------------------------------------------
# 1.2Loading Data
# Load raw dataset (Modify the filename if needed)
ddi <- read_ipums_ddi("/Users/erickbautista/Data/BigData_WBG-Census-Dis/ipumsi_00014.xml")
rawData <- read_ipums_micro(ddi)

# --------------------------------------------------------------------------------------------------------------------
# 2. DATA EXPLORATION
# --------------------------------------------------------------------------------------------------------------------
str(rawData)
names(rawData)
rawDataRows <- nrow(rawData)
rawDataCols <- ncol(rawData)
head(rawData,5)
summary(rawData)
# --------------------------------------------------------------------------------------------------------------------
# 3. DATA PRE-PROCESSING
# --------------------------------------------------------------------------------------------------------------------
# 3.1 Clean Data - Choose variables/columns
rawData <- rawData[,c("COUNTRY","YEAR", "URBAN", "PERWT","AGE","SEX","EDATTAIN","EMPSTAT","WGCARE","WGCOGN","WGCOMM","WGHEAR","WGMOBIL","WGVISION")]
names(rawData)
head(rawData,5)

# Extract unique countries and years
country_years_chart <- rawData %>%
  select(COUNTRY, YEAR) %>%
  distinct() %>%
  arrange(COUNTRY, YEAR)
# Print the result
print(country_years_chart)

# A tibble: 17 × 2
# COUNTRY                    YEAR
# <int+lbl>                 <int>
#  1  76 [Brazil]               2000
#  2  76 [Brazil]               2010
#  3 104 [Myanmar]              2014
#  4 360 [Indonesia]            2010
#  5 480 [Mauritius]            2011
#  6 504 [Morocco]              2014
#  7 686 [Senegal]              2013
#  8 704 [Vietnam]              2009
#  9 704 [Vietnam]              2019
# 10 710 [South Africa]         2016
# 11 716 [Zimbabwe]             2012
# 12 740 [Suriname]             2012
# 13 780 [Trinidad and Tobago]  2011
# 14 800 [Uganda]               2014
# 15 834 [Tanzania]             2012
# 16 858 [Uruguay]              2011
# 17 894 [Zambia]               2000

# Clean Data,Split by country - Important to check the 3%
countryID = 76
countryYear = 2000
country <- subset(rawData, (rawData$COUNTRY==countryID & rawData$YEAR==countryYear))

head(country,5)
summary(country)
nrow(country)
#country_0.03 <- nrow(country)*.03
#print(country_0.03)

# --------------------------------------------------------------------------------------------------------------------
# Clean Data - URBAN == 9 IS missing, delete when URBAN ==9 is less than 3% 
# URBAN               Urban-rural status
# 1                   Rural
# 2                   Urban
# 9                   Unknown

# 3.2 Clean Data - If (URBAN == 9) < 3% then MISSING
# Check percentage of missing URBAN values (URBAN == 9)
urban_9_count <- sum(country$URBAN == 9, na.rm = TRUE)
total_count <- nrow(country)
urban_9_percentage <- (urban_9_count / total_count) * 100
# Print the percentage of URBAN == 9
cat("\nPercentage of URBAN == 9 (missing values):", round(urban_9_percentage, 2), "%")

# Remove URBAN == 9 only if it is less than 3%
if (urban_9_percentage < 3) {
  country <- country %>% filter(URBAN != 9)
  cat("\nURBAN == 9 was removed because it was less than 3% of the dataset.")
} else {
  cat("\nURBAN == 9 was NOT removed because it is 3% or more of the dataset.")
}
# Print summary
cat("\nTotal rows after cleaning URBAN:", nrow(country))
rm(urban_9_count,total_count,urban_9_percentage)

# --------------------------------------------------------------------------------------------------------------------
# AGE                 Age
# 000                 Less than 1 year
# 001                 1 year
# ...
# 099                 99
# 100                 100+
# 999                 Not reported/missing

# 3.3 Clean Data - If (AGE < 10) then delete
#sum(country$AGE<10,na.rm = TRUE) 
country <- subset(country, (country$AGE > 9))
# Print summary
cat("\nTotal rows after cleaning AGE < 10:", nrow(country))

# 3.4 Clean Data - If (AGE == 999) < 3% then MISSING
# Check percentage of missing AGE values (AGE == 999)
age_999_count <- sum(country$AGE == 999, na.rm = TRUE)
total_count <- nrow(country)
age_999_percentage <- (age_999_count / total_count) * 100
# Print the percentage of AGE == 999
cat("\nPercentage of AGE == 999 (missing values):", round(age_999_percentage, 2), "%")

# Remove AGE == 999 only if it is less than 3%
if (age_999_percentage < 3) {
  country <- country %>% filter(AGE != 999)
  cat("\nAGE == 999 was removed because it was less than 3% of the dataset.")
} else {
  cat("\nAGE == 999 was NOT removed because it is 3% or more of the dataset.")
}
# Print summary
cat("\nTotal rows after cleaning AGE==9:", nrow(country))
rm(age_999_count,total_count,age_999_percentage)
# --------------------------------------------------------------------------------------------------------------------
# SEX                 Sex
# 1                   Male
# 2                   Female
# 9                   Unknown

# 3.5 Clean Data - If (SEX == 9) < 3% then MISSING
sex_9_count <- sum(country$SEX == 9, na.rm = TRUE)
total_count <- nrow(country)
sex_9_percentage <- (sex_9_count / total_count) * 100
# Print the percentage of SEX == 9
cat("\nPercentage of SEX == 9 (missing values):", round(sex_9_percentage, 2), "%")

# Remove SEX == 9 only if it is less than 3%
if (sex_9_percentage < 3) {
  country <- country %>% filter(SEX != 9)
  cat("\nSEX == 9 was removed because it was less than 3% of the dataset.")
} else {
  cat("\nSEX == 9 was NOT removed because it is 3% or more of the dataset.")
}
# Print summary
cat("\nTotal rows after cleaning SEX:", nrow(country))
rm(sex_9_count,total_count,sex_9_percentage)

# --------------------------------------------------------------------------------------------------------------------
# EDATTAIN            Educational attainment, international recode [general version]
# 0                   NIU (not in universe)
# 1                   Less than primary completed
# 2                   Primary completed
# 3                   Secondary completed
# 4                   University completed
# 9                   Unknown

# 3.6 Clean Data - If (EDATTAIN == 0) < 3% then MISSING
# Check percentage of missing EDATTAIN values (EDATTAIN == 0)
edattain_0_count <- sum(country$EDATTAIN == 0, na.rm = TRUE)
total_count <- nrow(country)
edattain_0_percentage <- (edattain_0_count / total_count) * 100

# Print the percentage of EDATTAIN == 0
cat("\nPercentage of EDATTAIN == 0 (missing values):", round(edattain_0_percentage, 2), "%")

# Remove EDATTAIN == 0 only if it is less than 3%
if (edattain_0_percentage < 3) {
  country <- country %>% filter(EDATTAIN != 0)
  cat("\nEDATTAIN == 0 was removed because it was less than 3% of the dataset.")
} else {
  cat("\nEDATTAIN == 0 was NOT removed because it is 3% or more of the dataset.")
}

# Print summary
cat("\nTotal rows after cleaning EDATTAIN:", nrow(country))
rm(edattain_0_count,total_count,edattain_0_percentage)
# --------------------------------------------------------------------------------------------------------------------
# 3.7 Clean Data - If (EDATTAIN == 9) < 3% then MISSING
# Check percentage of missing EDATTAIN values (EDATTAIN == 9)
edattain_9_count <- sum(country$EDATTAIN == 9, na.rm = TRUE)
total_count <- nrow(country)
edattain_9_percentage <- (edattain_9_count / total_count) * 100

# Print the percentage of EDATTAIN == 9
cat("\nPercentage of EDATTAIN == 9 (missing values):", round(edattain_9_percentage, 2), "%")

# Remove EDATTAIN == 9 only if it is less than 3%
if (edattain_9_percentage < 3) {
  country <- country %>% filter(EDATTAIN != 9)
  cat("\nEDATTAIN == 9 was removed because it was less than 3% of the dataset.")
} else {
  cat("\nEDATTAIN == 9 was NOT removed because it is 3% or more of the dataset.")
}
# Print summary
cat("\nTotal rows after cleaning EDATTAIN == 9:", nrow(country))
rm(edattain_9_count,total_count,edattain_9_percentage)
# --------------------------------------------------------------------------------------------------------------------
# EMPSTAT             Activity status (employment status) [general version]
# 0                   NIU (not in universe)
# 1                   Employed
# 2                   Unemployed
# 3                   Inactive
# 9                   Unknown/missing

# 3.8 Clean Data - If (EMPSTAT == 0) < 3% then MISSING
# Check percentage of missing EMPSTAT values (EMPSTAT == 0)
empstat_0_count <- sum(country$EMPSTAT == 0, na.rm = TRUE)
total_count <- nrow(country)
empstat_0_percentage <- (empstat_0_count / total_count) * 100

# Print the percentage of EMPSTAT == 0
cat("\nPercentage of EMPSTAT == 0 (missing values):", round(empstat_0_percentage, 2), "%")

# Remove EMPSTAT == 0 only if it is less than 3%
if (empstat_0_percentage < 3) {
  country <- country %>% filter(EMPSTAT != 0)
  cat("\nEMPSTAT == 0 was removed because it was less than 3% of the dataset.")
} else {
  cat("\nEMPSTAT == 0 was NOT removed because it is 3% or more of the dataset.")
}

# Print summary
cat("\nTotal rows after cleaning EMPSTAT == 0:", nrow(country))
rm(empstat_0_count,total_count,empstat_0_percentage)
# --------------------------------------------------------------------------------------------------------------------
# 3.9 Clean Data - If (EMPSTAT == 9) < 3% then MISSING
# Check percentage of missing EMPSTAT values (EMPSTAT == 9)
empstat_9_count <- sum(country$EMPSTAT == 9, na.rm = TRUE)
total_count <- nrow(country)
empstat_9_percentage <- (empstat_9_count / total_count) * 100
# Print the percentage of EMPSTAT == 9
cat("\nPercentage of EMPSTAT == 9 (missing values):", round(empstat_9_percentage, 2), "%")

# Remove EMPSTAT == 9 only if it is less than 3%
if (empstat_9_percentage < 3) {
  country <- country %>% filter(EMPSTAT != 9)
  cat("\nEMPSTAT == 9 was removed because it was less than 3% of the dataset.")
} else {
  cat("\nEMPSTAT == 9 was NOT removed because it is 3% or more of the dataset.")
}
# Print summary
cat("\nTotal rows after cleaning EMPSTAT == 9:", nrow(country))
rm(empstat_9_count,total_count,empstat_9_percentage)


# --------------------------------------------------------------------------------------------------------------------
# QUESTIONAIRE CATEGORIES OF WORLD GROUP
# Values different to [1~4] then 7
# 1                   No difficulty
# 2                   Some difficulty
# 3                   A lot of difficulty
# 4                   Cannot do at all
# 7                   NA
# 8                   Unknown
# 9                   NIU (not in universe)

# 3.10 Clean Data - If (Categories values not in [ 1,2,3,4 ] then NA
# Define the columns to clean
wg_columns <- c("WGCARE", "WGCOGN", "WGCOMM", "WGHEAR", "WGMOBIL", "WGVISION")

# Replace values not in [1,2,3,4] with NA
country <- country %>%
  mutate(across(all_of(wg_columns), ~ ifelse(. %in% c(1, 2, 3, 4), ., NA)))

# Print summary of changes
sapply(wg_columns, function(col) cat("\nInvalid values in", col, "removed:", sum(is.na(country[[col]]))))


# --------------------------------------------------------------------------------------------------------------------
# 3.11 Clean Data - If NA in all six categories then MISING 
# Define the columns to check for NA
wg_columns <- c("WGCARE", "WGCOGN", "WGCOMM", "WGHEAR", "WGMOBIL", "WGVISION")

# Remove rows where all six categories are NA
country <- country[rowSums(!is.na(country[wg_columns])) > 0, ]

# Print summary
cat("\nRows removed where all six categories were NA:", sum(rowSums(!is.na(country[wg_columns])) == 0))


# --------------------------------------------------------------------------------------------------------------------
# Convert responses:
# - 1 or 2 → 0 (NOT disabled)
# - 3 or 4 → 1 (disabled)
# Define the columns to transform
wg_columns <- c("WGCARE", "WGCOGN", "WGCOMM", "WGHEAR", "WGMOBIL", "WGVISION")
country <- country %>%
  mutate(across(all_of(wg_columns), ~ case_when(
    . %in% c(1, 2) ~ 0,  # Not disabled
    . %in% c(3, 4) ~ 1,  # Disabled
    TRUE ~ .             # Keep other values unchanged
  )))

# Print summary
sapply(wg_columns, function(col) cat("\nTransformation applied to:", col))

rm(wg_columns)
# --------------------------------------------------------------------------------------------------------------------
# 3.13 Totals
# Define disability columns
disability_cols <- c("WGCARE", "WGCOGN", "WGCOMM", "WGHEAR", "WGMOBIL", "WGVISION")

# Compute totals for each disability category
country_0 <- country %>%
  group_by(COUNTRY, YEAR) %>%
  summarise(across(all_of(disability_cols), 
                   ~ sum(PERWT[. == 0], na.rm = TRUE), 
                   .names = "{.col}"),
            .groups = "drop") %>%
  mutate(DIS_CAT = 0)

country_1 <- country %>%
  group_by(COUNTRY, YEAR) %>%
  summarise(across(all_of(disability_cols), 
                   ~ sum(PERWT[. == 1], na.rm = TRUE), 
                   .names = "{.col}"),
            .groups = "drop") %>%
  mutate(DIS_CAT = 1)

country_NA <- country %>%
  group_by(COUNTRY, YEAR) %>%
  summarise(across(all_of(disability_cols), 
                   ~ sum(PERWT[is.na(.)], na.rm = TRUE), 
                   .names = "{.col}"),
            .groups = "drop") %>%
  mutate(DIS_CAT = NA_real_)

# Combine results
countryTotal <- bind_rows(country_0, country_1, country_NA)

# Rename columns
colnames(countryTotal)[1] <- "COUNTRY"
colnames(countryTotal)[2] <- "YEAR"

# Round the weighted totals
countryTotal[3:9] <- round(countryTotal[3:9], 0)

# Print summary
print(countryTotal)

rm(disability_cols,country_0,country_1,country_NA)
gc()
# --------------------------------------------------------------------------------------------------------------------
# 3.14 Age 
# Define age intervals (bins)
range_age <- seq(9, max(country$AGE, na.rm = TRUE) + 10, by = 10)

# Create AGE group column
country <- country %>%
  mutate(AGE3 = cut(AGE, breaks = range_age, include.lowest = TRUE))

# Define disability columns
disability_cols <- c("WGCARE", "WGCOGN", "WGCOMM", "WGHEAR", "WGMOBIL", "WGVISION")

# Compute Numerator (Weighted Sum of Disabled People per Age Group)**
country_num <- country %>%
  group_by(COUNTRY, YEAR, AGE3) %>%
  summarise(across(all_of(disability_cols), ~ sum(. * PERWT, na.rm = TRUE), .names = "{.col}_COUNT"),
            .groups = "drop")

# Compute Denominator (Total Weighted Population per Age Group)**
country_den <- country %>%
  group_by(COUNTRY, YEAR, AGE3) %>%
  summarise(TOTAL_POP = sum(PERWT, na.rm = TRUE), .groups = "drop") %>%
  mutate(TOTAL_POP = round(TOTAL_POP, 0))  # Ensure whole numbers

# Compute Percentages with Six Decimals**
country_per <- country_num %>%
  left_join(country_den, by = c("COUNTRY", "YEAR", "AGE3")) %>%
  mutate(across(ends_with("_COUNT"), ~ round((. / TOTAL_POP) * 100, 6), .names = "{.col}_PCT"))

# Final Output: Combine Counts, Percentages, and Total Population**
country_Age_Chart <- country_num %>%
  left_join(select(country_per, COUNTRY, YEAR, AGE3, ends_with("_PCT"), TOTAL_POP),
            by = c("COUNTRY", "YEAR", "AGE3")) %>%
  arrange(COUNTRY, AGE3)

rm(range_age,country_num,country_den,country_per)

# Print final structured output
print(country_Age_Chart)
rm(disability_cols)
gc()
# --------------------------------------------------------------------------------------------------------------------
# 3.15 Education 
# Define disability columns
disability_cols <- c("WGCARE", "WGCOGN", "WGCOMM", "WGHEAR", "WGMOBIL", "WGVISION")

# Compute Denominator (Total Weighted Population per Education Level)**
country_den <- country %>%
  group_by(COUNTRY, YEAR, EDATTAIN) %>%
  summarise(TOTAL_POP = sum(PERWT, na.rm = TRUE), .groups = "drop") %>%
  mutate(TOTAL_POP = round(TOTAL_POP, 0))  # Ensure integer values

# Compute Numerator (Weighted Sum of Disabled People per Education Level)**
country_num <- country %>%
  group_by(COUNTRY, YEAR, EDATTAIN) %>%
  summarise(across(all_of(disability_cols), ~ sum(. * PERWT, na.rm = TRUE), .names = "{.col}_COUNT"),
            .groups = "drop") %>%
  mutate(across(ends_with("_COUNT"), round, 0))  # Ensure all counts are integers

#  Compute Percentages First (Before Further Rounding)**
country_per <- country_num %>%
  left_join(country_den, by = c("COUNTRY", "YEAR", "EDATTAIN")) %>%
  mutate(across(ends_with("_COUNT"), ~ (. / TOTAL_POP) * 100, .names = "{.col}_PCT")) %>%
  mutate(across(ends_with("_PCT"), ~ round(., 6)))  # Ensure six decimal places in percentages

#  Final Output: Ensure No Floating Point Errors**
country_Edu_Chart <- country_num %>%
  left_join(select(country_per, COUNTRY, YEAR, EDATTAIN, ends_with("_PCT"), TOTAL_POP),
            by = c("COUNTRY", "YEAR", "EDATTAIN")) %>%
  arrange(COUNTRY, EDATTAIN) %>%
  mutate(across(ends_with("_COUNT"), as.integer),  # Ensure all counts are stored as integers
         TOTAL_POP = as.integer(TOTAL_POP))  # Ensure TOTAL_POP is an integer

# Print final structured output
print(country_Edu_Chart)
rm(disability_cols,country_num,country_den,country_per)
gc()
# --------------------------------------------------------------------------------------------------------------------
# 3.16 Employment
# Define disability columns
disability_cols <- c("WGCARE", "WGCOGN", "WGCOMM", "WGHEAR", "WGMOBIL", "WGVISION")

# ** Compute Denominator (Total Weighted Population per Employment Status)**
country_den <- country %>%
  group_by(COUNTRY, YEAR, EMPSTAT) %>%
  summarise(TOTAL_POP = sum(PERWT, na.rm = TRUE), .groups = "drop")

#  Compute Numerator (Weighted Sum of Disabled People per Employment Status)**
country_num <- country %>%
  group_by(COUNTRY, YEAR, EMPSTAT) %>%
  summarise(across(all_of(disability_cols), ~ sum(. * PERWT, na.rm = TRUE), .names = "{.col}_COUNT"),
            .groups = "drop")

#  Compute Percentages First (Before Rounding)**
country_per <- country_num %>%
  left_join(country_den, by = c("COUNTRY", "YEAR", "EMPSTAT")) %>%
  mutate(across(ends_with("_COUNT"), ~ (. / TOTAL_POP) * 100, .names = "{.col}_PCT")) %>%
  mutate(across(ends_with("_PCT"), ~ round(., 6)))  # Ensure six decimal places in percentages

#  Round Counts and Total Population After Percentage Calculation**
country_per <- country_per %>%
  mutate(across(ends_with("_COUNT"), ~ round(., 0)),  # Ensure counts are whole numbers
         TOTAL_POP = round(TOTAL_POP, 0))  # Ensure total population is a whole number

#  Final Output: Counts, Percentages, and Total Population**
country_Emp_Chart <- country_num %>%
  left_join(select(country_per, COUNTRY, YEAR, EMPSTAT, ends_with("_PCT"), TOTAL_POP),
            by = c("COUNTRY", "YEAR", "EMPSTAT")) %>%
  arrange(COUNTRY, EMPSTAT)

# Print final structured output
print(country_Emp_Chart)
rm(disability_cols,country_num, country_den, country_per)
gc()
# --------------------------------------------------------------------------------------------------------------------
# 3.17 Sex
# Define disability columns
disability_cols <- c("WGCARE", "WGCOGN", "WGCOMM", "WGHEAR", "WGMOBIL", "WGVISION")

# Compute Denominator (Total Weighted Population per Sex)**
country_den <- country %>%
  group_by(COUNTRY, YEAR, SEX) %>%
  summarise(TOTAL_POP = sum(PERWT, na.rm = TRUE), .groups = "drop")

# Compute Numerator (Weighted Sum of Disabled People per Sex)**
country_num <- country %>%
  group_by(COUNTRY, YEAR, SEX) %>%
  summarise(across(all_of(disability_cols), ~ sum(. * PERWT, na.rm = TRUE), .names = "{.col}_COUNT"),
            .groups = "drop")

# Compute Percentages First (Before Rounding)**
country_per <- country_num %>%
  left_join(country_den, by = c("COUNTRY", "YEAR", "SEX")) %>%
  mutate(across(ends_with("_COUNT"), ~ (. / TOTAL_POP) * 100, .names = "{.col}_PCT")) %>%
  mutate(across(ends_with("_PCT"), ~ round(., 6)))  # Ensure six decimal places in percentages

# Round Counts and Total Population After Percentage Calculation**
country_per <- country_per %>%
  mutate(across(ends_with("_COUNT"), ~ round(., 0)),  # Ensure counts are whole numbers
         TOTAL_POP = round(TOTAL_POP, 0))  # Ensure total population is a whole number

# Final Output: Counts, Percentages, and Total Population**
country_Sex_Chart <- country_num %>%
  left_join(select(country_per, COUNTRY, YEAR, SEX, ends_with("_PCT"), TOTAL_POP),
            by = c("COUNTRY", "YEAR", "SEX")) %>%
  arrange(COUNTRY, SEX)

# Print final structured output
print(country_Sex_Chart)
rm(disability_cols,country_num, country_den, country_per)
gc()
# --------------------------------------------------------------------------------------------------------------------
# 3.18 Urban
# Define disability columns
disability_cols <- c("WGCARE", "WGCOGN", "WGCOMM", "WGHEAR", "WGMOBIL", "WGVISION")

# Compute Denominator (Total Weighted Population per Urban/Rural Status)**
country_den <- country %>%
  group_by(COUNTRY, YEAR, URBAN) %>%
  summarise(TOTAL_POP = sum(PERWT, na.rm = TRUE), .groups = "drop")

# Compute Numerator (Weighted Sum of Disabled People per Urban/Rural Status)**
country_num <- country %>%
  group_by(COUNTRY, YEAR, URBAN) %>%
  summarise(across(all_of(disability_cols), ~ sum(. * PERWT, na.rm = TRUE), .names = "{.col}_COUNT"),
            .groups = "drop")

# Compute Percentages First (Before Rounding)**
country_per <- country_num %>%
  left_join(country_den, by = c("COUNTRY", "YEAR", "URBAN")) %>%
  mutate(across(ends_with("_COUNT"), ~ (. / TOTAL_POP) * 100, .names = "{.col}_PCT")) %>%
  mutate(across(ends_with("_PCT"), ~ round(., 6)))  # Ensure six decimal places in percentages

# Round Counts and Total Population After Percentage Calculation**
country_per <- country_per %>%
  mutate(across(ends_with("_COUNT"), ~ round(., 0)),  # Ensure counts are whole numbers
         TOTAL_POP = round(TOTAL_POP, 0))  # Ensure total population is a whole number

#  Final Output: Counts, Percentages, and Total Population**
country_Urb_Chart <- country_num %>%
  left_join(select(country_per, COUNTRY, YEAR, URBAN, ends_with("_PCT"), TOTAL_POP),
            by = c("COUNTRY", "YEAR", "URBAN")) %>%
  arrange(COUNTRY, URBAN)


# Print final structured output
print(country_Urb_Chart)

# Remove temporary variables and free memory
rm(disability_cols,country_num, country_den, country_per)
gc()

# --------------------------------------------------------------------------------------------------------------------
# 3.19 print Charts
print(country_Age_Chart)
print(country_Edu_Chart)
print(country_Emp_Chart)
print(country_Sex_Chart)
print(country_Urb_Chart)







# --------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------