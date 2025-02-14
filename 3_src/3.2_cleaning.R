library(dplyr)

# Load raw data
#rawdata <- readRDS("1_data/1.1_raw/rawdata.RDS")

#EDA
nrow(rawdata)
head(rawdata)
#as_factor(rawdata)


# Select relevant columns
cleaned_data <- rawdata %>%
  select(COUNTRY, YEAR, URBAN, PERWT, AGE, SEX, EDATTAIN, EMPSTAT,
         WGCARE, WGCOGN, WGCOMM, WGHEAR, WGMOBIL, WGVISION)


# Remove rows where AGE < 10 (missing data)
cleaned_data <- cleaned_data %>% filter(AGE > 9)


# Extract unique countries and years
country_years <- cleaned_data %>%
  select(COUNTRY, YEAR) %>%
  distinct() %>%
  arrange(COUNTRY, YEAR)

# Print the result
print(country_years)

# A tibble: 17 × 2
# COUNTRY                    YEAR
# <int+lbl>                 <int>
# 1  76 [Brazil]               2000
# 2  76 [Brazil]               2010
# 3 104 [Myanmar]              2014
# 4 360 [Indonesia]            2010
# 5 480 [Mauritius]            2011
# 6 504 [Morocco]              2014
# 7 686 [Senegal]              2013
# 8 704 [Vietnam]              2009
# 9 704 [Vietnam]              2019
# 10 710 [South Africa]         2016
# 11 716 [Zimbabwe]             2012
# 12 740 [Suriname]             2012
# 13 780 [Trinidad and Tobago]  2011
# 14 800 [Uganda]               2014
# 15 834 [Tanzania]             2012
# 16 858 [Uruguay]              2011
# 17 894 [Zambia]               2000


# Filter by country (example: Brazil in 2000)
cleaned_data <- cleaned_data %>% filter(COUNTRY == 76 & YEAR == 2000)

# Count occurrences of AGE >100
count_100 <- cleaned_data %>%
  filter(AGE > 100) %>%
  summarise(count = n())
# Print the result
print(count_100)


# Save cleaned data
saveRDS(cleaned_data, "1_data/1.2_cleaned/cleaned_data.RDS")
