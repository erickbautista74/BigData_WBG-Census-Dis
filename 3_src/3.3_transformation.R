# Load cleaned data
cleaned_data <- readRDS("1_data/1.2_cleaned/cleaned_data.RDS")

head(cleaned_data)

# Recode disability categories
convert_disability <- function(x) {
  ifelse(x == 1 | x == 2, 0,
         ifelse(x == 3 | x == 4, 1, NA))
}

cols <- c("WGCARE", "WGCOGN", "WGCOMM", "WGHEAR", "WGMOBIL", "WGVISION")
cleaned_data[cols] <- lapply(cleaned_data[cols], convert_disability)

# Remove rows where all disability variables are NA
cleaned_data <- cleaned_data %>% filter(rowSums(is.na(cleaned_data[cols])) < 6)

# Save transformed data
saveRDS(cleaned_data, "1_data/1.3_transformed/transformed_data.RDS")
