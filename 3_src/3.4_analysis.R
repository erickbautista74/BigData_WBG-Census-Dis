# Load transformed data
transformed_data <- readRDS("1_data/1.3_transformed/transformed_data.RDS")
#transformed_data <- cleaned_data

# Compute weighted disability proportions by Urban area
agg_urban <- transformed_data %>%
  group_by(COUNTRY, YEAR, URBAN) %>%
  summarise(across(cols, ~ sum(. * PERWT, na.rm = TRUE))) %>%
  mutate(across(cols, ~ . / sum(PERWT) * 100))

saveRDS(agg_urban, "7_reports/agg_urban.RDS")

# Compute weighted disability proportions by Age
transformed_data$AGE_GROUP <- cut(transformed_data$AGE, breaks = 9)

agg_age <- transformed_data %>%
  group_by(COUNTRY, YEAR, AGE_GROUP) %>%
  summarise(across(cols, ~ sum(. * PERWT, na.rm = TRUE))) %>%
  mutate(across(cols, ~ . / sum(PERWT) * 100))

saveRDS(agg_age, "7_reports/agg_age.RDS")
