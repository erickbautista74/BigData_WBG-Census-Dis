agg_urban <- readRDS("7_reports/agg_urban.RDS")
agg_age <- readRDS("7_reports/agg_age.RDS")

# Export as CSV for Tableau / Excel
write.csv(agg_urban, "7_reports/agg_urban.csv", row.names = FALSE)
write.csv(agg_age, "7_reports/agg_age.csv", row.names = FALSE)

print("Reports exported successfully!")
