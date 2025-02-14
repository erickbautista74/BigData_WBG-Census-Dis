# Install required packages (only if not installed)
if (!require("ipumsr")) install.packages("ipumsr")
if (!require("dplyr")) install.packages("dplyr")

# Load libraries
library(ipumsr)
library(dplyr)

# Define path to data
ddi <- read_ipums_ddi("/Users/erickbautista/Data/BigData_WBG-Census-Dis/ipumsi_00014.xml")  
rawdata <- read_ipums_micro(ddi)

# Save raw data for later steps
saveRDS(rawdata, "1_data/1.1_raw/rawdata.RDS")
