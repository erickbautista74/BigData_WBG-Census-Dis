# 📊 WBG Disability Data Analysis

## 📝 Project Overview
This project analyzes **disability statistics from census data** across multiple countries.  
It uses **R** for **data ingestion, cleaning, transformation, aggregation, and reporting**.  
Results are **exported for Tableau visualization**.

## 🏗️ Folder Structure
```
📂 WBG_Disability_Analysis/
│── 📂 1_data/               # Data storage
│   ├── 1.1_raw/             # Raw census & survey data
│   ├── 1.2_cleaned/         # Processed & cleaned data
│   ├── 1.3_transformed/     # Feature-engineered datasets
│── 📂 2_queries/            # SQL queries for transformations (if needed)
│── 📂 3_src/                # R scripts for ETL pipeline
│   ├── 3.1_ingestion.R      # Loads census survey data
│   ├── 3.2_cleaning.R       # Data cleaning (filtering, missing values)
│   ├── 3.3_transformation.R # Feature engineering
│   ├── 3.4_analysis.R       # Aggregation by age, sex, urban, education
│   ├── 3.5_reporting.R      # Export results for Tableau
│── 📂 4_notebooks/          # Jupyter Notebooks or RMarkdown files
│── 📂 5_configs/            # Configuration files
│── 📂 6_logs/               # Logging & error tracking
│── 📂 7_reports/            # Final reports, visualizations
│── 📂 8_docs/               # Project documentation
│── 📜 requirements.txt      # R package dependencies
│── 📜 .gitignore            # Ignore unnecessary files
```

## 📊 Data Pipeline
1️⃣ **Ingestion** → Load census data from IPUMS  
2️⃣ **Cleaning** → Remove missing values, filter specific countries & years  
3️⃣ **Transformation** → Create new features, recode disability categories  
4️⃣ **Aggregation** → Calculate weighted disability statistics  
5️⃣ **Reporting** → Export CSVs for Tableau visualization  

## 📌 Dependencies
Ensure you have these R packages installed:
```r
install.packages(c("ipumsr", "dplyr", "ggplot2", "caret", "readr"))
```

## 🚀 Running the Project
1. Run data ingestion:
   ```r
   source("3_src/3.1_ingestion.R")
   ```
2. Run data cleaning:
   ```r
   source("3_src/3.2_cleaning.R")
   ```
3. Run transformation:
   ```r
   source("3_src/3.3_transformation.R")
   ```
4. Run analysis:
   ```r
   source("3_src/3.4_analysis.R")
   ```
5. Generate reports:
   ```r
   source("3_src/3.5_reporting.R")
   ```

