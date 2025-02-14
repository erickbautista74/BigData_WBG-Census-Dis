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
library('ipumsr')
library(dplyr)
df_Total <- data.frame()
df_Age <- data.frame()
df_Education <- data.frame()
df_Employment <- data.frame()
df_Sex <- data.frame()
df_Urban <- data.frame()

# --------------------------------------------------------------------------------------------------------------------
# 1.2Loading Data
setwd("~/WBG/2 Projects/1 Disability 6WG - Census/2 Data") 
ddi <- read_ipums_ddi("ipumsi_00008.xml")
rawData <- read_ipums_micro(ddi)
gc()

# ==========
setwd("~/WBG/2 Projects/1 Disability 6WG - Census/2 Data") 
rawData <-  read.csv("df_sample.csv")
# =========

# --------------------------------------------------------------------------------------------------------------------
# 2. DATA EXPLORATION
# --------------------------------------------------------------------------------------------------------------------
str(rawData)
names(rawData)
rawDataRows <- nrow(rawData)
rawDataCols <- ncol(rawData)
head(rawData,5)
#summary(rawData)
rawDataPerc <- rawDataRows*.03 # 305MM
gc()

# --------------------------------------------------------------------------------------------------------------------
# 3. DATA PRE-PROCESSING
# --------------------------------------------------------------------------------------------------------------------
# 3.1 Clean Data - Choose variables/columns
rawData <- rawData[,c("COUNTRY","YEAR", "URBAN", "PERWT","AGE","SEX","EDATTAIN","EMPSTAT","WGCARE","WGCOGN","WGCOMM","WGHEAR","WGMOBIL","WGVISION")]
names(rawData)
head(rawData,5)

# **********************************
#    Group.1 Group.2        x             Group.1
# 1       76    2000 20274412              Brazil
# 4       76    2010 20635472              Brazil
# 13     104    2014  5032818             Myanmar
# 5      360    2010 23603049           Indonesia
# 6      480    2011   126332           Mauritius
# 14     504    2014  3341426             Morocco
# 12     686    2013  1245551             Senegal
# 3      704    2009 14177590             Vietnam
# 16     710    2016  3328793        South Africa (employment, all in NA)
# 9      716    2012   654688            Zimbabwe
# 10     740    2012    53636            Suriname (Urban, all in NA)
# 7      780    2011   116917 Trinidad and Tobago
# 15     800    2014  3506546              Uganda
  # 11     834    2012  4498022            Tanzania
# 8      858    2011   328425             Uruguay
# 2      894    2000   996117              Zambia

# Clean Data,Split by country - Important to check the 3%
countryID = 894
countryYear = 2000
rm(country)
country <- subset(rawData, (rawData$COUNTRY==countryID & rawData$YEAR==countryYear))
gc()

names(country)
countryRows <- nrow(country)
countryCols <- ncol(rawData)
head(country,5)
#summary(country)
countryRows*.03
gc()


# --------------------------------------------------------------------------------------------------------------------
# Clean Data - delete missing when < 3% 
# URBAN               Urban-rural status
# 1                   Rural
# 2                   Urban
# 9                   Unknown

# 3.2 Clean Data - If (URBAN == 9) < 3% then MISSING
#sum(country$URBAN==9,na.rm = TRUE) 
country <- subset(country, (country$URBAN < 3))
gc()


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
gc()

# 3.4 Clean Data - If (AGE == 999) < 3% then MISSING
#sum(country$AGE==999,na.rm = TRUE) 
country <- subset(country, (country$AGE < 999))
gc()


# --------------------------------------------------------------------------------------------------------------------
# SEX                 Sex
# 1                   Male
# 2                   Female
# 9                   Unknown

# 3.5 Clean Data - If (SEX == 9) < 3% then MISSING
#sum(country$SEX==9,na.rm = TRUE) # 0
country <- subset(country, (country$SEX < 3))
gc()


# --------------------------------------------------------------------------------------------------------------------
# EDATTAIN            Educational attainment, international recode [general version]
# 0                   NIU (not in universe)
# 1                   Less than primary completed
# 2                   Primary completed
# 3                   Secondary completed
# 4                   University completed
# 9                   Unknown

# 3.6 Clean Data - If (EDATTAIN == 0) < 3% then MISSING
#sum(country$EDATTAIN==0,na.rm = TRUE) 
country <- subset(country, (country$EDATTAIN > 0))
gc()

# 3.7 Clean Data - If (EDATTAIN == 9) < 3% then MISSING
#sum(country$EDATTAIN==9,na.rm = TRUE) # 
country <- subset(country, (country$EDATTAIN < 5))
gc()


# --------------------------------------------------------------------------------------------------------------------
# EMPSTAT             Activity status (employment status) [general version]
# 0                   NIU (not in universe)
# 1                   Employed
# 2                   Unemployed
# 3                   Inactive
# 9                   Unknown/missing

# 3.8 Clean Data - If (EMPSTAT == 0) < 3% then MISSING
#sum(country$EMPSTAT==0,na.rm = TRUE) 
country <- subset(country, (country$EMPSTAT > 0))
gc()

# 3.9 Clean Data - If (EMPSTAT == 9) < 3% then MISSING
#sum(country$EMPSTAT==9,na.rm = TRUE) 
country <- subset(country, (country$EMPSTAT < 5))
gc()


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

# 3.10 Clean Data - If (Categories values not in [ 1,2,3,4 ] AND < 3% then MISSING 
#sum(country$WGCARE>4,na.rm = TRUE) 
country$WGCARE <- ifelse(country$WGCARE!=1 & country$WGCARE!=2 & country$WGCARE!=3 & country$WGCARE!=4, NA, country$WGCARE)
gc()

#sum(country$WGCOGN>4,na.rm = TRUE) 
country$WGCOGN <- ifelse(country$WGCOGN!=1 & country$WGCOGN!=2 & country$WGCOGN!=3 & country$WGCOGN!=4, NA, country$WGCOGN)
gc()

#sum(country$WGCOMM>4,na.rm = TRUE) 
country$WGCOMM <- ifelse(country$WGCOMM!=1 & country$WGCOMM!=2 & country$WGCOMM!=3 & country$WGCOMM!=4, NA, country$WGCOMM)
gc()

#sum(country$WGHEAR>4,na.rm = TRUE) 
country$WGHEAR <- ifelse(country$WGHEAR!=1 & country$WGHEAR!=2 & country$WGHEAR!=3 & country$WGHEAR!=4, NA, country$WGHEAR)
gc()

#sum(country$WGMOBIL>4,na.rm = TRUE) 
country$WGMOBIL <- ifelse(country$WGMOBIL!=1 & country$WGMOBIL!=2 & country$WGMOBIL!=3 & country$WGMOBIL!=4, NA,country$WGMOBIL)
gc()

#sum(country$WGVISION>4,na.rm = TRUE) 
country$WGVISION <- ifelse(country$WGVISION!=1 & country$WGVISION!=2 & country$WGVISION!=3 & country$WGVISION!=4, NA,country$WGVISION)
gc()


# --------------------------------------------------------------------------------------------------------------------
# 3.11 Clean Data - If NA in all six categories then MISING 
country <- subset(country,(!is.na(country$WGCARE)|!is.na(country$WGCOGN)|!is.na(country$WGCOMM)|!is.na(country$WGHEAR)|!is.na(country$WGMOBIL)|!is.na(country$WGVISION) ))
#nrow(country)
#head(country,5)
gc()


# --------------------------------------------------------------------------------------------------------------------
# 3.12 Responses 1 or 2 ar considered as NOT disabled, 3 or 4 are considered as disabled, 
country$WGCARE <- ifelse(country$WGCARE==1 | country$WGCARE==2 ,0,country$WGCARE)
country$WGCOGN <- ifelse(country$WGCOGN==1 | country$WGCOGN==2 ,0,country$WGCOGN)
country$WGCOMM <- ifelse(country$WGCOMM==1 | country$WGCOMM==2 ,0,country$WGCOMM)
country$WGHEAR <- ifelse(country$WGHEAR==1 | country$WGHEAR==2 ,0,country$WGHEAR)
country$WGMOBIL <- ifelse(country$WGMOBIL==1 | country$WGMOBIL==2 ,0,country$WGMOBIL)
country$WGVISION <- ifelse(country$WGVISION==1 | country$WGVISION==2 ,0,country$WGVISION)

country$WGCARE <- ifelse(country$WGCARE==3 | country$WGCARE==4 ,1,country$WGCARE)
country$WGCOGN <- ifelse(country$WGCOGN==3 | country$WGCOGN==4 ,1,country$WGCOGN)
country$WGCOMM <- ifelse(country$WGCOMM==3 | country$WGCOMM==4 ,1,country$WGCOMM)
country$WGHEAR <- ifelse(country$WGHEAR==3 | country$WGHEAR==4 ,1,country$WGHEAR)
country$WGMOBIL <- ifelse(country$WGMOBIL==3 | country$WGMOBIL==4 ,1,country$WGMOBIL)
country$WGVISION <- ifelse(country$WGVISION==3 | country$WGVISION==4 ,1,country$WGVISION)

#summary(country)
gc()


# --------------------------------------------------------------------------------------------------------------------
# 3.13 Totals
country_population <- sum(country$PERWT)
country_0 <- aggregate(x=(country[9:14]==0)*country$PERWT, FUN=sum, na.rm=TRUE, na.action=NULL, by = list(country$COUNTRY,country$YEAR))
country_0$DIS_CAT <- 0
country_1 <- aggregate(x=(country[9:14]==1)*country$PERWT, FUN=sum, na.rm=TRUE, na.action=NULL, by = list(country$COUNTRY,country$YEAR))
country_1$DIS_CAT <- 1
country_NA <- aggregate(x=(is.na(country[9:14])*country$PERWT), FUN=sum, na.rm=TRUE, na.action=NULL, by = list(country$COUNTRY,country$YEAR))
country_NA$DIS_CAT <- NA
countryTotal <- rbind(country_0,country_1,country_NA)
colnames(countryTotal)[1] <- "COUNTRY"
colnames(countryTotal)[2] <- "YEAR"
countryTotal <- countryTotal[,c('COUNTRY','YEAR','DIS_CAT','WGCARE','WGCOGN','WGCOMM','WGHEAR','WGMOBIL','WGVISION')]
countryTotal[4:9] = round(countryTotal[4:9],0)
#countryTotal
rm(country_0,country_1, country_NA)

# --------------------------------------------------------------------------------------------------------------------
# 3.14 Age 
country_num <- data.frame()
country_den <- data.frame()
country_per <- data.frame()
# Aggregate in groups
range_age <- seq(9, max(country$AGE) + 10, by = 10)
country$AGE3 <- cut(country$AGE, breaks = range_age)
# numerator
country_num <- aggregate(x=(country[9:14]*country$PERWT), FUN=sum, na.rm=TRUE, na.action=NULL, by = list(country$COUNTRY,country$YEAR,country$AGE3))
country_num <- country_num[order(country_num$Group.1, country_num$Group.3), ]
#country_num
# denominator
country_den <- aggregate(x=country$PERWT, FUN=sum, na.rm=TRUE, na.action=NULL, by = list(country$COUNTRY,country$YEAR,country$AGE3))
country_den[,c(4:9)]<- country_den$x
country_den <- country_den[order(country_den$Group.1, country_den$Group.3), ]
#country_den
#country_den[,c(2:7)] <- 2612760
# percentage
country_per <- country_den
country_per[,c(4:9)] <- country_num[,c(4:9)]/country_den[,c(4:9)]*100
#Total Chart
country_Age_Chart <- cbind(country_num, country_per[,c(4:9)],country_den$x)
country_Age_Chart[4:9] = round(country_Age_Chart[4:9],0)
country_Age_Chart[16] = round(country_Age_Chart[16],0)
#country_Age_Chart
rm(range_age,country_num,country_den,country_per)
gc()


# --------------------------------------------------------------------------------------------------------------------
# 3.15 Education 
country_num <- data.frame()
country_den <- data.frame()
country_per <- data.frame()
# numerator
country_num <- aggregate(x=(country[9:14]*country$PERWT), FUN=sum, na.rm=TRUE, na.action=NULL, by = list(country$COUNTRY,country$YEAR,country$EDATTAIN))
#country_num
# Denominator
country_den <- aggregate(x=country$PERWT, FUN=sum, na.rm=TRUE, na.action=NULL, by = list(country$COUNTRY,country$YEAR,country$EDATTAIN))
country_den[,c(4:9)]<- country_den$x
#country_den
# Percentage
country_per <- country_den
country_per[,c(4:9)] <- country_num[,c(4:9)]/country_den[,c(4:9)]*100
#country_per
#Total Chart
country_Edu_Chart <- cbind(country_num,country_per[,c(4:9)],country_den$x)
country_Edu_Chart[4:9] = round(country_Edu_Chart[4:9],0)
country_Edu_Chart[16] = round(country_Edu_Chart[16],0)
#country_Edu_Chart
rm(country_num,country_den,country_per)
gc()

# --------------------------------------------------------------------------------------------------------------------
# 3.16 Employment
country_num <- data.frame()
country_den <- data.frame()
country_per <- data.frame()
# numerator
country_num <- aggregate(x=(country[9:14]*country$PERWT), FUN=sum, na.rm=TRUE, na.action=NULL, by = list(country$COUNTRY,country$YEAR,country$EMPSTAT))
# Denominator
country_den <- aggregate(x=country$PERWT, FUN=sum, na.rm=TRUE, na.action=NULL, by = list(country$COUNTRY,country$YEAR,country$EMPSTAT))
country_den[,c(4:9)]<- country_den$x
# Percentage
country_per <- country_den
country_per[,c(4:9)] <- country_num[,c(4:9)]/country_den[,c(4:9)]*100
#Total Chart
country_Emp_Chart <- cbind(country_num,country_per[,c(4:9)],country_den$x)
country_Emp_Chart[4:9] = round(country_Emp_Chart[4:9],0)
country_Emp_Chart[16] = round(country_Emp_Chart[16],0)
#country_Emp_Chart
rm(country_num,country_den,country_per)
gc()

# --------------------------------------------------------------------------------------------------------------------
# 3.17 Sex
country_num <- data.frame()
country_den <- data.frame()
country_per <- data.frame()
# numerator
country_num <- aggregate(x=(country[9:14]*country$PERWT), FUN=sum, na.rm=TRUE, na.action=NULL, by = list(country$COUNTRY,country$YEAR,country$SEX))
# Denominator
country_den <- data.frame()
country_den <- aggregate(x=country$PERWT, FUN=sum, na.rm=TRUE, na.action=NULL, by = list(country$COUNTRY,country$YEAR,country$SEX))
country_den[,c(4:9)]<- country_den$x
# Percentage
country_per <- country_den
country_per[,c(4:9)] <- country_num[,c(4:9)]/country_den[,c(4:9)]*100
#Total Chart
country_Sex_Chart <- cbind(country_num,country_per[,c(4:9)],country_den$x)
country_Sex_Chart[4:9] = round(country_Sex_Chart[4:9],0)
country_Sex_Chart[16] = round(country_Sex_Chart[16],0)
#country_Sex_Chart
rm(country_num,country_den,country_per)
gc()

# --------------------------------------------------------------------------------------------------------------------
# 3.18 Urban
country_num <- data.frame()
country_den <- data.frame()
country_per <- data.frame()
# numerator
country_num <- aggregate(x=(country[9:14]*country$PERWT), FUN=sum, na.rm=TRUE, na.action=NULL, by = list(country$COUNTRY,country$YEAR,country$URBAN))
#country_num
# Denominator
#country_den <- country_num
#country_den[,c(2:7)] <- 2612760 # Quantity of people with disability
country_den <- data.frame()
country_den <- aggregate(x=country$PERWT, FUN=sum, na.rm=TRUE, na.action=NULL, by = list(country$COUNTRY,country$YEAR,country$URBAN))
country_den[,c(4:9)]<- country_den$x
#country_den
# Percentage
country_per <- country_den
country_per[,c(4:9)] <- country_num[,c(4:9)]/country_den[,c(4:9)]*100
#Total Chart
country_Urb_Chart <- cbind(country_num,country_per[,c(4:9)],country_den$x)
country_Urb_Chart[4:9] = round(country_Urb_Chart[4:9],0)
country_Urb_Chart[16] = round(country_Urb_Chart[16],0)
#country_Urb_Chart
rm(country_num,country_den,country_per)
gc()



# --------------------------------------------------------------------------------------------------------------------
# 3.19 Charts to df
# Repeat for every country
df_Total <- rbind(df_Total,countryTotal)
rm(countryTotal)
df_Total
df_Age <-rbind(df_Age,country_Age_Chart)
rm(country_Age_Chart)
df_Age
df_Education <-rbind(df_Education,country_Edu_Chart)
rm(country_Edu_Chart)
df_Education
df_Employment <-rbind(df_Employment,country_Emp_Chart)
rm(country_Emp_Chart)
df_Employment
df_Sex <-rbind(df_Sex,country_Sex_Chart)
rm(country_Sex_Chart)
df_Sex
df_Urban <-rbind(df_Urban,country_Urb_Chart)
rm(country_Urb_Chart)
df_Urban


# --------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------
