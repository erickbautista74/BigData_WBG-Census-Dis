# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

#Packages
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
install.packages('ipumsr')
library('ipumsr')
install.packages("dplyr")
library(dplyr)

# 1. Loading Data
#setwd("~/Users/erickbautista/Downloads/WBG ")
ddi <- read_ipums_ddi("/Users/erickbautista/Downloads/WBG/ipumsi_00014.xml")
rawdata <- read_ipums_micro(ddi)
nrow(rawdata)
head(rawdata)
as_factor(rawdata)

# 2.0 Clean Data, erase columns are not important, keep: COUNTRY, YEAR, URBAN, PERWT, AGE, AGE2, SEX, EDATTAIN, EMPSTAT, WGCARE, WGCOGN, WGCOMM, WGHEAR, WGMOBIL, WGVISION
rawdata <- rawdata[,c("COUNTRY","YEAR", "URBAN", "PERWT","AGE","AGE2","SEX","EDATTAIN","EMPSTAT","WGCARE","WGCOGN","WGCOMM","WGHEAR","WGMOBIL","WGVISION")]

# 2.1 Clean Data,Split by country
country <- subset(rawdata, (rawdata$COUNTRY==76 & rawdata$YEAR==2000))

# 2.2 Clean Data AGE, erase rows where Age < 10, keep AGE > 9  (AGE < 10 = missing data)
country <- subset(country, (country$AGE > 9))

# 2.3 Clean Data AGE, account age>100 
sum(country$AGE>100)

# 2.4 Clean Data W_CAT, Values W_cat, 1|2 <- 0,  3|4 <- 1, other Values <- NA, erase when NA in 6 categories
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

country$WGCARE <- ifelse(country$WGCARE!=0 & country$WGCARE!=1 & country$WGCARE!=2 ,NA,country$WGCARE)
country$WGCOGN <- ifelse(country$WGCOGN!=0 & country$WGCOGN!=1 & country$WGCOGN!=2 ,NA,country$WGCOGN)
country$WGCOMM <- ifelse(country$WGCOMM!=0 & country$WGCOMM!=1 & country$WGCOMM!=2 ,NA,country$WGCOMM)
country$WGHEAR <- ifelse(country$WGHEAR!=0 & country$WGHEAR!=1 & country$WGHEAR!=2 ,NA,country$WGHEAR)
country$WGMOBIL <- ifelse(country$WGMOBIL!=0 & country$WGMOBIL!=1 & country$WGMOBIL!=2 ,NA,country$WGMOBIL)
country$WGVISION <- ifelse(country$WGVISION!=0 & country$WGVISION!=1 & country$WGVISION!=2 ,NA,country$WGVISION)

# rows with NOT(NA in the 6 Categories) (MISSINGS)
country <- subset(country,(!is.na(country$WGCARE)|!is.na(country$WGCOGN)|!is.na(country$WGCOMM)|!is.na(country$WGHEAR)|!is.na(country$WGMOBIL)|!is.na(country$WGVISION) ))

# 2.4 Clean Data, If Q of URBAN == 9 & < 3% then MISSINGS
as_factor(aggregate(x=country$URBAN, FUN=length, by = list(country$URBAN)))
country <- subset(country,(country$URBAN==1 |country$URBAN==2))

# 2.5 Clean Data, If Q of SEX == 9 & < 3% then MISSINGS
as_factor(aggregate(x=country$SEX, FUN=length, by = list(country$SEX)))
country <- subset(country,(country$SEX==1 |country$SEX==2))

# 2.5 Clean Data, If Q of EDATTAIN == 9 & < 3% then MISSINGS
as_factor(aggregate(x=country$EDATTAIN, FUN=length, by = list(country$EDATTAIN)))
country <- subset(country,(country$EDATTAIN==1 | country$EDATTAIN==2 | country$EDATTAIN==3 | country$EDATTAIN==4))

# 2.6 Clean Data, If Q of EMPSTAT == 9 & < 3% then MISSINGS
as_factor(aggregate(x=country$EMPSTAT, FUN=length, by = list(country$EMPSTAT)))
country <- subset(country,(country$EMPSTAT==1 | country$EMPSTAT==2 | country$EMPSTAT==3))

# 2.7 Prepare Data, multiplicate Weight
country[10:15] <- country[10:15]*country$PERWT

# 3. URBAN
# Num
country_num <- aggregate(x=country[10:15], FUN=sum, na.rm=TRUE, na.action=NULL, by = list(country$COUNTRY,country$YEAR, country$URBAN))
country_num <- country_num[order(country_num$Group.1, country_num$Group.2, country_num$Group.3), ]
# Den
country_den <- country_num
country_den <- aggregate(x=country$PERWT, FUN=sum, by = list(country$COUNTRY,country$YEAR, country$URBAN))
country_den[5:9]<- country_den$x
# Percentage
country_per <- country_den
country_per[4:9] <- country_num[4:9]/country_den[4:9]*100
# Assign Country
bra_2000_URB <- as_factor(country_per)

# 7. AGE
# Num 
country$AGE3 <- cut(x=country$AGE,breaks=9)
country_num <- aggregate(x=country[10:15], FUN=sum, na.rm=TRUE, na.action=NULL, by = list(country$COUNTRY,country$YEAR, country$AGE3))
country_num <- country_num[order(country_num$Group.1, country_num$Group.2, country_num$Group.3), ]
# Den
country_den <- country_num
country_den <- aggregate(x=country$PERWT, FUN=sum, by = list(country$COUNTRY,country$YEAR, country$AGE3))
country_den[5:9]<- country_den$x
# Percentage
country_per <- country_den
country_per[4:9] <- country_num[4:9]/country_den[4:9]*100
# Assign Country
bra_2000_AGE <- as_factor(country_per)

# 8. SEX
# Num
country_num <- aggregate(x=country[10:15], FUN=sum, na.rm=TRUE, na.action=NULL, by = list(country$COUNTRY,country$YEAR, country$SEX))
country_num <- country_num[order(country_num$Group.1, country_num$Group.2, country_num$Group.3), ]
# Den
country_den <- country_num
country_den <- aggregate(x=country$PERWT, FUN=sum, by = list(country$COUNTRY,country$YEAR, country$SEX))
country_den[5:9]<- country_den$x
# Percentage
country_per <- country_den
country_per[4:9] <- country_num[4:9]/country_den[4:9]*100
# Assign Country
bra_2000_SEX <- as_factor(country_per)

# 9. Education
# Num
country_num <- aggregate(x=country[10:15], FUN=sum, na.rm=TRUE, na.action=NULL, by = list(country$COUNTRY,country$YEAR, country$EDATTAIN))
country_num <- country_num[order(country_num$Group.1, country_num$Group.2, country_num$Group.3), ]
# Den
country_den <- country_num
country_den <- aggregate(x=country$PERWT, FUN=sum, by = list(country$COUNTRY,country$YEAR, country$EDATTAIN))
country_den[5:9]<- country_den$x
# Percentage
country_per <- country_den
country_per[4:9] <- country_num[4:9]/country_den[4:9]*100
# Assign Country
bra_2000_EDU <- as_factor(country_per)

# 10. Employment
# Num
country_num <- aggregate(x=country[10:15], FUN=sum, na.rm=TRUE, na.action=NULL, by = list(country$COUNTRY,country$YEAR, country$EMPSTAT))
country_num <- country_num[order(country_num$Group.1, country_num$Group.2, country_num$Group.3), ]
# Den
country_den <- country_num
country_den <- aggregate(x=country$PERWT, FUN=sum, by = list(country$COUNTRY,country$YEAR, country$EMPSTAT))
country_den[5:9]<- country_den$x
# Percentage
country_per <- country_den
country_per[4:9] <- country_num[4:9]/country_den[4:9]*100
# Assign Country
bra_2000_EMP <- as_factor(country_per)

# 11. Delete variables
rm(country_num)
rm(country_den)
rm(country_per)
