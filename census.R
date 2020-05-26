setwd("~/Computer Science/Census Age")

#install necessary packages
#install.packages("tidyverse")
#install.packages("readxl")

#loads packages
library(tidyverse)
library(readxl)


#create path to excel file
url <- "Census_Age.xlsx"
excel_sheets(url)

#loads excel sheet in. Skips first line (skip = 1)
dfread <- read_excel(url, sheet="ACS_12_5YR_S0101_with_ann", skip=1)

colnames(dfread)
summary(dfread)

#select columns 
df2012 <- select(dfread, "Id2",
                 "Total; Estimate; Total population",
                 "Total; Estimate; SELECTED AGE CATEGORIES - 65 years and over",
                 "Total; Estimate; SUMMARY INDICATORS - Median age (years)")

#rename column names
colnames(df2012) <- c("id2","totalPop12","percage65_12","ageMed_12")

#convert to numerics
df2012$totalPop12 <- as.numeric(df2012$totalPop12)
df2012$percage65_12 <- as.numeric(df2012$percage65_12)
df2012$ageMed_12 <- as.numeric(df2012$ageMed_12)

summary(df2012)



#do the same for 2017 data


dfread2 <- read_excel(url, sheet = "ACS_17_5YR_S0101_with_ann", skip=1)

colnames(dfread2)

df2017 <- select(dfread2, "Id2",
                 "Total; Estimate; Total population",
                 "Total; Estimate; SELECTED AGE CATEGORIES - 65 years and over",
                 "Total; Estimate; SUMMARY INDICATORS - Median age (years)")

#rename column names
colnames(df2017) <- c("id2","totalPop17","age65_17","ageMed_17")

#convert to numerics
df2017$totalPop17 <- as.numeric(df2017$totalPop17)
df2017$age65_17 <- as.numeric(df2017$age65_17)
df2017$ageMed_17 <- as.numeric(df2017$ageMed_17)

summary(df2017)

#merge two sheets together by id2
dfage_1217 <- merge(x = df2017, y = df2012, by = "id2", all.x = TRUE)

#create new column for esitmated total of 65 and over for 2012
dfage_1217$age65_12 <- round(dfage_1217$totalPop12 * (dfage_1217$percage65_12 *0.01), 0)


dfage_1217$totalChange <- dfage_1217$totalPop17 - dfage_1217$totalPop12
dfage_1217$age65Change <- dfage_1217$age65_17 - dfage_1217$age65_12

#calculate percent change of 65 and older. (x/y)/y
dfage_1217$percentChange <- round(((dfage_1217$age65_17-dfage_1217$age65_12)/dfage_1217$age65_12)*100, 2)


#create index based on z score.
# z score = (value - mean of all values)/ standard deviation of all values

# zscore for change in age
dfage_1217$age65ChangeZ <- round((dfage_1217$age65Change - mean(dfage_1217$age65Change, na.rm = TRUE))
                                 /sd(dfage_1217$age65Change, na.rm = TRUE), 2)
#z score for percent change
dfage_1217$percentChangeZ <- round((dfage_1217$percentChange - mean(dfage_1217$percentChange, na.rm = TRUE))
                                 /sd(dfage_1217$percentChange, na.rm = TRUE), 2)

dfage_1217$age65_index <- (dfage_1217$age65ChangeZ * 0.4) + (dfage_1217$percentChangeZ * 0.6)

write.csv(dfage_1217, "dfage_1217.csv", na = "")
