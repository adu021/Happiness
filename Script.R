library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)

W2015 <- read.csv(file = "files/2015.csv", sep=",")
W2016 <- read.csv(file = "files/2016.csv", sep=",")
W2017 <- read.csv(file = "files/2017.csv", sep=",")
Alcohol <- read.csv(file = "files/HappinessAlcoholConsumption.csv")
Social2017 <- read.csv(file = "files/SocialAspects.csv")

Gender <- read_excel("files/PersonalWellBeing.xls", sheet = "Life Satisfaction", col_names = FALSE, skip = 5, n_max = 2)
Gender <- Gender[,c(2:8)]
names(Gender) <- c("Sex", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017")
Gender <- Gender %>% gather(., Year, Life_Satisfaction, Y2012:Y2017)
Gender$Year <- as.Date(Gender$Year, format="Y%Y")
Gender$Year <- year(Gender$Year)
Gender$Life_Satisfaction <- as.numeric(Gender$Life_Satisfaction)
ggplot(Gender, aes(x=Year, y=Life_Satisfaction, color=Sex)) + geom_line(size = 2)

Age <- read_excel("files/PersonalWellBeing.xls", sheet = "Life Satisfaction", col_names = FALSE, skip = 8, n_max = 16)
Age <- Age[,c(2:8)]
names(Age) <- c("Age_Group", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017")
Age <- Age %>% gather(., Year, Life_Satisfaction, Y2012:Y2017)
Age$Year <- Age$Year %>% as.Date(., format="Y%Y") %>% year()
Age$Life_Satisfaction <- as.numeric(Age$Life_Satisfaction)
ggplot(Age, aes(x=Year, y=Life_Satisfaction, color=Age_Group)) + geom_line() + theme(legend.position="top")
