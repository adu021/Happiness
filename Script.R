library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(gsubfn)
library(maps)

theme_happiness <- function (base_size = 11, base_family = "") {
  theme_bw() %+replace% 
    theme(
      legend.key = element_rect(colour="#000000", size=0.5),
      legend.box.margin = margin(8, 8, 8, 8),
      panel.grid.major  = element_line(color = "#FFFFFF"),
      panel.background = element_rect(fill = "#FFFFFF"),
      panel.border = element_rect(color = "#000000", fill = NA),
      axis.line = element_line(color = "#000000"),
      axis.ticks = element_line(color = "#000000"),
      axis.text = element_text(color = "#000000"),
      legend.background = element_rect(color = "#000000", fill = NA, size = 0.5)
      )
}



W2015 <- read.csv(file = "files/2015.csv", sep=",")
W2016 <- read.csv(file = "files/2016.csv", sep=",")
W2017 <- read.csv(file = "files/2017.csv", sep=",")
Alcohol <- read.csv(file = "files/HappinessAlcoholConsumption.csv")
Social2017 <- read.csv(file = "files/SocialAspects.csv")
Indicators <- read.csv(file = "files/Kag_happiness_indicators.csv")
E0817 <- read.csv(file = "files/Original_2017_full.csv", sep=",")

Gender <- read_excel("files/PersonalWellBeing.xls", sheet = "Life Satisfaction", col_names = FALSE, skip = 5, n_max = 2)
Gender <- Gender[,c(2:8)]
names(Gender) <- c("Sex", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017")
Gender <- Gender %>% gather(., Year, Life_Satisfaction, Y2012:Y2017)
Gender$Year <- as.Date(Gender$Year, format="Y%Y")
Gender$Year <- year(Gender$Year)
Gender$Life_Satisfaction <- as.numeric(Gender$Life_Satisfaction)
ggplot(Gender, aes(x=Year, y=Life_Satisfaction, color=factor(paste(Sex,'      ')))) + geom_line(size = 2) + ggtitle("Does gender influence happiness?") + theme_happiness() + theme(plot.title = element_text(hjust = 0.5)) + xlab("Year") + ylab("Life Satisfaction Score") + labs(color="Gender") + scale_color_brewer(palette = "OrRd")

Age <- read_excel("files/PersonalWellBeing.xls", sheet = "Life Satisfaction", col_names = FALSE, skip = 8, n_max = 16)
Age <- Age[,c(2:8)]
names(Age) <- c("Age_Group", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017")
Age <- Age %>% gather(., Year, Life_Satisfaction, Y2012:Y2017)
Age$Year <- Age$Year %>% as.Date(., format="Y%Y") %>% year()
Age$Life_Satisfaction <- as.numeric(Age$Life_Satisfaction)
ggplot(Age, aes(x=Year, y=Life_Satisfaction, color=factor(paste(Age_Group,'      ')))) + ggtitle("Does age influence happiness?") + geom_line() + theme_happiness() + theme(legend.position="top", plot.title = element_text(hjust = 0.5))+ xlab("Year") + ylab("Life Satisfaction Score") + labs(color="Age Group        ")

Indicators <- Indicators %>% unite(col=year, from=c("y94","y96","y98","y00","y02","y04","y06"), sep="-")
Indicators$year <- gsub("1-0-0-0-0-0-0", "1994", Indicators$year)
Indicators$year <- gsub("0-1-0-0-0-0-0", "1996", Indicators$year)
Indicators$year <- gsub("0-0-1-0-0-0-0", "1998", Indicators$year)
Indicators$year <- gsub("0-0-0-1-0-0-0", "2000", Indicators$year)
Indicators$year <- gsub("0-0-0-0-1-0-0", "2002", Indicators$year)
Indicators$year <- gsub("0-0-0-0-0-1-0", "2004", Indicators$year)
Indicators$year <- gsub("0-0-0-0-0-0-1", "2006", Indicators$year)
Work <- Indicators %>% filter(workstat != is.na(workstat)) %>% group_by(workstat) %>% mutate(Total=n()) %>% ungroup() %>% group_by(workstat, happy, Total) %>% summarize(Number = n()) %>% ungroup() %>% mutate(Percent = Number*100/Total)
ggplot(Work, aes(x=workstat, y=Percent, fill=factor(paste(happy,'      ')))) + geom_bar(stat="identity") + ggtitle("Happiness based on working situation") + xlab("Working situation") + ylab("Percentage") + labs(fill="Are you Happy?         ") + coord_flip() + theme_happiness() + theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) + scale_fill_brewer(palette = "OrRd")
Year9406 <- Indicators %>% group_by(year) %>% mutate(Total=n()) %>% ungroup() %>% group_by(year, happy, Total) %>% summarize(Number = n()) %>% ungroup() %>% mutate(Percent = Number*100/Total)
ggplot(Year9406, aes(x=year, y=Percent, fill=factor(paste(happy,'      ')))) + geom_bar(stat="identity") + ggtitle("Happiness from 1994 to 2006") + xlab("Year") + ylab("Percentage") + labs(fill="Are you Happy?         ") + theme_happiness() + theme(legend.position="right", plot.title = element_text(hjust = 0.5)) + scale_fill_brewer(palette = "OrRd")
Income <- Indicators %>% filter(income != is.na(income)) %>% group_by(income) %>% mutate(Total=n()) %>% ungroup() %>% group_by(income, happy, Total) %>% summarize(Number = n()) %>% ungroup() %>% mutate(Percent = Number*100/Total)
ggplot(Income, aes(x=income, y=Percent, fill=factor(paste(happy,'      ')))) + geom_bar(stat="identity") + ggtitle("Happiness based on income") + labs(fill="Are you Happy?         ") + coord_polar() + theme_happiness() + theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), panel.grid = element_blank(), axis.text.x = element_text(size = 8), axis.text.y = element_blank(), axis.title = element_blank(), axis.line = element_blank(), axis.ticks = element_blank(), panel.border = element_blank()) + scale_fill_brewer(palette = "OrRd") + ylim(-100,120)
ggplot(Indicators, aes(x=happy, y=prestige, fill=happy)) + geom_boxplot() + ggtitle("Happiness based on prestige") + xlab("Are you happy?") + ylab("Prestige") + labs(fill="Are you Happy?         ") + theme_happiness() + theme(legend.position="none", plot.title = element_text(hjust = 0.5)) + scale_fill_brewer(palette = "OrRd")

Year2017 <- left_join(W2017, Social2017, by = "Country")
world_map <- map_data("world")
colnames(Year2017)[1] <- "region"
world_map$region <- gsub("USA", "United States", world_map$region)
world_map$region <- gsub("UK", "United Kingdom", world_map$region)
Year2017 <- left_join(world_map, Whole2017, by = "region")
ggplot(Year2017, aes(long, lat, group = group)) + geom_polygon(aes(fill = Happiness.Score), color = "white") + scale_fill_viridis_c(option = "A", na.value = "black") + theme_happiness() + ggtitle("World Happiness - 2017")+ labs(fill="Happiness Score") + theme(plot.title = element_text(hjust = 0.5))
rm(world_map, Social2017, W2017)

E0817$year <- as.character(E0817$year)
E0817$year <- as.Date(E0817$year, format="%Y")
E0817 %>% filter(country == "Afghanistan" | country == "Greece" | country == "Syria" | country == "Togo" | country == "Norway" | country == "France" | country == "Russia" | country == "United States" | country == "China") %>% ggplot(., aes(x=year, y=Life.Ladder, color=country)) + geom_line(size = 1.5) + ggtitle("Happiness for 9 countries from 2008 to 2017") + xlab("Year") + ylab("Happiness Score") + labs(color="Country    ") + theme_happiness() + theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) + scale_color_brewer(palette = "OrRd")