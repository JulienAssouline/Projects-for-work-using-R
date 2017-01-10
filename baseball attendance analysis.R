#Scraping attendance data from baseball reference
library(XML)

#scraping one web page
url <- "http://www.baseball-reference.com/leagues/MLB/2016-misc.shtml"

data <- readHTMLTable(url, stringAsFactors = FALSE)
data[[1]]

head(data)

#scraping years 1970-2016
fetch_attendance <- function(year){
  url <- paste0("http://www.baseball-reference.com/leagues/MLB/", year, "-misc.shtml")
  data <- readHTMLTable(url, stringAsFactors = FALSE)
  data <- data[[1]]
  data$year <- year
  data
}

library(plyr) 

attendance <- ldply(1970:2016, fetch_attendance, .progress = "text")

head(attendance, n=60)

#Removing data columns

attendance$Managers <- NULL

attendance$`Est. Payroll` <- NULL

attendance$`#a-tA-S` <- NULL

attendance$`#A-S` <- NULL

head(attendance)

write.csv(attendance, file = "attendance.csv")

#cconverting factors to numeric

attendance$Attendance = gsub(",","", attendance$Attendance)

attendance$Attendance = as.numeric(attendance$Attendance)
is.numeric(attendance$Attendance)

attendance$year = as.numeric(attendance$year)
is.numeric(attendance$year)

attendance$`Attend/G` = gsub(",","", attendance$`Attend/G`)

attendance$`Attend/G` = as.numeric(attendance$`Attend/G`)
is.numeric(attendance$`Attend/G`)

mean(attendance$Attendance)

mean(attendance$`Attend/G`)
attendance

#finding the average attendance per year

average_attendance <- tapply(attendance$Attendance, attendance$year, mean)

#extracting the Brewers data

MIL_data <- subset(attendance, Tm == "MIL")

is.data.frame(MIL_data)

MIL_data

as.data.frame(average_attendance)

Year = c(1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979 ,
         1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991,
         1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,    
         2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)

avg_attendance = c(1197806, 1216392, 1123678, 1254539, 1251067, 1241246, 1304930, 
                   1488838, 562957, 1675015, 1654390, 1020938, 1714918, 1751551, 1720879, 1800938,
                   1827162, 2000443, 2038419, 2122042, 2108606, 2185145, 2148864, 2509212, 1786072, 1802473,
                   2146335, 2256025, 2353372, 2337979, 2378630, 2419370, 2264813, 2254335, 2434099,
                   2497176, 2534797, 2649491, 2620810, 2447686, 2435392, 2447522, 2495309, 2467568, 
                   2457987, 2457311, 2174964)

#converting to data frame

df <- data.frame(Year, avg_attendance)

names(df) <- c("year", "avg_attendance")

df

is.data.frame(df)

library(dplyr)

tail(attendance)

str(attendance)

library(ggplot2)

d <- ggplot(MIL_data, aes(x = year, y = Attendance))
d + geom_line()

p <- ggplot(data = attendance, aes(x = year, y = Attendance, group = Tm))
p + geom_line(aes(colour = Tm == "MIL")) + scale_color_manual(values = c("#CCCCCC","#000066"))

r <- ggplot()
r + geom_line(data = attendance, aes(x = year, y = Attendance, group = Tm), colour = "Grey") +
  geom_line(data = df, aes(x = Year, y = avg_attendance), colour = "red") 

k <- ggplot()
k + geom_line(data = attendance, aes(x = year, y = Attendance, group = Tm), colour = "Grey") +
  geom_line(data = df, aes(x = Year, y = avg_attendance), colour = "red") + 
  geom_line(data = MIL_data, aes(x = year, y = Attendance), colour = "blue") 
  
#Wins2 is from the "team wins data folder"

head(Wins2)
tail(attendance, n =30)


#renaming colnames
colnames(Wins2)[colnames(Wins2)=="Year"] <- "year"


#filtering data
Wins3 <- Wins2[Wins2$year > 2004,]

Wins4 <- Wins2[Wins2$year > 2006,]

attendance2 <- attendance[attendance$year > 2006,]

Wins4

attendance2

#renaming specific values in column Tm
attendance2$Tm[attendance2$Tm == "LAA"] <- "ANA"
attendance2$Tm[attendance2$Tm == "MIA"] <- "FLA"
attendance2$Tm[attendance2$Tm == "TBR"] <- "TBD"
attendance2$Tm[attendance2$Tm == "MON"] <- "WSN"

#merging data frames
wins2007 <- merge(Wins4, attendance2, by = c("Tm", "year"))

head(wins2007)

Wins3

#filtering years
attendance1 <- attendance[attendance$year > 2003,]

attendance1

#renaming values in columns
attendance1$Tm[attendance1$Tm == "LAA"] <- "ANA"
attendance1$Tm[attendance1$Tm == "MIA"] <- "FLA"
attendance1$Tm[attendance1$Tm == "TBR"] <- "TBD"
attendance1$Tm[attendance1$Tm == "MON"] <- "WSN"

#merging two data frames
complete <- merge(Wins3, attendance1, by = c("Tm", "year"))

s <- ggplot(complete, aes(x = wins, y = Attendance))
s + geom_point() + geom_smooth()
  
max(complete$Attendance)

library(reshape2)
library(plyr)
library(dplyr)

#calculating average teams wins and attendance from Teams
complete1 <- complete %>% group_by(Tm) %>% summarise(team_avg_wins=mean(wins), team_avg_attendance=mean(Attendance))

head(complete1, n=30)
tail(complete)

#making it a data frame
complete1 <- as.data.frame(complete1, stringsAsFactors = FALSE)

is.data.frame(complete1)

#creating plot by coloring a specific point
s <- ggplot(complete1, aes(x = team_avg_wins, y = team_avg_attendance))
s + geom_point(aes(colour = Tm == "MIL")) + 
  scale_color_manual(labels = c("League", "MIL"), values = c("grey", "blue"), guide = guide_legend(title = "Legend")) +
  ggtitle("Team Avg Attendance vs Team Avg Wins 2005-2016") + xlab("Wins") + ylab("Attendance")


wins2007

#calculating average teams wins and attendance from Teams
complete2 <- wins2007 %>% group_by(Tm) %>% summarise(team.avg.wins=mean(wins), team.avg.attendance=mean(Attendance))

#making it a data frame
complete2 <- as.data.frame(complete2, stringsAsFactors = FALSE)

complete2

v <- ggplot(data = complete2, aes(x = team.avg.wins, y = team.avg.attendance))
v + geom_point(data = complete2, aes(color = "grey")) + geom_point(data = complete2[16,], aes(color = "Blue")) + 
  scale_colour_manual(labels = c("MIL", "League"), values = c("blue", "grey"))  + 
  ggtitle("Team Avg Attendance vs Team Avg Wins 2007-2016") + ylab("Attendance") + xlab("Wins")

library(scales) 

#line plot with annotated square. Created legend, coloring specific lines, adding tick marks, changing y scale to commas 
z <- ggplot()
z + geom_line(data = attendance, aes(x = year, y = Attendance, group = Tm), colour = "Grey") +
  geom_line(data = df, aes(x = Year, y = avg_attendance, colour = "red")) + 
  geom_line(data = MIL_data, aes(x = year, y = Attendance, colour = "blue")) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(1970, 2016, 5)) + 
  annotate("rect", xmin = 2004, xmax = 2016, ymin = 2062382, ymax = 3071373, alpha = .3) +
  scale_colour_manual(labels = c("MIL", "League Avg"), values = c("blue", "red")) + 
  ggtitle("Brewers and League Average Attendance 1970-2016") + xlab("Year")

n <- ggplot()
n + geom_line(data = attendance, aes(x = year, y = Attendance, group = Tm), colour = "Grey") +
  geom_line(data = df, aes(x = Year, y = avg_attendance, colour = "red")) + 
  geom_line(data = MIL_data, aes(x = year, y = Attendance, colour = "blue")) + 
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(1970, 2016, 5)) + 
  scale_colour_manual(labels = c("MIL", "League Avg"), values = c("blue", "red")) + 
  ggtitle("Brewers and League Average Attendance 1970-2016") + xlab("Year")

  
#creating first model
model <- summary(lm(Attendance ~ wins, data = complete))
model

#analysing model
summary(lm(formula = Attendance ~ 0 + wins + Tm, data = complete))

#looking at correlation
cor(complete$wins, complete$Attendance)
cor.test(complete$wins, complete$Attendance)

#making residual a data frame
res <- residuals(model)
class(res)
res <- as.data.frame(res)
head(res)

#plotting residual
ggplot(res,aes(res)) + geom_histogram(fill="red")

head(complete)

MIL_data

df

#excluding lockout years
complete4 <- complete[! complete$year %in% c("1994")]

complete4 <- subset(complete, ! year %in% c(1994,1982,1972))

#new model controlling for teams
head(complete4)

model1 <- summary(lm(Attendance ~ wins, data = complete4))
model1

summary(lm(formula = Attendance ~ 0 + wins + Tm, data = complete4))
