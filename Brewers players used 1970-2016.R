
library(rvest)
library(xml2)
library(dplyr)
library(data.table)

### Position Players

# Scraping a single web page
url4 <- "http://www.baseball-reference.com/teams/MIL/2016.shtml"

Brewers2016 <- url4 %>% 
  read_html() %>% 
  html_nodes(xpath = '//*[@id="div_team_batting"]/table[1]') %>% 
  html_table()   

Brewers2016 <- as.data.frame(Brewers2016)

# Creating a vector for years, and grabbing all the urls 
years <- 1970:2016

urls <- paste0("http://www.baseball-reference.com/teams/MIL/", years, ".shtml")

# Scraping all the data
get_table <- function(url) {
  url %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="div_team_batting"]/table[1]') %>% 
    html_table()
}

results <- sapply(urls, get_table)

head(results)
results[1]
results[47]

is.list(results)

# Adding column for the years to the list of data frames
Brewers_players <- mapply(cbind, results, "Year"=1970:2016, SIMPLIFY = FALSE)
head(Brewers_players)

# Converting the list to a data frame
Brewers_players <- rbindlist(Brewers_players)

head(Brewers_players)

# Getting rid of the pitchers
Brewers_players <- Brewers_players[! Brewers_players$Pos %in% c("P"),]

Brewers_players <- Brewers_players[Brewers_players$Rk!="Rk",]

# Getting the count of players on every team per year
Brewers_players_average <- Brewers_players %>% group_by(Year) %>% summarise(Count = n()) %>% as.data.frame()
Brewers_players_average
str(Brewers_players)

# Converting column data type
Brewers_players$Year <- as.numeric(Brewers_players$Year)
Brewers_players$Players <- as.numeric(Brewers_players$Players)
Brewers_players$Name <- as.factor(Brewers_players$Name)

mean(Brewers_players_average$Count)


library(ggplot2)
 
# Plotting trend of position players used per year
p <- ggplot()
p + geom_point(data = Brewers_players_average, aes(x = Year, y = Count), colour = "DarkBlue") + 
  geom_line(data = Brewers_players_average, aes(x = Year, y = Count), colour = "DarkBlue", size = 1) +
  ggtitle("Yearly Brewers Position Players Used 1970-2016") +  
  theme(plot.background=element_rect(fill = "grey93", colour = "grey93")) +
  theme(plot.title=element_text(size = 12, face = "bold", hjust = .5)) +  
  theme(axis.text.x=element_text(size = 8)) +
  theme(axis.text.y=element_text(size = 8)) +
  theme(axis.title.x=element_text(size = 10)) + 
  theme(axis.title.y=element_text(size=10)) +
  theme(panel.grid.major.y=element_blank()) + ylab("") + xlab("") 



### Pitchers

# grabbing all the urls
years <- 1970:2016

urls <- paste0("http://www.baseball-reference.com/teams/MIL/", years, ".shtml")

# Scraping the data
get_table1 <- function(url) {
  url %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="div_team_pitching"]/table[1]') %>% 
    html_table()
}

brewers_pitchers <- sapply(urls, get_table1)
head(brewers_pitchers)

# Adding all of the years
brewers_pitchers <- mapply(cbind, brewers_pitchers, "Year"=1970:2016, SIMPLIFY = FALSE)

# Converting to data frame
brewers_pitchers1 <- rbindlist(brewers_pitchers1)

brewers_pitchers1 <- brewers_pitchers1[brewers_pitchers1$Rk!="Rk",]

# Getting the Count of every pitcher er year
Brewers_Pitchers_average <- Brewers.Pitchers %>% group_by(Year) %>% summarise(Count = n()) %>% as.data.frame()
head(Brewers_Pitchers_average)
Brewers_Pitchers_average

mean(Brewers_Pitchers_average$Count) 

str(Brewers_Pitchers_average)

# Plotting count of every pitcher used per year
d <- ggplot()
d +   geom_point(data = Brewers_Pitchers_average, aes(x = Year, y = Count), colour = "darkred") + 
  geom_line(data = Brewers_Pitchers_average, aes(x = Year, y = Count), colour = "darkred", size = 1) + 
  ggtitle("Yearly Brewers Pitchers Used 1970-2016") +  
  theme(plot.background=element_rect(fill = "grey93", colour = "grey93")) +
  theme(plot.title=element_text(size = 12, face = "bold", hjust = .5)) +  
  theme(axis.text.x=element_text(size = 8)) +
  theme(axis.text.y=element_text(size = 8)) +
  theme(axis.title.x=element_text(size = 10)) + 
  theme(axis.title.y=element_text(size=10)) +
  theme(panel.grid.major.y=element_blank()) + ylab("") + xlab("") + 
  scale_y_continuous(breaks = c(16,20,24,27)) 


Brewers_Pitchers_average
Brewers_players_average

# Merging both databases
Brewers_Overall <- left_join(Brewers_players_average, Brewers_Pitchers_average, by = c("Year"))
head(Brewers_Overall)

# Adding both pitchers and position players counts together into one column
Brewers_Overall_1 <- Brewers_Overall %>% mutate(Count = Count.x + Count.y)
head(Brewers_Overall_1)

# Plotting trend of every player ever used on a Brewers roster per year
r <- ggplot() 
r +   geom_point(data = Brewers_Overall_1, aes(x = Year, y = Count), colour = "Purple") + 
  geom_line(data = Brewers_Overall_1, aes(x = Year, y = Count), colour = "Purple", size = 1) + 
  ggtitle("Yearly Brewers Players Used 1970-2016") +  
  theme(plot.background=element_rect(fill = "grey93", colour = "grey93")) +
  theme(plot.title=element_text(size = 12, face = "bold", hjust = .5)) +  
  theme(axis.text.x=element_text(size = 8)) +
  theme(axis.text.y=element_text(size = 8)) +
  theme(axis.title.x=element_text(size = 10)) + 
  theme(axis.title.y=element_text(size=10)) +
  theme(panel.grid.major.y=element_blank()) + ylab("") + xlab("")

mean(Brewers_Overall_1$Count)



# The Wins2 data frame comes from the Wins in baseball 1970-2016 file. 

head(Wins2)

head(Brewers_Overall_1)
Brewers_Overall_1

colnames(Wins2)[colnames(Wins2) == "year"] <- "Year"

# Merging Brewers overall usage data and wins data
Brewers_Overall_and_Wins <- left_join(Brewers_Overall_1, Wins2, by = c("Year"))
head(Brewers_Overall_and_Wins)
Brewers_Overall_and_Wins

# Subsetting Brewers
Brewers_Overall_and_Wins <- Brewers_Overall_and_Wins[Brewers_Overall_and_Wins$Tm == "MIL",]
head(Brewers_Overall_and_Wins)

Label <- c(2016)
library(ggrepel)

# plotting relationship between winning and count of players used per year
l <- ggplot(Brewers_Overall_and_Wins, aes(x = Count, y = wins)) 
l + geom_point(aes(colour = Year == 2016)) + 
  scale_color_manual(labels = c("Other", 2016), values = c("grey", "blue"), guide = guide_legend(title = "Years")) +
  ggtitle("Relationship Between Brewers Wins and Players Used in a Season 1970-2016") +  
  theme(plot.background=element_rect(fill = "grey93", colour = "grey93")) +
  theme(plot.title=element_text(size = 10, face = "bold", hjust = .5)) +  
  theme(axis.text.x=element_text(size = 8)) +
  theme(axis.text.y=element_text(size = 8)) +
  theme(axis.title.x=element_text(size = 10)) + 
  theme(axis.title.y=element_text(size=10)) +
  theme(panel.grid.major.y=element_blank()) + ylab("Wins") + xlab("Players Used in a Season") +
  geom_smooth(colour = "red", seq = NA) + Julien_theme()
  
# Looking at correlation
cor(Brewers_Overall_and_Wins, use = "wins", method = "Count")

Count1 = Brewers_Overall_and_Wins$Count
wins12 = Brewers_Overall_and_Wins$wins
cor(Count1, wins12)
