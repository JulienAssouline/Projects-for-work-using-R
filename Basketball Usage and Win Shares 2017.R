library(xml2)
library(rvest)
library(dplyr)
library(tidyr)

# Scraping 2016-2017 basketball data 
Baseketball_2017_url <- "http://www.basketball-reference.com/leagues/NBA_2017_advanced.html"

Baseketball_2017 <- Baseketball_2017_url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="advanced_stats"]') %>%
  html_table() %>%
  as.data.frame()

str(Baseketball_2017)
head(Baseketball_2017)
Baseketball_2017
# Converting columns to numeric
Baseketball_2017 <- transform(Baseketball_2017, TS. = as.numeric(TS.),
                              USG. = as.numeric(USG.), 
                              WS = as.numeric(WS))

# Subsetting Westbrook, Embiid, Curry, Durant, Davies, Gobert
westbrook_2017 <- subset(Baseketball_2017, Player == "Russell Westbrook")
Embiid_2017 <- subset(Baseketball_2017, Player == "Joel Embiid")
Curry_2017 <- subset(Baseketball_2017, Player == "Stephen Curry")
Durant_2017 <- subset(Baseketball_2017, Player == "Kevin Durant")
Davis_2017 <- subset(Baseketball_2017, Player == "Anthony Davis")
Gobert_2017 <- subset(Baseketball_2017, Player == "Rudy Gobert")

head(Davis_2017)

# Looking at usage and true shooting percentage
v <- ggplot()
v + geom_point(data = Baseketball_2017, aes(x = TS., y = USG.), colour = "grey") +
  geom_point(data = westbrook_2017, aes(x = TS., y = USG.), colour = "blue") + 
  geom_point(data = Embiid_2017, aes(x = TS., y = USG.), colour = "red") + 
  Julien_theme()

# plotting relationship between usage and win shares. Pointing out outliers. Which players are using too much possession?
d <- ggplot()
d + geom_point(data = Baseketball_2017, aes(x = USG., y = WS), colour = "grey") +
  geom_point(data = westbrook_2017, aes(x = USG., y = WS), colour = "blue") + 
  geom_point(data = Embiid_2017, aes(x = USG., y = WS), colour = "red") + 
  geom_point(data = Curry_2017, aes(x = USG., y = WS), colour = "goldenrod1") + 
  geom_point(data = Gobert_2017, aes(x = USG., y = WS), colour = "darkblue") +
  geom_point(data = Durant_2017, aes(x = USG., y = WS), colour = "goldenrod1") + 
  geom_point(data = Davis_2017, aes(x = USG., y = WS), colour = "darkred") +
  annotate("Text", label = "Westbrook", x = 40, y = 3.8, colour = "blue", size = 3.5) + 
  annotate("Text", label = "EMBIID", x = 37, y = .8, colour = "red", size = 3.5, fontface = "bold") + 
  annotate("Text", label = "GOBERT", x = 14.8, y = 4.1, colour = "darkblue", size = 3.5, fontface = "bold") + 
  annotate("Text", label = "Curry", x = 29, y = 4.2, colour = "goldenrod1", size = 3.5) + 
  annotate("Text", label = "Durant", x = 28.8, y = 5, colour = "goldenrod1", size = 3.5) + 
  annotate("Text", label = "Davis", x = 34.1, y = 4, colour = "darkred", size = 3.5) + 
  ylab("Win Shares") + xlab("Usage Percentage") + 
  ggtitle("Joel Embiid shouldn't be using this much possession", subtitle = "Win shares and usage percentage 2016-2017") + 
  Julien_theme()

