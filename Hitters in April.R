library(readxl)
library(dplyr)
library(tidyr)

#importing data
excel_sheets("Hitters in April.xlsx")

Hitters_April <- as.data.frame(read_excel("Hitters in April.xlsx", sheet = "Sheet1"))
head(Hitters_April)

# every player must have at least 30 Plate Appearances
Hitters_April <- filter(Hitters_April, PA > 29)

# Subsetting to get the Brewers team data. 
Brewers_Hitters_April <- subset(Hitters_April, Team == "Brewers")
head(Brewers_Hitters_April)


min(Hitters_April_2017$PA)


Eric_Thames <- subset(Brewers_Hitters_April, Name == "Eric Thames")
head(Eric_Thames)
Travis_Shaw <- subset(Brewers_Hitters_April, Name == "Travis Shaw")
head(Travis_Shaw)

arrange(Hitters_April, desc(`wRC+`)) %>%
 head(100)


arrange(Brewers_Hitters_April, desc(WAR)) %>%
  head()
  

library(ggplot2)

# creating visualization to examine Thames' April in context to every Brewers players. 
r <- ggplot()
r + geom_point(data = Brewers_Hitters_April, aes(x = Year, y = `wRC+`), colour = "grey") + 
  geom_point(data = Eric_Thames, aes(x = Year, y = `wRC+`), colour = "#012143") + 
  geom_text(data = Eric_Thames, aes(x = Year, y = `wRC+`, label = Name), colour = "#012143", size = 3, fontface = "bold", vjust = -0.8) +
  geom_text(data = data.frame(), aes(x = 2011, y = 226, label = "Ryan Braun"), colour = "black", size = 3) + 
  geom_text(data = Travis_Shaw, aes(x = Year, y = `wRC+`, label = Name), colour = "black", size = 3, vjust = 1.5) +
  geom_text(data = data.frame(), aes(x = 2017, y = 170, label = "Ryan Braun"), colour = "black", size = 3, vjust = -0.8) + 
  geom_segment(data = data.frame(), aes(x = 1970, xend = 2020, y = 100, yend = 100), colour = "darkgrey", size = 0.4) + 
  geom_text(data = data.frame(), aes(x = 1971, y = 108, label = "Average"), colour = "darkgrey", size = 3) + 
  geom_segment(data = data.frame(), aes(x = 1971, xend = 1971, y = 101, yend = 105), colour = "darkgrey", size = 0.4) + 
  labs(caption = "Source: FanGraphs") + 
  ggtitle("Eric Thames is having the best hitting month of April in Brewers history", subtitle = "Brewers position players wRC+, 1974-2017. Min 30 PA.") + 
  Julien_theme()


# Contextualizing Thames' April compared to all of baseball
q <- ggplot()
q + geom_segment(data = Hitters_April, aes(x = 1, xend = 2, y = `wRC+`, yend = `wRC+`), colour = "gray83") + 
  geom_segment(data = Eric_Thames, aes(x = 1, xend = 2, y = `wRC+`, yend = `wRC+`), colour = "#012143") + 
  geom_text(data = Eric_Thames, aes(x = 1.5, y = `wRC+`, label = "Eric Thames (24th)"), color = "#012143", size = 3.5, fontface = "bold", vjust = 1.5) +
  geom_text(data = data.frame(), aes(x = 1.1, y = 333, label = "One player"), colour = "darkgrey", size = 3, vjust = -1.1) + 
  geom_curve(data = data.frame(), aes(x = 1.145, xend = 1.17, y = 342, yend = 335), colour = "darkgrey", curvature= -0.2, size = 0.3, arrow=arrow(length=unit(0.01, "npc"))) + 
  geom_text(data = data.frame(), aes(x = 1.7, y = 323, label = "Barry Bonds (2004)"), colour = "darkgrey", size = 3, vjust = 1.3) + 
  geom_segment(data = data.frame(), aes(x = 1, xend = 2, y = 100, yend = 100), colour = "darkgrey") + 
  geom_text(data = data.frame(), aes(x = 1.3, y = 100, label = "Average"), colour = "darkgrey", size = 3, vjust = 1.3) + 
  geom_text(data = data.frame(), aes(x = 1.7, y = 333, label = "Ken Singleton (1981)"), colour = "darkgrey", size = 3, vjust = -0.7) + 
  xlab("") +
  Julien_theme() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        panel.grid.major = element_line(colour = "grey93")) + 
  labs(caption = "Source: FanGraphs") + 
  ggtitle("Eric Thames April hitting, in historical context", subtitle = "Position players wRC+, 1974-2017. Min 30 PA.")
  
  
  
  
  
  
  


