library(dplyr)
library(rvest)
library(xml2)
library(tidyr)

#scrape war deaths data

war_deaths_link <- read_html("http://www.war-memorial.net/wars_all.asp?land=&submit=Find&q=3") 

war_deaths <- war_deaths_link %>% 
  html_nodes(".tbl") %>%
  html_table() %>%
  as.data.frame(stringsAsFactors = FALSE)
  

head(war_deaths)
war_deaths <- war_deaths[-c(1),]

#change col names
colnames(war_deaths)[colnames(war_deaths)=="X1"] <- "Name of War"
colnames(war_deaths)[colnames(war_deaths)=="X2"] <- "Years"
colnames(war_deaths)[colnames(war_deaths)=="X3"] <- "Fatalities"
colnames(war_deaths)[colnames(war_deaths)=="X4"] <- "Type"
colnames(war_deaths)[colnames(war_deaths)=="X5"] <- "Mem."
colnames(war_deaths)[colnames(war_deaths)=="X6"] <- "Note"

#seperate war years started and war years ended
war_deaths1 <- separate(data = war_deaths, col = Years, c("Year1", "Year_end"), sep = "-")

#make values numeric
head(war_deaths1)
str(war_deaths1)
war_deaths1$Year1 <- as.numeric(as.character(war_deaths1$Year1))
war_deaths1$Year_end <- as.numeric(as.character(war_deaths1$Year_end))
war_deaths1$Fatalities <- as.numeric(gsub(",","", war_deaths1$Fatalities))

#create new column for war duration
war_deaths1$War_Duration <- war_deaths1$Year_end - war_deaths1$Year1
head(war_deaths1)

colnames(war_deaths1)[colnames(war_deaths1)=="Name of War"] <- "Name_of_War"

war_deaths1

library(ggplot2)
library(scales)
install.packages("gapminder")
library(gapminder)
library(ggrepel)

#plot data
v <- ggplot(war_deaths1)
v + geom_point(aes(Year_end, War_Duration, size = Fatalities), colour = "red") + 
  geom_segment(aes(x = Year1, xend = Year_end, y = 0, yend = War_Duration), size = 0.09, colour = "grey60") + 
  scale_size(range = c(0,10), labels = comma) +
  theme(plot.background=element_rect(fill = "grey93", colour = "grey93")) +
  theme(plot.title=element_text(size = 15, face = "bold")) +  
  theme(axis.text.x=element_text(size = 8)) +
  theme(axis.text.y=element_text(size = 8)) +
  theme(axis.title.x=element_text(size = 10)) + 
  theme(axis.title.y=element_text(size=10)) + 
  theme(panel.grid.major = element_line(colour = "grey87")) +
  theme(panel.grid.minor = element_line(colour = "grey93")) +
  annotate("Text", label = "World War II", x = 1945, y = 11, size = 3, fontface = "bold") +
  annotate("Text", label = "Israel vs Palestine", x = 1999, y = 65, size = 3, fontface = "bold") +
  annotate("Text", label = "World War I", x = 1918, y = 8, size = 3, fontface = "bold") +
  theme(legend.key = element_rect(fill = "grey93", colour = "grey93")) +
  theme(legend.background = element_rect(fill = "grey93")) +   
  theme(legend.position = "top") +
  ggtitle("Wars we Remember", subtitle = "War Fatalities and Duration 1899-2015") + 
  ylab("War Duration") + xlab("") + labs(caption = "Data: The Polynational War Memorial", colour = "gray") + 
  theme(plot.caption = element_text(colour = "grey60"))
