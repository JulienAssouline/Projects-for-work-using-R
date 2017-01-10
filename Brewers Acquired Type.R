library(rvest)
library(xml2)
library(dplyr)
library(data.table)
library(tidyr)


#creating a vector of the years, and grabing the urls
years <- 1970:2016

brefurl <- paste0("http://www.baseball-reference.com/teams/MIL/", years, ".shtml")

head(brefurl)

#scraping Brewers data
fetch_Brewers_Aquired <- function(url){
  brefurl %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="div_players_value_batting"]/table[1]') %>%
    html_table()
}

Brewers_Aquired_1970_2016_list <- sapply(brefurl, fetch_Brewers_Aquired)
head(Brewers_Aquired_1970_2016_list)

#Adding the years to the list of data frames
Brewers_Aquired_1970_2016_list1 <- mapply(cbind, Brewers_Aquired_1970_2016_list, "Year"=1970:2016, SIMPLIFY = FALSE)

#converting list to data frame
Brewers_Aquired_1970_2016_list_2 <- rbindlist(Brewers_Aquired_1970_2016_list1)
head(Brewers_Aquired_1970_2016_list_2)

#renaming data frame
Brewers_Aquired_Type_1970_2016 <- Brewers_Aquired_1970_2016_list_2
head(Brewers_Aquired_Type_1970_2016)

str(Brewers_Aquired_Type_1970_2016)

# Summarising Acquired type by count,and naming new column type
Brewers_Aquired_Type_1970_2016_total <- Brewers_Aquired_Type_1970_2016 %>% group_by(Acquired) %>% summarise(Type = n()) %>% as.data.frame()
Brewers_Aquired_Type_1970_2016_total
Brewers_Aquired_Type_1970_2016_total <- Brewers_Aquired_Type_1970_2016_total[-c(1),]

#creating levels to then be able to change the names of certain variables
levels(Brewers_Aquired_Type_1970_2016_total$Acquired) <- c(levels(Brewers_Aquired_Type_1970_2016_total$Acquired), "Draft")

Brewers_Aquired_Type_1970_2016_total$Acquired[Brewers_Aquired_Type_1970_2016_total$Acquired == "Amateur Draft"] <- "Draft"

levels(Brewers_Aquired_Type_1970_2016_total$Acquired) <- c(levels(Brewers_Aquired_Type_1970_2016_total$Acquired), "Draft not signed")

Brewers_Aquired_Type_1970_2016_total$Acquired[Brewers_Aquired_Type_1970_2016_total$Acquired == "Amateur Draft--no sign"] <- "Draft not signed"

levels(Brewers_Aquired_Type_1970_2016_total$Acquired) <- c(levels(Brewers_Aquired_Type_1970_2016_total$Acquired), "Draft not signed")

# 46 years have passed from 1970-2016. I therefore calculated the yearly average of transaction type, by dividing the number of transaction types to the number of years passed.
Brewers_Aquired_Type_1970_2016_total$Average <- (Brewers_Aquired_Type_1970_2016_total$Type / 46) 
Brewers_Aquired_Type_1970_2016_total

#summarising the count of type by year and acquired 
Brewers_Aquired_Type_1970_2016_Yearly <- Brewers_Aquired_Type_1970_2016 %>% group_by(Year, Acquired) %>% summarise(Type = n()) %>% as.data.frame()

head(Brewers_Aquired_Type_1970_2016_Yearly)
Brewers_Aquired_Type_1970_2016_Yearly

#subsetting 2016 values
Brewers_Acquired_2016 <- subset(Brewers_Aquired_Type_1970_2016_Yearly, Year == 2016)

Brewers_Acquired_2016

Brewers_Aquired_Type_1970_2016_Yearly <- Brewers_Aquired_Type_1970_2016_Yearly[-c(276),]

library(ggplot2)

# Plotting bar chart to see how the Brewers were acquired in 2016
m <- ggplot(Brewers_Acquired_2016, aes( x = reorder(Acquired, Type), y = Type))
m + geom_bar(stat = "identity", fill = "dark red") + coord_flip() + ylab("") + xlab("") +
  ggtitle("How were the 2016 Brewers Constructed?", subtitle = "Number of Brewers Players on Roster and Their Transaction Type in 2016") +
  theme(plot.background=element_rect(fill = "grey93", colour = "grey93")) +
  theme(plot.title=element_text(size = 13, face = "bold")) +  
  theme(axis.text.x=element_text(size = 8)) +
  theme(axis.text.y=element_text(size = 8)) +
  theme(axis.title.x=element_text(size = 10)) + 
  theme(axis.title.y=element_text(size=10)) + 
  theme(panel.grid.major = element_line(colour = "grey87")) +
  theme(panel.grid.minor = element_line(colour = "grey93"))
  
# Plotting the average transaction types from 1970-2016
s <- ggplot(Brewers_Aquired_Type_1970_2016_total, aes(x = reorder(Acquired, Average), y = Average)) 
s + geom_bar(stat = "identity", fill = "DarkBlue") + coord_flip() + ylab("") + xlab("") + 
  ggtitle("Most Brewers Players Come from Trades" , subtitle = "Average Number of Brewers Players Acquired by Transaction Type 1970-2016") +
  theme(plot.background=element_rect(fill = "grey93", colour = "grey93")) +
  theme(plot.title=element_text(size = 13, face = "bold")) +  
  theme(axis.text.x=element_text(size = 8)) +
  theme(axis.text.y=element_text(size = 8)) +
  theme(axis.title.x=element_text(size = 10)) + 
  theme(axis.title.y=element_text(size=10)) + 
  theme(panel.grid.major = element_line(colour = "grey87")) +
  theme(panel.grid.minor = element_line(colour = "grey93")) 
  
# Plotting the trends of each transaction type
 l <- ggplot(Brewers_Aquired_Type_1970_2016_Yearly, aes(x = Year, y = Type, group = Acquired, colour = Acquired)) 
  l + geom_line(size = 1) + scale_colour_manual(values = c("red","Grey78","Grey78","Grey78","Grey78","Orange","Grey78","Grey78","Grey78","Blue","Grey78","Grey78","Purple")) +
  annotate("Text", label = "Trades", x = 1971, y = 28, colour = "Blue", fontface = "bold") +
  annotate("Text", label = "Amateur Draft",  x = 1986, y = 20, colour = "Red", fontface = "bold") +
  annotate("Text", label = "Free Agency", x = 2010, y = 20, colour = "Orange", fontface = "bold") +
  annotate("Text", label = "Waivers", x = 2016, y = 4, colour = "Purple", fontface = "bold") +
  theme(legend.position="none") + scale_y_continuous(breaks = c(0,10,20,27)) + 
  scale_x_continuous(breaks = c(1970,1980,1990,2000,2010,2016)) +     
  ggtitle("How the Brewers Are Creating Their Teams is Changing", subtitle = "Yearly Number of Players on a Brewers Roster by Their Acquired Transaction Type 1970-2016") +
  theme(plot.background=element_rect(fill = "grey93", colour = "grey93")) +
  theme(plot.title=element_text(size = 13, face = "bold")) +  
  theme(axis.text.x=element_text(size = 8)) +
  theme(axis.text.y=element_text(size = 8)) +
  theme(axis.title.x=element_text(size = 10)) + 
  theme(axis.title.y=element_text(size=10)) + 
  theme(panel.grid.major = element_line(colour = "grey87")) +
  theme(panel.grid.minor = element_line(colour = "grey93")) + xlab("") + ylab("Type") 
  
  # Plotting trends for each transaction type, using facets 
  z <- ggplot(Brewers_Aquired_Type_1970_2016_Yearly, aes(x = Year, y = Type, group = Acquired, colour = Acquired)) 
  z + geom_line(size = 1) + facet_wrap(~Acquired) + scale_colour_manual(values = c("red","Grey","black","pink","darkslategray","Orange","gold","green4","indianred2","Blue","lightsalmon3","peru","Purple")) +
    theme(legend.position="none") + scale_y_continuous(breaks = c(0,10,20)) + 
    scale_x_continuous(breaks = c(1970,1980,1990,2000,2010)) +     
    ggtitle("How the Brewers Are Creating Their Teams is Changing", subtitle = "Brewers Player Acquasition by Type 1970-2016") +
    theme(plot.background=element_rect(fill = "grey93", colour = "grey93")) +
    theme(plot.title=element_text(size = 13, face = "bold")) +  
    theme(axis.text.x=element_text(size = 8)) +
    theme(axis.text.y=element_text(size = 8)) +
    theme(axis.title.x=element_text(size = 10)) + 
    theme(axis.title.y=element_text(size=10)) + 
    theme(panel.grid.major = element_line(colour = "grey87")) +
    theme(panel.grid.minor = element_line(colour = "grey93")) 






