library(readxl)
library(dplyr)
library(tidyr)

### For this project, I wanted to analyze the starting pitchers who pitched in relief. 
# The data was easy to get, because I already had a built in database of Baseball Prospectus' data on pitchers datin back to 1967
# So, thats the data I used here. For anyone who would like to obtain the data, it can be collected at the Baseball Prospectus web site. 

head(BP_Pitcher_1967_2016)
# Becaue we I wanted to look at starting pitcher, first, I needed to define a starting pitcher. 
# Which I defined as a pitcher who made more than 50% of his plate appearances as a starter (GS/G > 50)
# I also set a games started limit of 3. 

BP_Pitcher_1967_2016$Difference <- (BP_Pitcher_1967_2016$GS / BP_Pitcher_1967_2016$G) * 100
head(BP_Pitcher_1967_2016)

Starters_1967_2016 <- BP_Pitcher_1967_2016[BP_Pitcher_1967_2016$Difference > 50,]
relievers_1967_2016 <- BP_Pitcher_1967_2016[BP_Pitcher_1967_2016$Difference < 50,]

Starters_1967_2016 <- Starters_1967_2016[Starters_1967_2016$GS > 3,]
head(Starters_1967_2016)
head(relievers_1967_2016)

# Then I wanted to find out the average number of innings pitched out of the pen by a starter. 

Average_starters_IP_Relief <- Starters_1967_2016 %>% 
  group_by(YEAR) %>% 
  summarise(IP_Relief_Avg = mean(IP.Relief), IP_Relief_Med = median(IP.Relief), WAR_Avg = mean(PWARP), WAR_Med = mean(PWARP)) %>% 
  as.data.frame()
head(Average_starters_IP_Relief)

Starters_1967_2016_IP_Relief <- Starters_1967_2016[Starters_1967_2016$IP.Relief > 0,]
head(Starters_1967_2016_IP_Relief)
tail(Starters_1967_2016_IP_Relief)

# Next I wanted to find the proportion of pitchers who pitched at least one inning in relief, by year. 
# So first I needed to get the count of every starter who pitched at least one inning in the pen by year. 

Starters_Pitched_relief_count <- Starters_1967_2016 %>% 
  group_by(YEAR) %>% 
  filter(IP.Relief > 0) %>% 
  summarise(Count = n(), Sum = sum(IP.Relief), Mean = mean(IP.Relief)) %>% 
  as.data.frame()

# Then I needed to get the count of every starter by year. 

Starters_count <- Starters_1967_2016 %>% 
  group_by(YEAR) %>% 
  summarise(Count = n(), Sum = sum(IP.Relief), Mean = mean(IP.Relief)) %>% 
  as.data.frame()

# And finally I can calculate the percentage.

Percentage_of_Sarters_IP_Relief <-  (Starters_Pitched_relief_count / Starters_count) * 100 

# The method I used got rid of all of my year, so I added the years, and got rid of the unneeded columns

Percentage_of_Sarters_IP_Relief$YEAR <- 1967:2016

Percentage_of_Sarters_IP_Relief$Sum <- NULL
Percentage_of_Sarters_IP_Relief$Mean <- NULL

Percentage_of_Sarters_IP_Relief$YEAR <- as.numeric(Percentage_of_Sarters_IP_Relief$YEAR)
Average_starters_IP_Relief

library(ggplot2)

# Now I can easily plot the trend of the percentage of starters who threw at least one inning in relief. 

p <- ggplot(Percentage_of_Sarters_IP_Relief, aes(x = YEAR, y = Count))
p + geom_line(colour = "Purple", size = 1) + Julien_theme() +
  ggtitle("Fewer starters are pitching innings in relief", subtitle = "Percentage of starters who threw at least one inning in relief, 1967-2016") + 
  labs(caption = "Data: Baseball Prospectus") + ylab("") + 
  scale_y_continuous(limits = c(0, 100), labels = c(0,25,50,75,"100%"))

# Then I plotted the average number of innings pitched in relief

q <- ggplot(Average_starters_IP_Relief, aes(x = YEAR, y = IP_Relief_Avg))
q + geom_line(colour = "blue", size = 1) + Julien_theme() + 
  ggtitle("On Average, starters are pitching fewer innings in relief", "Average innings pitched by starters in relief, 1967-2016") + 
  scale_y_continuous(limits = c(0, 8.5)) + ylab("") + labs(caption = "Data: Baseball Prospectus")

# After examining the trend, I wanted to examine the relationship between the innings pitched at relief and starter, and examine the most extreme cases. 

Starters_1967_2016 <- arrange(Starters_1967_2016, desc(IP.Relief))
Starters_1967_2016 <- arrange(Starters_1967_2016, desc(IP.Start))

# Once I find out the most extreme cases, I subsetted the pitchers who were the most extreme. 

Ken_Dixon <- subset(Starters_1967_2016, NAME == "Ken Dixon" & YEAR == 1985)
Juan_Eichelberger <- subset(Starters_1967_2016, NAME == "Juan Eichelberger" & YEAR == 1983)
Don_Aase <- subset(Starters_1967_2016, NAME == "Don Aase" & YEAR == 1980)
Wilbur_Wood <- subset(Starters_1967_2016, NAME == "Wilbur Wood" & YEAR == 1973)
Phil_Niekro <- subset(Starters_1967_2016, NAME == "Phil Niekro" & YEAR == 1978)
Jim_Colborn <- subset(Starters_1967_2016, NAME == "Jim Colborn" & YEAR == 1973)

# I then plotted the relationship with the outliers. 

ff <- ggplot()
ff + geom_point(data = Starters_1967_2016, aes(x = IP.Start, y = IP.Relief), colour = "grey") + 
  Julien_theme() + geom_smooth(data = Starters_1967_2016, aes(x = IP.Start, y = IP.Relief), method = "loess") +
  geom_point(data = Ken_Dixon, aes(x = IP.Start, IP.Relief), colour = "violetred3") + 
  geom_point(data = Juan_Eichelberger, aes(x = IP.Start, IP.Relief), colour = "slategray3") + 
  geom_point(data = Don_Aase, aes(x = IP.Start, IP.Relief), colour = "royalblue") + 
  geom_point(data = Phil_Niekro, aes(x = IP.Start, IP.Relief), colour = "purple") + 
  geom_point(data = Wilbur_Wood, aes(x = IP.Start, IP.Relief), colour = "red") + 
  geom_point(data = Jim_Colborn, aes(x = IP.Start, IP.Relief), colour = "orange") +
  annotate("Text", label = "Wilbur Wood", x = 354, y = 6.9, colour = "red", size = 3, fontface = "bold") + 
  annotate("Text", label = "Jim Colborn", x = 288.3, y = 27.5, colour = "orange", size = 3, fontface = "bold") + 
  annotate("Text", label = "Phil Niekro", x = 308, y = 5.5, colour = "purple", size = 3, fontface = "bold") + 
  annotate("Text", label = "Don Aase", x = 123, y = 50.5, colour = "royalblue", size = 3, fontface = "bold") + 
  annotate("Text", label = "Juan Eichelberger", x = 80.3, y = 52, colour = "slategray3", size = 3, fontface = "bold") + 
  annotate("Text", label = "Ken Dixon", x = 105.7, y = 57.8, colour = "violetred3", size = 3, fontface = "bold") +
  xlab("IP as starter") + ylab("IP as reliever") + 
  ggtitle("The starters who threw the most innings in relief", subtitle = "Starters innings pitched in relief, and as a starter, 1967-2016") + 
  labs(caption = "Data: Baseball Prospectus")
  
 
# Next I wanted to look at the starters who threw the most in relied for just 2016. 
# So I subsetted the 2016 data, and the outliers. 

Starters_2016 <- subset(Starters_1967_2016, YEAR == 2016)

Starters_2016 <- arrange(Starters_2016, desc(IP.Relief))

Luis_Perdomo <- subset(Starters_2016, NAME == "Luis Perdomo")
Jhoulys_Chacin <- subset(Starters_2016, NAME == "Jhoulys Chacin")
Ross_Stripling <- subset(Starters_2016, NAME == "Ross Stripling")
Patrick_Corbin <- subset(Starters_2016, NAME == "Patrick Corbin")
Albert_Suarez <- subset(Starters_2016, NAME == "Albert Suarez")
Clay_Buchholz <- subset(Starters_2016, NAME == "Clay Buchholz")
Tanner_Roark <- subset(Starters_2016, NAME == "Tanner Roark")

# Now it's easy, all we have to do is plot the graph. 

vv <- ggplot()
vv + geom_point(data = Starters_2016, aes(x = IP.Start, y = IP.Relief), colour = "grey") +
  geom_point(data = Luis_Perdomo, aes(x = IP.Start, y = IP.Relief), colour = "darkblue") + 
  geom_point(data = Jhoulys_Chacin, aes(x = IP.Start, y = IP.Relief), colour = "purple") + 
  geom_point(data = Ross_Stripling, aes(x = IP.Start, y = IP.Relief), colour = "dodgerblue") + 
  geom_point(data = Patrick_Corbin, aes(x = IP.Start, y = IP.Relief), colour = "black") + 
  geom_point(data = Albert_Suarez, aes(x = IP.Start, y = IP.Relief), colour = "darkorange2") + 
  geom_point(data = Clay_Buchholz, aes(x = IP.Start, y = IP.Relief), colour = "darkred") +
  geom_point(data = Tanner_Roark, aes(x = IP.Start, y = IP.Relief), colour = "red") +
  annotate("Text", label = "Luis Perdomo", x = 117, y = 31, colour = "darkblue", size = 3, fontface = "bold") + 
  annotate("Text", label = "Jhoulys Chacin", x = 115.3, y = 27.7, colour = "purple", size = 3, fontface = "bold") + 
  annotate("Text", label = "Ross Stripling", x = 75.7, y = 25.3, colour = "dodgerblue", size = 3, fontface = "bold") + 
  annotate("Text", label = "Patrick Corbin", x = 132.3, y = 24.5, colour = "black", size = 3, fontface = "bold") + 
  annotate("Text", label = "Albert Suarez", x = 58, y = 22, colour = "darkorange2", size = 3, fontface = "bold") + 
  annotate("Text", label = "Clay Buchholz", x = 123, y = 22, colour = "darkred", size = 3, fontface = "bold") + 
  annotate("Text", label = "Tanner Roark", x = 207.7, y = 3.3, colour = "red", size = 3, fontface = "bold") + 
  Julien_theme() + 
  ylab("IP as Reliever") + xlab("IP as Starter") + 
  ggtitle("Which starters threw the most innings in relief?", subtitle = "Starters innings pitched in relief, and as starter, 2016") + 
  labs(caption = "Data: Baseball Prospectus")
  
  
# Finally I wanted to examine which starters threw 200 or more innings and still pitched at least one inning in relief. 
# First I needed to filter the data. 

Starters_1967_2016_200 <- filter(Starters_1967_2016, IP.Start > 200)
head(Starters_1967_2016_200)
str(Starters_1967_2016)

# Then I needed to get the count of pitchers who pitched more than 200 innings and least one inning in relief. 

Count_Starters_1967_2016_200 <- Starters_1967_2016 %>% 
  group_by(YEAR) %>% 
  filter(IP.Start > 200 & IP.Relief > 0) %>%
  summarise(Count = n()) %>% as.data.frame()

# And the count of all pitchers

Count_Starters_1967_2016_200_1 <- Starters_1967_2016 %>% 
  group_by(YEAR) %>% 
  filter(IP.Start > 200) %>%
  summarise(Count = n()) %>% as.data.frame()
 
# The problem here is that the Count_Starters_1967_2016_200 variable is missing a few years, 
# because in some seasons there wasn't a pitcher who threw 200 innings and threw in the pen. 
# In order to fix this problem then, I created a new data frame with the missing values

library(data.table)

Count_Starters_1967_2016_200
Missing_data
Missing_data <- read.table(text = 
                       "YEAR|Count
                     1981|0
                     1994|0
                     2009|0
                     2015|0", sep = "|", header = TRUE, stringsAsFactors = FALSE) 

# Then I merged it to the original one and sorted the data. 

Count_Starters_1967_2016_200 <- rbind(Count_Starters_1967_2016_200, Missing_data)  
Count_Starters_1967_2016_200 <- arrange(Count_Starters_1967_2016_200, YEAR)

# Now we can make out calculations

Count_Starters_1967_2016_200_1_Percentage <- (Count_Starters_1967_2016_200 / Count_Starters_1967_2016_200_1) * 100

Count_Starters_1967_2016_200_1_Percentage$YEAR <- 1967:2016
Count_Starters_1967_2016_200_1_Percentage

# Now all we have to do is plot the trend. 

pp <- ggplot(Count_Starters_1967_2016_200_1_Percentage, aes(x = YEAR, y = Count))
pp + geom_line(colour = "red", size = 1) + Julien_theme() +
  ggtitle("Starters who pitch at least 200 innings don't pitch in relief, anymore", subtitle = "Percentage of starters who threw at least 200 innings and one inning in relief, 1967-2016") + 
  labs(caption = "Data: Baseball Prospectus") + ylab("") + 
  scale_y_continuous(limits = c(0, 100), labels = c(0,25,50,75,"100%"))


