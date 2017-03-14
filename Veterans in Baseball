
library(dplyr)
library(tidyr)

### This was a project to try to quantify which players were veterans in baseball
# To do this I wanted to weight the players age by the number of years they've played in the league

# The dataframe used here has already been scrapped from Baseball-Reference. 
# So I used an already built in database.


head(team_roster_1985_2016_1, 100)

# cleaning the Yrs column and making the Age and Yrs column numeric  
team_roster_1985_2016_1$Yrs <- gsub("st", "", team_roster_1985_2016_1.0$Yrs)


str(team_roster_1985_2016_1)
team_roster_1985_2016_1 <- transform(team_roster_1985_2016_1, Age = as.numeric(Age),
                                      Yrs = as.numeric(Yrs))

# exmaine the median age and yrs by year
baseball_veteran_summarised <- team_roster_1985_2016_1 %>% 
  group_by(year) %>% 
  summarise(median_Age = median(Age), median_Yrs = median(Yrs)) %>%
  as.data.frame() 
head(baseball_veteran_summarised)

head(team_roster_1985_2016_1)

# calculate the age weighted by years
team_roster_1985_2016_1$wAge  <- team_roster_1985_2016_1$Age * team_roster_1985_2016_1$Yrs
head(team_roster_1985_2016_1)
team_roster_1985_2016_1$League <- 1

# Getting 2016 data
team_roster_2016 <- subset(team_roster_1985_2016_1, year == 2016)
head(team_roster_2016)


# subsetting for potential players of interest to later plot
Trout_2016 <- subset(team_roster_2016, Name == "Mike Trout")
head(Trout_2016)
Bryant_2016 <- subset(team_roster_2016, Name == "Kris Bryant")
head(Bryant_2016)
Arrieta_2016 <- subset(team_roster_2016, Name == "Jake Arrieta")
head(Arrieta_2016)
Harper_2016 <- subset(team_roster_2016, Name == "Bryce Harper")
head(Harper_2016)
Harper_2016 <- subset(team_roster_2016, Name == "Bryce Harper")
head(Harper_2016)

# sorting wAge, and making G column numeric
team_roster_2016 <- arrange(team_roster_2016, wAge)
head(team_roster_2016)
summary(team_roster_2016$G)
team_roster_2016 <- transform(team_roster_2016, G = as.numeric(G))

# removing all the pitcher
team_roster_hitters_2016 <- team_roster_2016 %>% filter(P < C | P < `X1B` | P < `X2B` | P < `X3B` | P < SS | P < OF | P < DH) 
head(team_roster_hitters_2016)
str(team_roster_hitters_2016)

# Every player must have played at least 10 games
team_roster_hitters_2016_1 <- team_roster_hitters_2016 %>% filter(G > 9)
head(team_roster_hitters_2016_1)


# Load ggplot2, and plot the graph to see whether Mike Trout is a Veteran or not
library(ggplot2)

gg <- ggplot()
gg + geom_jitter(data = team_roster_hitters_2016_1, aes(x = wAge, y = League), colour = "grey", alpha = 0.5) + 
  geom_segment(data = data.frame(), aes(x = 135, xend = 135, y = 0.55, yend = 1.45), colour = "black", size = 0.7, linetype = 2) +
  scale_y_continuous(limits = c(0.5,1.5)) +
  Julien_theme() + 
  geom_label(data = data.frame(), aes(x = 199, y = 1, label = "Mike Trout"), colour = "grey93", fill = "grey93", size = 3.5, label.padding = unit(0.20, "lines")) +
  geom_text(data = data.frame(), aes(x = 199, y = 1, label = "MIKE TROUT"), colour = "red", size = 3, fontface = "bold") + 
  geom_point(data = Trout_2016, aes(x = wAge, y = League), colour = "red") + 
  annotate("Text", label = "Median", x = 135, y = 1.47, colour = "black", size = 3.5) + 
  ggtitle("Is Mike Trout a Veteran?", subtitle = "2016 position player age weighted by years played") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  labs(caption = "DATA: Baseball-Reference") + 
  xlab("Weighted Age") + 
  geom_text(data=data.frame(x=c(75, 200), label=c("← Not a \nVeteran", "Veteran →")), aes(x=x, y=0.55, label=label), family="Arial Narrow", size=3.5) 

  
### Now we want to find out whether Trout is a veteran based on performance 

median(team_roster_hitters_2016_1$wAge)
summary(team_roster_hitters_2016_1$wAge)

# We need to get total WAR, so we do a semi_join to connect the 2016 players with their past performance 

team_roster_2016_merged <- semi_join(team_roster_1985_2016_1, team_roster_hitters_2016_1, by = c("Name", "team"))
head(team_roster_2016_merged, 100)

# Then we get the Average and Total WAR for those players careers
Player_avg_WAR <- team_roster_2016_merged %>% group_by(Name) %>% summarise(mean_WAR = mean(WAR), sum_WAR = sum(WAR)) %>% data.frame()
head(Player_avg_WAR)

# Then we merge those dataframes together to get the Players total WAR and average WAR
team_roster_2016_merged_1 <- inner_join(team_roster_2016_merged, Player_avg_WAR)
head(team_roster_2016_merged_1)

# Then we calculate the weighted age based off of career WAR but first we remove the NA values 
is.na(team_roster_2016_merged_2$Age_WAR)
team_roster_2016_merged_2 <- team_roster_2016_merged_2[!(is.na(team_roster_2016_merged_2$Age_WAR)),]
team_roster_2016_merged_2 <- arrange(team_roster_2016_merged_2, desc(Age_WAR))

team_roster_2016_merged_1$Age_WAR <- team_roster_2016_merged_1$Age * team_roster_2016_merged_1$sum_WAR
head(team_roster_2016_merged_1)
tail(team_roster_2016_merged_1)

# Next we subset to only get the 2016 season. But, we still have their total WAR values
team_roster_2016_merged_2 <- subset(team_roster_2016_merged_1, year == 2016)

# We also want to examine a few points, especially Trout, so we subset a few points of possible interest
Trout_2016_Age_WAR <- subset(team_roster_2016_merged_2, Name == "Mike Trout")
head(Trout_2016_Age_WAR)
Bryant_2016_Age_WAR <- subset(team_roster_2016_merged_2, Name == "Kris Bryant")
head(Bryant_2016_Age_WAR)
Arrieta_2016_Age_WAR <- subset(team_roster_2016_merged_2, Name == "Jake Arrieta")
head(Arrieta_2016_Age_WAR)
Harper_2016_Age_WAR <- subset(team_roster_2016_merged_2, Name == "Bryce Harper")
head(Harper_2016_Age_WAR)

median(team_roster_2016_merged_2$Age_WAR)
head(team_roster_2016_merged_2)
summary(team_roster_2016_merged_2$Age_WAR)
median(team_roster_2016_merged_2$Age_WAR)

# Finally we can plot the data.

jj <- ggplot()
jj + geom_jitter(data = team_roster_2016_merged_2, aes(x = Age_WAR, y = League), colour = "grey", alpha = 0.7) + 
  geom_segment(data = data.frame(), aes(x = 30.6, xend = 16.05, y = 0.55, yend = 1.45), colour = "black", size = 0.7, linetype = 2) +
  Julien_theme() + 
  scale_y_continuous(limits = c(0.5,1.5)) +
  geom_point(data = Trout_2016_Age_WAR, aes(x = Age_WAR, y = League), colour = "red") + 
  annotate("Text", label = "MIKE TROUT", x = 1168.8, y = 1.04, colour = "red", size = 3, fontface = "bold") + 
  annotate("Text", label = "Median", x = 30.6, y = 1.47, colour = "black", size = 3.5) + 
  ggtitle('By performance, yes, Mike Trout is a "veteran"', subtitle = "2016 position player age weighted by total career WAR") + 
  xlab("Weighted Age") + 
  labs(caption = "DATA: Baseball-Reference") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  geom_text(data=data.frame(x=c(-75, 150), label=c("← Not a \nVeteran", "Veteran →")), aes(x=x, y=0.55, label=label), family="Arial Narrow", size=3.5)

