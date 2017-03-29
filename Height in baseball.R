### In this project, I wanted to examine the relationship between height and performance
# while examining how big of an outlier Jose Altuve was. 

library(xml2)
library(rvest)
library(dplyr)
library(tidyr)
library(data.table)

# First, we need to get the data. I did this by webscrapping total roster
# until from 1985 to 2016.
# First I need to egt the URLs

years <- 1950:2016

TOR_Bref_2016 <- "http://www.baseball-reference.com/teams/TOR/2016-roster.shtml"
str(TOR_Bref_2016)

TOR_Bref <- paste0("http://www.baseball-reference.com/teams/TOR/", years,"-roster.shtml")
BOS_Bref <- paste0("http://www.baseball-reference.com/teams/BOS/", years,"-roster.shtml")
NYY_Bref <- paste0("http://www.baseball-reference.com/teams/NYY/", years,"-roster.shtml")
TBR_Bref <- paste0("http://www.baseball-reference.com/teams/TBR/", years,"-roster.shtml")
ATL_Bref <- paste0("http://www.baseball-reference.com/teams/ATL/", years,"-roster.shtml")
ARI_Bref <- paste0("http://www.baseball-reference.com/teams/ARI/", years,"-roster.shtml")
CHC_Bref <- paste0("http://www.baseball-reference.com/teams/CHC/", years,"-roster.shtml")
CHW_Bref <- paste0("http://www.baseball-reference.com/teams/CHW/", years,"-roster.shtml")
CIN_Bref <- paste0("http://www.baseball-reference.com/teams/CIN/", years,"-roster.shtml")
CLE_Bref <- paste0("http://www.baseball-reference.com/teams/CLE/", years,"-roster.shtml")
COL_Bref <- paste0("http://www.baseball-reference.com/teams/COL/", years,"-roster.shtml")
DET_Bref <- paste0("http://www.baseball-reference.com/teams/DET/", years,"-roster.shtml")
HOU_Bref <- paste0("http://www.baseball-reference.com/teams/HOU/", years,"-roster.shtml")
KCR_Bref <- paste0("http://www.baseball-reference.com/teams/KCR/", years,"-roster.shtml")
LAA_Bref <- paste0("http://www.baseball-reference.com/teams/LAA/", years,"-roster.shtml")
LAD_Bref <- paste0("http://www.baseball-reference.com/teams/LAD/", years,"-roster.shtml")
MIA_Bref <- paste0("http://www.baseball-reference.com/teams/MIA/", years,"-roster.shtml")
MIL_Bref <- paste0("http://www.baseball-reference.com/teams/MIL/", years,"-roster.shtml")
NYM_Bref <- paste0("http://www.baseball-reference.com/teams/NYM/", years,"-roster.shtml")
OAK_Bref <- paste0("http://www.baseball-reference.com/teams/OAK/", years,"-roster.shtml")
PHI_Bref <- paste0("http://www.baseball-reference.com/teams/PHI/", years,"-roster.shtml")
PIT_Bref <- paste0("http://www.baseball-reference.com/teams/PIT/", years,"-roster.shtml")
SEA_Bref <- paste0("http://www.baseball-reference.com/teams/SEA/", years,"-roster.shtml")
SFG_Bref <- paste0("http://www.baseball-reference.com/teams/SFG/", years,"-roster.shtml")
STL_Bref <- paste0("http://www.baseball-reference.com/teams/STL/", years,"-roster.shtml")
SDP_Bref <- paste0("http://www.baseball-reference.com/teams/SDP/", years,"-roster.shtml")
TEX_Bref <- paste0("http://www.baseball-reference.com/teams/TEX/", years,"-roster.shtml")
WSN_Bref <- paste0("http://www.baseball-reference.com/teams/WSN/", years,"-roster.shtml")

team_roster_url <- c(TOR_Bref, WSN_Bref, TEX_Bref, SDP_Bref, STL_Bref, 
                     SFG_Bref, SEA_Bref, PIT_Bref, PHI_Bref, OAK_Bref, 
                     NYM_Bref, MIL_Bref, MIA_Bref, LAD_Bref, LAA_Bref, 
                     KCR_Bref, HOU_Bref, DET_Bref, COL_Bref, CLE_Bref,
                     CIN_Bref, CHW_Bref, CHC_Bref, ARI_Bref, ATL_Bref,
                     TBR_Bref, NYY_Bref, BOS_Bref)

# Then I grab the xpath
fetch_roster_2016 <- function(url){
  url %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="appearances"]') %>%
    html_table()
}

# Finally use a loop to scrape the data.
team_roster_1985_2016 <- sapply(team_roster_url, fetch_roster_2016)

head(team_roster_1985_2016)
max(team_roster_1985_2016$Yrs)

team_roster_1985_2016_1 <- team_roster_1985_2016

# It returned a list so I need to change it to a data.frame, while keeping the id to kow which year it is.
team_roster_1985_2016 <- unlist(team_roster_1985_2016, recursive = FALSE)
team_roster_1985_2016_1 <- rbindlist(team_roster_1985_2016_1, use.names = TRUE, fill = TRUE, idcol = "id")
head(team_roster_1985_2016_1)

# Then I need to clean the id column and covert to to years
team_roster_1985_2016_1 <- team_roster_1985_2016 %>% separate(id, c("id", "wtv", "wtv1", "wtv2", "team", "year"), sep = "/")

team_roster_1985_2016_1$id <- NULL
team_roster_1985_2016_1$wtv <- NULL
team_roster_1985_2016_1$wtv1 <- NULL
team_roster_1985_2016_1$wtv2 <- NULL

team_roster_1985_2016_1 <- team_roster_1985_2016_1 %>% separate(year, c("year", "wr"), "-")
team_roster_1985_2016_1$wr <- NULL
team_roster_1985_2016_1$V2 <- NULL
team_roster_1985_2016_1$V1 <- NULL

# getting rid of uneeded rows
team_roster_1985_2016_1 <- team_roster_1985_2016_1[team_roster_1985_2016_1$Name!="Name",]
team_roster_1985_2016_1 <- team_roster_1985_2016_1[team_roster_1985_2016_1$Name!="Team Total",]

# Converting Ht from feet & inches to inches. 
team_roster_1985_2016_1$Ht <- sapply(strsplit(team_roster_1985_2016_1$Ht,"'|\""),
       function(x){12*as.numeric(x[1]) + as.numeric(x[2])}) 

team_roster_1985_2016_1 <- transform(team_roster_1985_2016_1, year = as.numeric(year),
                                     WAR = as.numeric(WAR))

# Remove all pitchers
team_roster_1985_2016_2 <- team_roster_1985_2016_1 %>% filter(P < C | P < `1B` | P < `2B` | P < `3B` | P < SS | P < OF | P < DH) 
head(team_roster_1985_2016_2)

# Get the average and median height per year
Average_height_1985_2016 <- team_roster_1985_2016_2 %>%
  group_by(year) %>% 
  summarise(Avg_Ht = mean(Ht), Med_Ht = median(Ht)) %>%
  as.data.frame()

head(Average_height_1985_2016)

library(ggplot2)
library(ggrepel)

# Subset Mookie Betts and Jose Altuve
Mookie_Betts <- subset(team_roster_1985_2016_2, Name == "Mookie Betts" & year == 2016)
head(Mookie_Betts)
Jose_Altuve <- subset(team_roster_1985_2016_2, Name == "Jose Altuve" & year == 2016)
team_roster_2016 <- subset(team_roster_1985_2016_2, year == 2016)

Player_WAR_Ht_1985_2016[814,3] <- 20.8

# Get the players total WAR, height, and total games played, and round the data
Player_WAR_Ht_1985_2016 <- team_roster_1985_2016_2 %>% group_by(Name) %>% summarise(Ht = median(Ht), Total_WAR = sum(WAR), sum_PA = sum(G)) %>% as.data.frame()
head(Player_WAR_Ht_1985_2016)

Player_WAR_Ht_1985_2016$Total_WAR <- round(Player_WAR_Ht_1985_2016$Total_WAR, digits = 3)
Altuve_WAR_Career <- subset(Player_WAR_Ht_1985_2016, Name == "Jose Altuve")

head(Altuve_WAR_Career)

Player_WAR_Ht_1985_2016$Ht <- round(Player_WAR_Ht_1985_2016$Ht, digits = 0)

colnames(Player_WAR_Ht_1985_2016)[3] <- "Career WAR"

# Create plot 
pl <- ggplot()
pl + geom_point(data = Player_WAR_Ht_1985_2016, aes(x = Name, y = Ht, size = `Career WAR`), colour = "darkgrey", shape = 22, fill = "darkgrey") + 
  geom_point(data = Altuve_WAR_Career, aes(x = Name, y = Ht, size = Total_WAR), colour = "#FF7F00", shape = 22, fill = "#FF7F00", show.legend = FALSE) + 
  Julien_theme() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        panel.grid.major = element_line(colour = "grey93"),
        legend.position = c(0.7,0.99),
        legend.direction = "horizontal",
        legend.title = element_text(colour = "darkgrey"),
        legend.text = element_text(colour = "darkgrey"),
        legend.background = element_rect(linetype = "solid", colour = "darkgrey")) + 
  scale_y_continuous(breaks = c(66, 68, 70, 72, 74, 76, 78, 80), labels = c(66, 68, 70, 72, 74, 76, 78, 80)) + 
  annotate("text", label = "JOSE ALTUVE", x = 1650, y = 66.5, colour = "#FF7F00", size = 3, fontface = "bold") + 
  ggtitle("There's never been anyone like Altuve in baseball", subtitle = "Baseball position players by height, 1985-2016") + 
  labs(caption = "DATA: Baseball-Reference") + 
  annotate("text", label = "Tony Clark", x = 3000, y = 80.45, colour = "darkgrey", size = 3) +
  ylab("Height in inches") + 
  geom_text(data = data.frame(), aes(x = "David Eckstein", y = 65.6, label = "David Eckstein"), colour = "darkgrey", size = 3) + 
  annotate("text", label = "Nate Freiman ", x = 2300, y = 79.65, colour = "darkgrey", size = 3) 
  

# Next I want to examine the relationship between offense, and power and height

### Scraping more bref data to get the slg% and more offensive statistics 


Batting_Bref <- paste0("http://www.baseball-reference.com/leagues/MLB/",years,"-advanced-batting.shtml")
head(Batting_Bref)

fetch_batting_bref <- function(url){
  url %>%
    read_html() %>%
    xml_find_all(xpath = '//*[@id="players_advanced_batting"]') %>%
    html_table()
}

batting_bref_1985_2016 <- sapply(Batting_Bref, fetch_batting_bref)
head(batting_bref_1985_2016)

# Getting the data from list into a data frame, and getting the year

batting_bref_1985_2016_1 <- rbindlist(batting_bref_1985_2016, use.names = TRUE, fill = TRUE, idcol = "id")
head(batting_bref_1985_2016_1)

batting_bref_1985_2016_1 <- batting_bref_1985_2016_1 %>% separate(id, c("id", "wtv", "wtv1", "wtv2", "team", "year"), sep = "/")

batting_bref_1985_2016_1$id <- NULL
batting_bref_1985_2016_1$wtv <- NULL
batting_bref_1985_2016_1$wtv1 <- NULL
batting_bref_1985_2016_1$wtv2 <- NULL
batting_bref_1985_2016_1$team <- NULL

batting_bref_1985_2016_1 <- batting_bref_1985_2016_1 %>% separate(year, c("year", "delete"), sep = "-")

batting_bref_1985_2016_1$delete <- NULL

head(batting_bref_1985_2016_1)

# removing unwanted columns

batting_bref_1985_2016_1 <- batting_bref_1985_2016_1[batting_bref_1985_2016_1$Name!="Name",]

# selecting columns I want, and making some of them numeric

batting_bref_1985_2016_2 <- select(batting_bref_1985_2016_1, year, Name, Tm, PA, BAbip, BA, Age, OBP, SLG, OPS, `OPS+`, ISO)
tail(batting_bref_1985_2016_2, 100)

batting_bref_1985_2016_2 <- transform(batting_bref_1985_2016_2, year = as.numeric(year),
                                      ISO = as.numeric(ISO),
                                      `OPS+` = as.numeric(`OPS+`),
                                      BAbip = as.numeric(BAbip))

batting_bref_1985_2016_2 <- batting_bref_1985_2016_2[batting_bref_1985_2016_2$Name!="LgAvg per 600 PA",]
batting_bref_1985_2016_2 <- batting_bref_1985_2016_2[batting_bref_1985_2016_2$Name!="Name",]

batting_bref_1985_2016_2[is.na(batting_bref_1985_2016_2)] <- 0

# Getting rid of symbols

batting_bref_1985_2016_2$Name <- gsub("#","", batting_bref_1985_2016_2$Name)
batting_bref_1985_2016_2$Name <- gsub("[*]","", batting_bref_1985_2016_2$Name)
batting_bref_1985_2016_2

colnames(batting_bref_1985_2016_2)[3] <- "team" 

# removing repeating values
batting_bref_1985_2016_3 <- batting_bref_1985_2016_2 %>% unique(Name)

# making NA values 0
team_roster_1985_2016_2[is.na(team_roster_1985_2016_2)] <- 0
batting_bref_1985_2016_2[is.na(batting_bref_1985_2016_2)] <- 0

# Making sure each players has had at least one PA, and sorting the data. 
batting_bref_1985_2016_2 <- filter(batting_bref_1985_2016_2, PA > 0)
batting_bref_1985_2016_2 <- arrange(batting_bref_1985_2016_2, Name, year, team)

team_roster_1985_2016_2 <- arrange(team_roster_1985_2016_2, Name, year, team)

batting_bref_1985_2016_2$Name <- as.character(batting_bref_1985_2016_2$Name)
team_roster_1985_2016_2$Name <- as.character(team_roster_1985_2016_2$Name)

### Then I wanted to merge the data.frames, but for some reason it wasn't working
# so I extracted the data and the re-imported it, and then it worked. 
library(readxl)

write.csv(team_roster_1985_2016_2, "height in baseball.csv")
write.csv(batting_bref_1985_2016_2, "batting in baseball.csv")

team_roster_1985_2016_3 <- read.csv(file.choose())
batting_bref_1985_2016_3 <- read.csv(file.choose())

head(team_roster_1985_2016_3)
head(batting_bref_1985_2016_3)
team_roster_1985_2016_3$X <- NULL
batting_bref_1985_2016_3$X <- NULL

# Subsetting the players with the total teams. 
TOT_team <- subset(batting_bref_1985_2016_3, team == "TOT")
head(TOT_team)
head(TOT_team, 100) 

# cleaning column Name

batting_bref_1985_2016_3$Name <- gsub("\xca", " ", batting_bref_1985_2016_3$Name) 

# Joining dataframe with slg and OPS to dataframe with Ht and WAR
team_roster_1985_2016_complete <- inner_join(batting_bref_1985_2016_3, team_roster_1985_2016_3, by = c("Name", "year", "Age", "team")) 
head(team_roster_1985_2016_complete, 50) 

colnames(TOT_team)[3] <- "Team" 

# joining players with total years, so that now we have the players with their overall values for that year.
# These are players who were traded mid season. 
TOT_team_complete <- inner_join(TOT_team, team_roster_1985_2016_complete, by = c("Name", "year", "Age")) 
head(TOT_team_complete)

# selected columns I want
TOT_team_complete <- select(TOT_team_complete, year, Name, Team, PA.x, BAbip.x , BA.x, Age, OBP.x, SLG.x, OPS.x, OPS..x, ISO.x, Ht, Wt, WAR)
head(TOT_team_complete)

# Very important, now we removed the duplicate values. 
team_roster_1985_2016_complete_1 <- team_roster_1985_2016_complete[!(duplicated(team_roster_1985_2016_complete[c("Name", "year")])),]

# Make sure we are only examining qulified players. 
team_roster_1985_2016_complete_1 <- filter(team_roster_1985_2016_complete, PA > 502) 
TOT_team_complete_1 <- filter(TOT_team_complete, PA.x > 502)

# Getting ride of duplicate values again
TOT_team_complete_1 <- TOT_team_complete_1[!(duplicated(TOT_team_complete_1[c("Name", "year")])),]
head(TOT_team_complete_1)
TOT_team_complete_1

TOT_team_complete_2 <- setnames(TOT_team_complete_1, old = c("year", "Name", "Team", "PA.x", "BAbip.x", "BA.x", "Age", "OBP.x", "SLG.x", "OPS.x", "OPS..x", "ISO.x","Ht","Wt", "WAR"), 
                                      new = c("year", "Name", "team", "PA", "BAbip", "BA", "Age", "OBP", "SLG", "OPS", "OPS.", "ISO", "Ht", "Wt", "WAR"))   
head(TOT_team_complete_2)

# Now we want to join both dataframe because we want to get the players total production 
# for the year, and not just his production for his specific teams
team_roster_1985_2016_complete_total <- full_join(team_roster_1985_2016_complete_1, TOT_team_complete_2)  
head(team_roster_1985_2016_complete_total, 100) 

team_roster_1985_2016_complete_total <- arrange(team_roster_1985_2016_complete_total, Name, year)

# checking duplicate values
dup <- team_roster_1985_2016_complete_total[duplicated(team_roster_1985_2016_complete_total[,1:2]),] 
head(dup)

# Removing all duplicate values
team_roster_1985_2016_complete_total_1 <- team_roster_1985_2016_complete_total[!(duplicated(team_roster_1985_2016_complete_total[c("Name", "year")])),]

# Getting the average height per year. 
Average_WAR_Height_1985_2016 <- team_roster_1985_2016_complete_total_1 %>% group_by(Ht) %>% summarise(Avg_WAR = mean(WAR), Med_WAR = median(WAR)) %>% as.data.frame()
head(Average_WAR_Height_1985_2016)

# Examining trend in players height
gg <- ggplot()
gg + geom_line(data = Average_height_1985_2016, aes(x = year, y = Med_Ht), size = 1, colour = "red") + 
  Julien_theme() + ylab("Median Height") + 
  ggtitle("Baseball players are getting taller", subtitle = "Median height of MLB position players with minimum of 502 plate appearances, 1985-2016") +
  xlab("Year") + 
  labs(caption = "DATA: Baseball-Reference") + 
  scale_x_continuous(breaks = seq(1985,2016,5))

# plotting distribution
d <- ggplot(team_roster_1985_2016_complete_total_1, aes(Ht))
d + geom_histogram(colour = "darkgrey", fill = "grey60", binwidth = 1) + 
  Julien_theme() + 
  geom_segment(aes(x = 72.85726, y = 0, xend = 72.85726, yend = 700), size = 1, colour = "purple") + 
  ggtitle("Distrubution of baseball players heights 1985-2016") + 
  annotate("Text", label = "Average", x = 72.85726, y = 715, size = 3.5, colour = "Purple", fontface = "bold") + 
  xlab("Height") + ylab("Count") + 
  labs(caption = "Data: Baseball-Reference") + 
  geom_text(data = data.frame(), aes(x = 66, y= 100, label = "Jose Altuve"), colour = "darkgrey", size = 4, fontface = "bold") + 
  geom_curve(data = data.frame(), aes(x = 66, xend = 66, y = 80, yend = 40), colour = "darkgrey", curvature= 0, arrow=arrow(length=unit(0.01, "npc")))
  

Tony_Clark <- subset(team_roster_1985_2016_complete_total_1, Name == "Tony Clark")
Tony_Clark

# subsetting Betts and Altuve for 2016 and their careers
Mookie_Betts_2016 <- subset(team_roster_1985_2016_complete_total_1, Name == "Mookie Betts" & year == 2016)
Jose_Altuve_2016 <- subset(team_roster_1985_2016_complete_total_1, Name == "Jose Altuve" & year == 2016)
Jose_Altuve_All <- subset(team_roster_1985_2016_complete_total_1, Name == "Jose Altuve")
Mookie_Betts_All <- subset(team_roster_1985_2016_complete_total_1, Name == "Mookie Betts")

# plotting Altuve's and Betts WAR values in relation to height

jj <- ggplot() 
jj + geom_point(data = team_roster_2016_complete_total, aes(x = Ht, y = WAR), colour = "grey") +
  geom_point(data = Mookie_Betts_2016, aes(x = Ht, y = WAR), colour = "red") + 
  geom_point(data = Jose_Altuve_2016, aes(x = Ht, y = WAR), colour = "orange") +
  Julien_theme() + annotate("Text", label = "Mookie Betts", x = 69, y = 10.2, size = 3, colour = "red", fontface = "bold") + 
  annotate("Text", label = "Jose Altuve", x = 66, y = 8.2, size = 3, colour = "orange", fontface = "bold") + 
  labs(caption = "Data: Baseball-Reference") + xlab("Height") +
  ggtitle("There's no one like Altuve in today's game", subtitle = "WAR and Height relationship in 2016") 
  

WAR_1985_2016 = team_roster_1985_2016_complete_total_1$WAR
Ht_1985_2016 = team_roster_1985_2016_complete_total$Ht
OPS_1985_2016 = team_roster_2016_complete_total$OPS.

cor(Ht_1985_2016, OPS_1985_2016)
str(team_roster_2016_complete_total)
Jose_Altuve_2016

# Plotting relationship between OPS+ and Height. Shwoing Altuve as outlier
ee <- ggplot() 
 ee +  geom_point(data = team_roster_2016_complete_total, aes(x = Ht, y = OPS.), colour = "grey") + 
  geom_smooth(data = team_roster_2016_complete_total, aes(x = Ht, y = OPS.), method = "lm", colour = "darkgrey", se = FALSE) + 
  Julien_theme() + 
  geom_point(data = Jose_Altuve_2016, aes(x = Ht, y = OPS.), colour = "#FF7F00") + 
   annotate("Text", label = "JOSE ALTUVE", x = 66.3, y = 158, colour = "#FF7F00", size = 3, fontface = "bold") + 
   ylab("OPS+") + xlab("Height in inches") + labs(caption = "DATA: Baseball-Reference") + 
   ggtitle("There's no one like Altuve in today's game", subtitle = "OPS+ and Height relationship with minimum 502 plate appearances in 2016") + 
   scale_x_continuous(breaks = seq(66,76,2))
 
 
# Plotting relationship between ISO and height, showing Betts and Altuve as outliers
nn <- ggplot() 
nn + geom_point(data = team_roster_2016_complete_total, aes(x = Ht, y = ISO), colour = "grey") +
  geom_point(data = Mookie_Betts_2016, aes(x = Ht, y = ISO), colour = "#C60C30") + 
  geom_point(data = Jose_Altuve_2016, aes(x = Ht, y = ISO), colour = "#FF7F00") +
  Julien_theme() + 
  annotate("Text", label = "JOSE ALTUVE", x = 66.3, y = 0.205, colour = "#FF7F00", size = 3, fontface = "bold") + 
  annotate("Text", label = "Mookie Betts", x = 69, y = 0.225, colour = "#C60C30", size = 3) + 
  labs(caption = "Data: Baseball-Reference") + xlab("Height in inches") +
  ggtitle("Altuve and Betts are power hitting outliers", subtitle = "Relationship between ISO and Height for position players with minimum \n502 plate appearances in 2016") + 
  geom_smooth(data = team_roster_2016_complete_total, aes(x = Ht, y = ISO), method = "lm", se = FALSE, colour = "darkgrey")  

