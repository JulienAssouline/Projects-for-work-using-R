# In the document, I analyzed and visualized Position players, Pitchers, and Designated hitter salaries and performance. 
# I did it in somewhat three sections. First position players, second pitchers, and finally DH. 
# Every new section starts with ###

### Position players

library(xml2)
library(rvest)
library(dplyr)
library(tidyr)


years <- 1985:2016

#grabing all the urls (probably not the most efficient approach)

team_bref_urlBOS <- paste0("http://www.baseball-reference.com/teams/BOS/",years,".shtml")
team_bref_urlTOR <- paste0("http://www.baseball-reference.com/teams/TOR/",years,".shtml")
team_bref_urlNYY <- paste0("http://www.baseball-reference.com/teams/NYY/",years,".shtml")
team_bref_urlTBR <- paste0("http://www.baseball-reference.com/teams/TBR/",years,".shtml")
team_bref_urlARI <- paste0("http://www.baseball-reference.com/teams/ARI/",years,".shtml")
team_bref_urlATL <- paste0("http://www.baseball-reference.com/teams/ATL/",years,".shtml")
team_bref_urlCHC <- paste0("http://www.baseball-reference.com/teams/CHC/",years,".shtml")
team_bref_urlCHW <- paste0("http://www.baseball-reference.com/teams/CHW/",years,".shtml")
team_bref_urlCIN <- paste0("http://www.baseball-reference.com/teams/CIN/",years,".shtml")
team_bref_urlCLE <- paste0("http://www.baseball-reference.com/teams/CLE/",years,".shtml")
team_bref_urlCOL <- paste0("http://www.baseball-reference.com/teams/COL/",years,".shtml")
team_bref_urlDET <- paste0("http://www.baseball-reference.com/teams/DET/",years,".shtml")
team_bref_urlHOU <- paste0("http://www.baseball-reference.com/teams/HOU/",years,".shtml")
team_bref_urlKCR <- paste0("http://www.baseball-reference.com/teams/KCR/",years,".shtml")
team_bref_urlLAA <- paste0("http://www.baseball-reference.com/teams/LAA/",years,".shtml")
team_bref_urlLAD <- paste0("http://www.baseball-reference.com/teams/LAD/",years,".shtml")
team_bref_urlMIA <- paste0("http://www.baseball-reference.com/teams/MIA/",years,".shtml")
team_bref_urlMIL <- paste0("http://www.baseball-reference.com/teams/MIL/",years,".shtml")
team_bref_urlNYM <- paste0("http://www.baseball-reference.com/teams/NYM/",years,".shtml")
team_bref_urlOAK <- paste0("http://www.baseball-reference.com/teams/OAK/",years,".shtml")
team_bref_urlPHI <- paste0("http://www.baseball-reference.com/teams/PHI/",years,".shtml")
team_bref_urlPIT <- paste0("http://www.baseball-reference.com/teams/PIT/",years,".shtml")
team_bref_urlSDP <- paste0("http://www.baseball-reference.com/teams/SDP/",years,".shtml")
team_bref_urlSEA <- paste0("http://www.baseball-reference.com/teams/SEA/",years,".shtml")
team_bref_urlSFG <- paste0("http://www.baseball-reference.com/teams/SFG/",years,".shtml")
team_bref_urlSTL <- paste0("http://www.baseball-reference.com/teams/STL/",years,".shtml")
team_bref_urlTEX <- paste0("http://www.baseball-reference.com/teams/TEX/",years,".shtml")
team_bref_urlWSN <- paste0("http://www.baseball-reference.com/teams/WSN/",years,".shtml")

team_bref_url

team_bref_url <- c(team_bref_urlBOS, team_bref_urlTOR, team_bref_urlWSN, team_bref_urlTEX, team_bref_urlSTL,
                   team_bref_urlSFG, team_bref_urlSEA, team_bref_urlSDP, team_bref_urlPIT, team_bref_urlPHI,
                   team_bref_urlOAK, team_bref_urlNYM, team_bref_urlMIL, team_bref_urlMIA, team_bref_urlLAD,
                   team_bref_urlLAA, team_bref_urlKCR, team_bref_urlHOU, team_bref_urlDET, team_bref_urlCOL, 
                   team_bref_urlCLE, team_bref_urlCIN, team_bref_urlCHW, team_bref_urlCHC, team_bref_urlATL, 
                   team_bref_urlATL, team_bref_urlARI, team_bref_urlTBR, team_bref_urlNYY)

# Scraping the web pages
fetch_team_2016 <- function(url){
    url %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="players_value_batting"]') %>%
    html_table()
}

team_salaries_1985_2016 <- sapply(team_bref_url, fetch_team_2016)
head(team_salaries_1985_2016)
team_salaries_1985_2016_1 <- team_salaries_1985_2016

library(data.table)

#unlist the list of list data
team_salaries_1985_2016_2 <- unlist(team_salaries_1985_2016_1, recursive = FALSE)
team_salaries_1985_2016_3 <- rbindlist(team_salaries_1985_2016_2)
is.data.frame(team_salaries_1985_2016_3)
head(team_salaries_1985_2016_3)

# Put the id of the list into column to have the years
team_salaries_1985_2016_3$id <- rep(names(team_salaries_1985_2016_2), sapply(team_salaries_1985_2016_2, nrow))
head(team_salaries_1985_2016_3)

# Cleaning the id column to only be left with the year values, by seperating columns, and rmoving ones I don't want. 
team_salaries_1985_2016_4 <- team_salaries_1985_2016_3 %>% separate(id, c("id", "wtv", "wtv1", "wtv2", "team", "year"), sep = "/")

head(team_salaries_1985_2016_4)
team_salaries_1985_2016_4$id <- NULL
team_salaries_1985_2016_4$wtv <- NULL
team_salaries_1985_2016_4$wtv1 <- NULL
team_salaries_1985_2016_4$wtv2 <- NULL

team_salaries_1985_2016_4 <- team_salaries_1985_2016_4 %>% separate(year, c("year", "k"))
head(team_salaries_1985_2016_4)
team_salaries_1985_2016_4$k <- NULL

# Getting rid of all rows with "Name"
team_salaries_1985_2016_5 <- team_salaries_1985_2016_4[team_salaries_1985_2016_4$Name!="Name",]
head(team_salaries_1985_2016_5)

team_salaries_1985_2016_5$Salary[team_salaries_1985_2016_5$Salary == ""] <- 0

# Cleaning Salary column by getting rid of commas, and making it numeric. Getting rid of NA values
team_salaries_1985_2016_5$Salary <- gsub(",","", team_salaries_1985_2016_5$Salary)

team_salaries_1985_2016_5 <- team_salaries_1985_2016_5 %>% separate(Salary, c("Salary1", "Salary"))

team_salaries_1985_2016_5$Salary1 <- NULL

team_salaries_1985_2016_5[is.na(team_salaries_1985_2016_5)] <- 0

team_salaries_1985_2016_5$Salary <- as.numeric(team_salaries_1985_2016_5$Salary)

str(team_salaries_1985_2016_5)

team_salaries_1985_2016_6 <- team_salaries_1985_2016_5[team_salaries_1985_2016_5$Salary > 0,]
head(team_salaries_1985_2016_6)

# Getting rid of "team total" rows
team_salaries_1985_2016_6 <- team_salaries_1985_2016_6[team_salaries_1985_2016_6$Name!="Team Total",]

team_salaries_1985_2016_6 <- transform(team_salaries_1985_2016_6, year= as.numeric(year))

# Getting average & median league salaries
Average_League_salaries_1985_2016 <- team_salaries_1985_2016_6 %>% group_by(year) %>% summarise(Average_Salary = mean(Salary), Median_Salary = median(Salary)) %>% as.data.frame()

# Getting rid of all pitchers 
team_salaries_1985_2016_7 <- team_salaries_1985_2016_6[team_salaries_1985_2016_6$`Pos Summary`!=1,]
team_salaries_1985_2016_7 <- team_salaries_1985_2016_7[team_salaries_1985_2016_7$`Pos Summary`!="/1",]
team_salaries_1985_2016_7 <- team_salaries_1985_2016_7[team_salaries_1985_2016_7$Name!="Team Total",]

# Getting the positions most often played. The first position or number in the Pos Summary column is the position most often played
# First, Seperating values, then getting rid of the / and other symbols
team_salaries_1985_2016_8 <- separate(team_salaries_1985_2016_7, `Pos Summary`, into = c("Position", "summary"), sep = 2)
head(team_salaries_1985_2016_8, n = 50)

team_salaries_1985_2016_8$Position <- gsub("/","", team_salaries_1985_2016_8$Position)
team_salaries_1985_2016_8$Position <- sub("^[^[:alnum:]]","", team_salaries_1985_2016_8$Position)
team_salaries_1985_2016_9 <- separate(team_salaries_1985_2016_8, Position, into = c("Position", "wtv"), sep = 1)
head(team_salaries_1985_2016_9, n= 50)
team_salaries_1985_2016_9$wtv <- NULL
team_salaries_1985_2016_9$summary <- NULL


team_salaries_1985_2016_9$Position[team_salaries_1985_2016_9$Position == "D"] <- 10

Average_position_salary
head(team_salaries_1985_2016_9)

# Making culmns numeric
team_salaries_1985_2016_9 <- transform(team_salaries_1985_2016_9, year = as.numeric(year),
                                       Position = as.numeric(Position),
                                       WAR = as.numeric(WAR))


# Getting the average salary by position and year
Average_position_salary <- team_salaries_1985_2016_9 %>% group_by(Position, year) %>% summarise(Average_Salary = mean(Salary), Average_WAR = mean(WAR)) %>% as.data.frame()

Average_position_salary <- Average_position_salary[Average_position_salary$Position!=1,]

# Joining databases to get team_salaries_1985_2016_9 the average league salary 
team_salaries_1985_2016_9 <- left_join(team_salaries_1985_2016_9, Average_League_salaries_1985_2016)
head(team_salaries_1985_2016_9)

# Creating an aditional column with adjusted salaries to 2015 values
Average_League_salaries_1985_2016$scales <- 4361726.8/Average_League_salaries_1985_2016$Average_Salary
Average_League_salaries_1985_2016

team_salaries_1985_2016_9$Scaled_Salary_2015 <- team_salaries_1985_2016_9$scales * team_salaries_1985_2016_9$Salary

# Summarising the average salary, WAR, Scaled Salary, Median WAR, Median Scaled Salary by position
Positin_Player_WAR_Salary_1985_2015 <- team_salaries_1985_2016_9 %>% group_by(Position) %>% summarise(Average_salary = mean(Salary), Average_WAR = mean(WAR), Scaled_Salary_2015 = mean(Scaled_Salary_2015), Median_WAR = median(WAR), Median_Scaled_2015_Salary = median(Scaled_Salary_2015)) %>% as.data.frame()

head(Positin_Player_WAR_Salary_1985_2015)

# Getting average salaries for position players/hitters
Average_salaries_hitters_1985_2016 <- team_salaries_1985_2016_7 %>% group_by(year) %>% summarise(Average_Salary = mean(Salary), Median_Salary = median(Salary)) %>% as.data.frame()
Average_salaries_hitters_1985_2016

head(Average_League_salaries_1985_2016)

# Making year numeric
Average_salaries_hitters_1985_2016 <- transform(Average_salaries_hitters_1985_2016, year = as.numeric(year))

head(Average_League_salaries_1985_2016)
Average_League_salaries_1985_2016 <- transform(Average_League_salaries_1985_2016, year = as.numeric(year))

# Getting DH as seperate dataframe
DH <- subset(Average_position_salary, Position == 10)
DH

#### Pitchers 

# Scarping pitcher data from baseball reference
fetch_team_2016_pitchers <- function(url){
  url %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="players_value_pitching"]') %>%
    html_table()
}


team_salaries_pitchers_1985_2016 <- sapply(team_bref_url, fetch_team_2016_pitchers)
head(team_salaries_pitchers_1985_2016)
team_salaries_pitchers_1985_2016_1 <- team_salaries_pitchers_1985_2016

# Unlisting the list of lists 
team_salaries_pitchers_1985_2016_2 <- unlist(team_salaries_pitchers_1985_2016_1, recursive = FALSE)
# Making list into a data frame while putting the list id as a column
team_salaries_pitchers_1985_2016_2 <- rbindlist(team_salaries_pitchers_1985_2016_2, use.names = TRUE, idcol = "lst")
head(team_salaries_pitchers_1985_2016_2)
str(team_salaries_pitchers_1985_2016_2)

# Cleaning id column so that there is only the year left, and changing the name to year, doing this by seperating lst column
team_salaries_pitchers_1985_2016_2 <- team_salaries_pitchers_1985_2016_2 %>% separate(lst, c("id", "wtv", "wtv1", "wtv2", "team", "year"), sep = "/")
head(team_salaries_pitchers_1985_2016_2)

team_salaries_pitchers_1985_2016_2$id <- NULL
team_salaries_pitchers_1985_2016_2$wtv <- NULL
team_salaries_pitchers_1985_2016_2$wtv1 <- NULL
team_salaries_pitchers_1985_2016_2$wtv2 <- NULL

team_salaries_pitchers_1985_2016_2 <- team_salaries_pitchers_1985_2016_2 %>% separate(year, c("year", "wr"))
head(team_salaries_pitchers_1985_2016_2)
team_salaries_pitchers_1985_2016_2$wr <- NULL

# Getting rid of all rows with "Name"
team_salaries_pitchers_1985_2016_2 <- team_salaries_pitchers_1985_2016_2[team_salaries_pitchers_1985_2016_2$Name!="Name",]

team_salaries_pitchers_1985_2016_2$Salary[team_salaries_pitchers_1985_2016_2$Salary == ""] <- 0

# Cleaning Salary column by removing the commas, getting rid of NA values
team_salaries_pitchers_1985_2016_2$Salary <- gsub(",","", team_salaries_pitchers_1985_2016_2$Salary)

team_salaries_pitchers_1985_2016_2 <- team_salaries_pitchers_1985_2016_2 %>% separate(Salary, c("Salary1", "Salary"))

team_salaries_pitchers_1985_2016_2$Salary1 <- NULL

team_salaries_pitchers_1985_2016_2[is.na(team_salaries_pitchers_1985_2016_2)] <- 0

head(team_salaries_pitchers_1985_2016_2)

team_salaries_pitchers_1985_2016_3 <- team_salaries_pitchers_1985_2016_2[team_salaries_pitchers_1985_2016_2$Salary > 0,]
head(team_salaries_pitchers_1985_2016_3, n = 50)

# Getting rid of all rows with "Team Team"
team_salaries_pitchers_1985_2016_3 <- team_salaries_pitchers_1985_2016_3[team_salaries_pitchers_1985_2016_3$Name!="Team Total",]

# Making some columns numeric
team_salaries_pitchers_1985_2016_3 <- transform(team_salaries_pitchers_1985_2016_3, G = as.numeric(G),
          GS = as.numeric(GS),
          year = as.numeric(year),
          Salary = as.numeric(Salary),
          WAR = as.numeric(WAR))


# Finding out if a pitcher is a starter or reliever step 1. Get percentage of games started
team_salaries_pitchers_1985_2016_3$G_difference <- (team_salaries_pitchers_1985_2016_3$GS / team_salaries_pitchers_1985_2016_3$G) * 100
head(team_salaries_pitchers_1985_2016_3)

# Joining yearly salary average with pitcher database
team_salaries_pitchers_1985_2016_3  <- left_join(team_salaries_pitchers_1985_2016_3, Average_League_salaries_1985_2016) 
head(team_salaries_pitchers_1985_2016_3)

# Creating new column with scaled salary. The Average_League_salaries_1985_2016 dataframe already had the scales, so no need to redue them
team_salaries_pitchers_1985_2016_3$Scaled_2015_Salary <- team_salaries_pitchers_1985_2016_3$Salary * team_salaries_pitchers_1985_2016_3$scales

head(team_salaries_pitchers_1985_2016_3)

# If a pitcher had more than 50% of his appearances at GS then he's a started if not he's a reliever
reliver_salaries_1985_2016 <- team_salaries_pitchers_1985_2016_3[team_salaries_pitchers_1985_2016_3$G_difference < 50, ]
starters_salaries_1985_2016 <- team_salaries_pitchers_1985_2016_3[team_salaries_pitchers_1985_2016_3$G_difference > 50, ]
head(reliver_salaries_1985_2016)

reliver_salaries_1985_2016$Position <- "RP"
starters_salaries_1985_2016$Position <- "SP"

reliver_salaries_1985_2016[is.na(reliver_salaries_1985_2016)] <- 0
starters_salaries_1985_2016[is.na(starters_salaries_1985_2016)] <- 0

# Summarising the average salary, WAR, Scaled Salary, Median WAR, Median Scaled Salary, by starters and relievers
Reliever_WAR_Salary_1985_2015 <- reliver_salaries_1985_2016 %>% group_by(Position) %>% summarise(Average_Salary = mean(Salary), Average_WAR = mean(WAR), Scaled_2015_Salary = mean(Scaled_2015_Salary), Median_WAR = median(WAR), Median_Scaled_2015_Salary = median(Scaled_2015_Salary)) %>% as.data.frame()
head(Reliever_WAR_Salary_1985_2015)
Starter_WAR_Salary_1985_2015 <- starters_salaries_1985_2016 %>% group_by(Position) %>% summarise(Average_Salary = mean(Salary), Average_WAR = mean(WAR), Scaled_2015_Salary = mean(Scaled_2015_Salary), Median_WAR = median(WAR), Median_Scaled_2015_Salary = median(Scaled_2015_Salary)) %>% as.data.frame() 
head(Starter_WAR_Salary_1985_2015)   

# Summarising the yearly average salary, WAR, Scaled Salary, Median WAR, Median Scaled Salary, for starters and relievers
Average_reliver_salaries_1985_2016 <- reliver_salaries_1985_2016 %>% group_by(year) %>% summarise(Average_Salary = mean(Salary), Median_Salary = median(Salary), Average_WAR = mean(WAR), Scaled_2015_Salary = mean(Scaled_2015_Salary)) %>% as.data.frame()  
head(Average_reliver_salaries_1985_2016)  

Average_starters_salaries_1985_2016 <- starters_salaries_1985_2016 %>% group_by(year) %>% summarise(Average_Salary = mean(Salary), Median_Salary = median(Salary), Average_WAR = mean(WAR), Scaled_2015_Salary = mean(Scaled_2015_Salary)) %>% as.data.frame()
tail(Average_starters_salaries_1985_2016)

head(Average_position_salary)
library(ggplot2)
library(scales)

# Making positions a factor
Average_position_salary$Position <- as.factor(Average_position_salary$Position)

# Plotting growth of salaries for all positions (This dataframe is in the hitters section), inlcuding , DH, starting pitcher and relief pitchers. 
p <- ggplot()
p + geom_line(data = Average_position_salary, aes(x = year, y = Average_Salary, group = Position), colour = "grey", size = 1) +
    geom_line(data = DH, aes(x = year, y = Average_Salary), colour = "purple", size = 1, alpha = 0.7) +
    geom_line(data = Average_reliver_salaries_1985_2016, aes(x = year, y = Average_Salary), colour = "red", size = 1) +
    geom_line(data = Average_starters_salaries_1985_2016, aes(x = year, y = Average_Salary), colour = "darkorange", size = 1) +
    geom_line(data = Average_League_salaries_1985_2016, aes(x = year, y = Average_Salary), colour = "black", size = 1) +
    scale_y_continuous(labels = comma) +
    annotate("Text", label = "RP", x = 2016, y =  2271893.8, colour = "red", size = 4, fontface = "bold") +
    annotate("Text", label = "DH", x = 2016, y =  9912884.6, colour = "purple", size = 4, fontface = "bold") +
    annotate("Text", label = "SP", x = 2016, y =  6079260.8, colour = "darkorange", size = 4, fontface = "bold") +
    annotate("Text", label = "Lg Avg", x = 2017, y =  4361726.8, colour = "black", size = 4, fontface = "bold") +
    scale_x_continuous(breaks = c(1985,1990,1995,2000,2005,2010,2015)) +
    Julien_theme() + 
    ylab("Average Salary") + xlab("Year") + ggtitle("Reliever salaries aren't rising all that much", subtitle = "Average salary by position 1985-2015") + 
    labs(caption = "Data: Baseball-Reference") +
    theme(plot.margin = unit(c(.5, .5, 1, 1), "lines"))

# Ploting only adjusted reliever salaries
NN <- ggplot()
NN +  geom_line(data = Average_reliver_salaries_1985_2016, aes(x = year, y = Scaled_2015_Salary), size = 1, colour = "red") + 
  Julien_theme() + scale_y_continuous(labels = comma, limits = c(0, 4000000)) + 
  ylab("Adjusted Salary") + 
  ggtitle("Reliever salaries are actualy down, when values are adjsuted", subtitle = "Reliever salary adjusted to 2015 values, 1985-2015") + 
  annotate("Text", label = "Avg RP", x = 1993, y = 3446117, colour = "red", size = 3.5, fontface = "bold") + 
  labs(caption = "data: baseball reference")


# Plotting scatter plot of adjusted salaries and WAR by positions
d + ggplot()
d + geom_point(data = Positin_Player_WAR_Salary_1985_2015, aes(x = Average_WAR, y = Scaled_Salary_2015), colour = "grey", shape = 21, size = 2) + 
  geom_point(data = Positin_Player_WAR_Salary_1985_2015, aes(x = Average_WAR, y = Scaled_Salary_2015), colour = "grey", alpha = 0.5, size = 2) +
  geom_point(data = Reliever_WAR_Salary_1985_2015, aes(x = Average_WAR, y = Scaled_2015_Salary), colour = "red", alpha = 0.5, size = 2) + 
  geom_point(data = Reliever_WAR_Salary_1985_2015, aes(x = Average_WAR, y = Scaled_2015_Salary), colour = "red", shape = 21, size = 2) + 
  geom_point(data = Starter_WAR_Salary_1985_2015, aes(x = Average_WAR, y = Scaled_2015_Salary), colour = "Orange", shape = 21, size = 2) + 
  geom_point(data = Starter_WAR_Salary_1985_2015, aes(x = Average_WAR, y = Scaled_2015_Salary), colour = "Orange", alpha = 0.5, size = 2) + 
  geom_point(data = DH1, aes(x = Average_WAR, y = Scaled_Salary_2015), colour = "plum4", shape = 21, size = 2) + 
  geom_point(data = DH1, aes(x = Average_WAR, y = Scaled_Salary_2015), colour = "plum4", alpha = 0.5, size = 2) + 
  annotate("Text", label = "RP", x = 0.4455921, y = 2278681, colour = "red", size = 3, fontface = "bold") + 
  annotate("Text", label = "SP", x = 1.64929, y = 6278967, colour = "Orange", size = 3, fontface = "bold") +
  annotate("Text", label = "DH", x =  0.8306163, y = 6636086, colour = "plum4", size = 3, fontface = "bold") +
  annotate("Text", label = "C", x =  0.7731154, y =  2882830, colour = "gray58", size = 3) +
  annotate("Text", label = "CF", x =  1.7078003, y = 4726911, colour = "gray58", size = 3) +
  annotate("Text", label = "LF", x =  1.0697745, y = 4651919, colour = "gray58", size = 3) +
  annotate("Text", label = "1B", x =  1.3454031, y = 6502030, colour = "gray58", size = 3) +
  annotate("Text", label = "2B", x =  1.2887877, y = 3592776, colour = "gray58", size = 3) +
  annotate("Text", label = "SS", x =  1.2414242, y = 4368599, colour = "gray58", size = 3) +
  annotate("Text", label = "RF", x =  1.3965050, y = 5518527, colour = "gray58", size = 3) +
  annotate("Text", label = "3B", x =  1.3988178, y = 4441662, colour = "gray58", size = 3) +
  Julien_theme() + 
  scale_y_continuous(labels = comma) +
  ggtitle("Relievers aren't being overpaid, but designated hitters are", subtitle = "Average WAR and average salary 1985-2015, adjusted to 2015 salary values") + 
  xlab("Average WAR") + ylab("Average Salary") + 
  labs(caption = "Data: Baseball-Reference")

# Subsetting 2015 data to examine and explore
Average_position_salary_2015 <- subset(Average_position_salary, year == 2015) 
DH_2015 <- subset(DH, year ==2015)
Average_reliver_salaries_2015 <- subset(Average_reliver_salaries_1985_2016, year == 2015)
Average_starters_salaries_2015 <- subset(Average_starters_salaries_1985_2016, year == 2015)
Average_League_salaries_2015 <- subset(Average_League_salaries_1985_2016, year == 2015)
Average_starters_salaries_2015

# Scatter plot of WAR and salaries only 2015 data
m <- ggplot()
m + geom_point(data = Average_position_salary_2015, aes(x = Average_WAR, y = Average_Salary), colour = "grey", alpha = 0.5, size = 2) +
  geom_point(data = Average_position_salary_2015, aes(x = Average_WAR, y = Average_Salary), colour = "grey", shape = 21, size = 2) +
  geom_point(data = DH_2015, aes(x = Average_WAR, y = Average_Salary), color = "purple", alpha = 0.5, size = 2) +
  geom_point(data = DH_2015, aes(x = Average_WAR, y = Average_Salary), color = "purple", shape = 21, size = 2) +
  geom_point(data = Average_reliver_salaries_2015, aes(x = Average_WAR, y = Average_Salary), colour = "red", alpha = .5, size = 2) +
  geom_point(data = Average_reliver_salaries_2015, aes(x = Average_WAR, y = Average_Salary), colour = "red", shape = 21, size = 2) +
  geom_point(data = Average_starters_salaries_2015, aes(x = Average_WAR, y = Average_Salary), colour = "darkorange", alpha = 0.5, size = 2) +
  geom_point(data = Average_starters_salaries_2015, aes(x = Average_WAR, y = Average_Salary), colour = "darkorange", shape = 21, size = 2) +
  annotate("Text", label = "RP", x = 0.400813, y = 2471894, colour = "red", size = 3, fontface = "bold") + 
  annotate("Text", label = "SP", x = 1.491613, y = 6279261, colour = "Orange", size = 3, fontface = "bold") +
  annotate("Text", label = "DH", x =  1.038462, y = 9712885, colour = "plum4", size = 3, fontface = "bold") +
  annotate("Text", label = "C", x =  0.6887097, y = 2798519, colour = "gray58", size = 3) +
  annotate("Text", label = "CF", x =  1.7731707, y = 3658123, colour = "gray58", size = 3) +
  annotate("Text", label = "LF", x =  0.6014706, y = 5456994, colour = "gray58", size = 3) +
  annotate("Text", label = "1B", x =  1.4191489, y = 5765489, colour = "gray58", size = 3) +
  annotate("Text", label = "2B", x =  1.2957447, y = 3941548, colour = "gray58", size = 3) +
  annotate("Text", label = "SS", x =  1.3083333, y = 4677060, colour = "gray58", size = 3) +
  annotate("Text", label = "RF", x =  1.7468085, y = 7022474, colour = "gray58", size = 3) +
  annotate("Text", label = "3B", x =  1.7162791, y = 4648639, colour = "gray58", size = 3) +
  Julien_theme() +
  scale_y_continuous(labels = comma) +
  labs(caption = "Data: Baseball-Reference") +
  xlab("Average WAR") + ylab("Average Salary") + 
  ggtitle("In 2015, designated hitter's are still being overpaid", subtitle = "Average WAR and Salary by Position for 2015")


### DH analysis

# Subsetting the DH 
DH_salaries_1985_2016 <- subset(team_salaries_1985_2016_9, Position == 10)
head(DH_salaries_1985_2016)
DH_salaries_1985_2016 <- DH_salaries_1985_2016[with(DH_salaries_1985_2016, order(-Salary)),]
head(DH_salaries_1985_2016)

DH_salaries_1985_2016 <- transform(DH_salaries_1985_2016, Age = as.numeric(Age))

# Graphing the distribution of DH age 
gg <- ggplot(DH_salaries_1985_2016, aes(x = Age)) 
gg + geom_histogram(binwidth = 1, colour = "darkred", fill = "red3") + 
  Julien_theme() + ggtitle("Distribution of age for designated hitters, 1985-2015") + 
  labs(caption = "data: baseball reference")

head(DH_salaries_1985_2016)  

# Some exploratory data viz
gg_dh <- ggplot()
gg_dh + geom_point(data = DH_salaries_1985_2016, aes(x = WAR, y = Scaled_Salary_2015, size = Age)) +
  scale_y_continuous(labels = comma) + 
  scale_size(range = c(0,2)) + 
  stat_smooth(data = DH_salaries_1985_2016, aes(x = WAR, y = Scaled_Salary_2015, size = Age), method = "lm")

gg_dh <- ggplot()
gg_dh + geom_bar(data = DH_salaries_1985_2016, aes(x = Age, y = Scaled_Salary_2015), stat = "identity") +
  scale_y_continuous(labels = comma) 

# Grouping scaled DH salaries, and WAR by age. 
DH_salaries_1985_2016_average <- DH_salaries_1985_2016 %>% group_by(Age) %>% summarise(Scaled_Salary_2015 = mean(Scaled_Salary_2015), WAR = mean(WAR)) %>% as.data.frame()
DH_salaries_1985_2016_average

tail(DH_salaries_1985_2016, n = 200)

# Graphing DH adjusted average salary by age
v <- ggplot()
v + geom_bar(data = DH_salaries_1985_2016_average, aes(x = Age, y = Scaled_Salary_2015), colour = "grey60", fill = "grey60", stat = "identity") +
  scale_y_continuous(labels = comma) + 
  Julien_theme() + ylab("Average adjusted salary by age") +
  ggtitle("Designated hitters are usually old", subtitle = "Average DH salary by age from 1985-2015, adjusted to 2015 salary values") +
  geom_segment(aes(x = 31.5, y = 0, xend = 31.5, yend = 11000000), size = 1, colour = "purple") + 
  annotate("Text", label = "Average Age", x = 29.5, y = 10000000, color = "purple", fontface = "bold", size = 3) + 
  labs(caption = "data: baseball reference")




  
# More exploratory analysis
head(DH_salaries_1985_2016)
DH_salaries_1985_2016_average
mean(DH_salaries_1985_2016_average$Age)

b <- ggplot()
b + geom_bar(data = DH_salaries_1985_2016_average, aes(x = Age, y = WAR), stat = "identity") +
  scale_y_continuous(labels = comma)

# Getting just the names of the players who played DH
DH_salaries_1985_2016_Name <- select(DH_salaries_1985_2016, Name)
head(DH_salaries_1985_2016_Name)

# Merging them with team_salaries_1985_2016_merged in order to get the careers of every player who played at the DH position
DH_salaries_1985_2016_with_history  <- semi_join(team_salaries_1985_2016_merged, DH_salaries_1985_2016_Name) 
head(DH_salaries_1985_2016_with_history)
tail(DH_salaries_1985_2016_with_history, 50)
head(DH_salaries_1985_2016)
DH_salaries_1985_2016_with_history$Position[DH_salaries_1985_2016_with_history$Position == "NA"] <- 0
DH_salaries_1985_2016_with_history <- DH_salaries_1985_2016_with_history[complete.cases(DH_salaries_1985_2016_with_history[22]),]


# Getting rid of DH position
DH_salaries_1985_2016_without_DH <- DH_salaries_1985_2016_with_history[DH_salaries_1985_2016_with_history$Position!=10,]

# Getting count of every other position played by a DH
DH_most_common_position <- DH_salaries_1985_2016_without_DH %>% group_by(Position) %>% summarise(count = n()) %>% as.data.frame()
DH_most_common_position <- DH_most_common_position[complete.cases(DH_most_common_position),]

# Renaming columns
DH_salaries_1985_2016_without_DH$Position[DH_salaries_1985_2016_without_DH$Position == 9] <- "RF" 
DH_salaries_1985_2016_without_DH$Position[DH_salaries_1985_2016_without_DH$Position == 7] <- "LF" 
DH_salaries_1985_2016_without_DH$Position[DH_salaries_1985_2016_without_DH$Position == 8] <- "CF" 
DH_salaries_1985_2016_without_DH$Position[DH_salaries_1985_2016_without_DH$Position == 6] <- "SS" 
DH_salaries_1985_2016_without_DH$Position[DH_salaries_1985_2016_without_DH$Position == 5] <- "3B" 
DH_salaries_1985_2016_without_DH$Position[DH_salaries_1985_2016_without_DH$Position == 4] <- "2B" 
DH_salaries_1985_2016_without_DH$Position[DH_salaries_1985_2016_without_DH$Position == 3] <- "1B" 
DH_salaries_1985_2016_without_DH$Position[DH_salaries_1985_2016_without_DH$Position == 2] <- "C" 

# Graphing bar plot of positions by count 
z <- ggplot()
z + geom_bar(data = DH_most_common_position, aes(x = reorder(Position, count), y = count), stat = "identity", colour = "darkblue", fill = "darkblue") +
  Julien_theme() + coord_flip() + xlab("Position") + 
  ggtitle("What other positions did designated hitters play?", subtitle = "Count of designated hitters playing other positions, 1985-2015")


head(DH_salaries_1985_2016_without_DH)


str(DH_salaries_1985_2016_with_history)

mean(DH_salaries_1985_2016_with_history$Position)

# Sortig data by name
DH_salaries_1985_2016_with_history <- arrange(DH_salaries_1985_2016_with_history, Name, year, Acquired)

head(DH_salaries_1985_2016_with_history)

# Cleaning "Name" column by seperating and uniting columns in order to get rid of the *
DH_salaries_1985_2016_with_history <- DH_salaries_1985_2016_with_history %>% separate(Name, c("Name", "ok"), by = 2)
DH_salaries_1985_2016_with_history <- DH_salaries_1985_2016_with_history %>% unite(Name, Name, ok, sep = " ")

DH_salaries_1985_2016_with_history$Name <- as.factor(DH_salaries_1985_2016_with_history$Name)
# Exploring alex rodriguez
subset(DH_salaries_1985_2016_with_history, Name == "Alex Rodriguez")

# Renaming Alex Rodriguez name in 2015  which had improper name type. 
DH_salaries_1985_2016_with_history[93,21] <- "Free Agency"

# Grouping the values by Name and Acquired which had a Position of 10 (DH). 
# Before I had the whole players career. I only wanted the players acquired type for when he played at least one season of DH
DH_salaries_1985_2016_Acquired_DH <- DH_salaries_1985_2016_with_history %>% group_by(Name, Acquired) %>% filter(any(Position == 10)) %>% as.data.frame()


subset(DH_salaries_1985_2016_Acquired_DH, Acquired == "Amateur Free Agent")

# Changing Greg Vaughn from amateur draft no--sign to amateur drafted as he was drafted and signed in 1986. 
DH_salaries_1985_2016_Acquired_DH[491,21] <- "Amateur Draft"
DH_salaries_1985_2016_Acquired_DH[492,21] <- "Amateur Draft"
DH_salaries_1985_2016_Acquired_DH[493,21] <- "Amateur Draft"

# Count of DH players by acquired type
DH_salaries_1985_2016_Acquired_Count <- DH_salaries_1985_2016_Acquired_DH %>% group_by(Acquired) %>% summarise(Count = n()) %>% as.data.frame()

# Summarising mean of the WAR and Scaled Salary by Acquired type
DH_salaries_1985_2016_Acquired_average <- DH_salaries_1985_2016_Acquired_DH %>% group_by(Acquired) %>% summarise(Scaled_Salary_2015 = mean(Scaled_Salary_2015), WAR = mean(WAR)) %>% as.data.frame()
head(DH_salaries_1985_2016_Acquired_average)

# Plotting count of DH by acquired type
ll <- ggplot()
ll + geom_bar(data = DH_salaries_1985_2016_Acquired_Count, aes(x = reorder(Acquired, Count), y = Count), stat = "Identity", colour = "Purple", fill = "Purple") + 
  coord_flip() + Julien_theme() + 
  xlab("Acquired Type") + ggtitle("Most players who become designated hitters are acquired\nby free agency", subtitle = "Count of players who play at least one season at DH by acquired type, 1985-2015") + 
  labs(caption = "data: Baseball Reference")

# Plotting DH by acquired type and adjusted avarage salary
GG <- ggplot()
GG + geom_bar(data = DH_salaries_1985_2016_Acquired_average, aes(x = reorder(Acquired, Scaled_Salary_2015), y = Scaled_Salary_2015), stat = "Identity", colour = "deeppink4", fill = "deeppink4") + 
  coord_flip() + Julien_theme() + 
  xlab("Acquired Type") + ylab("Adjusted Average Salary") + scale_y_continuous(labels = comma) + 
  ggtitle("It's not clear that general managers are overpaying free agents who\ngo non to become designated hitters", subtitle = "Acquired type for players who played at least one season at DH and\naverage adjusted salary, 1985-2015") + 
  labs(caption = "data: baseball reference")
  
# Scatter plot of relationship between War and DH adjusted salary
PP <- ggplot()
PP + geom_point(data = DH_salaries_1985_2016_Acquired_average, aes(x = WAR, y = Scaled_Salary_2015, colour = Acquired)) +
  Julien_theme()

head(DH_salaries_1985_2016_Acquired_DH) 

# Gouping by Name and Acquired type and selecting the first value
DH_position_1985_2016_Acquired_first <- DH_salaries_1985_2016_Acquired_DH %>% group_by(Name, Acquired) %>% slice(1) %>% as.data.frame()

# Gouping by Name and Acquired type and selecting the last value
DH_position_1985_2016_Acquired_last <- DH_salaries_1985_2016_Acquired_DH %>% group_by(Name, Acquired) %>% slice(n()) %>% as.data.frame()
head(DH_position_1985_2016_Acquired_first)

# Finding the number of players who played DH in first year of acquisition
length(which(DH_position_1985_2016_Acquired_first$Position == 10)) 

# Calculating percentage
(151/305) * 100

# Finding the number of players who played DH in last year of acquisition and calculating percentage
length(which(DH_position_1985_2016_Acquired_last$Position == 10)) 
(224/305) * 100
