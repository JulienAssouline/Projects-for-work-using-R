#scraping single baseball reference page
library(XML)

url2 <- "http://www.baseball-reference.com/leagues/MLB/"

data2 <- as.data.frame(readHTMLTable(url2, stringsAsFactor = FALSE))
head(data2)
is.data.frame(data2)

#changing names of columns

Wins <- data.frame(Year =  teams_team_wins3000.Year, G = teams_team_wins3000.G, ARI = teams_team_wins3000.ARI,
ATL = teams_team_wins3000.ATL, BLA = teams_team_wins3000.BLA, BAL = teams_team_wins3000.BAL, BOS =  teams_team_wins3000.BOS,
CHC = teams_team_wins3000.CHC, CHW = teams_team_wins3000.CHW, CIN = teams_team_wins3000.CIN)

colnames(data2)[colnames(data2)=="teams_team_wins3000.Year"] <- "Year"

library(data.table)

setnames(data2, old=c("teams_team_wins3000.G", "teams_team_wins3000.ARI"), new=c("G","ARI"))

setnames(data2, old=c("teams_team_wins3000.ATL","teams_team_wins3000.BLA","teams_team_wins3000.BAL","teams_team_wins3000.BOS",
"teams_team_wins3000.CHC","teams_team_wins3000.CHW","teams_team_wins3000.CIN","teams_team_wins3000.CLE"), new=c("ATL","BLA","BAL","BOS","CHC","CHW","CIN","CLE"))

setnames(data2, old=c("teams_team_wins3000.COL","teams_team_wins3000.DET","teams_team_wins3000.HOU","teams_team_wins3000.KCR","teams_team_wins3000.ANA","teams_team_wins3000.LAD",
"teams_team_wins3000.FLA","teams_team_wins3000.MIL","teams_team_wins3000.MIN","teams_team_wins3000.NYM","teams_team_wins3000.NYY","teams_team_wins3000.OAK","teams_team_wins3000.PHI",
"teams_team_wins3000.PIT","teams_team_wins3000.SDP","teams_team_wins3000.SFG","teams_team_wins3000.SEA","teams_team_wins3000.STL","teams_team_wins3000.TBD","teams_team_wins3000.TEX",
"teams_team_wins3000.TOR","teams_team_wins3000.WSN"), new=c("COL","DET","HOU","KCR","ANA","LAD","FLA","MIL","MIN","NYM","NYY","OAK","PHI","PIT","SDP","SFG","SEA","STL","TBD","TEX","TOR","WSN"))

head(data2)

library(tidyr)

#making columns rows.
Wins <- gather(data2, Tm, wins, ARI:WSN, factor_key = TRUE)

head(Wins)

Wins

str(Wins)


#changing factors to numeric
Wins$wins = as.numeric(Wins$wins)
Wins$Year = as.numeric(Wins$Year)
transform(Wins, Year = as.numeric(Year))


#filtering
Wins1 <- Wins[Wins$Year > 1969,]

sapply(Wins, class)
sapply(Wins, mode)

Wins$Year <- as.numeric(as.character(Wins$Year))



head(Wins)
str(Wins)

#getting rid of NA values
head(Wins1)
tail(Wins1)
Wins1
Wins2 <- Wins1[complete.cases(Wins1),]
sum(is.na(Wins1))
colSums(is.na(Wins1))

tail(Wins2)

library(plyr)
library(dplyr)

#calculating average wins per year
avg_wins <- tapply(Wins2$wins, Wins2$Year, mean)

year = c(1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985,
          1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 
          2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)

avg_wins = c(80.95833, 80.70833, 77.41667, 80.91667, 80.87500, 80.54167, 80.79167, 80.88462,
             80.84615, 80.65385, 80.80769, 53.42308, 81.00000, 81.00000, 80.92308, 80.80769,
             80.84615, 80.96154, 80.69231, 80.88462, 80.96154, 80.92308, 81.00000, 81.00000,
             57.10714, 72.00000, 80.92857, 80.92857, 81.00000, 80.90000, 80.93333, 80.93333,
             80.83333, 80.96667, 80.93333, 81.00000, 80.96667, 81.03333, 80.93333, 81.00000,
             81.00000, 80.96667, 81.00000, 81.03333, 81.00000, 80.96667, 74.56667)


#making data into data.frame
average.wins <- data.frame(year, avg_wins)

average.wins

#extracting Brewers data
MIL1 <- subset(Wins2, Tm == "MIL")

is.data.frame(MIL1)

MIL1

library(ggplot2)
d <- ggplot(MIL1, aes(x = Year, y = wins, group = Tm))
d + geom_line()

#line plot with annotated square. Created legend, coloring specific lines, adding tick marks, changing y scale to commas 
l <- ggplot()
l + geom_line(data = Wins2, aes(x = year, y = wins, group = Tm), colour = "Grey") +
  geom_line(data = MIL1, aes(x = year, y = wins, colour = "Blue")) + ggtitle("Brewers Yearly Wins") +
  geom_line(data = average.wins, aes(x = year, y = avg_wins, colour = "red")) +
  scale_x_continuous(breaks = seq(1970, 2016, 5)) +  
  scale_colour_manual(labels = c("MIL", "League Avg"), values = c("blue", "red"))


