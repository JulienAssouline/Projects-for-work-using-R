library(xml2)
library(rvest)
library(tidyr)
library(dplyr)

# Scraping wikipedia for Cy young data, first NL and AL.
Cy_young_url <- "https://en.wikipedia.org/wiki/Cy_Young_Award" 

NL_Cy_Young_1967_2016 <- Cy_young_url %>% 
  read_html() %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/table[4]') %>%
  html_table() %>%
  as.data.frame()

head(NL_Cy_Young_1967_2016)

AL_Cy_Young_1967_2016 <- Cy_young_url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/table[5]') %>%
  html_table() %>%
  as.data.frame()

head(AL_Cy_Young_1967_2016)
head(NL_Cy_Young_1967_2016)  
 

# Importing Baseball Prospectus pitcher data. 
BP_Pitcher_1967_2016  <- read.csv(file.choose())
head(BP_Pitcher_1967_2016)


# Subsetting AL and NL data. 
BP_Pitcher_1967_2016_AL <- subset(BP_Pitcher_1967_2016, League == "AL")
head(BP_Pitcher_1967_2016_AL)
BP_Pitcher_1967_2016_NL <- subset(BP_Pitcher_1967_2016, League == "NL")
BP_Pitcher_1967_2016_NL

# Getting the average WARP per year for AL and NL pitchers
Best_Pitchers_By_WARP_NL <- BP_Pitcher_1967_2016_NL %>% group_by(YEAR) %>% summarise(WARP = max(PWARP)) %>% as.data.frame()
Best_Pitchers_By_WARP_AL <- BP_Pitcher_1967_2016_AL %>% group_by(YEAR) %>% summarise(WARP = max(PWARP)) %>% as.data.frame()

# Getting rid of extra name in pitcher column by seperating the ',' and then new column. 
AL_Cy_Young_1967_2016 <- separate(data = AL_Cy_Young_1967_2016, col = Pitcher, c("Name", "Pitcher"), sep = ",")
NL_Cy_Young_1967_2016 <- separate(data = NL_Cy_Young_1967_2016, col = Pitcher, c("Name", "Pitcher"), sep = ",")

AL_Cy_Young_1967_2016$Name <- NULL
NL_Cy_Young_1967_2016$Name <- NULL

NL_Cy_Young_1967_2016

# Getting rid of the * symbole
NL_Cy_Young_1967_2016$Pitcher <- gsub('[*]','', NL_Cy_Young_1967_2016$Pitcher)
head(NL_Cy_Young_1967_2016, n = 100)

AL_Cy_Young_1967_2016$Pitcher <- gsub('[*]','', AL_Cy_Young_1967_2016$Pitcher)

# The final problem, is that the first names are now attached. 
#I couldn't figuer out how to seperate or get rid of the double names,
# so I manually did it in excel and then re-imported the data. 
# I will update this file, once I figuer out how to solve that problem.


library(readxl)
# Importing AL cy young data
 excel_sheets("Cy Young AL.xlsx")
 
 AL_Cy_Young_1967_2016 <- read_excel("Cy Young AL.xlsx", sheet = "Cy Young AL.csv")
 
AL_Cy_Young_1967_2016  <- as.data.frame(AL_Cy_Young_1967_2016, stringsAsFactors = FALSE)
 head(AL_Cy_Young_1967_2016)
 head(BP_Pitcher_1967_2016_AL)
 AL_Cy_Young_1967_2016

# Changing names
 names(AL_Cy_Young_1967_2016) <- c("YEAR", "NAME", "Team", "Record", "Saves", "era", "K's")
 
# We now need to get the WARP values for the pitchers who won the Cy young in the AL
# Joining the Cy young winners data base with the overall BP data 
AL_Cy_Young_1967_2016_BP  <- semi_join(BP_Pitcher_1967_2016_AL, AL_Cy_Young_1967_2016)
 
head(AL_Cy_Young_1967_2016_BP) 
AL_Cy_Young_1967_2016_BP

# Importing NL Cy Young pitchers
NL_Cy_Young_1967_2016
excel_sheets("Cy Young AL.xlsx")

NL_Cy_Young_1967_2016 <- read_excel("Cy Young AL.xlsx", sheet = "Sheet3")
head(NL_Cy_Young_1967_2016)
NL_Cy_Young_1967_2016
NL_Cy_Young_1967_2016 <- as.data.frame(NL_Cy_Young_1967_2016, stringsAsFactors = FALSE)

names(NL_Cy_Young_1967_2016) <- c("YEAR", "NAME", "Team", "Record", "Saves", "era")

# We now need to get the WARP values for the pitchers who won the Cy young in the NL
# Joining the Cy young winners data base with their BP data 
NL_Cy_Young_1967_2016_BP  <- semi_join(BP_Pitcher_1967_2016_NL, NL_Cy_Young_1967_2016)

head(NL_Cy_Young_1967_2016_BP)
str(NL_Cy_Young_1967_2016_BP)

# Now, we need to join the CY Young winners the BP WARP leaders. So we do that for the AL and NL
NL_Cy_Young_1967_2016_and_Leaders <- left_join(Best_Pitchers_By_WARP_NL, NL_Cy_Young_1967_2016_BP)
NL_Cy_Young_1967_2016_and_Leaders1 <- NL_Cy_Young_1967_2016_and_Leaders[-c(31),]
head(NL_Cy_Young_1967_2016_and_Leaders1)
AL_Cy_Young_1967_2016_and_Leaders <- left_join(Best_Pitchers_By_WARP_AL, AL_Cy_Young_1967_2016_BP)
AL_Cy_Young_1967_2016_and_Leaders

# Getting the difference between the BP WAR leaders and CY young winners WARP
NL_Cy_Young_1967_2016_and_Leaders1$PWARP_Difference <- NL_Cy_Young_1967_2016_and_Leaders1$WARP - NL_Cy_Young_1967_2016_and_Leaders1$PWARP
NL_Cy_Young_1967_2016_and_Leaders1

AL_Cy_Young_1967_2016_and_Leaders$PWARP_Difference <- AL_Cy_Young_1967_2016_and_Leaders$WARP - AL_Cy_Young_1967_2016_and_Leaders$PWARP
AL_Cy_Young_1967_2016_and_Leaders

library(ggplot2)
library(ggalt)
install.packages("ggalt")
devtools::install_github("hrbrmstr/ggalt", force = TRUE)
library(ggrepel)

# Creating annotations 
annot <- read.table(text = 
"PWARP|YEAR|text
9.65|1978| Sometimes they get it right", sep = "|", header = TRUE, stringsAsFactors = FALSE) 
head(annot)

annot1 <- read.table(text = 
"PWARP|YEAR|text
0.09|1982| Pete Vuckovich, 1982, 0.09 PWARP... YIKES!", sep = "|", header = TRUE, stringsAsFactors = FALSE) 
head(annot1)

annot2 <- read.table(text = 
"PWARP|YEAR|text
10.61|1985| Sometimes they get it right", sep = "|", header = TRUE, stringsAsFactors = FALSE) 
head(annot2)


# Creating a time series dumbbell plot to visualize the difference between the BP WARP leaders and the CY young winners WARP values for the AL
p <- ggplot()
p + geom_dumbbell(data = AL_Cy_Young_1967_2016_and_Leaders, aes(x=PWARP, xend = WARP, y = YEAR), colour = "gray48", point.colour.r = "darkred", point.size.l = 2, point.size.r = 2) +
  coord_flip() +
  ggtitle("How Good is the BBWAA at Voting for the Cy Young in the AL?", subtitle = "Difference Between Cy Young Winner and Best Pitcher by PWARP, 1967-2016") +
  labs(caption = "Data: Baseball Prospectus, Wikipedia") + ylab("") + 
  geom_text_repel(data = annot, aes(x=PWARP, y=YEAR, label = text), size = 3, force = 10, fontface = "bold") +
  geom_text_repel(data = annot1, aes(x=PWARP, y=YEAR, label = text), size = 3, fontface = "bold", arrow = arrow(length = unit(0.01, 'npc')), nudge_y = 5) + 
  annotate("Text", label = "PWARP Leader", x = 10.8, y = 1973, colour = "darkred", fontface = "bold", size = 3) + 
  annotate("Text", label = "Cy Young Winner", x = 3.8, y = 1973, colour = "gray48", fontface = "bold", size = 3) +
  Julien_theme() +
  scale_y_continuous(breaks = seq(1970,2016,5)) +
  scale_x_continuous(breaks = seq(0,10,2))

# Creating a time series dumbbell plot to visualize the difference between the BP WARP leaders and the CY young winners WARP values for the NL

z <- ggplot() 
z + geom_dumbbell(data = NL_Cy_Young_1967_2016_and_Leaders1, aes(x=PWARP, xend = WARP, y = YEAR), colour = "darkslategray3", point.colour.r = "darkslategray", point.size.l = 2, point.size.r = 2) +
  coord_flip() +
  annotate("Text", label = "PWARP Leader", x = 9.5, y = 2015, colour = "darkslategray", fontface = "bold", size = 3) + 
  annotate("Text", label = "Cy Young Winner", x = 5.5, y = 2015, colour = "darkslategray3", fontface = "bold", size = 3) + 
  geom_text_repel(data = annot2, aes(x=PWARP, y=YEAR, label = text), size = 3, fontface = "bold", force= 10) + 
  ggtitle("How Good is the BBWAA at Voting for the Cy Young in the NL?", subtitle = "Difference Between Cy Young Winner and Best Pitcher by PWARP, 1967-2016") +
  labs(caption = "Data: Baseball Prospectus, Wikipedia") + ylab("") + Julien_theme() +
  scale_y_continuous(breaks = seq(1970,2016,5)) + 
  scale_x_continuous(breaks = seq(1,13,2)) +
  annotate("Text", label = "Steve Bedrosian, 1.2 PWARP", x = 1.20, y = 1993, fontface = "bold", size = 3) +
  annotate("Text", label = "No-Brainer! Randy Johnson, 12.82 PWARP", x= 13.2, y = 2001, fontface = "bold", size = 3)
  
 
b <- ggplot(Best_Pitchers_By_WARP_AL, aes(x= YEAR, y=WARP)) 
b + geom_line(colour = "mediumorchid4", size = 1) + Julien_theme() 

n <- ggplot(Best_Pitchers_By_WARP_NL, aes(x= YEAR, y=WARP)) 
n + geom_line(colour = "lightsalmon4", size = 1) + Julien_theme() 
AL_Cy_Young_1967_2016_and_Leaders
 

