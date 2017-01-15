library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Here i'm going to look at what causes Americans to die. 
# I got data from the Centers for Disease Control and Prevention. 

# Loading data into R

Cause_of_Deaths  <- read_excel("Cause of deaths 1999-2015.xlsx", sheet = "Sheet3")
head(Cause_of_Deaths, 100)

Cause_of_Deaths <- as.data.frame(Cause_of_Deaths, stringsAsFactors = FALSE)

# Cleaning the UCD - 15 Leading Causes of Death cause of death column and changing it's name. 
# I need to get rid of the # signs, and then seperate the parts in the brackets. 

Cause_of_Deaths$`UCD - 15 Leading Causes of Death` <- gsub("#", "", Cause_of_Deaths$`UCD - 15 Leading Causes of Death`) 

Cause_of_Deaths <- Cause_of_Deaths %>% separate(`UCD - 15 Leading Causes of Death`, c("Leading Causes of Death", "wtv"), sep = "[(]")
Cause_of_Deaths$wtv <- NULL

# Diseases of heart and Malignant neoplasms are by far the most prominent causes of deaths, 
# so we're going to subset them in order to colour them in the plot. 

Cancer <- subset(Cause_of_Deaths, `Leading Causes of Death` == "Malignant neoplasms ")
head(Cancer)
Heart <- subset(Cause_of_Deaths, `Leading Causes of Death` == "Diseases of heart ")
head(Heart)
str(Cause_of_Deaths)

# Now we create a lign graph of the data, while emphasising the Heart Disease and Cancer trends.  

pp <- ggplot()
pp + geom_line(data = Cause_of_Deaths, aes(x = `Year `, y = Deaths, group = `Leading Causes of Death`), colour = "grey") +
  Julien_theme() +
  theme(legend.position = "none") + 
  geom_line(data = Cancer, aes(x = `Year `, y = Deaths, group = `Leading Causes of Death`), colour = "Purple") +
  geom_line(data = Heart, aes(x = `Year `, y = Deaths, group = `Leading Causes of Death`), colour = "Red") +
  annotate("Text", label = "Cancer", x = 2000, y = 573091, size = 3.5, colour = "Purple", fontface = "bold") + 
  annotate("Text", label = "Heart Disease", x = 2005, y = 672091, size = 3.5, colour = "red", fontface = "bold") + 
  scale_y_continuous(labels = comma) +
  ggtitle("Heart disease related deaths are down, but cancer is up", subtitle = "US causes of death, 1999-2015") + 
  labs(caption = "Data: Centers for Disease Control and Prevention")



