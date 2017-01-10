
# Importing BP data on plate appearances
Brewers_PA <- read.csv(file.choose())


Brewers_PA
library(ggplot2)
library(ggalt)
install.packages("ggalt")
devtools::install_github("hrbrmstr/ggalt")

# Creating a dumbbell plot to examine the difference between plate appearances from 2015 to 2016

f <- ggplot()
f + geom_dumbbell(data = Brewers_PA, aes(x=PA.2015, xend=PA.2016, y=Name), colour = "darkblue", point.colour.l = "darkred", point.colour.r = "darkBlue", point.size.l = 2.5, point.size.r = 2.5) + 
  geom_text(data=data.frame(), aes(x=391, y= "Scooter Gennett", label="2015"), color="darkred", hjust=1, size=3, nudge_x=-10) +
  geom_text(data=data.frame(), aes(x=542, y= "Scooter Gennett", label="2016"),color="darkBlue", hjust=0, size=3, nudge_x=10) +
  theme(plot.background=element_rect(fill = "grey93", colour = "grey93")) +
  theme(plot.title=element_text(size = 11, face = "bold", hjust = 0)) + 
  theme(axis.text.x=element_text(size = 8)) +
  theme(axis.text.y=element_text(size = 8)) +
  theme(axis.title.x=element_text(size = 9)) + 
  theme(axis.title.y=element_text(size=9)) + ylab("") + xlab("") + 
  ggtitle("Brewers Change in Plate Appearance 2015-2016") + theme(axis.ticks=element_blank()) +
  theme(panel.grid.major.y=element_blank()) + scale_x_continuous(breaks = seq(0, 679, 200)) 
   

# Importing BP data on WARP

library(readxl)

excel_sheets("Brewers change in PA 2015-2016.xlsx")

Brewers_2016_WARP <- read_excel("Brewers change in PA 2015-2016.xlsx", sheet = "Sheet1")


head(Brewers_2016_WARP)
Brewers_2016_WARP <- as.data.frame(Brewers_2016_WARP, stringsAsFactors = FALSE)

library(data.table)

setnames(Brewers_2016_WARP, old = c("WARP 2016", "WARP 2015"), new = c("WARP_2016", "WARP_2015"))

# Creating a dumbbell plot to look at the difference between Brewers WARP from 2015 to 2016
s <- ggplot()
s + geom_dumbbell(data = Brewers_2016_WARP, aes(x=WARP_2015, xend=WARP_2016, y =Name), colour = "#458B74", point.colour.l = "#8B7355", point.colour.r = "#458B74", point.size.l = 2.5, point.size.r = 2.5) +
  geom_text(data=data.frame(), aes(x= -0.2, y= "Scooter Gennett", label="2015"), color="#8B7355", hjust=1, size=3, nudge_x=-0.1) +
  geom_text(data=data.frame(), aes(x= 2.0, y= "Scooter Gennett", label="2016"),color="#458B74", hjust=0, size=3, nudge_x=.1) +
  theme(plot.background=element_rect(fill = "grey93", colour = "grey93")) +
  theme(plot.title=element_text(size = 11, face = "bold", hjust = 0)) + 
  theme(axis.text.x=element_text(size = 8)) +
  theme(axis.text.y=element_text(size = 8)) +
  theme(axis.title.x=element_text(size = 9)) + 
  theme(axis.title.y=element_text(size=9)) + ylab("") + xlab("") + 
  ggtitle("Brewers Change in WARP 2015-2016") + theme(axis.ticks=element_blank()) +
  theme(panel.grid.major.y=element_blank()) + scale_x_continuous(breaks = seq(-1, 6, 1.5)) 






  
  


