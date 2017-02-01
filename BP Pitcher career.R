library(readxl)
library(dplyr)
library(tidyr)

# Reading in BP Career pitcher data
excel_sheets("BP Pitcher Career .xlsx")
BP_DRA_1951_2016 <- as.data.frame(read_excel("BP Pitcher Career .xlsx", sheet = "bpstats_02-01-2017"))

head(BP_DRA_1951_2016)

# Filter data so that each pitcher has pitched at least 900 innings in his career
# and had a DRA- above zero. 
BP_DRA_1951_2016_filter <- filter(BP_DRA_1951_2016, IP > 900 & `DRA-` > 0) 
# Sorting data to have the best DRA- career numbers of all time. 
BP_DRA_1951_2016_filter <- arrange(BP_DRA_1951_2016_filter, `DRA-`)

head(BP_DRA_1951_2016_filter, 20)

# Subsetting Randy Johnson, Billy Wagner, Pedro Martinez, Curt Schilling, Felix Hernandez, Sandy Koufax, and Klayton Kershaw
DRA_top_6 <- subset(BP_DRA_1951_2016_filter, `DRA-` < 75)
# Removing Kershaw
DRA_top_6 <- DRA_top_6[-c(7),]
DRA_top_6

library(ggplot2)
library(ggrepel)

# Creating scatter plot to showing relationship between DRA- and innings pitched between selected pitchers  
vvv <- ggplot()
vvv + geom_point(data = DRA_top_6, aes(x = `DRA-`, y = IP, colour = NAME, size = IP)) + 
  Julien_theme() + theme(legend.position = "none") + 
  scale_colour_manual(values = c("red", "grey", "grey", "orange", "blue", "grey")) + 
  annotate("Text", label = "Randy Johnson", x = 63.3, y = 4335.3, colour = "blue", size = 3, fontface = "bold") +
  annotate("Text", label = "Billy Wagner", x = 63.3, y = 1003, colour = "red", size = 3, fontface = "bold") +
  annotate("Text", label = "Pedro Martinez", x = 64, y = 3027.3, colour = "orange", size = 3, fontface = "bold") +
  annotate("Text", label = "Curt Schilling", x = 68, y = 3461.0, colour = "grey", size = 3, fontface = "bold") +
  annotate("Text", label = "Felix Hernandez", x = 72, y = 2615.7, colour = "grey", size = 3, fontface = "bold") +
  annotate("Text", label = "Sandy Koufax", x = 73.8, y = 2124.3, colour = "grey", size = 3, fontface = "bold") + 
  ggtitle("Randy Johnson is the fun fact", subtitle = "Top six career DRA- pitchers relationship with innings pitched, 1951-2016 ") +
  labs(caption = "Data: Baseball Prospectus")
  


  
