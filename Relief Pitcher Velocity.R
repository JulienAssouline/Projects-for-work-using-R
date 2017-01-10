# Importing relief pitchers velocity

excel_sheets("velocity pitchers 2007-2016.xlsx")
relief_velo_2013_2016 <- read_excel("velocity pitchers 2007-2016.xlsx", sheet = "2013-2016 velo")
relief_velo_2013_2016 <- as.data.frame(relief_velo_2013_2016, stringsAsFactors = FALSE)
head(relief_velo_2013_2016)

excel_sheets("velocity pitchers 2007-2016.xlsx")
relief_fg_value_2013_2016 <- read_excel("velocity pitchers 2007-2016.xlsx", sheet = "2013-2016")
relief_fg_value_2013_2016 <- as.data.frame(relief_fg_value_2013_2016, stringsAsFactors = FALSE)
head(relief_fg_value_2013_2016)

# Merging relief pitcher velocity and FG values

relief_value_merged <- inner_join(relief_fg_value_2013_2016, relief_velo_2013_2016) 
head(relief_value_merged, n = 10)

# Calculation strikeout to walk ratios
relief_value_merged$`K/BB` <- relief_value_merged$`K/9` - relief_value_merged$`BB/9` 

# Subsetting Koji Uehara Aroldis Chapman, Andrew Miller, Kenley Jansen, and Dellin Betances data

Koji_Uehara_2013_2016 <- subset(relief_value_merged, Name == "Koji Uehara") 
head(Koji_Uehara_2013_2016)

Chapman_2013_2016 <- subset(relief_value_merged, Name == "Aroldis Chapman") 
head(Chapman_2013_2016)

Miller_2013_2016 <- subset(relief_value_merged, Name == "Andrew Miller")
head(Miller_2013_2016)

Jensen_2013_2016 <- subset(relief_value_merged, Name == "Kenley Jansen")
head(Jensen_2013_2016)

Betances <- subset(relief_value_merged, Name == "Dellin Betances")
head(Betances)

# plotting relationship between strikeouts and velocity
b <- ggplot() 
b + geom_point(data = relief_value_merged, aes(x = vFA, y = `K/9`), colour = "grey", size = 2, alpha = .7) + 
geom_point(data = Koji_Uehara_2013_2016, aes(x = vFA, y = `K/9`), colour = "red", size = 2, alpha = .5) +
geom_point(data = Koji_Uehara_2013_2016, aes(x = vFA, y = `K/9`), colour = "red", shape = 21, size = 2) +
geom_point(data = Chapman_2013_2016, aes(x = vFA, y = `K/9`), colour = "dimgray", shape = 21, size = 2) + 
  annotate("Text", label = "UEHARA", x = 88, y = 12.3, colour = "red", size = 3, fontface = "bold") + 
  annotate("Text", label = "Chapman", x = 98.5, y = 15.80, colour = "dimgray", size = 3) +
  Julien_theme() + 
  ggtitle("Koji Uehara doesn't fit the traditional hard thrower high strikeout mold", subtitle = "Relationship between reliever strikeouts and fastball velocity from 2013-2016") +
  labs(caption = "data: Fangraphs") + 
  xlab("Velocity") + ylab("Strikeouts per nine")

# Plotting relationship between velocity and K/BB
z <- ggplot()
z + geom_point(data = relief_value_merged, aes(x = vFA, y = `K/BB`), colour = "grey", alpha = .7) +
  geom_point(data = Koji_Uehara_2013_2016, aes(x = vFA, y = `K/BB`), colour = "red", size = 2, alpha = .5) +
  geom_point(data = Koji_Uehara_2013_2016, aes(x = vFA, y = `K/BB`), colour = "red", shape = 21, size = 2) +
  geom_point(data = Jensen_2013_2016, aes(x = vFA, y = `K/BB`), colour = "blue", size = 2, alpha = .5) +
  geom_point(data = Jensen_2013_2016, aes(x = vFA, y = `K/BB`), colour = "blue", size = 2, shape = 21) +
  geom_point(data = Chapman_2013_2016, aes(x = vFA, y = `K/BB`), colour = "dimgray", size = 2, shape = 21) +
  geom_point(data = Betances, aes(x = vFA, y = `K/BB`), colour = "dimgray", size = 2, shape = 21) +
  geom_point(data = Miller_2013_2016, aes(x = vFA, y = `K/BB`), colour = "brown4", size = 2, alpha = .5) +
  geom_point(data = Miller_2013_2016, aes(x = vFA, y = `K/BB`), colour = "brown4", shape = 21, size = 2) +
  annotate("Text", label = "UEHARA", x = 88, y = 10.5, colour = "red", size = 3, fontface = "bold") +
  annotate("Text", label = "Miller", x = 93.5, y = 12.3, colour = "brown4", size = 3) +
  annotate("Text", label = "Jensen", x = 94.1, y = 11.3, colour = "blue", size = 3) +
  annotate("Text", label = "Chapman", x = 99.5, y = 12.3, colour = "dimgray", size = 3) +
  annotate("Text", label = "Betances", x = 97, y = 11.4, colour = "dimgray", size = 3) +
  Julien_theme() +
  ggtitle("Uehara is one of the more unique pitchers in baseball", subtitle ="Reliever strikeout to walk ratio and fastball velocity relationsip 2013-2016") +
  labs(caption = "data: Fangraphs") + xlab("Velocity") + ylab("Strikeout and walk ratio") 


#exploring relationship between velocity and walks
v <- ggplot()
v + geom_point(data = relief_value_merged, aes(x = vFA, y = `BB/9`), colour = "grey") +
  geom_point(data = Koji_Uehara_2013_2016, aes(x = vFA, y = `BB/9`), colour = "red", size = 2, alpha = .5) +
  geom_point(data = Koji_Uehara_2013_2016, aes(x = vFA, y = `BB/9`), colour = "red", shape = 21, size = 2) 

# Sorting walks
relief_value_merged1 <- relief_value_merged[with(relief_value_merged, order(`BB/9`)),]
head(relief_value_merged, n = 10)

# Quick linear model
summary(lm(`K/BB` ~ vFA, data = relief_value_merged))

summary(lm(`K/9` ~ vFA, data = relief_value_merged))


# Sorting by velocity
velocity_value_fg_merge1 <- velocity_value_fg_merge[with(velocity_value_fg_merge, order(-vFA)),]
head(velocity_value_fg_merge1)

tail(velocity_value_fg_merge3, n = 100)

#removing empty values and NA values
velocity_value_fg_merge2 <- velocity_value_fg_merge1[-c(2165,1497:1488),]
velocity_value_fg_merge3 <- velocity_value_fg_merge2[-c(1488:1497),]
velocity_value_fg_merge3 <- velocity_value_fg_merge3[!is.na(velocity_value_fg_merge3$vFA),]

library(dplyr)

# Getting the average yearly velocity
Avg_velo_2007_2016 <- velocity_value_fg_merge3 %>% group_by(Year) %>% summarise(Avg.vFA = mean(vFA)) %>% as.data.frame()
Avg_velo_2007_2016

# Plotting Average yearly velocity
m <- ggplot()
m + geom_point(data = Avg_velo_2007_2016, aes(x = Year, y = Avg.vFA), colour = "red") + 
geom_line(data = Avg_velo_2007_2016, aes(x = Year, y = Avg.vFA), colour = "red") + Julien_theme() +
  ggtitle("Relievers keep throwing harder", subtitle = "Average reliever velocity 2007-2016") +
  ylab("Velocity") + xlab("") + labs(caption = "data: Fangraphs")
  



