library(readxl)
library(dplyr)
library(tidyr)

### All of the data I got came from the Economic Policy Insitute. Here I'll start by leading it form my excel file 
# and once it's in I selected the columns I needed. 
# I wanted to first examine the trend of the gender pay gap, with women's salaries as a ratios of men.
# Fortunetly the data from Economic Policy Insitute is already clean so no need to do a lot of cleaning. 
excel_sheets("Gender Pay Gap US.xlsx")

Pay_gap_trend <- as.data.frame(read_excel("Gender Pay Gap US.xlsx", sheet = "Sheet4")) 

Pay_gap_trend <- select(Pay_gap_trend, Year, Women, Men)

str(Pay_gap_trend) 
Pay_gap_trend

# I changed one of the columns to numeric, and then added a value, which showed up as NA, but was suppose to be 0.8274
Pay_gap_trend <- transform(Pay_gap_trend, Women = as.numeric(Women)) 
Pay_gap_trend[37,2] <- 0.8274

# Making the women column into a percentage 
Pay_gap_trend$Women <- Pay_gap_trend$Women * 100
Pay_gap_trend$Difference <- Pay_gap_trend$Men - Pay_gap_trend$Women
head(Pay_gap_trend)

library(ggplot2)
library(ggalt)
library(scales)

str(Pay_gap_trend)

# Now, we'll create a graph displaying the trend with women's salaries as a ratios of men.
p <- ggplot()
p + geom_line(data = Pay_gap_trend, aes(x = Year, y = Women), size = 1, colour = "Purple") + 
  Julien_theme() + 
  geom_hline(yintercept = 100, colour = "black", linetype="dashed") + 
  scale_y_continuous(limits = c(0, 100), labels = c(0,25,50,75,"100 %")) + 
  ggtitle("The Gender Pay Gap is Stalling", subtitle = "Women's hourly wages as a ratio of men's hourly wages at the median, 1979-2015") +
  geom_ribbon(data = Pay_gap_trend, aes(x = Year, ymin = Women, ymax = Men), fill = "grey", alpha = 0.5) + 
  labs(caption = "Data: Economic Policy Institue") + ylab("") + 
  annotate("Text", label = "Gender Wage Gape", x = 1988, y = 89, colour = "grey93", size = 7, fontface = "bold")
  

# Next I wanted to look at the gender pay gap in 2015, by pay group. 

excel_sheets("Gender Pay Gap US.xlsx")

Gender_Pay_gap <- as.data.frame(read_excel("Gender Pay Gap US.xlsx", sheet = "Sheet2"))  

Gender_Pay_gap <- transform(Gender_Pay_gap, `Wage Percentile`, as.character(`Wage Percentile`))

str(Gender_Pay_gap)
Gender_Pay_gap

Gender_Pay_gap$`Wage Percentile` <- as.character(Gender_Pay_gap$`Wage Percentile`)

# The data was already clean, so we can go straight into making the graph
# Here I created a dumbbell graph to display the difference between men and women. 

l <- ggplot()
l + geom_dumbbell(data = Gender_Pay_gap, aes(x = Women, xend = Men, y = `Wage Percentile`), colour = "grey", point.colour.r = "lightgoldenrod4", point.colour.l = "hotpink4", point.size.l = 2, point.size.r = 2) +  
  Julien_theme() + xlab("") +
  annotate("Text", label = "Women", x = 48.03, y = 10.3, colour = "hotpink4", size = 3, fontface = "bold") + 
  annotate("Text", label = "Men", x = 65.06, y = 10.3, colour = "lightgoldenrod4", size = 3, fontface = "bold") +
  ggtitle("The higher the income group the bigger the gender gap", subtitle = "Hourly wages by gender and wage percentile, 2015") + 
  labs(caption = "Data: Economic Policy Insitute") 

# Importing more data
# This time I had to do some cleaning, formatting. 

Gender_Pay_Gap_education <- as.data.frame(read_excel("Gender Pay Gap US.xlsx", sheet = "Sheet5"))  
Gender_Pay_Gap_education

# I only wanted to look at 2015, for now, so I subsetted the 2015 data
Gender_Pay_Gap_education_2015 <- subset(Gender_Pay_Gap_education, Date == 2015)
  
 head(Gender_Pay_Gap_education_2015)
 
 # Then we need to seperate the men and women, columns, and changed the data from wide to long. 
 
 Women_education_2015 <- Gender_Pay_Gap_education_2015 %>% 
   select(`Women Less than HS`, `Women High school`, `Women Some college`, `Women College`, `Women Advanced degree`, Date) %>%
   gather(Education, Wages_Women, `Women Less than HS`:`Women Advanced degree`, factor_key=TRUE)
  
 Men_education_2015 <- Gender_Pay_Gap_education_2015 %>% 
   select(`Men Less than HS`, `Men High school`, `Men Some college`, `Men College`, `Men Advanced degree`, Date) %>%
   gather(Education, Wages_Men, `Men Less than HS`:`Men Advanced degree`, factor_key=TRUE)
 
 # Now I want to merge the data again, so in order to do that, we'll merge by date and Education.
 # Meaning that we need to get rid of the gender variable in the education column.
 
 Women_education_2015$Education <- gsub("Women", "", Women_education_2015$Education) 
 Women_education_2015
 
 Men_education_2015$Education <- gsub("Men", "", Men_education_2015$Education)
 
 Gender_Pay_Gap_education_2015_long <- left_join(x = Men_education_2015, y = Women_education_2015) 
 str(Gender_Pay_Gap_education_2015_long)
 
 # Now we can visualize the data, again, creating a dumbbell plot to show the difference.
gg <- ggplot()
gg + geom_dumbbell(data = Gender_Pay_Gap_education_2015_long, aes(x = Wages_Women, xend = Wages_Men, y = reorder(Education, Wages_Men)), colour = "grey", point.colour.r = "firebrick3", point.colour.l = "darkorchid3", point.size.l = 2, point.size.r = 2) + 
     Julien_theme() + xlab("Average Hourly Wages") + ylab("") + 
  ggtitle("The higher the education level, the higher the gender wage gap", subtitle = "Average hourly wages, by gender and education, 2015") + 
  annotate("Text", label = "Men", x = 45.85, y = 5.3, size = 3, fontface = "bold", colour = "firebrick3") +
  annotate("Text", label = "Women", x = 33.65, y = 5.3, size = 3, fontface = "bold", colour = "darkorchid3") + 
  labs(caption = "Data: Economic Policy Insitute")

# Next, I want to look at the trend between higher education men and women. 
# Fortunetly the original data set is in the perfect format to do that. 
# No need for cleaning I can get straight t creating a graph.

bb <- ggplot()
  bb + geom_line(data = Gender_Pay_Gap_education, aes(x = Date, y = `Women Advanced degree`), colour = "deeppink4", size = 1) + 
  geom_line(data = Gender_Pay_Gap_education, aes(x = Date, y = `Men Advanced degree`), colour = "dodgerblue4", size = 1) +
  geom_line(data = Gender_Pay_Gap_education, aes(x = Date, y = `Women College`), colour = "deeppink", size = 1) +
  geom_line(data = Gender_Pay_Gap_education, aes(x = Date, y = `Men College`), colour = "dodgerblue", size = 1) +
  annotate("Text", label = "Men Advanced", x = 2014, y = 46.84, colour = "dodgerblue4", size = 3, fontface = "bold") +
  annotate("Text", label = "Women Advanced", x = 2014, y = 31.65, colour = "deeppink4", size = 3, fontface = "bold") +
  annotate("Text", label = "Men College", x = 2014, y = 36.23, colour = "dodgerblue", size = 3, fontface = "bold") +
  annotate("Text", label = "Women College", x = 2014, y = 24.51, colour = "deeppink", size = 3, fontface = "bold") +
  Julien_theme() + theme(plot.margin = unit(c(1, 1, 1, 1), "lines")) +
  ylab("Wages per hour") + xlab("") + 
  ggtitle("Women with advanced degrees make less than men with college degrees", subtitle = "Average hourly wages, by gender for advanced and college degrees, 1973-2015")
