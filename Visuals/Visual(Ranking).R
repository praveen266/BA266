####3 Ranking
#Used to compare the position or performance of multiple items with respect to each other. Actual values matters somewhat less than the ranking.

##3.1 Ordered Bar chart:
#Ordered Bar Chart is a Bar Chart that is ordered by the Y axis variable. Just sorting the dataframe by the variable of interest isn't enough to order the bar chart. In order for the bar chart to retain the order of the rows, the X axis variable (i.e. the categories) has to be converted into a factor.

library(ggplot2)
data(mpg)
str(mpg)

cty_mpg= aggregate(mpg$cty, by= list(mpg$manufacturer), FUN= mean)       # aggregate
cty_mpg
colnames(cty_mpg)= c("make", "city_mileage")  #change the column names
cty_mpg= cty_mpg[order(cty_mpg$city_mileage),]  #sort
head(cty_mpg)
theme_set(theme_bw())

ggplot(cty_mpg, aes(make, city_mileage))+
  geom_bar(stat = "identity", width= 0.5, fill= "tomato3")+
  labs(title= "Ordered bar chart",
       subtitle = "make vs city_mileage",
       caption = "source:mpg")+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.6))

cty_mpg$make= factor(cty_mpg$make, levels = cty_mpg$make) # to retain the order in plot

##3.2 Lollipop chart:
#Lollipop charts conveys the same information as in bar charts by reducing the thick bars into thin lines

ggplot(cty_mpg, aes(make, city_mileage))+
  geom_point(size=3, col= "green")+
  geom_segment(aes(x= make,
                   xend= make,
                   y=0,
                   yend= city_mileage
                   ))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
##3.3 Dot plot:
#Dot plots are very similar to lollipops, but without the line and is flipped to horizontal position. It emphasizes more on the rank ordering of items with respect to actual values and how far apart are the entities with respect to each other.

library(scales)
theme_set(theme_classic())
ggplot(cty_mpg, aes(make, city_mileage))+
  geom_point(size=3, col= "green")+
  geom_segment(aes(x= make,
                   xend= make,
                   y= min(city_mileage),
                   yend= max(city_mileage)))+
  coord_flip()

##3.4 Dumbbell chart:





##3.5 Slope chart:
