##### Primarily, there are 8 types of objectives to construct plots.
##1. Correlation:
# Scatterplot, Scatterplot with encircling,Jitter plot, Counts charts, Bubble Chart, Animated Bubble plot, Marginal Histogram/Boxplot, Correlogram

##2. Deviation:
# Diverging Bars, Diverging Lollipop chart, Diverging Dot Plot, Area chart

##3. Ranking:
# Ordered bar chart,Lollipop chart, Dot plot, Slope chart, Dumbbell plot

##4. Distribution:
# Histogram, Density plot, Box Plot, Dot+ Box plot, Tufte Boxplot, Violin Plot, Population pyramid

##5. Composition
# Waffle chart, Pie chart, Treemap, Bar chart

##6. Change:
# Time Series plots(From a data frame, Format to maonthly X axis, Format to yearly X axis, From long data format, From wide data format), Stacked area chart, Calender heat map, Slope chart, Seasonal plot

##7. Groups:
# Dendogram, Clusters

##8. Spatial:
# Open street map, Google road map, Google hybrid map


library(ggplot2)
##turn-off scientific notation like 1e+48
options(scipen = 999)
## Pre-set the bw theme
data("midwest")
summary(midwest)
str(midwest)
View(midwest)

#### 1.Correlation
##1.1 Scatterplot: to understand the nature of relationship between two variables(1st choice). "geom_point" for scatterplot, "geom_smooth" draws smoothing line(based on loess), "method= 'lm'" for best fit setting.

gg= ggplot(midwest, aes(x= area, y= poptotal))+
  geom_point(aes(col= state, size= popdensity))+
  geom_smooth(method = "loess", se= F)+
  xlim(c(0,0.1))+
  ylim(c(0,500000))+
  labs(subtitle = "Area Vs Population", y= "Population", x="Area", title = "Scatterplot", caption = "Source: Midwest")
plot(gg)

ggplot(midwest,aes(area,poptotal))+        
  geom_point()                           # for scatter point

ggplot(midwest, aes(area, poptotal))+
  geom_point(aes(col= state))            # for coloring of points

ggplot(midwest, aes(area, poptotal))+
  geom_point(aes(col= state,size= popdensity)) # For size of points

ggplot(midwest,aes(x= area, y= poptotal))+
  geom_point(aes(col= state, size= popdensity))+
  geom_smooth(method = "lm", se=F)       # 'auto', 'glm', 'lm', 'gam', 'loess' or function(for drawing smoothing line)

ggplot(midwest,aes(x= area, y= poptotal))+
  geom_point(aes(col= state, size= popdensity))+
  geom_smooth(method = "gam", se=F)+
  xlim(c(0,0.1))+
  ylim(c(0,500000))                      # For separating outliers(by putting limits)                    
g=ggplot(midwest,aes(x= area, y= poptotal))

g+geom_point(aes(col= state, size= popdensity))+
  geom_smooth(method = "gam", se=F)+
  xlim(c(0,0.1))+
  ylim(c(0,500000))+
  labs(subtitle = "Area Vs Population",
       y= "Population",
       x="Area", 
       title = "Scatterplot", 
       caption = "Source: Midwest")       # For labeling

##1.2 Scatterplot with encircling :
#To encirlce certain special group of points or region in the chart so as to draw the attention to those peculiar cases.

library(ggalt)
midwest_select= midwest[midwest$poptotal> 350000 &
                          midwest<= 500000 &
                          midwest$area> 0.01 &
                          midwest$area< 0.1,] 
ggplot(midwest, aes(x= area, y= poptotal))+
  geom_point(aes(col= state, size= popdensity))+
  geom_smooth(method = "loess", se= F)+
  xlim(c(0,0.1))+
  ylim(c(0,500000))+
  geom_encircle(aes(x= area, y= poptotal),
                data= midwest_select,
                color= "red",
                size= 2,
                expand= 0.08)+
  labs(subtitle = "Area Vs Population",
       y= "Population",
       x= "Area",
       title = "Scatterplot + Encircle",
       caption = "Source: Midwest")
gg= ggplot(midwest, aes(x= area, y= poptotal))+
  geom_point(aes(col= state, size= popdensity))+
  geom_smooth(method = "loess", se= F)+
  xlim(c(0,0.1))+
  ylim(c(0,500000))
g+geom_encircle(aes(area, poptotal),
                midwest_select)
  
g+geom_encircle(aes(area, poptotal),
                midwest_select,
                size=3)
g+geom_encircle(aes(area, poptotal),
                midwest_select,
                size=3,
                expand= 0.1)
##1.3 Jitter plot:

data("mpg")
dim(mpg)  
names(mpg)
#Scatterplot
g=ggplot(mpg,aes(cty, hwy))
g+geom_point()+
  geom_smooth(method = "lm", se= F)+
  labs(subtitle = "mpg: city vs highway mileage",
       y= "hwy", x= "cty",
       title = "scatterplot with overlapping points",
       caption = "source: mpg")
#The original data has 234 data points but the chart seems to display fewer points.This is because there are many overlapping points appearing as a single dot.

g+geom_point()+
  geom_smooth(method = "lm", se= F)+
  geom_jitter(width = 1, size= 1)+     
  labs(subtitle ="mpg: city vs highway mileage",
        y= "hwy", x= "cty",
       title = "Jiiter points")
g+geom_point()+
  geom_smooth(method = "lm", se= F)+
  geom_jitter(width = 0.5, size= 1)+
  labs(subtitle ="mpg: city vs highway mileage",
        y= "hwy", x= "cty",
       title = "Jiiter points")
g+geom_point()+
  geom_smooth(method = "lm", se= F)+
  geom_jitter(width = 0.1, size= 1)+
  labs(subtitle ="mpg: city vs highway mileage",
        y= "hwy", x= "cty",
       title = "Jiiter points")
#More the width, more the points are moved jittered from their original position.

##1.4 Counts chart:

#The second option to overcome the problem of data points overlap is to use what counts chart. Whereever there is more points overlap, the size of the circle gets bigger.

#Scatterplot
g+ geom_count(col= "tomato3", show.legend = F)+
  labs(subtitle ="mpg: city vs highway mileage",
        y= "hwy", x= "cty",
       title = "counts plot")

##1.5: Bubble plot:   
#Bubble charts are more suitable if you have 4-Dimensional data where two of them are numeric (X and Y) and one other categorical (color) and another numeric variable (size).

str(mpg)
mpg_select= mpg[mpg$manufacturer %in% c("audi","ford", "honda", "hyundai"),]
g= ggplot(mpg_select, aes(displ, cty)) + 
  labs(subtitle="mpg: Displacement vs City Mileage",
       title="Bubble chart")

g+geom_jitter(aes(col= manufacturer, size= hwy))+
  geom_smooth(aes(col= manufacturer, method= "lm",se= F))

g+geom_jitter(aes(col= manufacturer, size= hwy))+
  geom_smooth(aes(col= manufacturer, method= "lm",se= F))

##1.6: Animated bubble chart:

#It is same as the bubble chart, but, you have to show how the values change over a fifth dimension (typically time).

library(ggplot2)
library(plotly)
library(gapminder)
library(devtools)

data("gapminder")
str(gapminder)

g2= ggplot(gapminder, aes(gdpPercap, lifeExp, color= continent))+
  geom_point(aes(size= pop, frame = year, ids= country))+
scale_x_log10() # Convert to log scale
g2
ggplotly(g2)

ggplotly(ggplot(gapminder, aes(gdpPercap, lifeExp, color= continent,size= pop, frame = year, ids= country))+
  geom_point()+
scale_x_log10())

##1.7 Marginal histogram/boxplot:
#To show the relationship as well as the distribution in the same chart.  Histogram of the X and Y variables at the margins of the scatterplot.

library(ggExtra)
str(mpg)
mpg_select1= mpg[mpg$hwy>= 35 & mpg$cty>27,]

g3= ggplot(mpg, aes(cty, hwy))+
  geom_count()+
  geom_smooth(method= "lm", se= F)
plot(g3)
ggMarginal(g3, type = "histogram", fill="transparent")
ggMarginal(g3, type = "boxplot", fill= "transparent")

##1.8 Correlogram:
#Corellation of multiple continuous variables present in the same dataframe

library(ggcorrplot)
library(dplyr)

str(mtcars)
corr= round(cor(mtcars),2)
ggcorrplot(corr, hc.order = TRUE,
           type= "lower",
           lab= TRUE,
           lab_size = 3,
           method = "circle",
           colors = c("tomato2", "white", "springgreen3"),
           title = "Correlogram of mtcars",
           ggtheme = theme_bw)



  
  
  




  
  
  
  
 


