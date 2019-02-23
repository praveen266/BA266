####4 Ditribution:
#When you have lots and lots of data points and want to study where and how the data points are distributed.

##4.1: Histogram:
#By default, if only one variable is supplied, the geom_bar() tries to calculate the count. In order for it to behave like a bar chart, the stat=identity option has to be set and x and y values must be provided.
# For continuous variable: 'geom_bar' or 'geom_histogram' can be used
#'geom_histogram':" We can control the number of bars using the bins option. Else, we can set the range covered by each bin using binwidth. The value of binwidth is on the same scale as the continuous variable on which histogram is built. Since, geom_histogram gives facility to control both number of bins as well as binwidth, it is the preferred option to create histogram on continuous variables.

# Diverging : BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
# Qualitative: Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
# Sequential: Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd

### Histogram on a Continuous (Numeric) Variable

library(ggplot2)
data(mpg)
str(mpg)
View(mpg)
ggplot(mpg, aes(displ))

g= ggplot(mpg, aes(displ))+scale_fill_brewer(palette = 'Spectral')
g
g+geom_histogram(aes(fill=class),
                 binwidth = 0.1,
                 col= "black",
                 size= 0.1)     #change binwidth
g+geom_histogram(aes(fill= class),
                 bins = 5,
                 col= "black",
                 size=0.1)      # change number of bins
