# The main test script

library(dplyr)
head(mtcars)

filter(mtcars, mpg > 20, cyl > 5) # no cars with mpg > 20 and more than 6 cylinders

head(arrange(mtcars, cyl,desc(wt)))

head(select(mtcars, mpg, hp))
distinct(mtcars, gear)
x = mtcars %>% as_tibble() %>% mutate(
  Performance = hp/wt
)
head(x)
mean(mtg,mpg)
summarise(mtcars,mean, mpg)
mtcars %>% summarise(mean=mean(mpg))
filter(mtcars, cyl ==6) %>% summarise(mean(hp))
 
install.packages('tidyr')
install.packages('data.table')
library(tidyr)
library(data.table)


######### GATHER #################
comp <- c(1,1,1,2,2,2,3,3,3)
yr <- c(1998,1999,2000,1998,1999,2000,1998,1999,2000)
q1 <- runif(9, min=0, max=100)
q2 <- runif(9, min=0, max=100)
q3 <- runif(9, min=0, max=100)
q4 <- runif(9, min=0, max=100)
df <- data.frame(comp=comp,year=yr,Qtr1 = q1,Qtr2 = q2,Qtr3 = q3,Qtr4 = q4)

gather(df,Quarter,Revenue,Qtr1:Qtr4)

df %>% gather(Quarter, Revenue, Qtr1:Qtr4)
#############################################

############# SPREAD ##############
stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10,0,1),
  Y = rnorm(10,0,2),
  Z = rnorm(10,0,4)
)

stocks
stocks.gathered <- stocks %>% gather(stock,price,X,Y,Z)
head(stocks.gathered)
stocks.gathered %>% spread(stock,price)
spread(stocks.gathered, time,price)


df <- data.frame(x=c(NA,"a.x", "b.y", "c.z"))
df.sep <- df %>% separate(x, c("ABC", "XYZ"))

######## UNITE ##############
unite(df.sep,new.joined.col,ABC,XYZ)


######## Visualizations with GGplot2 ""#################
install.packages("ggplot2")
library(ggplot2)
pl <- ggplot(data=mtcars,aes(x=mpg,y=hp))
pl <- pl+geom_point()
pl + facet_grid(cyl ~ .)


pl <-ggplot(data=mtcars,aes(x=mpg,y=hp))+geom_point()
pl2 <-pl + facet_grid(cyl ~ .) + stat_smooth()
pl2 + coord_cartesian(xlim = c(15,25))


######## HISTORGRAM Lecture ################


install.packages('ggplot2movies')
library(ggplot2movies)

# Data & Aesthetics

pl <- ggplot(movies,aes(x=rating))

# Geometry

pl2 <- pl + geom_histogram(binwidth=0.1, color='red', fill='pink',
                          alpha=0.4)
pl3 <- pl2 + xlab('Movie Ratings') + ylab('Count')
print(pl3)

print(pl3 + ggtitle('titteli'))

############## Scatterplots ##############

library(ggplot2)
df <- mtcars

# Data & Aesthetics

pl <- ggplot(df, aes(x=wt,y=mpg))
 
# Geometry

pl + geom_point(size=5, alpha=0.5)
print(pl + geom_point(aes(size=hp)))

pl + geom_point(aes(size=factor(cyl)))
pl2 <- pl + geom_point(aes(shape=factor(cyl),size=5,color=hp))
pl2 + scale_color_gradient(low='blue', high='red')

############# Barplots ##################

df <-mpg

pl <- ggplot(df, aes(x=class))

print(pl + geom_bar(aes(fill=drv),position="dodge"))
pl + geom_bar(aes(fill=drv), position="fill")

####### Boxplots #################

df <- mtcars

pl <- ggplot(df, aes(x=cyl,y=mpg)) # Wrong
pl + geom_boxplot()

pl <- ggplot(df, aes(x = factor(cyl),y=mpg))
pl2 <- pl + geom_boxplot()
pl2 + coord_flip()

pl + geom_boxplot((aes(fill=factor(cyl))))


######## 2 Variable Plotting ################
library(ggplot2)
library(ggplot2movies)

pl <- ggplot(movies,aes(x=year, y=rating))

pl + geom_bin2d() + scale_fill_gradient(high='red',low='green')

install.packages('hexbin')
pl2 <- pl + geom_hex()
pl2

pl2 <- pl + geom_density2d()
pl2


####### coordinates and faceting


pl <- ggplot(mpg,aes(x=displ, y=hwy)) + geom_point()
pl2 <- pl + coord_cartesian(xlim=c(1,4),ylim=c(15,30))
pl3 <- pl + coord_fixed(ratio=1/3)
pl3

pl + facet_grid(. ~cyl)

pl + facet_grid(drv ~ .)

pl + facet_grid(drv ~ cyl)

############### Themes ################

theme_set(theme_minimal())
pl <- ggplot(mtcars, aes(x=wt, y=mpg)) +geom_point()
pl

pl + theme_dark()

install.packages('ggthemes')
library(ggthemes)

pl + theme_calc()
pl + theme_pander()


###### Plot.ly ###############


install.packages('plotly')
library(plotly)

pl <- ggplot(mtcars, aes(mpg,wt)) + geom_point()

pl
gpl <- ggplotly(pl)
gpl
