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
