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


############# Exercises ############

head(mtcars)

pl <- ggplot(mpg, aes(hwy))
pl + geom_histogram()


pl <- ggplot(mpg, aes(x=manufacturer))
pl + geom_bar(aes(fill=factor(cyl)))

head(txhousing)

pl <- ggplot(txhousing, aes(y=volume, x=sales))
pl2 <- pl + geom_point(alpha=0.5, aes(color=sales))
pl2 + geom_smooth()


######### Teh Economistii #############

help(fread)
df <- fread('Economist_Assignment_Data.csv', drop=1)
head(df)

pl <- ggplot(df, aes(x=CPI, y=HDI))
pl2 <- pl+ geom_point(aes(color=Region), size=5,shape=1)
pl2 <-pl2 + geom_smooth(aes(group=1),method='lm',formula=y~log(x), se=FALSE,color='Red')

pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")

pl3 <- pl2 + geom_text(aes(label=Country), color="gray20",
                       data = subset(df, Country %in% pointsToLabel),check_overlap = TRUE)

pl4 <- pl3 + theme_bw()
pl4 + scale_x_continuous(name="Corruption Percetion Index", limits=c(1,10), breaks=c(1:10))+
  ggtitle("Corruption and Human Development") +   theme(plot.title = element_text(hjust = 0.5))


################ The Moneyball project ###############

batting <- read.csv('Batting.csv')
head(batting)
str(batting)
head(batting$X2B)
head(batting$AB)

batting$BA <- batting$H / batting$AB
head(batting$BA)
tail(batting$BA, 5)

batting$OBP <- (batting$H+batting$BB+batting$HBP)/(batting$AB+batting$BB+batting$HBP+batting$SF)
head(batting$OBP)
batting$X1B = (batting$H-batting$X2B-batting$X3B-batting$HR)
batting$SLG = (batting$X1B + (2*batting$X2B)+ (3*batting$X3B) +
                 (4*batting$HR))/batting$AB
str(batting)

sal <- read.csv('salaries.csv')
head(sal)
tmpBatting <- batting
help(subset)
duh <- subset(tmpBatting,tmpBatting$yearID>1984)
summarise(duh)
summary(duh)
head(tmpBatting$yearID)
head(tmpBatting$yearID >1984)

batting <- duh
help(merge)
combo <- merge(batting, sal, by=c('playerID', 'yearID'))
summary(combo)

lost_players <- subset(combo, playerID %in% c('giambja01','damonjo01','saenzol01'))
lost_players <- subset(lost_players, yearID == 2001)
lost_players2 <- subset(lost_players,select=c('playerID','H','X2B','X3B',
                                              'HR','OBP','SLG','BA','AB'))
head(lost_players2)
mean(lost_players2)
summary(lost_players2,mean)
colMeans(x=lost_players2)
lost_players2

pl <- ggplot(df, aes(x=wt,y=mpg))

# Geometry

pl + geom_point(size=5, alpha=0.5)
print(pl + geom_point(aes(size=hp)))

pl + geom_point(aes(size=factor(cyl)))
pl2 <- pl + geom_point(aes(shape=factor(cyl),size=5,color=hp))
pl2 + scale_color_gradient(low='blue', high='red')

head(arrange(mtcars, cyl,desc(wt)))


############# ##################
library(dplyr)
bah <- arrange(combo,desc(AB),desc(OBP) )
head(bah)
pl <- ggplot(combo, aes(x=AB,y=OBP))
pl + geom_point(aes(color=salary))
colMeans(lost_players2)
meanOBP <- mean(lost_players2$OBP)
sumAB <- sum(lost_players2$AB)

OBP_dudes <- subset(combo,OBP>meanOBP)
head(OBP_dudes)
teh_dudes <- arrange(OBP_dudes,desc(AB))
teh_dues <-subset(OBP_dudes, AB>(sumAB/3))
cheap_duuds <- arrange(teh_dues,desc(-salary))
head(cheap_duuds)
head(teh_dudes)
help(arrange)



##### logistic regression

df.train <- read.csv('titanic_train.csv')
head(df.train)
install.packages('Amelia')
library(Amelia)
help('missmap')
missmap(df.train, main='Missing Map', col = c
        ('yellow','black'),legend=FALSE)

library(ggplot2)
ggplot(df.train,aes(Survived)) + geom_bar()
ggplot(df.train, aes(Pclass)) + geom_bar(aes(fill=factor(Pclass)))

ggplot(df.train, aes(Sex)) + geom_bar(aes(fill=factor(Sex))) 

ggplot(df.train,aes(Age)) + geom_histogram(bins=20, alpha=0.5, fill='blue')

ggplot(df.train, aes(SibSp)) + geom_bar()

ggplot(df.train,aes(Fare)) + geom_histogram( fill=
      'green',color='black', alpha=0.5)

pl <- ggplot(df.train,aes(Pclass,Age))
pl <- pl + geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha=0.4))
pl + scale_y_continuous(breaks = seq(min(0),max(80),by=2))


############# Impation of AGE based on CLASS

impute_age <- function(age,class) {
  out <-age
  for(i in 1:length(age)) {
    
     if(is.na(age[i])){
      
      if(class[i] ==1){
        out[i] <- 37
      }else if(class[i] == 2){
        out[i] <- 29
      }else{
        out[i] <- 24
      }
    }else{
    out[i] <- age[i]
    }
  }
return(out)
}
#####################
fixed.ages <- impute_age(df.train$Age, df.train$Pclass)
##############

df.train$Age <- fixed.ages
##########

missmap(df.train,main='Imputation check', col =c('yellow','black'),legend=FALSE)

str(df.train)

library(dplyr)
df.train <- select(df.train,-PassengerId,-Name,-Ticket,-Cabin)
head(df.train,3)

df.train$Survived <- factor(df.train$Survived)
df.train$Pclass <- factor(df.train$Pclass)
df.train$Parch <- factor(df.train$Parch)
df.train$SibSp <- factor(df.train$SibSp)

str(df.train)

################ Train model

log.model <- glm(Survived ~ ., family=binomial(link='logit'), 
                 data=df.train)
summary(log.model)
library(caTools)
set.seed(101)

split <- sample.split(df.train$Survived, SplitRatio=0.7)

final.train <- subset(df.train, split == TRUE)
final.test <- subset(df.train, split == FALSE)

final.log.model <- glm(Survived ~ ., family = binomial(link='logit'), data=final.train)
summary(final.log.model)


fitted.probabilities <- predict(final.log.model,final.test, type='response')
fitted.results <- ifelse(fitted.probabilities>0.5, 1,0)
misClassError <- mean(fitted.results != final.test$Survived)
print(1-misClassError)


## Confusion Matrix

table(final.test$Survived, fitted.probabilities>0.5)




############### Decision trees

install.packages('rpart')
library("rpart")
str(kyphosis)

tree <- rpart(Kyphosis ~ ., method='class', data=kyphosis)
printcp(tree)

plot(tree,uniform=T,main='Kyphosis tree')
text(tree,use.n=T,all=T)

install.packages('rpart.plot')
library(rpart.plot)

prp(tree)


install.packages('randomForest')
library(randomForest)
rfmodel <- randomForest(Kyphosis ~., data = kyphosis)
rfmodel
