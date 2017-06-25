df <- read.csv('student-mat.csv', sep=";")
head(df)
any(is.na(df))
str(df)

library(ggplot2)
library(ggthemes)
library(dplyr)

# Num only
num.cols <- sapply(df, is.numeric)
cor.data <- cor(df[,num.cols])
head(cor.data)

install.packages('corrgram')
install.packages('corrplot')

library(corrgram)
library(corrplot)

corrplot(cor.data)

corrgram(df)

corrgram(df, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt)

ggplot(df,aes(x=G3)) + geom_histogram(bins=20,alpha=0.5,fill='blue')

################# Lecture  2 ##########################

install.packages('caTools')
set.seed(101)
#split up sample
sample <- sample.split(df$G3,SplitRatio = 0.7)
# 70% of data -> train
train <- subset(df,sample == TRUE)
# 30% will be test
test <- subset(df,sample == FALSE)


################ Model Building ###############

# model <- lm(y ~ x1 + x2, data)

# Train and Build the model
model <- lm(G3 ~ ., train)            ## . for all data
#Run Model

#Interpret the Model

summary(model)

m2 <- lm (G3~train$famsup, train)
summary(m2)

res <- residuals(model)
res <- as.data.frame(res)
ggplot(res,aes(res))+ geom_histogram()
library(ggplot2)
