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
