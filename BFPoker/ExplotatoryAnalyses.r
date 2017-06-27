#### Explotatory Analyses

install.packages('corrgram')
install.packages('corrplot')

library(foreign)
library(corrgram)
library(corrplot)
library(ggplot2)
DataDir = "C:\\dropbox\\Dropbox\\BFData\\"
## Read Data


##tonicDF = read.spss(paste(DataDir,'tonic_data.sav', sep=""))
tonicDF = read.csv(paste(DataDir,'tonic_results.csv', sep=""), sep=" ", dec=",")
#corrgram(tonicDF)
#corrgram(tonicDF, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt)

fullData = read.spss(paste(DataDir,'phasic_data_16seconds_neu.sav',sep=""))
fullDF <- as.data.frame(fullData)
newDF <- fullDF[fullDF$second > -4 & fullDF$secon < 4,]
