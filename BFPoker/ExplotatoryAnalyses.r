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

Data = read.spss(paste(DataDir,'data_for_r.sav',sep=""))
DF <- as.data.frame(Data)
DF <- na.omit(DF)

corrgram(DF)
corrgram(DF, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt)


#tehDF <- DF[, c('subject_id','feedback', 'poker_face', 'ln_eda_total_sd', 'totalBAS', 'BASDrive', 'BASFunSeeking', 'BASRewardResp',
      #          'BIS', 'CN_EDA', 'CN_EMG1', 'CN_EMG2','CN_EMG3','eda_change','emg1_change','emg2_change','emg3_change')]

# EDA Change etc did not correlate with anything, removed
tehDF <- DF[, c('subject_id','feedback', 'poker_face', 'ln_eda_total_sd', 'totalBAS', 'BASDrive', 'BASFunSeeking', 'BASRewardResp',
                'BIS', 'CN_EDA', 'CN_EMG1', 'CN_EMG2','CN_EMG3')]


corrgram(tehDF, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt)
corrgram(tehDF, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt)

#simplest model
model <- lm(CN_EDA~feedback, tehDF)
model <- lm(CN_EDA~feedback + feedback*ln_eda_total_sd, tehDF)
model
summary(model)
pfmodel <- lm(poker_face~feedback + feedback*ln_eda_total_sd, tehDF)
summary(pfmodel)




install.packages('flexmix')

Data <- tehDF
library(dplyr)
#Data %>% group_by('subject_id') %>% summarise_each(funs(means))
jaahas <- Data %>% group_by(subject_id,feedback) %>% summarize(sum_CN_EDA = sum(CN_EDA))

bah <- left_join(Data,jaahas)
head(bah,2)

bart1 <- lapply(split(Data,list(Data$subject_id,Data$feedback)), function(y){apply(y[,-1],2,function(x){sum(x)})})
bart2 <- lapply(split(Data,list(Data$subject_id,Data$feedback)), function(y){apply(y,2,function(x){sum(x)})})

mart <- as.data.frame(bart1)
tart <- t(mart)

head(bart)

#tapply(Data$CN_EDA, Data$feedback, sum)



 
all_means <- lapply(split(Data, Data$subject_id), function(y) { apply(y[,3:5],2,function(x){mean(x)})})
lapply(split(data_p, data_p$Part),function(y) {apply(y[,FEATURE_RANGE], 2, function(x){x/sd(x,na.rm=TRUE) }) })

aperativ <- do.call(rbind,all_means)
tmpdata2 <- left_join(tmpdata, as.data.frame(aperativ))
tmpdata[,3:5] <- do.call(rbind,all_means)
