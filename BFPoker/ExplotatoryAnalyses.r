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

#fullData = read.spss(paste(DataDir,'phasic_data_16seconds_neu.sav',sep=""))
fullData = read.spss(paste(DataDir,'fbPoker3sec.sav',sep=""))

fullDF <- as.data.frame(fullData)
newDF <- fullDF[fullDF$second > -4 & fullDF$secon < 4,]

Data = read.spss(paste(DataDir,'data_for_r.sav',sep=""))
DF <- as.data.frame(Data)
DF <- na.omit(DF)

corrgram(DF)
corrgram(DF, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt)


#tehDF <- DF[, c('subject_id','feedback', 'poker_face', 'ln_eda_total_sd', 'totalBAS', 'BASDrive', 'BASFunSeeking', 'BASRewardResp',
      #          'BIS', 'CN_EDA', 'CN_EMG1', 'CN_EMG2','CN_EMG3','eda_change','emg1_change','emg2_change','emg3_change')]

# EDA Change etc did not correlate with anything, removed
tehDF <- DF[, c('subject_id','feedback', 'poker_face', 'ln_eda_total_sd', 'totalBAS', 'BASDrive', 'BASFunSeeking', 'BASRewardResp',
                'BIS', 'CN_EDA', 'CN_EMG1', 'CN_EMG2','CN_EMG3')]

corur <- cor(tehDF)
#corrgram(tehDF, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt)
corrgram(tehDF, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt)

tehDF <- DF[, c('subject_id','feedback', 'poker_face', 'ln_eda_total_sd', 'totalBAS', 'BASDrive', 'BASFunSeeking', 'BASRewardResp',
                'BIS', 'agg_nobf','agg_bf','total_aggr')]

corrgram(tehDF, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt)

corrgram(fullDF, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt)


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


Data = tehDF
 
fb_means <- lapply(split(Data, list(Data$subject_id,Data$feedback)), function(y) { apply(y[,c('CN_EDA','CN_EMG1','CN_EMG2','CN_EMG3')],2,function(x){mean(x)})})


other_means <- lapply(split(Data, Data$subject_id), function(y) { apply(y[, !(colnames(y) %in%  c('CN_EDA','CN_EMG1','CN_EMG2','CN_EMG3'))],2,function(x){mean(x)})})
all_means <- lapply(split(Data, list(Data$subject_id,Data$feedback)), function(y) { apply(y[,],2,function(x){mean(x)})})

all_means2 <- do.call(rbind,all_means)
all_means3 <- as.data.frame(all_means2)

finalDF <- unique(within(all_means3, {
  eda_sd <- ave(ln_eda_total_sd, subject_id, FUN = sum)
}))
#lapply(split(data_p, data_p$Part),function(y) {apply(y[,FEATURE_RANGE], 2, function(x){x/sd(x,na.rm=TRUE) }) })

fDF <- as.data.frame(all_means2)


model <- lm(CN_EDA~feedback + feedback*ln_eda_total_sd, data=fDF)

summary(model)

model2 <- lm(CN_EDA~feedback + feedback*BIS, data=fDF)

summary(model2)

model3 <- lm(CN_EDA~feedback, data=Data)

summary(model3)
model3.1 <- lm(CN_EDA~feedback,data=fDF)

model4 <- lm(CN_EDA~feedback + BIS*feedback, data=Data)

summary(model4)
summary(model3.1)

model5 <- lm(ln_eda~feedback, data=fullDF)
summary(model5)
aperativ <- do.call(rbind,fb_means)
taperativ <- t(aperativ)
tmpdata2 <- left_join(tmpdata, as.data.frame(aperativ))
tmpdata[,3:5] <- do.call(rbind,all_means)

bastu <- do.call(rbind,other_means)

#### REDUCE THE DATA

duh <- as.data.frame(aperativ[1:23,])
duh2 <- as.data.frame(aperativ[24:46,])
rownames(duh)[24]
duh3 <- cbind(duh,duh2)

colnames(duh3) <- c('CN_EDA_0', 'CN_EMG1_0', 'CN_EMG2_0','CN_EMG3_0','CN_EDA_1','CN_EMG1_1', 'CN_EMG2_1', 'CN_EMG3_3')
duh3



