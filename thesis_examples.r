A=c(1,2,3,4,5,6,7)
B=c(3,4,5,6,7,8,9)

t.test(A,B)
dummies = as.factor(c(0,0,0,0,0,0,0,1,1,1,1,1,1,1))
data= c(A,B)

regi = lm(data~dummies)
reg

duh <- data.frame(data,dummies)

pl<- ggplot(duh, aes(y=as.numeric(data),x=dummies))
pl + geom_point() + geom_abline(intercept=1, slope=2)#+ geom_smooth(method="lm", se=FALSE)

regi
summary(reg)

X = model.matrix(~dummies)
X

xtxi = solve(t(X) %*% (X))
xtxi

betas <- xtxi%*%t(X)%*%data
betas

2/1.1547 # P-value for slove
4/0.8165  # p-value for intercept

C= c(7,8,9,10,11,12,13)
dummies = as.factor(c(0,0,0,0,0,0,0,1,1,1,1,1,1,1,2,2,2,2,2,2,2))
data = c(A,B,C)
X= model.matrix(~dummies)
X
xtxi = solve(t(X) %*% (X))
betas = xtxi%*%t(X)%*%data
betas

summary(aov(data~dummies))
reg2=lm(data~dummies)
summary(reg2)

install.packages('flexmix')
