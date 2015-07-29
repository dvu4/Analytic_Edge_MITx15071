#DETECTING FLU EPIDEMICS VIA SEARCH ENGINE QUERY DATA 

#this sort of data is called "skew right" -->  handling a skewed dependent variable, it is often useful to predict the logarithm of the dependent variable instead of the dependent variable itself -- this prevents the small number of unusually large or small observations from having an undue influence on the sum of squared errors of predictive models. 

FluTrain = read.csv("FluTrain.csv")
summary(FluTrain)

which.max(FluTrain$ILI)
FluTrain$Week[303]

which.max(FluTrain$Queries)
FluTrain$Week[303]


hist(FluTrain$ILI)
plot(FluTrain$Queries,log(FluTrain$ILI))
FluTrend1 = lm(log(ILI) ~ Queries, data=FluTrain)
summary(FluTrend1)


cor(log(FluTrain$ILI),FluTrain$Queries)


FluTest = read.csv("FluTest.csv")
summary(FluTest)

PredTest1 = exp(predict(FluTrend1,newdata=FluTest))
which(FluTest$Week == "2012-03-11 - 2012-03-17")
FluTest$ILI[11]
PredTest1[11]

SSE = sum((FluTest$ILI-PredTest1)^2)
RMSE = sqrt(SSE/nrow(FluTest))

install.packages("zoo")
library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI),-2,na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
str(FluTrain)

hist(FluTrain$ILILag2)
plot(log(FluTrain$ILILag2),log(FluTrain$ILI))

FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2),data=FluTrain)

summary(FluTrend2)
summary(FluTrend1)

ILILag2 = lag(zoo(FluTest$ILI),-2,na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
str(FluTest)

FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]
str(FluTest)

PredTest2 = exp(predict(FluTrend2,newdata=FluTest))
SSE = sum((FluTest$ILI-PredTest2)^2)
RMSE = sqrt(SSE/nrow(FluTest))
RMSE

?arima
?coredata
?lag
