#READING TEST SCORES
pisa2009train = read.csv("pisa2009train.csv")
pisa2009test = read.csv("pisa2009test.csv")

tapply(pisa2009train$readingScore,pisa2009train$male,mean)

#remove NA values in variable
pisa2009train = na.omit(pisa2009train)
pisa2009test = na.omit(pisa2009test)

# set reference level for unordered factor
pisa2009train$raceeth = relevel(pisa2009train$raceeth,"White")
pisa2009test$raceeth = relevel(pisa2009test$raceeth,"White")


lmScore = lm(readingScore ~.,data=pisa2009train)
summary(lmScore)


predictScore = predict(lmScore,newdata=pisa2009test)
summary(predictScore)


sqrt(mean(residuals(lmScore)^2))
sqrt(mean(lmScore$residuals^2))



SSE = sum((pisa2009test$readingScore-predictScore)^2)
SSE


SST = sum((mean(pisa2009train$readingScore)-pisa2009test$readingScore)^2)

mean(pisa2009train$readingScore)


RMSE = sqrt(SSE/nrow(pisa2009test))
RMSE

R2 = 1-SSE/SST
