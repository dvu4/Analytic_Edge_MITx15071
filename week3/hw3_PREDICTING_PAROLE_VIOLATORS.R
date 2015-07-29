# PREDICTING PAROLE VIOLATORS

parole = read.csv("parole.csv")
str(parole)

table(parole$violator)

parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
str(parole)

set.seed(144)
library(caTools)
split = sample.split(parole$violator,SplitRatio=0.7)
split
train = subset(parole,split == TRUE)
test = subset(parole,split == FALSE)
ParoleLog = glm(violator ~.,data=train,family=binomial)
summary(ParoleLog)

#odds = exp(beta . x) and p(x) = 1/[1 + exp(-beta . x)]

#exp(-4.2411574 +  0.3869904*male + 0.8867192*race + -0.0001756*age + 0.4433007*state2 + 0.8349797*state3  + -3.3967878*state4 + -0.1238867*time.served + 0.0802954*max.sentence + 1.6119919*multiple.offenses + 0.6837143*crime2 + -0.2781054*crime3 + -0.0117627*crime4)


#male=1, race=1, age=50, state2=0, state3=0, state4=0, time.served=3, max.sentence=12, multiple.offenses=0, crime2=1, crime3=0, crime4=0

odd = exp(-4.2411574 +  0.3869904*1 + 0.8867192*1 - 0.0001756*50 + 0.4433007*0+ 0.8349797*0  - 3.3967878*0  - 0.1238867*3 + 0.0802954*12 + 1.6119919*0 + 0.6837143*1 + -0.2781054*0 - 0.0117627*0)

prob = 1/(1+1/odd)

pred = predict(ParoleLog,newdata=test,type="response")
summary(pred)

table(test$violator, pred >=0.5)

12/(12+11)
167/(167+12)
(167+12)/(167+12+12+11)

table(test$violator)
179/(179+23)

table(test$violator, pred >=0.75)
(177+3)/(177+3+2+20)

ROCRpred = prediction(pred, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)