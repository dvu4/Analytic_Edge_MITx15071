#STATE DATA

statedata = read.csv("statedata.csv")
str(statedata)

plot(statedata$x,statedata$y)

tapply(statedata$HS.Grad,statedata$state.region,max)

which.max(tapply(statedata$HS.Grad,statedata$state.region,max))

boxplot(statedata$Murder ~ statedata$state.region)

#find outlier
NortheastData = subset(statedata, state.region == "Northeast")
NortheastData$Murder

NortheastData$state.abb

lmLifeExp = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area,data=statedata)

summary(lmLifeExp)


plot(statedata$Income, statedata$Life.Exp)

#remove some insignificant variable from the model
lmLifeExp2 = step( lmLifeExp)

summary(lmLifeExp2)

predLifeExp = predict(lmLifeExp2)

which.max(predLifeExp)
statedata$state.abb[47]
which.max(statedata$Life.Exp)
statedata$state.abb[11]


which.min(predLifeExp)
statedata$state.abb[1]
which.min(statedata$Life.Exp)
statedata$state.abb[40]


which.min(lmLifeExp2$residuals)
statedata$state.abb[19]

which.max(lmLifeExp2$residuals)
statedata$state.abb[11]

