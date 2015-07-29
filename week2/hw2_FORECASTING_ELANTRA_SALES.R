#FORECASTING ELANTRA SALES

elantra = read.csv("elantra.csv")
summary(elantra)

elantra_train = subset(elantra,Year <= 2012)
elantra_test = subset(elantra,Year > 2012)


lmElantraSales = lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all,data= elantra_train)
summary(lmElantraSales)

lmElantraSales = lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + Month,data= elantra_train)
summary(lmElantraSales)


predSales = predict(lmElantraSales,newdata=elantra_test)


# NUMERIC VS. FACTORS 
elantra_train$MonthFact = as.factor(elantra_train$Month)
elantra_test$MonthFact = as.factor(elantra_test$Month)

str(elantra_train)


lmElantraSales = lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + MonthFact,data= elantra_train)

summary(lmElantraSales)


cor(elantra_train[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])

lmElantraSales2 = step(lmElantraSales)

summary(lmElantraSales2)

predSales = predict(lmElantraSales2,newdata=elantra_test)


SSE = sum((predSales - elantra_test$ElantraSales)^2)

mean(elantra_train$ElantraSales)

SST = sum((mean(elantra_train$ElantraSales)-elantra_test$ElantraSales)^2)

R2 = 1 - SSE/SST


max(abs(predSales - elantra_test$ElantraSales))
which.max(abs(predSales - elantra_test$ElantraSales))

elantra_test$Year[5]
elantra_test$Month[5]

