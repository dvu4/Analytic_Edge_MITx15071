# CLIMATE CHANGE
climate = read.csv(climate_change.csv)

climate = read.csv("climate_change.csv")
summary(climate)

climate_train = subset(climate,Year <= 2006)
climate_test = subset(climate,Year > 2006)
summary(climate_train)


ClimateRegs = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols,data = climate_train)

summary(ClimateRegs)

cor(climate_train)


ClimateRegs2 = lm(Temp ~ MEI + CO2 + N2O + CFC.11 + TSI + Aerosols,data = climate_train)
summary(ClimateRegs2)

ClimateRegs3 = lm(Temp ~ MEI + CO2 + TSI + Aerosols,data = climate_train)
summary(ClimateRegs3)

ClimateRegs4 = step(ClimateRegs)
summary(ClimateRegs4)

predictTest = predict(ClimateRegs4,newdata=climate_test)
SSE = sum((climate_test$Temp-predictTest)^2)
SST = sum((mean(climate_train$Temp)-climate_test$Temp)^2)

R2 = 1 - SSE/SST

