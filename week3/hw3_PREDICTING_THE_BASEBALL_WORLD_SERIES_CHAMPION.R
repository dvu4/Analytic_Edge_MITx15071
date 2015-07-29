#PREDICTING THE BASEBALL WORLD SERIES CHAMPION

baseball = read.csv("baseball.csv")
str(baseball)

table(baseball$Year)

length(table(baseball$Year))

#LIMITING TO TEAMS MAKING THE PLAYOFFS 
baseball = subset(baseball,Playoffs ==1)
str(baseball)

#the number of teams making the playoffs in some season
PlayoffTable = table(baseball$Year)




#add an important predictor  Num Competitors to dataset

PlayoffTable[c("1990","2001")]


names(PlayoffTable)

baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]

#playoff team/year pairs are there in our dataset from years where 8 teams were invited to the playoffs
baseball8 = subset(baseball,NumCompetitors == 8)

#BIVARIATE MODELS FOR PREDICTING WORLD SERIES WINNER 

baseball$WorldSeries = as.numeric(baseball$RankPlayoffs str(baseball)


table(baseball$WorldSeries)


#build and compare bivariate models
summary(glm(WorldSeries~Year, data=baseball, family="binomial"))
summary(glm(WorldSeries~ League, data=baseball, family="binomial"))
summary(glm(WorldSeries~ RS, data=baseball, family="binomial"))
summary(glm(WorldSeries~ RA, data=baseball, family="binomial"))
summary(glm(WorldSeries~ W , data=baseball, family="binomial"))
summary(glm(WorldSeries~ OBP , data=baseball, family="binomial"))
summary(glm(WorldSeries~ SLG , data=baseball, family="binomial"))
summary(glm(WorldSeries~ BA , data=baseball, family="binomial"))
summary(glm(WorldSeries~ RankSeason  , data=baseball, family="binomial"))
summary(glm(WorldSeries~ OOBP  , data=baseball, family="binomial"))
summary(glm(WorldSeries~ OSLG  , data=baseball, family="binomial"))
summary(glm(WorldSeries~ NumCompetitors  , data=baseball, family="binomial"))

#BUILD MULTIVARIATE MODELS FOR PREDICTING WORLD SERIES WINNER 
summary(glm(WorldSeries~ . -  Team - Playoffs - RankPlayoffs - G, data=baseball, family="binomial"))

#CORRELATION MATRIX OF THE DIFFERENT VARIABLE

cor(baseball[,unlist(lapply(baseball,is.numeric))])

cor(baseball[c("Year", "RA", "RankSeason", "NumCompetitors")])
 
 
# build 10 different logistic regression models
mod1 = glm(WorldSeries ~ Year, data=baseball, family="binomial")
mod2 = glm(WorldSeries ~ RA, data=baseball, family="binomial")
mod3 = glm(WorldSeries ~ RankSeason, data=baseball, family="binomial")
mod4 = glm(WorldSeries ~ NumCompetitors, data=baseball, family="binomial")
mod5 = glm(WorldSeries ~ Year + NumCompetitors, data=baseball, family="binomial")
mod6 = glm(WorldSeries ~ RA + NumCompetitors, data=baseball, family="binomial")
mod7 = glm(WorldSeries ~ RankSeason+ NumCompetitors, data=baseball, family="binomial")
mod8 = glm(WorldSeries ~ RA + RankSeason, data=baseball, family="binomial")
mod9 = glm(WorldSeries ~ Year + RankSeason, data=baseball, family="binomial")
mod10 = glm(WorldSeries ~ Year + RA, data=baseball, family="binomial")

# compare AIC value of each model
mod1$aic
mod2$aic
mod3$aic
mod4$aic
mod5$aic
mod6$aic
mod7$aic
mod8$aic
mod9$aic
mod10$aic


