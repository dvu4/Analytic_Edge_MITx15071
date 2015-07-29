#INTERNET PRIVACY POLL


poll=read.csv("AnonymityPoll.csv")
summary(poll)
str(poll)


table(poll$Smartphone)
summary(poll$Smartphone)

table(poll$State,poll$Region == "Midwest") 

sort(tapply(poll$Region =="South", poll$State,sum))

#or
#to find which states are in the Midwest region we could have used:
MidwestInterviewees = subset(poll, Region=="Midwest")
table(MidwestInterviewees$State) 

#and to find the number of interviewees from each South region state we could have used: 
SouthInterviewees = subset(poll, Region=="South")
table(SouthInterviewees$State)


summary(poll$Internet.Use)
summary(poll$Smartphone)



 limited = subset(poll,poll$Internet.Use == T | poll$Smartphone == T)
 nrow(limited)
 
mean(limited$Info.On.Internet)

table(limited$Info.On.Internet)

table(limited$Worry.About.Info)
prop.table(table(limited$Worry.About.Info),)
#or proportion = mean in case of value of variable is 1 or 0
summary(limited$Tried.Masking.Identity)


table(limited$Tried.Masking.Identity),
prop.table(table(limited$Tried.Masking.Identity),)
#or proportion = mean in case of value of variable is 1 or 0
summary(limited$Tried.Masking.Identity)


hist(poll$Age)

max(table(limited$Age, limited$Info.On.Internet))


jitter(c(1, 2, 3))
jitter(c(1, 2, 3))
jitter(c(1, 2, 3))


plot(jitter(limited$Age), jitter(limited$Info.On.Internet),col="blue")

tapply(poll$Info.On.Internet,poll$Smartphone == T,mean,na.rm=T)
#or check the mean for '1' and '0'
tapply(limited$Info.On.Internet, limited$Smartphone, summary)

tapply(poll$Tried.Masking.Identity,poll$Smartphone == T,mean,na.rm=T)
#or check the mean for '1' and '0'
tapply(limited$Tried.Masking.Identity, limited$Smartphone, summary)
#or
tapply(limited$Tried.Masking.Identity, limited$Smartphone, table)





