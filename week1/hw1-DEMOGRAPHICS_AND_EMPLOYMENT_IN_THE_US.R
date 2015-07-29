# # # DEMOGRAPHICS AND EMPLOYMENT IN THE UNITED STATES


CPS = read.csv("CPSData.csv")
nrow(CPS)

summary(CPS)

table(CPS$Industry) 

sort(table(CPS$State)) 

table(CPS$Citizenship)

t = table(CPS$Citizenship)
(margin.table(t) - t["Non-Citizen"])/margin.table(t)

table(CPS$Race, CPS$Hispanic)

#which(is.na(CPS))

mean(as.numeric(CPS$PeopleInHousehold))
mean(as.numeric(CPS$State))
mean(as.numeric(CPS$MetroAreaCode))
mean(as.numeric(CPS$Age))
mean(as.numeric(CPS$Married))
mean(as.numeric(CPS$Sex))
mean(as.numeric(CPS$Education))
mean(as.numeric(CPS$Race))
mean(as.numeric(CPS$Hispanic))
mean(as.numeric(CPS$CountryOfBirthCode))
mean(as.numeric(CPS$Citizenship))
mean(as.numeric(CPS$EmploymentStatus)

#or just look at NA's in each variable
summary(CPS)

table(CPS$Region, is.na(CPS$Married)) 
table(CPS$Age, is.na(CPS$Married)) 
table(CPS$Citizenship, is.na(CPS$Married))
tablet(CPS$Sex, is.na(CPS$Married)) 

#  11     0 1721
#  12     0 1797
#  13     0 1802
#  14     0 1790
#  15  1795    0
#  16  1751    0
#  17  1764    0
#  18  1596    0


### Alaska                   0  1590
### Wyoming                  0  1624

### Rhode Island          2209     0
### New Jersey            2567     0
### District of Columbia  1791     0

table(CPS$State, is.na(CPS$MetroAreaCode))

ta = table(CPS$Region, is.na(CPS$MetroAreaCode))
ta
margin.table(ta,1)

margin.table(ta,1)[1]
margin.table(ta,1)[2]
margin.table(ta,1)[3]
margin.table(ta,1)[4]

ta[1,1]/margin.table(ta,1)[1]
ta[1,2]/margin.table(ta,1)[1]

ta[2,1]/margin.table(ta,1)[2]
ta[2,2]/margin.table(ta,1)[2]
 
ta[3,1]/margin.table(ta,1)[3] 
ta[3,2]/margin.table(ta,1)[3]

ta[4,1]/margin.table(ta,1)[4]
ta[4,2]/margin.table(ta,1)[4]
### or 
prop.table(table(CPS$Region, is.na(CPS$MetroAreaCode)),1)


tapply(is.na(CPS$MetroAreaCode),CPS$State,mean)
###or 
ta = prop.table(table(CPS$State,is.na(CPS$MetroAreaCode)),1)
max(ta[,2])



MetroAreaCode = read.csv("MetroAreaCodes.csv")
CountryOfBirthCode  = read.csv("CountryCodes.csv")

summary(MetroAreaCode)
summary(CountryOfBirthCode)

CPS = merge(CPS, MetroAreaCode, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

table(is.na(CPS$MetroArea))
#or check NAs in MetroArea
summary(CPS)


ta = table(CPS$MetroArea)
ta["Atlanta-Sandy Springs-Marietta, GA"]
ta["Baltimore-Towson, MD"]
ta["Boston-Cambridge-Quincy, MA-NH"]
ta["San Francisco-Oakland-Fremont, CA"]

sort(tapply(CPS$Hispanic,CPS$MetroArea,mean))

which(prop.table(table(CPS$Race == "Asian", CPS$MetroArea),2)[2,] >= 0.2)
 #or
 sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))


a = subset(CPS, CPS$Age >14)
sort(tapply(a$Education == "No high school diploma", a$MetroArea, mean))

#or
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,na.rm =TRUE))
#Iowa City, IA                                                                      #0.02912621



CPS = merge(CPS, CountryOfBirthCode, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

str(CPS)
#or
table(is.na(CPS$Country))


sort(table(CPS$Country))
 
table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA" , CPS$Country != "United States")
prop.table(table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA" , CPS$Country != "United States"),1)


a = subset(CPS,CPS$Country == "India")
sort(table(a$MetroArea))

b = subset(CPS,CPS$Country == "Brazil")
sort(table(b$MetroArea))

c = subset(CPS,CPS$Country == "Somalia")
sort(table(c$MetroArea))

#or
sort(tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm=TRUE))
sort(tapply(CPS$Country == "Brazil", CPS$MetroArea, sum, na.rm=TRUE))
sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm=TRUE))