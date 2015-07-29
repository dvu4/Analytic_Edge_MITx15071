# AN ANALYTICAL DETECTIVE - CHICAGO

mvt = read.csv("mvtWeek1.csv")
str(mvt)
nrow(mvt)
max(mvt$ID)

min(mvt$Beat)
numa = subset(mvt, Arrest == TRUE)
str(numa)

ldalley = subset(mvt, LocationDescription == "ALLEY")
str(ldalley)

mvt$Date[1]

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)

mvt$Month = months(DateConvert)

mvt$Weekday = weekdays(DateConvert)

mvt$Date = DateConvert

str(mvt)

which.min(mvt$Arrest)

table(mvt$Month)
which.min(table(mvt$Month))
table(mvt$Weekday)
which.max(table(mvt$Weekday))

hist(mvt$Date, breaks=100)


# # #In a boxplot, the bold horizontal line is the median value of the data, the box shows the range of values between the first quartile and third quartile, and the whiskers (the dotted lines extending outside the box) show the minimum and maximum values, excluding any outliers (which are plotted as circles). Outliers are defined by first computing the difference between the first and third quartile values, or the height of the box. This number is called the Inter-Quartile Range (IQR). Any point that is greater than the third quartile plus the IQR or less than the first quartile minus the IQR is considered an outlier.

mvtnew = subset(mvt,mvt$Arrest == "TRUE")
boxplot(mvtnew$Date)

boxplot(mvt$Date ~ mvt$Arrest)


a2001 = subset(mvt,mvt$Arrest == "TRUE" & mvt$Year == "2001")
total2001 = subset(mvt, mvt$Year == "2001")
p2001 = nrow(a2001)/nrow(total2001)
p2001

a2007 = subset(mvt,mvt$Arrest == "TRUE" & mvt$Year == "2007")
total2007 = subset(mvt, mvt$Year == "2007")
p2007 = nrow(a2007)/nrow(total2007)
p2007


a2012 = subset(mvt,mvt$Arrest == "TRUE" & mvt$Year == "2012")
total2012 = subset(mvt, mvt$Year == "2012")
p2012 = nrow(a2012)/nrow(total2012)
p2012

sort(table(mvt$LocationDescription))

top5$LocationDescription = factor(top5$LocationDescription)
LocationArrest = table(top5$LocationDescription, top5$Arrest)
LocationArrest

margin.table(LocationArrest)
margin.table(LocationArrest,1)
margin.table(LocationArrest,2)

prop.table(LocationArrest,1)


LocationWeekday  = table(top5$LocationDescription, top5$Weekday)

LocationWeekday["GAS STATION",]
which.max(LocationWeekday["GAS STATION",])

LocationWeekday["DRIVEWAY - RESIDENTIAL",]
which.min(LocationWeekday["DRIVEWAY - RESIDENTIAL",])

