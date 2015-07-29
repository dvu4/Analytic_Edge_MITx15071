# # # STOCK DYNAMICS - IBM, General Electric (GE), Procter and Gamble, Coca Cola, and Boeing

IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")


str(IBM)

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")

GE$Date = as.Date(GE$Date, "%m/%d/%y")

CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")

ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")


summary(IBM)
summary(CocaCola)
summary(GE)
summary(Boeing)
sd(ProcterGamble$StockPrice)


plot(CocaCola$Date,CocaCola$StockPrice,xlab = "Date", ylab="Stock Price", main="CocaCola stock price in period of 1970-2009",col="blue",type="l")


lines(ProcterGamble$Date, ProcterGamble$StockPrice,col="red",lty=4)
abline(v=as.Date(c("2000-03-01")), lwd=2,col="green")



plot(CocaCola$Date,CocaCola$StockPrice,xlab = "Date", ylab="Stock Price", main="CocaCola stock price in period of 1970-2009",col="blue",type="l")
lines(ProcterGamble$Date, ProcterGamble$StockPrice,col="red",lty=4)
abline(v=as.Date(c("1983-01-01")), lwd=2,col="green")

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432],xlab="Date", ylab="Stock Price",main="Period of 1995-2005", type="l", col="red", ylim=c(0,210))


lines(IBM$Date, IBM$StockPrice,col="blue")
lines(ProcterGamble$Date, ProcterGamble$StockPrice,col="green")
lines(Boeing$Date, Boeing$StockPrice,col="purple")
lines(GE$Date, GE$StockPrice,col="BLACK")
abline(v=as.Date(c("1997-11-01")), lwd=3,col="tan1")
abline(v=as.Date(c("1997-09-01")), lwd=3,col="wheat")
abline(v=as.Date(c("2004-01-01")), lwd=2)
abline(v=as.Date(c("2005-01-01")), lwd=2)


tapply(IBM$StockPrice, months(IBM$Date), mean)
mean(IBM)

