install.packages("TTR")
install.packages("forecast")
install.packages("quantmod")
install.packages("moments")

library(quantmod)
library(xts)
library(moments)
library(TTR)
library(forecast)


longStk<-"RELINFRA.NS.Close"
shortStk<-"NSEI.Close"

getSymbols("RELINFRA.NS", from=Sys.Date()-1599);
long<-to.monthly(RELINFRA.NS)


getSymbols("^NSEI", from=Sys.Date()-1599)
short<-to.monthly(NSEI)



//long RELINFRA
long.Close<-long[,longStk]
long.ret<-diff(long.Close)
last(long.ret,48)
plot(long.ret)
hist(long.ret,breaks=15)

//short NIFTY
short.Close<-short[,shortStk]
short.ret<-diff(short.Close)
last(short.ret,100)
plot(short.ret)
hist(short.ret,breaks=15)

short.ratio<- long[,longStk] / short[, shortStk]

chartSeries(short.ratio,theme="white");

short1by1.profit<-(1000*(abs(long.ret))-50*abs(short.ret))
last(short1by1.profit,48)
hist(short1by1.profit,breaks=30,48)

short.compare1by1<-short1by1.profit

profitAnalysis<- data.frame(short.ratio,short1by1.profit)
profitAnalysis


returnAnalysis<-data.frame(long.ret,short.ret)
returnAnalysis