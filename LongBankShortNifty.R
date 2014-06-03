install.packages("TTR")
install.packages("forecast")
install.packages("quantmod")
install.packages("moments")

library(quantmod)
library(xts)
library(moments)
library(TTR)
library(forecast)

getSymbols("^NSEBANK", from=Sys.Date()-499);
long<-to.monthly(NSEBANK)

getSymbols("^NSEI", from=Sys.Date()-499)
short<-to.monthly(NSEI)

longStk<-"NSEBANK.Close"
shortStk<-"NSEI.Close"

//long BANKNIFTY
long.Close<-long[,longStk]
long.ret<-diff(long.Close)
last(long.ret,100)
plot(long.ret)
hist(long.ret,breaks=15)

//short NIFTY
short.Close<-short[,shortStk]
short.ret<-diff(short.Close)
last(short.ret,100)
plot(short.ret)
hist(short.ret,breaks=15)

short.ratio<- long[,longStk] / short[, shortStk]
hist(short.ratio,breaks=15)

chartSeries(short.ratio,theme="white");

short2by3.profit<-(25*2*abs(long.ret)-3*50*abs(short.ret))
last(short2by3.profit,48)
last(short.ratio,10)
hist(short2by3.profit,48)
last(short2by3.profit,48)

short1by1.profit<-(25*(abs(long.ret)-100)-50*abs(short.ret))
last(short1by1.profit,48)
hist(short1by1.profit,breaks=30,48)

short4by5.profit<-(25*4*abs(long.ret)-50*5*abs(short.ret))
last(short4by5.profit,48)
hist(short4by5.profit,breaks=30,48)

short.compare2by3<-short2by3.profit
short.compare1by1<-short1by1.profit
short.compare4by5<-short4by5.profit

profitAnalysis<- data.frame(short.ratio,2*short2by3.profit,short4by5.profit,4*short1by1.profit)
profitAnalysis
/*
returnAnalysis<-data.frame(short.ratio,long.ret,short.ret,abs((long.ret-2*short.ret)),25*abs(long.ret-2*short.ret),abs((2*long.ret-3*2*short.ret)),25*abs((2*long.ret-3*2*short.ret)))
returnAnalysis
returnAnalysis<-data.frame(long.ret,short.ret,5*25*abs(long.ret-2*short.ret))
returnAnalysis
*/
