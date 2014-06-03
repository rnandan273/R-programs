install.packages("TTR")
install.packages("forecast")
install.packages("quantmod")
install.packages("moments")

library(quantmod)
library(xts)
library(moments)
library(TTR)
library(forecast)

getSymbols("HCLTECH.NS", from=Sys.Date()-499)
long<-to.monthly(HCLTECH.NS)
head(long)
tail(long)


getSymbols("TECHM.NS", from=Sys.Date()-499)
short<-to.monthly(TECHM.NS)

longStk<-"HCLTECH.NS.Close"
shortStk<-"TECHM.NS.Close"

//long 
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

chartSeries(short.ratio,theme="white");

// HclTech / Nifty

short2by3.profit<-1*(500*(long.ret)-3*250*short.ret)
short1by1.profit<-2*(250*(long.ret)-250*short.ret)

profitAnalysis<- data.frame(long.ret,short.ret,short2by3.profit,short1by1.profit)
profitAnalysis
last(short.ratio,48)
last(short.profit,48)
hist(short.profit)

returnAnalysis<-data.frame(long.ret,short.ret,1*(500*abs(long.ret)-3*50*abs(short.ret)))
returnAnalysis