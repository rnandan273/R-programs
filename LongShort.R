install.packages("TTR")
install.packages("forecast")
install.packages("quantmod")
install.packages("moments")

library(quantmod)
library(xts)
library(moments)
library(TTR)
library(forecast)

getSymbols("TATAMOTOR.NS", from=Sys.Date()-499)
long<-to.monthly(TATAMOTOR.NS)
head(long)
tail(long)
longStk<-"TATAMOTOR.NS.Close"

getSymbols("^NSEBANK", from=Sys.Date()-499);
long<-to.monthly(NSEBANK)

getSymbols("^NSEI", from=Sys.Date()-499)
short<-to.monthly(NSEI)


USDINR<-(1/(getSymbols('INR/USD', src='oanda', auto.assign = FALSE)))
usdinr<-to.monthly(USDINR)

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

chartSeries(short.ratio,theme="white");

short2by3.profit<-100*(2*(long.ret)-3*short.ret)
last(short2by3.profit,48)
last(short.ratio,10)
hist(short2by3.profit,48)

short1by1.profit<-150*(1*(long.ret)-2*short.ret)
last(short1by1.profit,48)
hist(short1by1.profit,breaks=30,48)

short.compare2by3<-short2by3.profit
short.compare1by1<-short1by1.profit

profitAnalysis<- data.frame(short2by3.profit,short1by1.profit)
profitAnalysis

returnAnalysis<-data.frame(long.ret,short.ret,abs((long.ret-2*short.ret)),25*abs(long.ret-2*short.ret),abs((2*long.ret-3*2*short.ret)),25*abs((2*long.ret-3*2*short.ret)))
returnAnalysis

returnAnalysis<-data.frame(long.ret,short.ret,abs((long.ret-2*short.ret)),4*25*abs(long.ret-2*short.ret))
returnAnalysis

//IBS Analysis
NSEI
nifty.ibs = (NSEI[, "NSEI.Close"]- NSEI[, "NSEI.Low"])/(NSEI[, "NSEI.High"]- NSEI[, "NSEI.Low"])
hist(nifty.ibs,12)
// Tata / Nifty

short2by3.profit<-2*(2000*(long.ret)-3*50*short.ret)
short1by1.profit<-(2000*(long.ret)-100*short.ret)
short1by1.profit<-6*(1000*(long.ret)-50*short.ret)
profitAnalysis<- data.frame(short2by3.profit,short1by1.profit)
profitAnalysis
last(short.ratio,48)
last(short.profit,48)
hist(short.profit)

returnAnalysis<-data.frame(long.ret,short.ret,abs((2000*long.ret-50*short.ret)))
returnAnalysis

USDINR<-(1/(getSymbols('INR/USD', src='oanda', auto.assign = FALSE,from=Sys.Date()-499)))
usdinr<-to.monthly(USDINR)

usdinr.Close<-usdinr[,"USDINR.Close"]
usdinr
usdinr.ret<-diff(usdinr.Close)
hist(usdinr.ret,breaks=10)
last(usdinr.ret,20)
summary(usdinr.ret)

usdinr.profit<-5*((5000*(usdinr.ret)-50*short.ret))
last(usdinr.profit,499)
