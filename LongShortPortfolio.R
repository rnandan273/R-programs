install.packages("TTR")
install.packages("forecast")
install.packages("quantmod")
install.packages("moments")

library(quantmod)
library(xts)
library(moments)
library(TTR)
library(forecast)

longStk1<-"AXISBANK.NS.Close"
longStk2<-"NSEBANK.Close"
longStk3<-"TATAMOTOR.NS.Close"
longStk4<-"HCLTECH.NS.Close"
longStk5<-"IDFC.NS.Close"
longStk6<-"MCDOWELL-.NS.Close"

shortStk<-"NSEI.Close"

getSymbols("AXISBANK.NS", from=Sys.Date()-699);
long1<-to.monthly(AXISBANK.NS)

getSymbols("^NSEBANK", from=Sys.Date()-699);
long2<-to.monthly(NSEBANK)

getSymbols("TATAMOTOR.NS", from=Sys.Date()-699)
long3<-to.monthly(TATAMOTOR.NS)

getSymbols("HCLTECH.NS", from=Sys.Date()-699)
long4<-to.monthly(HCLTECH.NS)

getSymbols("IDFC.NS", from=Sys.Date()-699);
long5<-to.monthly(IDFC.NS)



getSymbols("^NSEI", from=Sys.Date()-699)
short<-to.monthly(NSEI)


//long AXISBANK
long1.Close<-long1[,longStk1]
long1.ret<-diff(long1.Close)
last(long1.ret,48)
plot(long1.ret)
hist(long1.ret,breaks=15)
long1Profit<-250*(abs(long1.ret))
last(long1Profit,48)

//long BANKNIFTY
long2.Close<-long2[,longStk2]
long2.ret<-diff(long2.Close)
last(long2.ret,100)
plot(long2.ret)
hist(long2.ret,breaks=15)
long2Profit<-25*(abs(long2.ret))
last(long2Profit,48)

//long TATAMOTOR
long3.Close<-long3[,longStk3]
long3.ret<-diff(long3.Close)
last(long3.ret,100)
plot(long3.ret)
hist(long3.ret,breaks=15)
long3Profit<-1000*(abs(long3.ret))
last(long3Profit,48)

//long HCL
long4.Close<-long4[,longStk4]
long4.ret<-diff(long4.Close)
last(long4.ret,100)
plot(long4.ret)
hist(long4.ret,breaks=15)
long4Profit<-250*(abs(long4.ret))
last(long4Profit,48)

//long IDFC
long5.Close<-long5[,longStk5]
long5.ret<-diff(long5.Close)
last(long5.ret,100)
plot(long5.ret)
hist(long5.ret,breaks=15)
long5Profit<-4000*(abs(long5.ret))
last(long5Profit,48)

//long United Spirits
long6.Close<-long6[,longStk6]
long6.ret<-diff(long6.Close)
last(long6.ret,100)
plot(long6.ret)
hist(long6.ret,breaks=15)
long6Profit<-125*(abs(long6.ret))
last(long6Profit,48)

//short NIFTY
short.Close<-short[,shortStk]
short.ret<-diff(short.Close)
last(short.ret,100)
plot(short.ret)
hist(short.ret,breaks=15)
shortProfit<-50*(abs(short.ret))
last(shortProfit,48)



short1by1.profit<-long1Profit+long2Profit+long3Profit+long4Profit+long5Profit - (5*shortProfit)-20000
last(short1by1.profit,48)
hist(short1by1.profit,breaks=30,48)

axis.profit<-long1Profit - (shortProfit)-5000
bn.profit<-long2Profit - (shortProfit)-5000
tata.profit<-long3Profit - (shortProfit)-5000
hcl.profit<-long4Profit - (shortProfit)-5000
idfc.profit<-long5Profit - (shortProfit)-5000


profitAnalysis<- data.frame(short1by1.profit,axis.profit,bn.profit,tata.profit,hcl.profit,idfc.profit)
profitAnalysis

profitAnalysis<- data.frame(short1by1.profit,2*bn.profit,2*idfc.profit,2*axis.profit)
profitAnalysis

matplot(profitAnalysis, type = c("b"),pch=1,col = 0:2)

