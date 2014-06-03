install.packages("TTR")
install.packages("forecast")
install.packages("quantmod")
install.packages("moments")

library(quantmod)
library(xts)
library(moments)
library(TTR)
library(forecast)

sysarr<-new.env()
LongCloseTag<-c("AXISBANK.NS.Close", "NSEBANK.Close","TATAMOTOR.NS.Close","HCLTECH.NS.Close","IDFC.NS.Close","NMDC.NS.Close","AUROPHARM.NS.Close","TATAPOWER.NS.Close","ANDHRABAN.NS.Close","TATASTEEL.NS.Close","RELCAPITA.NS.Close","KTKBANK.NS.Close","ADANIENT.NS.Close","YESBANK.NS.Close","CENTURYTE.NS.Close","BPCL.NS.Close","IOC.NS.Close","AMBUJACEM.NS.Close")
LongSymbol<-c("AXISBANK.NS", "^NSEBANK","TATAMOTOR.NS","HCLTECH.NS","IDFC.NS","NMDC.NS","AUROPHARM.NS","TATAPOWER.NS","ANDHRABAN.NS","TATASTEEL.NS","RELCAPITA.NS","KTKBANK.NS","ADANIENT.NS","YESBANK.NS","CENTURYTE.NS","BPCL.NS","IOC.NS","AMBUJACEM.NS")
LongLotSize<-c(250,25,1000,250,4000,2000,2000,4000,4000,1000,1000,4000,2000,1000,1000,1000,1000,2000)

getSymbols("^NSEI", from=Sys.Date()-699)
short<-to.monthly(NSEI)
i=1,2,3,4,11,7,12,13,14
i=9
getSymbols(LongSymbol[i+1:0], from=Sys.Date()-699)
if(i==0) long<-to.monthly(AXISBANK.NS)
if(i==1) long<-to.monthly(NSEBANK)
if(i==2) long<-to.monthly(TATAMOTOR.NS)
if(i==3) long<-to.monthly(HCLTECH.NS)
if(i==4) long<-to.monthly(IDFC.NS)
if(i==5) long<-to.monthly(NMDC.NS)
if(i==6) long<-to.monthly(AUROPHARM.NS)
if(i==7) long<-to.monthly(TATAPOWER.NS)
if(i==8) long<-to.monthly(ANDHRABAN.NS)
if(i==9) long<-to.monthly(TATASTEEL.NS)
if(i==10) long<-to.monthly(RELCAPITA.NS)
if(i==11) long<-to.monthly(KTKBANK.NS)
if(i==12) long<-to.monthly(ADANIENT.NS)
if(i==13) long<-to.monthly(YESBANK.NS)
if(i==14) long<-to.monthly(CENTURYTE.NS)
if(i==15) long<-to.monthly(BPCL.NS)
if(i==16) long<-to.monthly(IOC.NS)
if(i==17) long<-to.monthly(AMBUJACEM.NS)

print(long)

//long
long.Close<-long[,LongCloseTag[i+1]]
long.ret<-diff(long.Close)
last(long.ret,48)
plot(long.ret)
hist(long.ret,breaks=15)
longProfit<-LongLotSize[i+1]*(abs(long.ret))
last(longProfit,48)

//short NIFTY
short.Close<-short[,shortStk]
short.ret<-diff(short.Close)
last(short.ret,100)
plot(short.ret)
hist(short.ret,breaks=15)
shortProfit<-50*(abs(short.ret))
last(shortProfit,48)



profitAnalysis<- data.frame(shortProfit,longProfit,2*longProfit-2*shortProfit-5000)
profitAnalysis


