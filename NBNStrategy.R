nifty <- read.csv("~/R/nifty.csv")
banknifty <- read.csv("~/R/banknifty.csv")
bnn<- data.frame(nifty$Open,banknifty$Open)
bnn["ratio"]<-banknifty$Open/nifty$Open
plot(bnn$ratio)
bnn["niftyprofit"]<-nifty$Open-nifty$Close
bnn["bankniftyprofit"]<-banknifty$Open-banknifty$Close
bnn["buyBNsellNifty"]<-25*bnn$bankniftyprofit-50*bnn$niftyprofit
bnn["buy2BNsell3Nifty"]<-25*2*bnn$bankniftyprofit-50*3*bnn$niftyprofit
bnn["bnnRatio"]<-banknifty$Open/nifty$Open
str(bnn)

plot(bnn$bnnRatio,bnn$buyBNsellNifty)
plot(bnn$bnnRatio,bnn$buy2BNsell3Nifty)
hist(bnn$bnnRatio)
hist(bnn$buyBNsellNifty)
hist(bnn$buy2BNsell3Nifty)

niftyMonthly <- read.csv("niftyMonthly.csv")
niftyMonthly <- read.csv("~/R/niftyMonthly.csv")
bankniftyMonthly<-read.csv("~/R/bankniftyMonthly.csv")
bnnMonthly<- data.frame(niftyMonthly$Open,bankniftyMonthly$Open)
bnnMonthly["ratio"]<-bankniftyMonthly$Open/niftyMonthly$Open
niftyWeekly <- read.csv("niftyWeekly.csv")
niftyWeekly <- read.csv("~/R/niftyWeekly.csv")
bankniftyWeekly <- read.csv("~/R/bankniftyWeekly.csv")
bnnWeekly<- data.frame(niftyWeekly$Open,bankniftyWeekly$Open)
bnnWeekly["ratio"]<-bankniftyWeekly$Open/niftyWeekly$Open
bnnWeekly["niftyRange"]<-niftyWeekly$Close - niftyWeekly$Open
bnnWeekly["bankniftyRange"]<-bankniftyWeekly$Close - bankniftyWeekly$Open
bnnWeekly["tradeRange"]<-bnnWeekly$bankniftyRange - bnnWeekly$niftyRange
bnnMonthly["niftyRange"]<-niftyMonthly$Close - niftyMonthly$Open
bnnMonthly["bankniftyRange"]<-bankniftyMonthly$Close - bankniftyMonthly$Open
bnnMonthly["tradeRange"]<-bnnMonthly$bankniftyRange - bnnMonthly$niftyRange
bnnWeekly["niftyprofit"]<-niftyWeekly$Open-niftyWeekly$Close
bnnMonthly["niftyprofit"]<-niftyMonthly$Open-niftyMonthly$Close
bnnMonthly["bankniftyprofit"]<-bankniftyMonthly$Open-bankniftyMonthly$Close
bnnWeekly["bankniftyprofit"]<-bankniftyWeekly$Open-bankniftyWeekly$Close
bnnMonthly["buyBNsellN"]<-25*bnnMonthly$bankniftyprofit - 50*bnnMonthly$niftyprofit
bnnWeekly["buyBNsellN"]<-25*bnnWeekly$bankniftyprofit - 50*bnnWeekly$niftyprofit

hist(bnnMonthly$niftyRange)
hist(bnnMonthly$bankniftyRange)

hist(bnnMonthly$tradeRange)
hist(bnnWeekly$tradeRange)
plot(bnnMonthly$tradeRange,bnnMonthly$ratio)
plot(bnnMonthly$ratio)
plot(bnnWeekly$ratio)

// KNN algorithm
wbcd<-read.csv("~/R/wdbc.csv",stringsAsFactors=FALSE);
str(wbcd)
wbcd<-wbcd[-1]
table(wbcd$diagnosis)
wbcd$diagnosis<-factor(wbcd$diagnosis,levels=c("="B","M"),labels=c("Benign","Malignant")
                       )
round(prop.table(table(wbcd$diagnosis))*100,digits=1)
summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])
normalize<-function(x){ 
return ((x-min(x)) / (max(x)-min(x)));
}
normalize(c(10,20,30,40,50))
wbcd_n<-as.data.frame(lapply(wbcd[2:31],normalize))
summary(wbcd_n$area_mean)
wbcd_train<-wbcd_n[1:469,]
wbcd_test<-wbcd_n[470:569,]
summary(wbcd_train)
summary(wbcd_test)
install.packages("class")
library(class)
wbcd_test_pred<-knn(train=wbcd_train,test=wbcd_test,cl=wbcd_train_labels,k=21)
wbcd_train_labels<-wbcd[1:469,1]
wbcd_test_labels<-wbcd[470:569,1]
install.packages("gmodels")
library(gmodels)
summary(wbcd_test_labels)
CrossTable(x=wbcd_test_labels,y=wbcd_test_pred,prop.chisq=FALSE)
summary(wbcd_test_pred)
wbcd_test_pred[11]


//MySQL db access 

install.packages("RMySQL");
library(RMySQL);
con = dbConnect(MySQL(), user='TAUSER', password='w3lc0m31', dbname='ta_report_db', host='ta-db.dc1.tumri.net');
dbListTables(con)
query<-'select DATE_FORMAT(m.date_info,'%Y-%m-%d') as TIME, c.name as CAMPAIGN, concat(t.width, ' x ' , t.height) as SIZE, e.name as EXPERIENCE, concat((m.recipeid >> 32) & 0xFFFFFFFF, ': ', LPAD((m.recipeid & 0x3F), 2, '0'), '-', LPAD(((m.recipeid >> 6) & 0x3F), 2, '0'), '-', LPAD(((m.recipeid >> 12) & 0x3F), 2, '0'), '-', LPAD(((m.recipeid >> 18) & 0x3F), 2, '0'), '-', LPAD(((m.recipeid >> 24) & 0x3F), 2, '0')) as VARIATION, p.placement_name as PLACEMENT, p.placement_id as PLACEMENT_ID, s.site_name as SITE, s.site_id as SITE_ID, (sum(m.mcpm) / 1000) as COST ,  (sum(m.mcpm) / sum(m.impression_count)) * 1000 as ECPM, loc.name as LOCATION,loc.id as LOCATION_ID, sum(m.impression_count) as IMPRESSIONS, sum(m.click_count) as CLICKS, 100.0 * sum(m.click_count)/sum(m.impression_count) as CTR, sum(m.engagement_count) as ENGAGEMENTS, 100.0 * sum(m.engagement_count)/sum(m.impression_count) as ENGAGEMENT_RATE, sum(m.CTC_0 + m.ETC_0 + m.VTC_0) as CONVERSIONS, 100.0 * sum(m.CTC_0 + m.ETC_0 + m.VTC_0)/sum(m.impression_count) as CONVERSION_RATE, sum(m.CTC_REVENUE_0 + m.ETC_REVENUE_0 + m.VTC_REVENUE_0) as REVENUE from performance_daily m left outer join placements p on p.advertiser_id = m.advertiserid and p.placement_id = m.externalpageid  left outer join locations loc on loc.id = m.locationid and loc.advertiserid = m.advertiserid  left outer join site s  on s.advertiser_id = m.advertiserid and s.site_id = m.externalsiteid  left outer join experiences e on e.id = ((m.recipeid >> 32) & 0xFFFFFFFF) left outer join adtypes t on t.adtype = m.adtype  left outer join campaigns c on c.advertiserid = m.advertiserid and c.id = m.campaignid where m.advertiserid = 3249 and m.date_info between '2013-11-01' and '2013-12-31' and m.adtype in ('skyscraper', 'mediumrectangle', 'leaderboard', 'UNKNOWN') and  ( m.campaignid = 4693 and m.recipeid  in (40651865456640 , 40651865460736 , 40651865456704 , 40651865460800 , 40656160423936 , 40656160428032 , 40656160424000 , 40656160428096 , 40660455391232 , 40660455395328 , 40660455391296 , 40660455395392 , -1 ) ) group by m.date_info, m.campaignid, m.adtype, EXPERIENCE, VARIATION, m.externalpageid, m.externalsiteid, null, null, m.locationid order by TIME ASC limit 0, 200';
rs <- dbSendQuery(con,query);
data <- fetch(rs, n=10)
huh <- dbHasCompleted(rs)
dbClearResult(rs)
dbDisconnect(con)


install.packages("TTR");
library(TTR)
install.packages("forecast");
library(forecast)
nifty <- read.csv("~/R/nifty.csv")
summary(nifty)
str(nifty)
niftyClose<- data.frame(nifty$Close)
str(niftyClose)
niftyTS<-ts(niftyClose)
str(niftyTS)
plot.ts(niftyTS)
plot.ts(SMA(niftyTS,n=100))
niftyTSF <- HoltWinters(niftyTS, beta=FALSE, gamma=FALSE)
plot(niftyTSF)
niftyTSF2 <- forecast.HoltWinters(niftyTSF, h=8)
plot(niftyTSF2)
nfusd <- read.csv("~/R/nfusd.csv")
str(nfusd)
usdnfratio<-nfusd$nifty/nfusd$usd
plot(usdnfratio)
hist(usdnfratio)


getMetals(c('XAU', 'XAG'), from=Sys.Date()-365)
ls()
head(XAUUSD)
chartSeries(XAUUSD,theme="white");
silver = data.frame(XAUUSD);
silver$date=as.Date(rownames(silver));
colnames(silver)[1]='price';

library(ggplot2)
ggplot(data=silver, aes(x=date, y=price)) + geom_line() + theme_bw()
library("TTR")

//// using the google source
install.packages("quantmod")
install.packages("moments")
library(quantmod)
library(xts)
library(moments)

getSymbols("^NSEI", from=Sys.Date()-2000)
nseMonthly<-to.monthly(NSEI)
head(nseMonthly)
//head(NSEI)
//tail(NSEI)
//Daily
NSEI.Close <- NSEI[, "NSEI.Close"]
NSEI.ret<-diff(NSEI.Close)
NSEI.ret <- NSEI.ret[-1]
last(NSEI.ret,100)
plot(NSEI.ret)
hist(NSEI.ret)
//Monthly
nseMonthly.Close<-nseMonthly[,"NSEI.Close"]
nseMonthly.ret<-diff(nseMonthly.Close)
last(nseMonthly.ret,100)
plot(nseMonthly.ret)
hist(nseMonthly.ret)

getSymbols("^NSEBANK", from=Sys.Date()-2000);
bnMonthly<-to.monthly(NSEBANK)
head(NSEBANK)
tail(NSEBANK)
NSEBANK.Close <- NSEBANK[, "NSEBANK.Close"]
NSEBANK.ret<-diff(NSEBANK.Close)
NSEBANK.ret <- NSEBANK.ret[-1]
last(NSEBANK.ret,100)
plot(NSEBANK.ret)
hist(NSEBANK.ret)

//Monthly
bnMonthly.Close<-bnMonthly[,"NSEBANK.Close"]
bnMonthly.ret<-diff(bnMonthly.Close)
last(bnMonthly.ret,10)
plot(bnMonthly.ret)
hist(bnMonthly.ret)

NSEI.ratio<- NSEBANK[,"NSEBANK.Close"] / NSEI[, "NSEI.Close"]
plot(NSEI.ratio)
chartSeries(NSEI.ratio,theme="white");

nseMonthly.ratio<- bnMonthly[,"NSEBANK.Close"] / nseMonthly[, "NSEI.Close"]
plot(nseMonthly.ratio)
chartSeries(nseMonthly.ratio,theme="white");

last(nseMonthly.ratio,10)

nseMonthly2by3.profit<-(75*bnMonthly.ret - 100*nseMonthly.ret)
last(nseMonthly2by3.profit,10)
last(nseMonthly2by3.ratio,10)
hist(nseMonthly2by3.profit)

nseMonthly1by1.profit<-5*(50*(bnMonthly.ret)-100*nseMonthly.ret)
last(nseMonthly1by1.profit,48)
hist(nseMonthly1by1.profit)