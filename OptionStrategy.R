nifty<-c(5500,5600,5700,5800,5900)


nifty<-seq(length=10,from=5500,by=50)
nifty
buyorder<-c(5523)
nifty-buyorder
sellorder<-c(5550)
sellorder2<-c(5600)
sellorder-nifty
sellorder2-nifty
sellorder3<-c(5800)
sellorder+sellorder2-nifty-nifty+2*(sellorder3-nifty)

nifty<-seq(length=10,from=5500,by=100)
nifty
//Buy 5700C option
buy5700C<-c(5700+133)
buy5700Ccoeff<-c(0,0,0,1,1,1,1,1,1,1)
PayOff5700CBuy<-(1-buy5700Ccoeff)*-133+(nifty-buy5700C)*buy5700Ccoeff
PayOff5700CBuy

//Sell 5700Call option
PayOff5700CSell<-(1-buy5700Ccoeff)*133+(buy5700C-nifty)*buy5700Ccoeff
PayOff5700CSell

PayOff5700CBuy+PayOff5700CSell
plot(nifty,PayOff5700CBuy+PayOff5700CSell)

//Buy 5700Put option
buy5700P<-c(5700-77)
buy5700Pcoeff<-c(1,1,0,0,0,0,0,0,0,0)
PayOff5700PBuy<-(1-buy5700Pcoeff)*-77+(buy5700P-nifty)*buy5700Pcoeff
PayOff5700PBuy

// Sell 5700Put option
PayOff5700PSell<-(1-buy5700Pcoeff)*77+(nifty-buy5700P)*buy5700Pcoeff
PayOff5700PSell
PayOff5700PSell+PayOff5700PBuy
plot(nifty,PayOff5700PSell)

banknifty<-seq(length=20,from=9500,by=100)

//Call
bankniftycoeff<-numeric(20)
bankniftycoeff<-array()
length(bankniftycoeff)<-20
for(i in 1:20){
  if(banknifty[i]<10000)
    bankniftycoeff[i]=0
  else
    bankniftycoeff[i]=1
}
bankniftycoeff
  
buy10000C<-c(10000+330)

PayOffBN10000CBuy<-(1-bankniftycoeff)*-330+(banknifty-buy10000C)*bankniftycoeff
PayOffBN10000CBuy

PayOffBN10000CSell<-(1-bankniftycoeff)*330+(buy10000C-banknifty)*bankniftycoeff
PayOffBN10000CSell

PayOffBN10000CSell+PayOffBN10000CBuy

//Put option
for(i in 1:20){
  if(banknifty[i]<10000)
    bankniftycoeff[i]=1
  else
    bankniftycoeff[i]=0
}
bankniftycoeff

buy10000P<-c(10000-77)

PayOff10000PBuy<-(1-bankniftycoeff)*-77+(buy10000P-banknifty)*bankniftycoeff
PayOff10000PBuy

PayOff10000PSell<-(1-bankniftycoeff)*77+(banknifty-buy10000P)*bankniftycoeff
PayOff10000PSell

PayOff10000PBuy+PayOff10000PSell



