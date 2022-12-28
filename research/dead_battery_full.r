#HydroWind Analysis - 2nd Review

library(foreign) # loads package for reading stata binary data read.dta
library(nlme) #for running gls
library(TTR) #for moving averages
library(Amelia) #for imputing data see:  http://r.iq.harvard.edu/docs/amelia
library(sandwich)  #newey west (HAC)
library(tseries) # for Dickey-Fuller among others
library(dynlm)
library(zoo) #for matching irregular time series

#setwd("/Users/johannesmauritzen/Dropbox/hydrowind/RhydroWind") #mac
setwd("C:/Users/Johannes/Dropbox/hydrowind/RhydroWind") #pc

NO1prod<-read.table("NO1production.csv", header=TRUE, sep=",", fill=TRUE, comment.char = "#") #production in MWh/h
priceWind<- read.dta("hydroWind20_8_11.dta", convert.dates = TRUE, convert.factors = TRUE, missing.type = FALSE, convert.underscore = FALSE, warn.missing.labels = TRUE)
windVar<-read.dta("WindVar12_8_10.dta", convert.dates = TRUE, convert.factors = TRUE, missing.type = FALSE, convert.underscore = FALSE, warn.missing.labels = TRUE)
DKWCap<-read.table("DKWCapacities.csv", header=TRUE, sep=",", fill=TRUE, comment.char = "#")

colnames(DKWCap)<-c("date", "cap")

priceWind$date<-as.Date(priceWind$date,origin="1960-01-01")
NO1prod$sdate<-as.character(NO1prod$date)
NO1prod$date<-as.Date(NO1prod$date, format="%d.%m.%Y")
windVar$date<-as.Date(windVar$date, format="%d.%m.%Y")
DKWCap$date<-as.Date(DKWCap$date, format="%Y-%m-%d")

#create zoo objects and merge into one zoo date frame
DKWCap<-zoo(DKWCap$cap, order.by=DKWCap$date)


NO1Prod<-zoo(NO1prod$sum, order.by=NO1prod$date) #1

NO1Price<-zoo(priceWind$norPrice, order.by=priceWind$date) #2

DKWPrice<-zoo(priceWind$dwtDA, order.by=priceWind$date) #3

DKEPrice<-zoo(priceWind$detDA, order.by=priceWind$date) #4

DKWCons<-zoo(priceWind$DKWCons, order.by=priceWind$date) #5

DKECons<-zoo(priceWind$DKECons,  order.by=priceWind$date)#6

DKWind<-zoo(priceWind$windProd_total, order.by=priceWind$date) #7

DK_NO<-zoo(priceWind$NODKExport, order.by=priceWind$date)#8

SpotP<-zoo(windVar$spotprTotal, order.by=windVar$date)#9

norCons<-zoo(priceWind$ncTot, order.by=priceWind$date) #10

LHS<- DKWPrice
RHS<-merge(DKWind)
#RHS2<-merge(DKWCons + DKECons, norCons, DKWind, all=TRUE, fill=NA)
#RHS3<-merge(DKWCons + DKECons, norCons, DKWind*exp, DKWind*imp, all=TRUE, fill=NA)

LHS<-LHS[index(LHS)>="2002-1-1"&index(LHS)<"2008-1-1",]
RHS<-RHS[index(RHS)>="2002-1-1"&index(RHS)<"2008-1-1",]

#imputation of Data
LHS<-na.approx(LHS)
RHS<-na.approx(RHS)

exp<- (DK_NO>0)*1 #export to Norway
imp<-(DK_NO<0)*1 #Import to Denmark

#now create four indicator variables for DKE
#expEqual<-(equalDKW==1 & exp==1)*1
#impEqual<-(equalDKW==1 & imp==1)*1
#expDiff <-(equalDKW==0 & exp==1)*1
#impDiff <-(equalDKW==0 & imp==1)*1

ln_LHS<-log(LHS)
ln_RHS<-log(RHS)

#create logarithm


#Arima models with interaction term - effect on prices 
arima.DKW<-arima(ln_LHS, order=c(3,0,0), seasonal=list(order=c(2,0,0), period=7), xreg=ln_RHS)


#Arima models with interaction term only imports lumped - effect on prices in DKE
#Make sure to change the above definitions first to DKWEqual
arima.DKE<-arima(RHS, order=c(3,0,0), seasonal=list(order=c(2,0,0), period=7), xreg=ln_Data[,c(5,6,10,13:15)])
arima.DKW<-arima(ln_Data[,3], order=c(3,0,0), seasonal=list(order=c(2,0,0), period=7), xreg=ln_Data[,c(5,6,10,13:15)])
arima.NO<-arima(ln_Data[,2], order=c(3,0,0), seasonal=list(order=c(2,0,0), period=7), xreg=ln_Data[,c(5,6,10,13:15)])

#now looking at effect when including an interaction term.  
#now include interaction affect
arima.DKE.int<-arima(ln_Data[,4], order=c(3,0,0), seasonal=list(order=c(2,0,0), period=7), xreg=cbind(ln_Data[,7], equalDKE.wind))
arima.DKW.int<-arima(ln_Data[,3], order=c(3,0,0), seasonal=list(order=c(2,0,0), period=7), xreg=cbind(ln_Data[,7], equalDKW.wind))






#HydroWind Analysis - 2nd Review - 2nd part

# setwd("/Users/johannesmauritzen/Dropbox/marketPower")  #mac
# #setwd("/Users/Johannes/Dropbox/marketPower") #pc
# NOExtraTrade<-read.table("NOExtraTrade", header=TRUE, sep=",", fill=TRUE, comment.char = "#", col.names=c("date", "extraTrade")) #production i

# setwd("/Users/johannesmauritzen/Dropbox/hydrowind/RhydroWind") #mac
# #setwd("C:/Users/Johannes/Dropbox/hydrowind/RhydroWind") #pc

# NO1prod<-read.table("NO1production.csv", header=TRUE, sep=",", fill=TRUE, comment.char = "#") #production in MWh/h
# priceWind<- read.dta("hydroWind20_8_11.dta", convert.dates = TRUE, convert.factors = TRUE, missing.type = FALSE, convert.underscore = FALSE, warn.missing.labels = TRUE)
# windVar<-read.dta("WindVar12_8_10.dta", convert.dates = TRUE, convert.factors = TRUE, missing.type = FALSE, convert.underscore = FALSE, warn.missing.labels = TRUE)
# NO1flow<-read.table("NO1Flow", header=TRUE, sep=",", fill=TRUE, comment.char = "#") #flow from Norway
# DKSEtrade<-read.table("DKSEtrade", header=TRUE, sep=",", fill=TRUE, comment.char = "#") # trade from DK to SE

# priceWind$date<-as.Date(priceWind$date,origin="1960-01-01")
# NO1prod$sdate<-as.character(NO1prod$date)
# NO1prod$date<-as.Date(NO1prod$date, format="%d.%m.%Y")
# windVar$date<-as.Date(windVar$date, format="%d.%m.%Y")
# colnames(NO1flow)<-c("date", "flow")
# NO1flow$date<-as.Date(NO1flow$date, format="%Y-%m-%d")
# NOExtraTrade$date<-as.Date(NOExtraTrade$date, format="%Y-%m-%d")
# DKSEtrade$date<-as.Date(DKSEtrade$date, format="%d.%m.%Y")

# DKSEtrade<-DKSEtrade[!is.na(DKSEtrade$date),] #get rid of NA's
# duplicates<-DKSEtrade[duplicated(DKSEtrade$date),]
# DKSEtrade<-DKSEtrade[row.names(DKSEtrade)!=row.names(duplicates),]

#create zoo objects and merge into one zoo date frame
NO1flow<-zoo(NO1flow$flow, order.by=NO1flow$date) #1

DK_NO<-zoo(priceWind$NODKExport, order.by=priceWind$date)#2

NOTradeE<-zoo(NOExtraTrade$extraTrade, order.by=NOExtraTrade$date) #3

NO1Prod<-zoo(NO1prod$sum, order.by=NO1prod$date) #4

DKWCons<-zoo(priceWind$DKWCons, order.by=priceWind$date) #5

DKECons<-zoo(priceWind$DKECons,  order.by=priceWind$date)#5

norCons<-zoo(priceWind$ncTot, order.by=priceWind$date) #6

DKWind<-zoo(priceWind$windProd_total, order.by=priceWind$date) #7

NOtemp<-zoo(priceWind$norTemp, order.by=priceWind$date) #8

DKSEtrade<-zoo(DKSEtrade$sum, order.by=DKSEtrade$date)

rm(NO1prod, priceWind, windVar, NOExtraTrade)

#export and import from Denmark
exp<- (DK_NO>0)*1 #export to Norway
imp<-(DK_NO<0)*1 #Import to Denmark

#Left Hand Side and Right Hand Side
LHS<- -NOTradeE
LHS2<-DK_NO
LHS3<-DK_NO-DKSEtrade

RHS<-merge(DKWind, all=TRUE, fill=NA)
RHS2<-merge((DKWCons + DKECons)*24,  norCons, DKWind, all=TRUE, fill=NA)
RHS3<-merge((DKWCons + DKECons)*24, norCons, DKWind*exp, DKWind*imp, all=TRUE, fill=NA)

LHS<-LHS[index(LHS)>="2002-1-1"&index(LHS)<"2008-1-1",]
RHS<-RHS[index(RHS)>="2002-1-1"&index(RHS)<"2008-1-1",]

LHS2<-LHS2[index(LHS2)>="2002-1-1"&index(LHS2)<"2008-1-1",]
RHS2<-RHS2[index(RHS2)>="2002-1-1"&index(RHS2)<"2008-1-1",]

LHS3<-LHS3[index(LHS3)>="2002-1-1"&index(LHS3)<"2008-1-1",]
RHS3<-RHS3[index(RHS3)>="2002-1-1"&index(RHS3)<"2008-1-1",]


#imputation of Data
LHS<-na.approx(LHS)
LHS2<-na.approx(LHS2)
LHS3<-na.approx(LHS3)
RHS<-na.approx(RHS)
RHS2<-na.approx(RHS2)
RHS3<-na.approx(RHS3)

#test whether addition of consumption variables makes any difference
arima.Trade<-arima(LHS, order=c(6,0,0), seasonal=list(order=c(2,0,0), period=7), xreg=RHS)
arima.Trade2<-arima(LHS, order=c(6,0,0), seasonal=list(order=c(2,0,0), period=7), xreg=RHS2)
arima.Trade3<-arima(LHS, order=c(6,0,0), seasonal=list(order=c(2,0,0), period=7), xreg=RHS3)

arima.Trade21<-arima(LHS2, order=c(6,0,0), seasonal=list(order=c(2,0,0), period=7), xreg=RHS)
arima.Trade22<-arima(LHS2, order=c(6,0,0), seasonal=list(order=c(2,0,0), period=7), xreg=RHS2)
arima.Trade23<-arima(LHS2, order=c(6,0,0), seasonal=list(order=c(2,0,0), period=7), xreg=RHS3)

arima.Trade31<-arima(LHS3, order=c(6,0,0), seasonal=list(order=c(2,0,0), period=7), xreg=RHS)
arima.Trade32<-arima(LHS3, order=c(6,0,0), seasonal=list(order=c(2,0,0), period=7), xreg=RHS2)
arima.Trade33<-arima(LHS3, order=c(6,0,0), seasonal=list(order=c(2,0,0), period=7), xreg=RHS3)

arima.Trade
arima.Trade2
arima.Trade3

arima.Trade21
arima.Trade22
arima.Trade23

arima.Trade31
arima.Trade32
arima.Trade33

arima.Trade21$aic
arima.Trade22$aic
arima.Trade23$aic

arima.Trade31$aic
arima.Trade32$aic
arima.Trade33$aic






#part 3 analysis **************************************************************************



#read in data sets
# NO1prod<-read.table("NO1production.csv", header=TRUE, sep=",", fill=TRUE, comment.char = "#") #production in MWh/h
# priceWind<- read.dta("hydroWind20_8_11.dta", convert.dates = TRUE, convert.factors = TRUE, missing.type = FALSE, convert.underscore = FALSE, warn.missing.labels = TRUE)

# #get date formats to match
# priceWind$date<-as.Date(priceWind$date,origin="1960-01-01")
# NO1prod$sdate<-as.character(NO1prod$date)
# NO1prod$date<-as.Date(NO1prod$date, format="%d.%m.%Y")

# #merge together data sets by date
# HydroData<-merge(priceWind, NO1prod, by="date")
# rm(NO1prod)
# rm(priceWind)

#remove duplicates that were created


#create time series
#start from 2002: 
#NO1prod<-NO1prod[!NO1prod$year==2001, ]
#NO1prod<-NO1prod[-1,]
prod.ts<-ts(HydroData$sum[HydroData$date>="2002-1-31"&HydroData$date<"2008-01-01"]) 
price.ts<-ts(HydroData$norPrice[HydroData$date>="2002-1-31"&HydroData$date<"2008-01-01"])
wind.ts<-ts(HydroData$wind[HydroData$date>="2002-1-31"&HydroData$date<"2008-01-01"])
date<-HydroData$date[HydroData$date>="2002-1-31"&HydroData$date<"2008-01-01"]
wind.ts<-wind.ts*1000 # make it match mwh
dkTOno.ts<-ts(HydroData$NODKExport[HydroData$date>="2002-1-31"&HydroData$date<"2008-01-01"])
NOConsum.ts<-ts(HydroData$ncTot[HydroData$date>="2002-1-31"&HydroData$date<"2008-01-01"])
NOtemp.ts<-ts(HydroData$norTemp[HydroData$date>="2002-1-31"&HydroData$date<"2008-01-01"])


#imputation of missing data data using Amelia II
data<-data.frame(prod.ts, wind.ts, price.ts, dkTOno.ts, NOConsum.ts, NOtemp.ts, date)
a.data<-amelia(data, ts="date", m=1)
ImpData<-a.data$imputations[[1]]
prod.ts<-ImpData$prod.ts
wind.ts<-ImpData$wind.ts
price.ts<-ImpData$price.ts
dkTOno.ts<-ImpData$dkTOno.ts
NOConsum.ts<-ImpData$NOConsum.ts
NOtemp.ts<-ImpData$NOtemp.ts

rm(data, ImpData)


#dummy variables for net export or import

exp<- ts((dkTOno.ts>0)*1) #export to Norway
imp<-ts((dkTOno.ts<0)*1) #Import to Denmark

#expoenential moving averag
MAprod.ts<-EMA(a.prod.ts, n=7)
MAwind.ts<-EMA(a.wind.ts, n=7)

plot(date[-1:-6], MAprod.ts[-1:-6], col="red", type="l")
points(a.prod.ts[-1:-6], type="l", col="gray")

#kernel smoothing 
#production
plot(date, prod.ts, main="Norwegian production and Kernel Smoothing", ylab="production")
lines(ksmooth(date, prod.ts, bandwidth=7), col="red")
lines(ksmooth(date, prod.ts, bandwidth=30), col="blue")

#wind production
plot(date, wind.ts, main="Wind Power Production and Kernel Smoothing", ylab="production")
lines(ksmooth(date, wind.ts, bandwidth=7), col="red")
lines(ksmooth(date, wind.ts, bandwidth=30), col="blue")

#production and export
plot(date, prod.ts, type="l")
par(new=TRUE)
plot(date, dkTOno.ts*-1, type="l", yaxt='n', ylab="", col="red")
axis(4, col="red", ylab="export")

#Denmark to Norway Export
plot(date, dkTOno.ts, type="l", ylab="Denmark to Norway Electricity Trade, MWh/h", xlab="")

#inversed
plot(date, prod.ts, type="l")
par(new=TRUE)
plot(date, dkTOno.ts*-1, type="l", yaxt='n', ylab="", col="red")
axis(4, col="red", ylab="export")

#create lagged series
wind.ts.l1<-lag(wind.ts)
prod.ts.l1<-lag(prod.ts, K=1)
prod.ts.l2<-lag(prod.ts, K=2)
prod.ts.l3<-lag(prod.ts, K=3)
prod.ts.l6<-lag(prod.ts, K=6)
prod.ts.l7<-lag(prod.ts, K=7)

#put all in one object
windX<-ts.intersect(prod.ts, wind.ts, lag(wind.ts), NOConsum.ts, NOtemp.ts, wind.ts*exp, wind.ts*imp) # combine multiple time series

#with ar terms for hydro production
windX.lm<-ts.intersect(prod.ts, wind.ts, lag(wind.ts), prod.ts.l1, NOConsum.ts, NOtemp.ts, wind.ts*exp, wind.ts*imp) # combine multiple time series


#plot.ts(wind.ts)

#test for stationarity
adf.test(prod.ts, alternative = "stationary",
         k = trunc((length(prod.ts)-1)^(1/3)))

adf.test(wind.ts, alternative = "stationary",
         k = trunc((length(prod.ts)-1)^(1/3)))



#analysis using arma and ar models
arma.hydro<-arima(windX[,1], order=c(3,0,2), seasonal=list(order=c(1,0,1), period=7), xreg=windX[,2:3])
arma.hydro

layout(1,2)
acf(resid(arma.hydro))
pacf(resid(arma.hydro))
w<-resid(arma.hydro)
Box.test(w, 12, type="Ljung") #null hypothesis of independent

#first difference arima model
arima.hydro<-arima(windX[,1], order=c(2,1,2), seasonal=list(order=c(1,0,1), period=7), xreg=windX[,2:3])
arima.hydro
acf(resid(arima.hydro))
pacf(resid(arima.hydro))
wi<-resid(arima.hydro)
Box.test(wi, 12, type="Ljung")

#now arima model with consumption term and split into export and import
arima.hydro.cons<-arima(windX[,1], order=c(2,1,2), seasonal=list(order=c(1,0,1), period=7), xreg=windX[,2:5])
arima.hydro.int<-arima(windX[,1], order=c(2,1,2), seasonal=list(order=c(1,0,1), period=7), xreg=windX[,2:6])
arima.hydro.int2<-arima(windX[,1], order=c(2,1,2), seasonal=list(order=c(1,0,1), period=7), xreg=windX[,3:7])
arima.hydro.cons
arima.hydro.int
arima.hydro.int2

acf(resid(arima.hydro.int))
pacf(resid(arima.hydro.int))
wii<-resid(arima.hydro.int)

#Box Ljung test for autocorrelation
Box.test(wii, 12, type="Ljung")

#simple analysis just using gls
gls.hydro<-gls(prod.ts ~ wind.ts)
gls.hydro<-gls(windX[,1] ~ windX[,2]+windX[,3])
summary(gls.hydro)
gls.D1.hydro<-gls(D1.windX[,1] ~ D1.windX[,2:3])
summary(gls.D1.hydro)
#simple analysis using OLS and newey west corrections for AC

#using differencing
D1.windX<-diff(windX)
lm.D1.hydro<-lm(D1.windX[,1]~D1.windX[,2:4])
summary(lm.D1.hydro)
NWcov<-NeweyWest(lm.D1.hydro)
coeftest(lm.D1.hydro, vcov=NeweyWest)

#calculation of benefits for Norway in kroners
ArbitData<-data.frame(cbind(price.ts, wind.ts, dkTOno.ts, exp, imp))
meanPrice<-mean(ArbitData$price.ts, na.rm=TRUE)
meanEXP<-mean(ArbitData$price.ts[ArbitData$exp==1], na.rm=TRUE)
meanIMP<-mean(ArbitData$price.ts[ArbitData$imp==1], na.rm=TRUE)
length(ArbitData$price.ts[ArbitData$exp==1])
length(ArbitData$price.ts[ArbitData$imp==1])
ArbitData$arbit<-(meanEXP-meanIMP)*.30*wind.ts*ArbitData$exp

#with harmonics
Time<-date
gls.harm<-lm(windX[,1] + sin(2*pi*Time) + cos(2*pi*Time)) 

#arma.NW<-NeweyWest(arma.hydro) needs an lm object for Newey west

#include consumption term

#devide into periods of net imports and net exports

#harmonics
SIN<-COS <- matrix(nr = length(prod.ts), nc=6) #creates 6 columns for various frequencies of harmonics
for(i in 1:6) {
 COS[,i]<-cos(2*pi*i*time(prod.ts))
 SIN[,1]<-sin(2*pi*i*time(prod.ts))
 }
 

 # make wind into mwh/h as well

#get the units to be the same so that we can do a kwh to kwh comparison
#production is in MWH

acf(prod.ts)
pacf(prod.ts)


