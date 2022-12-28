#run WindErrorCleanWest
library(lattice)
library(zoo)
library(xts)
library(foreign) # loads package for reading stata binary data read.dta
library(nlme) #for running gls
#library(TTR) #for moving averages
#library(Amelia) #for imputing data see:  http://r.iq.harvard.edu/docs/amelia
library(sandwich)  #newey west (HAC)
library(tseries) # for Dickey-Fuller among others
library(dynlm)
library(chron)
library(stats)
library(lmtest)
library(boot)
library(Hmisc)

# png("/Users/johannesmauritzen/Google Drive/Research/WindError/EJSubmission/Final.png", 
#   width = 35, height = 21, units = "cm", res=300, pointsize=10)
# print(cost_over_time_plot)
# dev.off()

#source("C:/Users/johannesm.IFN/Google Drive/WindError/WindErrorCleanWest.R") #pc #skip this and load cleaned data file   #below
#source("/Users/johannesmauritzen/Google Drive/WindError/WindErrorCleanWest.R") #mac

#source("C:/Users/johannesm.IFN/Google Drive/Rwork/my_functions/cond_expectation.R") #pc #use location of file on your computer
source("/Users/johannesmauritzen/Google Drive/Rwork/my_functions/cond_expectation.R") #mac
#cond.expectation<-function(cond.data, bins, band.width) cond.data - includes x, y variable

#source("C:/Users/johannesm.IFN/Google Drive/Rwork/my_functions/table_formatting.R") #pc  
source("/Users/johannesmauritzen/Google Drive/Rwork/my_functions/table_formatting.R") #mac


setwd("/Users/johannesmauritzen/Google Drive/WindError")
#write.table(Data, file="ForecastError.csv", sep=";")
#Data<-read.table("ForecastError.csv", sep=";", stringsAsFactors=FALSE) #use this to read in data

# Danish.production<-with(Data, cbind(date,time,windSetW))
# write.csv(Danish.production, file="windProduction_Denmark.csv")

Data<-read.csv("WindError_data.csv",sep=",", stringsAsFactors=FALSE)
#Analysis part*************************************************************************************************
#1 wDate,  2 wTime, 3 windSetW,4  windEstW, 5 diff, 6 ElbasDKW,
#7 BalanceDKW,  8 DKWElbON, 9 error_plus, 10 error_neg, 11 error_plus_sq, 12 error_neg_sq
#13 PriceDKW, 14 BalanceDKWON
#simple linear probability model of whether Elbas market is used or not

#average
#mean(DKWElbon)

neg.reg.down.price<-subset(Data, bal.price.down<0)
  
#plotting************************************
#plainData<-coredata(Data)
Data$windSetW<-Data$windSetW/100 #put in 100MW units

with(data=Data, plot(bal.price.up, ElbasDKW, xlab="Regulation Up Price", ylab="Elbas Price", xlim=c(0,200), ylim=c(0,150)))
     abline(a=0, b=1, lwd=3, col="red")

with(data=Data, plot(bal.price.down, ElbasDKW, xlab="Regulation Down Price", ylab="Elbas Price", xlim=c(0,150), ylim=c(0,150)))
  abline(a=0, b=1, lwd=3, col="red")

with(data=Data, plot(index(Data), bal.price.up-ElbasDKW))
  abline(a=0, b=0, lwd=3, col="red")


with(data=Data, plot(PriceDKW, ElbasDKW, xlab="Day-ahead Price", ylab="Elbas Price", ylim=c(0,150), xlim=c(-10,150)))
  abline(a=0, b=1, lwd=3, col="red")


with(data=Data, plot(PriceDKW, bal.price.down, xlab="Day-ahead Price", ylab="Regulation Down Price", ylim=c(-100,150), xlim=c(0,150)))
  abline(a=0, b=1, lwd=3, col="red")
  abline(a=0, b=0, lwd=2, lty=2)



with(data=Data, plot(PriceDKW, bal.price.up, xlab="Day-ahead Price", ylab="Regulation Up Price", ylim=c(0,300), xlim=c(0,150)))
  abline(a=0, b=1, lwd=3, col="red")

with(data=Data, plot(sys.imbalance.pos, bal.to.down, xlab="System imbalance", ylab="Regulating Up/down Volume", ylim=c(-600,600), xlim=c(-1000,1000)))
with(data=Data, points(sys.imbalance.neg, bal.to.up))
abline(a=0, b=-1, lwd=3, col="red")




#relationship between system imbalance, upward balancing and total wind power*******************************
with(data=Data, plot(sys.imbalance, windSetW, xlab="sys.imbalance", ylab="Total Wind Power"))

lm.sys.imbal1<-lm(sys.imbalance~windSetW + error_neg + error_neg_sq + error_plus + error_plus_sq , data=Data) #effect of total amount of wind
coeftest(lm.sys.imbal1, vcov=NeweyWest)
         
coeftest(lm.sys.imbal2, vcov=NeweyWest)
summary(lm.sys.imbal2)

lm.sys.imbal3<-lm(neg.sys.imb.dummy~error_neg + error_neg_sq + error_plus + error_plus_sq, data=Data)
coeftest(lm.sys.imbal3, vcov=NeweyWest)
summary(lm.sys.imbal3)

#probability of neg system imbalance
lm.sys.imbal4.1<-lm(neg.sys.imb.dummy~windEstW + error_neg + error_neg_sq + error_plus + error_plus_sq, data=Data)
coeftest(lm.sys.imbal4.1, vcov=NeweyWest)
#summary(lm.sys.imbal4.1)

#probability of pos system imbalance
lm.sys.imbal4.2<-lm(neg.sys.imb.dummy~windSetW + error_neg + error_neg_sq + error_plus + error_plus_sq, data=Data)
coeftest(lm.sys.imbal4.2, vcov=NeweyWest)
#summary(lm.sys.imbal4.2)
         
#now include information on elbas
lm.sys.imbal5<-lm(neg.sys.imb.dummy~ windEstW, data=Data)
coeftest(lm.sys.imbal5, vcov=NeweyWest)
         
         
#price in balancing market
lm.price.up<-lm(bal.price.up~ windSetW +error_neg + error_neg_sq + error_plus + error_plus_sq, data=Data)        
coeftest(lm.price.up, vcov=NeweyWest)
         
lm.price.down<-lm(bal.price.down~ windSetW +error_neg + error_neg_sq + error_plus + error_plus_sq, data=Data)        
coeftest(lm.price.down, vcov=NeweyWest)
         
         
#probability of regulation trade
lm.reg.up<-lm(reg.up.dummy ~ windSetW + error_neg + error_neg_sq + error_plus + error_plus_sq, data=Data)        
coeftest(lm.reg.up, vcov=NeweyWest)

lm.reg.down<-lm(reg.down.dummy ~ windSetW + error_neg + error_neg_sq + error_plus + error_plus_sq, data=Data)        
coeftest(lm.reg.down, vcov=NeweyWest)

#volume of regulation trade   
Data$bal.to.down[which(is.na(Data$bal.to.down))]<-0
Data$bal.to.up[which(is.na(Data$bal.to.up))]<-0   

# lm.reg.up<-lm(DKwElbon ~ windSetW + error_neg + error_neg_sq + error_plus + error_plus_sq, data=Data)        
# coeftest(lm.reg.up, vcov=NeweyWest)

# lm.reg.down<-lm(bal.to.down ~ windSetW + error_neg + error_neg_sq + error_plus + error_plus_sq, data=Data)        
# coeftest(lm.reg.down, vcov=NeweyWest)

#plot with bins and draw smoothed line
WindData<-Data[order(Data$windSetW),]

WindData$bins<-cut(WindData$windSetW, 200, ordered_result=TRUE) #cut into 200 lengths
#mean function without
meanFunc<-function(FuncData){return(sum(FuncData)/length(FuncData))}

WindMean<-tapply(WindData$windSetW, WindData$bins, meanFunc)
ave.imbalance<-tapply(WindData$sys.imbalance, WindData$bins, meanFunc)

#smoother
smoothData<-as.data.frame(cbind(ave.imbalance, WindMean))
#smoothData<-smoothData[smoothData$AveError>-7 & smoothData$AveError<7,]
smoothData<-smoothData[!is.na(smoothData$ave.imbalance),]
smoothData<-smoothData[!is.na(smoothData$WindMean),]

smoothProb<-ksmooth(smoothData$WindMean, smoothData$ave.imbalance,  kernel="normal", bandwidth=2)

plot(smoothData$WindMean, smoothData$ave.imbalance,
     xlab="Total Wind Power, 100mwh", ylab="System Imbalance" )
lines(smoothProb, col=2, lwd=2)
abline(a=0, b=0, lty=2)






#Kernel smoother of the forecast errors and system imbalance:*****************
WindData<-Data[order(Data$diff),]

WindData$bins<-cut(WindData$diff, 300, ordered_result=TRUE) #cut into 200 lengths
ave.error<-tapply(WindData$diff, WindData$bins, meanFunc)
ave.imbalance<-tapply(WindData$sys.imbalance, WindData$bins, meanFunc)

#smoother
smoothData<-as.data.frame(cbind(ave.error, ave.imbalance))
#smoothData<-smoothData[smoothData$AveError>-7 & smoothData$AveError<7,]
smoothData<-smoothData[!is.na(smoothData$ave.error),]
smoothData<-smoothData[!is.na(smoothData$ave.imbalance),]

smoothProb<-ksmooth(smoothData$ave.error, smoothData$ave.imbalance,  kernel="normal", bandwidth=2)

plot(smoothData$ave.error, smoothData$ave.imbalance,
     xlab="Wind Forecast Error, 100mwh", ylab="System Imbalance" , xlim=c(-8,8))
lines(smoothProb, col=2, lwd=2)
abline(a=0, b=0, lty=2)


#now with probability*******************************
prob.imbalance<-tapply(WindData$neg.sys.imb.dummy, WindData$bins, meanFunc)

#smoother
smoothData<-as.data.frame(cbind(prob.imbalance, WindMean))
#smoothData<-smoothData[smoothData$AveError>-7 & smoothData$AveError<7,]
smoothData<-smoothData[!is.na(smoothData$prob.imbalance),]
smoothData<-smoothData[!is.na(smoothData$WindMean),]

smoothProb<-ksmooth(smoothData$WindMean, smoothData$prob.imbalance,  kernel="normal", bandwidth=2)

plot(smoothData$WindMean, smoothData$prob.imbalance,
     xlab="Total Wind Power, 100mwh", ylab="Probability of Negative System Imbalance" )
lines(smoothProb, col=2, lwd=2)
abline(a=.5, b=0, lty=2)


#test for stationarity of error series##################################
diff.noNA<-Data$diff[!is.na(Data$diff)]
adf.test(diff.noNA)



#simple non-linear model for trading (0,1) - interaction term for negative or positive
#lmElbasNL<-lm(WData[,8]~WData[,9:10])
#summary(lmElbasNL)
#error_plusNA<-error_plus
#error_negNA<-error_neg
#error_plus_sqNA<-error_plus_sq
#error_neg_sqNA<-error_neg_sq



lmElbasNL2<-lm(DKWElbON~ error_plus + error_neg + error_plus_sq + error_neg_sq, data=Data)
coeftest(lmElbasNL2, vcov=NeweyWest)
summary(lmElbasNL2)
ElbasNLFitted<-lmElbasNL2$fitted


lmElbasNL3<-lm(DKWElbON~ error_plus + error_neg + error_plus_sq + error_neg_sq +
  pos.sys.imb.dummy + neg.sys.imb.dummy, data=Data)
coeftest(lmElbasNL3, vcov=NeweyWest)
summary(lmElbasNL3)


#create fitted line
Error<-seq(-6, 6, by=.02)
prob_plus <- .47 - .052*Error + .0057*(Error^2)
prob_neg <- .47 -.066*Error - .0043*(Error^2)
prob_plus[which(Error<0)]<-NA
prob_neg[which(Error>0)]<-NA
plot(Error, prob_plus, type="l", ylim=c(0,1), ylab="Probability of Elbas Trade", xlab="Forecast Error, 100 mWh")
lines(Error, prob_neg)
abline(v=0, lty=2)

#non parametric effect of error on probability of operation (fix!)
smoothData<-cond.expectation(cbind(Data$diff, Data$DKWElbON),200, 1) #200 bins, 2 bw

plot(smoothData$x.mean, smoothData$y.mean, xlim=c(-6,6), ylim=c(0,1),
     xlab="Forecast Error, 100mwh", ylab="Probability of Elbas Trade" )
lines(smoothData$x.smooth, smoothData$y.smooth, col=2)


prob_plus_s = .47 - .052*smoothData$x.mean + .0057*(smoothData$x.mean^2)
prob_neg_s = .47 -.066*smoothData$x.mean - .0043*(smoothData$x.mean^2)
prob_plus_s[which(smoothData$x.mean<0)]<-NA
prob_neg_s[which(smoothData$x.mean>0)]<-NA
lines(smoothData$x.mean, prob_plus_s)
lines(smoothData$x.mean, prob_neg_s)
legend(1, c("Quadratic", "Non-parametric"), cex=0.8, col=c(1,2), lty=1, bty="n");



#Histogram
error<-Data$diff
error<-error[order(error)]
bins<-seq(-25,20,by=0.2)
hist(error, breaks=bins, freq=FALSE, xlim=c(-7,7), xlab="Forecast Error, 100mwh", main="")

#seperately for positive and negative forecast errors

lmElbasNL3<-lm(DKWElbON~error_plusNA + error_plus_sqNA) 
lmElbasNL4<-lm(DKWElbON~error_negNA + error_neg_sqNA) 
coeftest(lmElbasNL3, vcov=NeweyWest)
coeftest(lmElbasNL4, vcov=NeweyWest)






#correlation with actual wind power production*********************************************
#1 wDate,  2 wTime, 3 windSetW,4  windEstW, 5 diff, 6 ElbasDKW,
#7 BalanceDKW,  8 DKWElbON, 9 error_plus, 10 error_neg, 11 error_plus_sq, 12 error_neg_sq
#13




#create bins and smooth of error and SettledWindW
input<-as.data.frame(cbind(Data$diff, Data$windSetW))
smoothData<-cond.expectation(input,300,.5)

plot(smoothData$x.mean, smoothData$y.mean, xlim=c(-7,7),
     xlab="Forecast Error, 100mwh", ylab="Total Wind Power" )
lines(smoothData$x.smooth, smoothData$y.smooth, col=2)

#regression including total wind power as well
lmElbasNL3<-lm(DKWElbON~ error_plus + error_neg + error_plus_sq + error_neg_sq + SettledWindW)
coeftest(lmElbasNL3, vcov=NeweyWest)
summary(lmElbasNL3)




#regression on actual elbas prices*********************************************************
SettledWindW<-as.numeric(Data[,3])/100
SettledWindW_sq<-SettledWindW^2
logWindW<-log(SettledWindW)
logWindW[which(logWindW==NaN)]<-NA
logWindW[which(logWindW==Inf)]<-NA

ElbasPrice<-as.numeric(Data[,6])
#ElbasPrice[which(ElbasPrice==0)]<-NA
#ElbasPrice[which(ElbasPrice<0)]<-NA
logElbasPrice<-log(ElbasPrice)

logerror_plus<-log(error_plusNA)
logerror_neg<-log(error_negNA)
logerror_plus_sq<-log(error_plus_sqNA)
logerror_neg_sq<-log(error_neg_sqNA)

logerror_plus[is.na(logerror_plus)]<-0
logerror_neg[is.na(logerror_neg)]<-0
logerror_plus_sq[is.na(logerror_plus_sq)]<-0
logerror_neg_sq[is.na(logerror_neg_sq)]<-0

priceData<-cbind(logElbasPrice, logerror_plus, logerror_plus_sq, logerror_neg, logerror_neg_sq, logWindW)
#priceData<-priceData[!is.na(priceData[,1]),]
#logElbasPrice<-priceData[,1]
#logerror_plus<-priceData[,2]
#logerror_neg<-priceData[,4]


#log price estimate
lmElbasPrice<-lm(logElbasPrice~logerror_plus + logerror_neg)
coeftest(lmElbasPrice, vcov=NeweyWest)

lmElbasPrice2<-lm(logElbasPrice~logerror_plus + logerror_neg +logWindW)

#in linear terms.  
ElbasPrice1<-lm(ElbasPrice~error_plus + error_plus_sq + error_neg + error_neg_sq)
coeftest(ElbasPrice1)
ElbasPrice2<-lm(ElbasPrice~error_plus + error_plus_sq + error_neg + error_neg_sq + SettledWindW + SettledWindW_sq)
coeftest(ElbasPrice2, vcov=NeweyWest)
summary(ElbasPrice2)

#Non-parametric for effect of Settled Wind on prices
#order according to error
error<-as.numeric(Data[,5])
date<-as.Date(as.character(Data[,1]), format="%Y-%m-%d")
NPData<-as.data.frame(cbind(error, date, SettledWindW, ElbasPrice))
NPData<-NPData[order(NPData$SettledWindW),]

NPData$bins<-cut(NPData$SettledWindW, 200, ordered_result=TRUE) #cut into 100 lengths
#mean function without
meanFunc<-function(FuncData){return(mean(FuncData,na.rm=TRUE))}

priceMean<-tapply(NPData$ElbasPrice, NPData$bins, meanFunc)
AveWind<-tapply(NPData$SettledWindW, NPData$bins, meanFunc)

#smoother
smoothData<-as.data.frame(cbind(AveWind, priceMean))
smoothData<-smoothData[!is.na(smoothData$priceMean), ]

smoothProb<-ksmooth(smoothData$AveWind, smoothData$priceMean, kernel="normal", bandwidth=4)

plot(smoothData$AveWind, smoothData$priceMean,
     xlab="Total Settled Wind Power, 100mwh", ylab="Price")
lines(smoothProb, col=2)

#Effect of forecast errors on price difference between Elbas and Spot price for every hour******************
SpotDKW<-as.numeric(Data[,13])
Pricediff<-as.numeric(Data[,6])-as.numeric(Data[,13]) #Elbas-Spot
ElbasPrice<-as.numeric(Data[,6])
plot(SpotDKW, ElbasPrice)
abline(a=0,b=1)

ElbasSpot<-lm(Pricediff~error_plus + error_plus_sq + error_neg + error_neg_sq + SettledWindW + SettledWindW_sq)
coeftest(ElbasSpot, vcov=NeweyWest)
summary(ElbasSpot)

ElbasSpot2<-lm(Pricediff~error_plus + error_plus_sq + error_neg + error_neg_sq)
coeftest(ElbasSpot2, vcov=NeweyWest)
summary(ElbasSpot2)


#order according to the hour - see if hours farther away had higher error.  *******************************
meanNA<-function(FunData){mean(FunData, na.rm=TRUE)}
errorAve<-by(abs(WData$diff),WData$wTime, meanNA)

#WData<-WData[order(WData$wTime),]
#plot(WData$wTime, WData$diff


#check the effect of wind forecast error on volume/turn over

#check the dynamics of the 0 1 variable
acf(WData[,8])
pacf(WData[,8])

acf(diff, na.action=na.pass)
acf(error_pos, na.)

# is the mean of the difference different from zero
wDiffMean<-mean(diff, na.rm=TRUE)
wDiffSD<-sd(diff,na.rm=TRUE)
n<-length(diff)
#answer is zero.  

plot(dates(index(diff)), diff)
SE_WD<-wDiffSD/sqrt(n)
error<-qt(.90, df=n-1)*priceDiffSD/sqrt(n)

#create moving average
diff<-diff[!is.na(diff),]
SmoothDiff<-SMA(diff, n=168)
plot(dates(index(diff)),diff, col="gray")
lines(dates(index(diff)), SmoothDiff, col="black")
abline(h=0)

acf(diff, na.action=na.pass)
pacf(diff, na.action=na.pass, xaxt="n", xlab="")
axis(1, ylab="lags")
plot(1:48, )


#30_12_05Analysis of Prices

hist.up<-hist(Data$bal.price.up, freq=T, breaks=400)
hist.down<-hist(Data$bal.price.down, freq=T, new=T, breaks=200)
plot(hist.up, xlim=c(-50,150), col="#0000FF50", main="", xlab="prices")
plot(hist.down, add=T, col="#FF000050")
legend("topleft", c("Regulation Price Up", "Regulation Price Down"), cex=0.8, col=c("#0000FF50","#FF000050"), lty=1, lwd=3, bty="n");

boxplot(cbind(Data$bal.price.up, Data$bal.price.down), ylim=c(-200,400), xaxt='n')
axis(1, at= 1:2, labels=c("Reg-up Prices", "Reg-down Prices")





#smoothed curve of regulation prices and wind power************** 
     
WindData<-as.data.frame(cbind(Data$bal.price.up, Data$bal.price.down, Data$PriceDKW, Data$windSetW))
     colnames(WindData)<-c("bal.price.up", "bal.price.down", "PriceDKW", "windSetW")
WindData<-WindData[order(WindData$windSetW),]

WindData$bins<-cut(WindData$windSetW, 300, ordered_result=TRUE) #cut into 300 lengths
#mean function without
meanFunc<-function(FuncData){return(sum(FuncData)/length(FuncData))}

WindMean<-tapply(WindData$windSetW, WindData$bins, meanFunc)
bal.price.up.mean<-tapply(WindData$bal.price.up, WindData$bins, meanFunc)
bal.price.down.mean<-tapply(WindData$bal.price.down, WindData$bins, meanFunc)
PriceDKW.mean<-tapply(WindData$PriceDKW, WindData$bins, meanFunc)

#smoother
smoothData<-as.data.frame(cbind(bal.price.up.mean, bal.price.down.mean,PriceDKW.mean, WindMean))
#smoothData<-smoothData[smoothData$AveError>-7 & smoothData$AveError<7,]
smoothData<-smoothData[!is.na(smoothData$bal.price.up.mean), ]
smoothData<-smoothData[!is.na(smoothData$bal.price.down.mean), ]     

smoothProb<-ksmooth(smoothData$WindMean, smoothData$bal.price.up.mean, kernel="normal", bandwidth=5)
smoothProb.down<-ksmooth(smoothData$WindMean, smoothData$bal.price.down.mean, kernel="normal", bandwidth=5)
smoothProb.dkw<-ksmooth(smoothData$WindMean, smoothData$PriceDKW.mean, kernel="normal", bandwidth=5)
     
plot(smoothData$WindMean, smoothData$bal.price.up.mean, ylim=c(0, 70),
     xlab="Total Wind Power, 100mwh", ylab="Regulation Prices, EUR/MWH" )
lines(smoothProb, col="red", lwd=3)
         
points(smoothData$WindMean, smoothData$bal.price.down.mean, col="grey")
lines(smoothProb.down, col="red", lwd=3)
points(smoothData$WindMean, smoothData$PriceDKW.mean, pch="*")
legend("bottomleft", c("Regulation Price Up", "Regulation Price Down", "Spot Price"), cex=0.8, pch=c(1,1,"*"), col=c("black", "grey", "black"), bty="n");
    
     
#now prices and wind power error****************
     
     WindData<-as.data.frame(cbind(Data$bal.price.up, Data$bal.price.down, Data$PriceDKW, Data$diff))
     colnames(WindData)<-c("bal.price.up", "bal.price.down", "PriceDKW", "diff")
     WindData<-WindData[order(WindData$diff),]
     
     WindData$bins<-cut(WindData$diff, 300, ordered_result=TRUE) #cut into 300 lengths
     #mean function without
     meanFunc<-function(FuncData){return(sum(FuncData)/length(FuncData))}
     
     diff.mean<-tapply(WindData$diff, WindData$bins, meanFunc)
     bal.price.up.mean<-tapply(WindData$bal.price.up, WindData$bins, meanFunc)
     bal.price.down.mean<-tapply(WindData$bal.price.down, WindData$bins, meanFunc)
     PriceDKW.mean<-tapply(WindData$PriceDKW, WindData$bins, meanFunc)
     
     #smoother
     smoothData<-as.data.frame(cbind(bal.price.up.mean, bal.price.down.mean,PriceDKW.mean, diff.mean))
     #smoothData<-smoothData[smoothData$AveError>-7 & smoothData$AveError<7,]
     smoothData<-smoothData[!is.na(smoothData$bal.price.up.mean), ]
     smoothData<-smoothData[!is.na(smoothData$bal.price.down.mean), ]     
     
     smoothProb<-ksmooth(smoothData$diff.mean, smoothData$bal.price.up.mean, kernel="normal", bandwidth=5)
     smoothProb.down<-ksmooth(smoothData$diff.mean, smoothData$bal.price.down.mean, kernel="normal", bandwidth=5)
     smoothProb.dkw<-ksmooth(smoothData$diff.mean, smoothData$PriceDKW.mean, kernel="normal", bandwidth=5)
     
     plot(smoothData$diff.mean, smoothData$bal.price.up.mean, ylim=c(0, 70), xlim=c(-6,6),
          xlab="Forecast Error, 100mwh", ylab="Regulation Prices, EUR/MWH" )
     lines(smoothProb, col="red", lwd=3)
     
     points(smoothData$diff.mean, smoothData$bal.price.down.mean, col="grey")
     lines(smoothProb.down, col="red", lwd=3)
     points(smoothData$diff.mean, smoothData$PriceDKW.mean, pch="*")
     legend("bottomleft", c("Regulation Price Up", "Regulation Price Down", "Spot Price"), cex=0.8, pch=c(1,1,"*"), col=c("black", "grey", "black"), bty="n");
     abline(v=0, lty=2)
 
     
     # Forecast Errors and balance market turnover #I should make a function that does this automatically.  
     
     WindData<-as.data.frame(cbind(Data$bal.to.up, Data$bal.to.down,  Data$diff))
     colnames(WindData)<-c("bal.to.up", "bal.to.down",  "diff")
     WindData$bal.to.up[which(is.na(WindData$bal.to.up))]<-0
     WindData$bal.to.down[which(is.na(WindData$bal.to.down))]<-0
     WindData<-WindData[order(WindData$diff),]
     
     WindData$bins<-cut(WindData$diff, 300, ordered_result=TRUE) #cut into 300 lengths
     #mean function without
     meanFunc<-function(FuncData){return(sum(FuncData)/length(FuncData))}
     
     diff.mean<-tapply(WindData$diff, WindData$bins, meanFunc)
     bal.to.up.mean<-tapply(WindData$bal.to.up, WindData$bins, meanFunc)
     bal.to.down.mean<-tapply(WindData$bal.to.down, WindData$bins, meanFunc)
     
     #smoother
     smoothData<-as.data.frame(cbind(bal.to.up.mean, bal.to.down.mean, diff.mean))
     #smoothData<-smoothData[smoothData$AveError>-7 & smoothData$AveError<7,]
     #smoothData$bal.to.up.mean[is.na(smoothData$bal.to.up.mean), ]
     #smoothData<-smoothData[!is.na(smoothData$bal.to.down.mean), ]     
     
     smoothProb<-ksmooth(smoothData$diff.mean, smoothData$bal.to.up.mean, kernel="normal", bandwidth=15)
     smoothProb.down<-ksmooth(smoothData$diff.mean, smoothData$bal.to.down.mean, kernel="normal", bandwidth=15)
     
     plot(smoothData$diff.mean, smoothData$bal.to.up.mean, xlim=c(-6,6), ylim=c(-100, 150),
          xlab="Forecast Error, 100mwh", ylab="Regulation Turnover" )
     #lines(smoothProb, col="red", lwd=3)
     
     points(smoothData$diff.mean, smoothData$bal.to.down.mean, col="grey")
     #lines(smoothProb.down, col="red", lwd=3)
     legend("bottomleft", c("Regulation Turnover Up", "Regulation Turnover Down"), cex=0.8, pch=1, col=c("black", "grey"), bty="n");
     abline(v=0, lty=2)
     
#Effect on balance prices
source("/Users/johannesmauritzen/Google Drive/Rwork/my_functions/cond_expectation.R") #mac
     #up
bal.price.data<-as.data.frame(cbind(Data$diff, Data$bal.price.up))
c.data<-cond.expectation(bal.price.data)
        
plot(c.data$x.mean, c.data$y.mean,
      xlab="Forecast Error, 100mwh", xlim=c(-7,7), ylim=c(40,70), ylab="Regulation Price Up" )
lines(c.data$x.smooth, c.data$y.smooth, col="red", lwd=3)
abline(v=0, lty=2)
    
     
     #down     
bal.price.data<-as.data.frame(cbind(Data$diff, Data$bal.price.down))
c.data<-cond.expectation(bal.price.data, 400, 3)
     
plot(c.data$x.mean, c.data$y.mean,
   xlab="Forecast Error, 100mwh", xlim=c(-7,7), ylim=c(0,60), ylab="Regulation Price Up" )
lines(c.data$x.smooth, c.data$y.smooth, col="red", lwd=3)
abline(v=0, lty=2)
     
     














#Regressions with data on consumption updated 16.12.2013 for resubmission********************************************************
WindData<-read.csv("/Users/johannesmauritzen/Google Drive/WindError/WindError_data.csv", stringsAsFactors=FALSE)
WindData$DK.West.and.Germany.mwh_h<-as.numeric(WindData$DK.West.and.Germany.mwh_h) 
#show relationship between Elbas trade and consumption errors
     
    
#first just with total amount of wind power
lm.1<-lm(DKWElbON~windSetW, data=WindData) #effect of total amount of wind
nw.1<-coeftest(lm.1, vcov=NeweyWest)

#now just with errors
lm.2<-lm(DKWElbON~error_neg + error_neg_sq + error_plus + error_plus_sq , data=WindData) #effect of total amount of wind
nw.2<-coeftest(lm.2, vcov=NeweyWest)
     
#now with
lm.3<-lm(DKWElbON~windSetW + error_neg + error_neg_sq + error_plus + error_plus_sq , data=WindData) #effect of total amount of wind
nw.3<-coeftest(lm.3, vcov=NeweyWest)
     
#now with consumption
#are wind and consumption errors correlated
lm.error<-lm(cons.diff.dkw~diff, data=WindData)
coeftest(lm.error)
cor(x=WindData$cons.diff.dkw, y=WindData$diff, use="pairwise.complete.obs")
cor(x=WindData$cons.diff.dk, y=WindData$diff, use="pairwise.complete.obs")
     
coeftest(lm.error)

WindData$cons.error.plus.w.sq<-WindData$cons.error.plus.w^2
WindData$cons.error.neg.w.sq<-WindData$cons.error.neg.w^2

lm.4<-lm(DKWElbON~windSetW + error_neg + error_neg_sq + error_plus + error_plus_sq + cons.error.plus +cons.error.neg + DK.West.and.Germany.mwh_h, data=WindData) #effect of total amount of wind
nw.4<-coeftest(lm.4, vcov=NeweyWest)
     
#put into table format with p-values
#test<-summary(lm.1)
#head(test$coefficients)
col.1<-table.paren(lm.1, nw.1)
col.2<-table.paren(lm.2, nw.2)
col.3<-table.paren(lm.3, nw.3)
col.4<-table.paren(lm.4, nw.4)
columns<-list(col.1, col.2, col.3, col.4)
lm.table<-multi.merge(columns)


