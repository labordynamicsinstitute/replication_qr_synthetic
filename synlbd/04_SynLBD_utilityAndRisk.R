##Function to calculate utility and risk
##PSD 2018 code for "Synthetic data via Quantile Regression for Heavy-Tailed and Heteroskedastic Data"
##By Pistner, Slavkovic, and Vilhuber

source("config.R",echo=TRUE)

##Loading the required libraries
##See https://www2.vrdc.cornell.edu/news/synthetic-data-server/step-4-using-the-sds/ for instructions on how to download R packages
library(foreign)
library(MatrixModels,lib=dir.Rpackages)
library(SparseM,lib=dir.Rpackages)
library(quantreg,lib=dir.Rpackages)
library("data.table",lib=dir.Rpackages)
library(doParallel, lib=dir.Rpackages)
library("rpart",lib=dir.Rpackages)
library("dbscan",lib=dir.Rpackages)




##Function to calculate pMSE utility function
utility <- function(data,inds){
  mod=rpart(inds~., data=data, method="class",minbucket=5,cp=1e-4)
  preds=predict(mod)[,2]
  score=sum((preds-0.5)^2)/nrow(data)
  return(score)
}


##Empty variables
qr.risk=rep(NA,10)
qr.ut=rep(NA,10)
tmp.risk=rep(NA,5)
tmp.ut=rep(NA,5)
qr.ut.sd=rep(NA,10)
qr.risk.sd=rep(NA,10)

##SIC codes for which to calculate utility and risk measures
labels=c(178,239,328,354,473,511,542,703,829,865)

##Loading in the "true" data
##NOte that the "true" data in this case is the synLBD
##Note that "longitudinalSynLBD.csv" is just the year-by-year files merged together by SIC code
df=fread(paste(mydata,"longitudinalSynLBD.csv",sep="/"),na.strings="NA",integer64="numeric")


##First, calculating utility/risk for CART syntheses
##This itterates over all labels and different syntheses
##There are 6 different labels and 5 different syntheses
tmp.risk=rep(NA,5)
tmp.ut=rep(NA,5)
cart.ut.sd=rep(NA,10)
cart.risk.sd=rep(NA,10)
cart.ut=rep(NA,10)
cart.risk=rep(NA,10)

for(i in 1:10){
  for(j in 1:5){
    dir=paste(home.dir,"/Data/CART/CART",j,sep="")
    #setwd(dir)
    file=paste(dir,paste("CART_SynLBD_",labels[i],".csv",sep=""),sep="/")
    syns=fread(file)
    syns=round(syns)
    data=df[which(df$sic3==labels[i]),]
    data=as.data.frame(data)
    syns=as.data.frame(syns)
    ut.data=rbind(syns,data)
    inds=c(rep(0,nrow(syns)),rep(1,nrow(data)))
    tmp.ut[j]=utility(ut.data,inds)
    tmp.count = 0
    for(k in 1976:2000){
      vars.list = c("firstyear","lastyear","mu",paste("emp_",k,sep=""),paste("pay_",k,sep=""))
      data=syns[,vars.list]
      data=as.matrix(na.omit(data))
      res = lof(data,k=25)
      inds=ifelse(res>1.25,1,0)
      tmp.count = tmp.count + table(inds)[2]/nrow(data)
    }
    tmp.risk[j]= tmp.count/25
  }
  cart.ut[i]=mean(tmp.ut)
  cart.ut.sd[i]=sd(tmp.ut)
  
  cart.risk[i]=mean(tmp.risk)
  cart.risk.sd[i]=sd(tmp.risk)
  print(i)
}
#Writing results
cart.results = as.data.frame(cbind(labels,cart.ut,cart.ut.sd,cart.risk,cart.risk.sd))
fwrite(cart.results,paste(results,"CART_results.csv",sep="/"))


##First, calculating utility/risk for QR syntheses
##This itterates over all labels and different syntheses
tmp.risk=rep(NA,5)
tmp.ut=rep(NA,5)

for(i in 1:10){
  for(j in 1:5){
    dir=paste(home.dir,"/Data/QR/QR",j,sep="")
    #setwd(dir)
    file=paste(dir,paste("QR_SynLBD_",labels[i],".csv",sep=""),sep="/")
    syns=fread(file)
    syns=round(syns)
    data=df[which(df$sic3==labels[i]),]
    data=as.data.frame(data)
    syns=as.data.frame(syns)
    ut.data=rbind(syns,data)
    inds=c(rep(0,nrow(syns)),rep(1,nrow(data)))
    tmp.ut[j]=utility(ut.data,inds)
    
    tmp.count = 0
    for(k in 1976:2000){
      vars.list = c("firstyear","lastyear","mu",paste("emp_",k,sep=""),paste("pay_",k,sep=""))
      data=syns[,vars.list]
      data=as.matrix(na.omit(data))
      res = lof(data,k=25)
      inds=ifelse(res>1.25,1,0)
      tmp.count = tmp.count + table(inds)[2]/nrow(data)
    }
    tmp.risk[j]= tmp.count/25
  }
  qr.ut[i]=mean(tmp.ut)
  qr.ut.sd[i]=sd(tmp.ut)
  
  qr.risk[i]=mean(tmp.risk)
  qr.risk.sd[i]=sd(tmp.risk)
  print(i)
}

qr.ut
qr.ut.sd

qr.risk
qr.risk.sd


##Writing results
qr.results = as.data.frame(cbind(labels,qr.ut,qr.ut.sd,qr.risk,qr.risk.sd))
fwrite(qr.results,paste(results,"QR_results.csv",sep="/"))



