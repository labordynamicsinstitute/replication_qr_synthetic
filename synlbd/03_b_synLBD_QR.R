##Generating synthetic data using quantile regression
##PSD 2018 code for "Synthetic data via Quantile Regression for Heavy-Tailed and Heteroskedastic Data"
##By Pistner, Slavkovic, and Vilhuber

source("config.R",echo=TRUE)

##Loading the required libraries
##See https://www2.vrdc.cornell.edu/news/synthetic-data-server/step-4-using-the-sds/ for instructions on how to download R packages

library(foreign)
library(MatrixModels,lib=dir.Rpackages)
library(SparseM,lib=dir.Rpackages)
library(quantreg,lib=dir.Rpackages)
library(stringr,lib=dir.Rpackages)
library("data.table",lib=dir.Rpackages)
library(doParallel, lib=dir.Rpackages)
library("rpart",lib=dir.Rpackages)
library("ggplot2",lib=dir.Rpackages)
library("here",lib=dir.Rpackages)
library(foreach,lib=dir.Rpackages)
library(doParallel,lib=dir.Rpackages)


##Now, declaring the functions for generating synthetic data
##This function is as general as possible

qr.synth <- function(df,j,labels, bins=50,type="rq",rq.method="fn",stop.val=2,binary=FALSE,dir=""){
  ##Subsetting data for the desired label
  sic= labels[j]
  data=data.frame(df[which(df$sic3==sic),-1])
  vars=sample(1:nrow(data),nrow(data),replace=TRUE)
  vars=data.frame(data[vars,1])
  names(vars)=names(data)[c(1)]
  
  ##This code truncates (0,1) into partitions and finds the median quantile of each bin
  ##The number of bins determines the number of partitions
  ##The median of each bin is then what is used as the quantile in each regression model
  part.vals=seq(0.0000000000001,0.999999999999,.000001)
  part=tapply(part.vals,cut(part.vals,bins),median)
  part=data.frame(part)
  part=part[!duplicated(part),]
  part=data.frame(part)
  part$count=1:nrow(part)
  part$names=rownames(part)
  part.endpoints=data.frame(do.call('rbind',strsplit(as.character(part$names),",",fixed=TRUE)))
  part.endpoints$X1=gsub("[(]","",part.endpoints$X1)
  part.endpoints$X2=gsub("]","",part.endpoints$X2)
  part.endpoints$X1=as.numeric(part.endpoints$X1)
  part.endpoints$X2=as.numeric(part.endpoints$X2)
  
##Declaring variables for later use
  p=ncol(data) ##Number of variables
  n=nrow(data) ##Number of observations
  vars=data.frame(vars) ##Verifying that "vars" is a data frame
##Declaring empty vectors for later use
  k=rep(NA,n)
  syns=rep(NA,n)
  tau=rep(NA,n)

  ##Stop.val is the indicator of the first variable to be synthesized
  ##Now, looping over every variable to be synthesized
  for(i in stop.val:p){
    ##Last predictor to be used
    stop=i-1
    ##Saving the names of the variables that will be the synthetic data
    tmpNames=names(data)[1:i]
    ##Actual synthesis function
    ##We save the synthesis after each step (when it is still incomplete)
    ##This is because QR might run into singularity issues
    ##Then, all work is not lost!
    #setwd(dir)
    if(i > 3){vars=fread(paste(dir,"tmp.csv",sep="/"))}
    vars=bin.reg(data,vars,part,stop,rq.method,part.endpoints,binary,tmpNames,dir)
    if(is.na(vars)){return(NA)}
    ##Making sure the naming is conssitent
    names(vars)=tmpNames
    ##Just a consistency check
    print(dim(vars))
  }#end of for
  ##Making sure the names are consistent
  colnames(vars)=colnames(data)
  vars$sic3=sic
  ##Saving synthetic data 
  #setwd(dir)
  file_name=paste(dir,"/","QR_SynLBD_",sic,".csv",sep="")
  fwrite(vars,file=file_name,append=FALSE)
  print(sic)
  print(j)
  return(i)
}#end of function

bin.reg <-function(data,vars,part,stop,rq.method="fn",part.endpoints,binary,tmpNames,dir){
  ##Declaring empty vectors that will be used later
  syn=c()
  vars2=c()
  vars3=c()
  count=1
  n.obs=nrow(data)
  
  ##The next chunk of code is used to determine what predictors are used to fit the models
  ##This comes from the structure of the data set
  ##If the vars is either "lastyear" or "mu", just use those predictors
  ##If it is one of the pay or emp variables, save "firstyear", "lastyear", and "mu" automatically
  if(stop <= 3) {names=colnames(data)[1:stop]} else {names=colnames(data)[1:3]}
  pred=colnames(data)[(stop+1)]

##Determine the formula to use

##Include and "emp.check" and "pay.check" variable because these formulas have different forms
##Have an indicator variable if this is a birth year for a business.
##At the bare minimum, we need the names we had from before
##For mod.births, no firstyear is used (because of colinearity issues -> firstyear will be the same for all observations)
##Later, the model for births won't have lagged pay and employement information
  mod.formula = paste(pred,"~",paste(names,collapse="+"),sep="")
  mod.births = paste(pred,"~",paste(names[-1],collapse="+"),sep="")
  mod.noFy=mod.births
##Check is used to screen for "pay" and "emp" variables
  check=substr(pred,1,3)
    if(check == "emp" | check == "pay"){
      year=substr(pred,5,8)
      ##Year determines what lagged variables to use
      ##IF year = 1976, we want the models to look like this:
      ##emp_1976 ~ lastyear + mu
      ##pay_1976 ~ lastyear + mu + emp_1976
      if(year=="1976"){
	      if(check=="emp"){
	        mod.formula = paste(pred,"~",paste(names[-1],collapse="+"),sep="")
	      } else {
	        mod.formula = paste(pred,"~",paste(paste(names[-1],collapse="+"),c("emp_1976","emp_1976*mu"),sep="+"),sep="")} ##Just need to add "emp_1976" in this case
      } else {
        ##Otherwise, we want models of the form
        ##Using 1977 as an example
        ##emp_1977 ~ firstyear + lastyear + mu + emp_1976
        ##pay_1977 ~ firstyear + lastyear + mu + emp_1977 + pay_1976
	    year.int=as.numeric(year)
	    if(check=="emp"){ ##Now, the year is not 1976
	      names=c(names,paste("emp_",year.int-1,sep=""),paste("mu*emp_",year.int-1,sep=""))

        mod.formula = paste(pred,"~",paste(names,collapse="+"),sep="")
	    } else {
	      names=c(names,paste("pay_",year.int-1,sep=""),paste("mu*pay_",year.int-1,sep=""),paste("emp_",year.int,sep=""),paste("mu*emp_",year.int-1,sep=""))
        mod.formula = paste(pred,"~",paste(names,collapse="+"),sep="")
	    }}}
  df.model=data
  vars.model=vars

##Now, we need to subset if the given data is on its firstyear
##Recall, we fit different models for establishments in their birth year
  if(check == "emp" | check == "pay"){
      year=substr(pred,5,8)
      ##Add birth model here too
      ##Subsetting the data
      df.model=data[which(data$firstyear < year & data$lastyear > year),]
      df.na= data[which(data$firstyear > year | data$lastyear <= year),]
      df.birth=data[which(data$firstyear== year),]
      
      df.model=data_jitter(df.model)
      df.birth=data_jitter(df.birth)
      
      vars.model=vars[which(vars$firstyear < year & vars$lastyear > year),]
      vars.na= vars[which(vars$firstyear > year | vars$lastyear <= year),]
      vars.birth=vars[which(vars$firstyear == year),]
      
      
      if(nrow(df.birth)>2){
        ##The tryCatch function is used so errors don't stop the function and instead return NA
        mod.birth=rq(mod.births,method=rq.method,tau=part$part,data=df.birth)
  ##Return if model doesn't work out/otherwise fit for predicted variables    
        if(is.na(mod.birth)){return(NA)}
        fits.birth=predict(mod.birth,vars.birth)
      } else {vars.model=rbind(vars.model,vars.birth)}
    }

    ##Now, fitting the model on the bulk of the data
    ##Again, the tryCatch function tries to prevent errors
    names.tmp=names(df.model)
    mod=rq(mod.formula,method=rq.method,tau=part$part,data=df.model)

    ##Return if model doesn't work out/otherwise fit for predicted variables    
    if(is.na(mod)){return(NA)}
      fits=predict(mod,vars.model)
      k=findInterval(runif(nrow(vars.model)),part.endpoints$X1,rightmost.closed=TRUE) ##Finding the interval for each tau
      for(kk in 1:nrow(fits)){
        syn[kk]=fits[kk,k[kk]]
      }#end of for

  
  ##This next chuck of code basically accounts for the fact that we have 3 different "models"
  ##First, the bulk of the synthetic data
  vars1=cbind(vars.model,syn)
  names(vars1) = tmpNames

  ##Second, any birth observations
  if((check == "emp" | check == "pay")){
    if(nrow(df.birth) > 2){
      k=findInterval(runif(nrow(vars.birth)),part.endpoints$X1,rightmost.closed=TRUE,binary) ##Finding the interval for each tau
      syn=c()
      for(kk in 1:nrow(fits.birth)){
        syn[kk]=fits.birth[kk,k[kk]]
      }#end of for
      syn=syn#*max.val+min.val)/(exp(syn)+1)
      vars2=cbind(vars.birth,syn)
      names(vars2)= tmpNames}
      syn=c()
      syn=rep(NA,nrow(vars.na))
      vars3=cbind(vars.na,syn)
      names(vars3)=tmpNames
  }#end of if

  var = rbind(vars1,vars2,vars3)

  if(pred=="mu"){
    var$mu=round(var$mu)
  }
  if(pred == "lastyear"){
    var$lastyear=round(var$lastyear)
    var$lastyear = ifelse(var$lastyear<=var$firstyear,var$firstyear+1,var$lastyear)
  }
  #setwd(dir)
  fwrite(var,paste(dir,"tmp.csv",sep="/"))
  return(var)
}#end of function


##Jittering the data
##Sometimes we need to add a little bit of noise so we don't have singular design issues
data_jitter <- function(data){
  for(i in 1:53){
    names=names(data)
    tmp=data[,names[i]]
    tmp=tmp + runif(length(tmp),-1,1)
    data[,names[i]]=tmp
  }
  return(data)
}


##Loading in the synthetic data
df=fread(paste(mydata,"longitudinalSynLBD.csv",sep="/"),na.strings="NA",integer64="numeric")


##Labels of interest
labels=c(178,239,328,354,473,511,542,703,829,865)


##This generates 5 synthetic data sets for each of the labels
##The while loop is used incase a singular design matrix error happens
##If it does happen, it just reloads the synthesis up until that point and continues on
for(q in 1:5){
  set.seed(q)
  dir = paste(home.dir,"/Data/QR/QR",q,sep="")
  for(k in 1:10){
    stop.val=2
    while(stop.val<=53){
      syns <- foreach(j=k, .errorhandling="pass",.inorder=FALSE, .packages=c("quantreg","data.table")) %do% {
        return.val=qr.synth(df,j,labels,bins=100,stop.val=stop.val,dir=dir) }#end of for
        stop.val=ncol(fread(paste(dir,"tmp.csv",sep="/")))+1
    }
  }
  print(q)
}

