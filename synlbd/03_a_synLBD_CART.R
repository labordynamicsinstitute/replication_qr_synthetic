##CART-based syntheses
##PSD 2018 code for "Synthetic data via Quantile Regression for Heavy-Tailed and Heteroskedastic Data"
##By Pistner, Slavkovic, and Vilhuber

source("config.R",echo=TRUE)


##Loading the libraries
##See https://www2.vrdc.cornell.edu/news/synthetic-data-server/step-4-using-the-sds/ for instructions on how to download R packages

library("synthpop",lib=dir.Rpackages)
library("data.table",lib=dir.Rpackages)
library("doParallel",lib=dir.Rpackages)

##Function to generate a synthetic data
##Basically just a wrapper to the R package synthpop
##Only special consideration is generating the predictor matrix (what variables to use in the synthesis)
Cart.synth <- function(df,j,labels,dir){  
  sic= labels[j]
  
  data=df[which(df$sic3==sic),-1]
  names=names(data)
  
  ##Really, the only thing we need to do is specify the predictor matrix and the methods to be used
  methods=c("sample",rep("ctree",ncol(data)-1))
  ##Now, generating the predictor matrix
  ##I did this by exploiting some properties of the blocks fo the matrix
  years=seq(1976,2000,by=1)
  names=c("firstyear","lastyear","mu",paste("emp_",years,sep=""),paste("pay_",years,sep=""))
  n.pay=25
  n.emp=25
  n.else=3 ##Number of variables that fall into each group
  q1=diag(n.else+n.pay-1)
  q1[2:(n.else+n.pay-1),1]=1
  q1[3:(n.else+n.pay-1),2]=1
  q1[4:(n.else+n.pay-1),3]=1
  
  ##Now, we need to add a first row and last column of zeros
  q1=cbind(q1,rep(0,n.else+n.pay-1))
  q1=rbind(c(0,rep(0,n.else+n.pay-1)),q1)
  
  q2=matrix(0,nrow=(n.emp+n.else),ncol=n.pay)
  
  q3=cbind(rep(1,n.pay),rep(1,n.pay),rep(1,n.pay),diag(n.pay))
  q4=cbind(diag(n.pay-1),rep(0,n.pay-1))
  q4=rbind(rep(0,n.pay),q4)
  
  Q=rbind(cbind(q1,q2),cbind(q3,q4))
  
  row.names(Q)=names
  colnames(Q)=names
  
  syns=data.frame(syn(data,method=methods, predictor.matrix=Q)$syn)
  syns$sic3=sic
  #setwd(dir)
  file_name=paste(dir,"/","CART_SynLBD_",sic,".csv",sep="")
  fwrite(syns,file=file_name,append=FALSE)
  
  return(sic)
  
}#end of function

##Labels of interest
labels=c(178,239,328,354,473,511,542,703,829,865)

## Load the SynLBD data

df=fread(paste(mydata,"longitudinalSynLBD.csv",sep="/"),na.strings="NA",integer64="numeric")

##Synthesizing 5 times over each of the labels specified above.
set.seed(2018)
for(q in 1:5){
  dir = paste(home.dir,"/Data/CART/CART",q,sep="")
  sics_run <- foreach(j=c(1:length(labels)), .errorhandling="remove", .inorder=FALSE) %do%{
    Cart.synth(df,j,labels,dir)
  }#end of for
  print(q)
}
