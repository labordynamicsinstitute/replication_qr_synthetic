##Now, for the synthetic data set challenge data
##Census of Scotland from 1901
library(quantreg)
library(synthpop)
library(rpart)
library(data.table)
library(dbscan)

set.seed(2018)
##Loading in the challenge data
base = getwd()
setwd(paste(base,"data",sep="/"))
load("challenge.Rdata")
names(challenge)
str(challenge)

##Variables we want
vars.list <- c("sex","mar_stat","employ","age","nservants","nboarders","nlodgers","nvisitors","nfamgteq15","nfamlt15","nkn")

##Subsetting data
df=challenge[,vars.list]
##Let's forget about NAs for now
df=na.omit(df)
names(df)

utility <- function(data,inds){
  require(rpart)
  utilityModel=rpart(inds~.,data=data,method="class",minbucket=5,cp=1e-4)
  preds=predict(utilityModel)[,2]
  utilityScore=sum((preds-0.5)^2)/nrow(data)
  return(utilityScore)
}##end of function

synsQRreg <- function(data,vars,bins=25,stop.val=2){
  part.vals=seq(0,1,1e-5)
  part=tapply(part.vals,cut(part.vals,bins),median)
  part=data.frame(part)
  part=part[!duplicated(part),]
  part=data.frame(part)
  part$count=1:nrow(part)
  part$names=rownames(part)
  
  part.endpoints=data.frame(do.call('rbind',strsplit(as.character(part$names),',',fixed=TRUE)))
  part.endpoints$X1=gsub("[(]","",part.endpoints$X1)
  part.endpoints$X2=gsub("]","",part.endpoints$X2)
  part.endpoints$X1=as.numeric(part.endpoints$X1)
  part.endpoints$X2=as.numeric(part.endpoints$X2)
  
  n=length(data)
  n.obs=nrow(data)
  vars=data.frame(vars)
  k=rep(NA,n.obs)
  syns=NA
  tau=rep(NA,n.obs)
  
  for(i in stop.val:n){
    stop=i-1
    tau=runif(nrow(vars))
    k=findInterval(tau,part.endpoints$X1,rightmost.closed=TRUE)
    count=1
    names=colnames(data)[1:stop]
    pred=colnames(data)[(stop+1)]
    X.train=data[,names]
    Y.train=data[,pred]
    if(pred!="age"){
      names=c(names[-1],"age*sex","age^2") ##Adding the interactions with age and age^2
    }
    
    data.train=data.frame(Y.train,X.train)
    names(data.train)[1]=pred
    qrr=rq(paste(pred,"~",paste(names,collapse="+"),sep=""),data=data.train,tau=part$part,method="fn")
    fits=predict(qrr,vars)
    for(kk in 1:nrow(fits)){
      syns[kk]=fits[kk,k[kk]]
    }
    syns=round(syns,0)
    vars=cbind(vars,syns)
    names(vars)=names(data[1:i])
    print(i)
  }#end of for
  colnames(vars)=colnames(data)
  return(vars)
}##End of function

names(df)
vars=df[,1:3]
data=df[,1:11]
names(data)

m=10
qr.ut=rep(NA,m)
qr.risk=rep(NA,m)
c.ut=rep(NA,m)
c.risk=rep(NA,m)

for(i in 1:m){
  syns=synsQRreg(data,vars=vars,bins=250,stop.val=4)
  summary(syns)
  summary(data)
  ut.data=rbind(syns,data)
  inds=c(rep(0,nrow(syns)),rep(1,nrow(data)))
  qr.ut[i]=utility(ut.data,inds)
  res=lof(syns[,4:11],k=25)
  res.inds=ifelse(res>1.5,1,0)
  qr.risk[i]=table(res.inds)[2]/nrow(syns)
  
  syns1=as.data.frame(syn(data,method=c(rep("",3),rep("ctree",8)))$syn)
  ut.data=rbind(syns1,data)
  inds=c(rep(0,nrow(syns1)),rep(1,nrow(data)))
  c.ut[i]=utility(ut.data,inds)
  res=lof(syns1[,4:11],k=25)
  res.inds=ifelse(res>1.5,1,0)
  c.risk[i]=table(res.inds)[2]/nrow(syns1)
  
  
  print(i)
}


results.df = data.frame(cbind(rbind("Mean", "St dev"),rbind(mean(qr.ut),sd(qr.ut)), rbind(mean(c.ut),sd(c.ut)), rbind(mean(qr.risk), sd(qr.risk)), rbind(mean(c.risk),sd(c.risk))))

names(results.df) = c("","Utility (QR)", "Utility (CART)", "Risk (QR)", "Risk (CART)")

fwrite(results.df,paste(base,"results","results.csv",sep="/"))

dat = data.frame(Servants=c(plyr::count(syns$nservants)$x,plyr::count(syns1$nservants)$x,plyr::count(data$nservants)$x),Count=c(plyr::count(syns$nservants)$freq,plyr::count(syns1$nservants)$freq,plyr::count(data$nservants)$freq),Method = c(rep("QR",nrow(plyr::count(syns$nservants))),rep("CART",nrow(plyr::count(syns1$nservants))),rep("Original",nrow(plyr::count(data$nservants)))))
dat = dat[which(dat$Servants>=5),]
ggplot(data=dat, aes(Servants,Count)) +
  geom_bar(stat="identity",aes(fill = Method),position="dodge")

ggsave(paste(base,"results","nServants.pdf",sep="/"))
