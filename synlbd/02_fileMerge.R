##Libraries needed
library(plyr)
library(foreign)
library("data.table",lib="/home/fs01/spec827/Rpackages")


##Setting the directories
base = "/rdcprojects/tr/tr00612"
version = "2.0.2"
myid="spec827"
prefix="synlbd"


inputs=paste(base,"/data/synlbd/",version,sep="")
mydata=paste(base,"/programs/users/",myid,"/data",sep="")

##Now, we want to read in the data and merge the data frames together
setwd(inputs)
vars.list=c("lbdnum","sic3","firstyear","lastyear","mu","emp","pay")

##Now, read in for every year and merge together
for(i in 1976:2000){
  read.file=paste("synlbd",i,"c.dta",sep="")
  df.tmp=read.dta(read.file)
  df.tmp=df.tmp[,vars.list] ##Restrict on variables
  names(df.tmp)[2:length(vars.list)]= paste(vars.list,"_",i,sep="")[2:length(vars.list)]##
  if(i==1976){df.main=df.tmp} else {df.main=join(df.main,df.tmp,by="lbdnum",type="full")}
  print(i)
}#end of for

##Now, we only need one first year var, last year var, and mu var
names(df.main)
##First, we want to reorginize the variables
##We will do this with a loop
fy=c()
ly=c()
mu=c()
pay=c()
emp=c()
sics=c()
for(i in 1976:2000){
  fy=c(fy,paste("firstyear_",i,sep=""))
  ly=c(ly,paste("lastyear_",i,sep=""))
  mu=c(mu,paste("mu_",i,sep=""))
  emp=c(emp,paste("emp_",i,sep=""))
  pay=c(pay,paste("pay_",i,sep=""))
  sics=c(sics,paste("sic3_",i,sep=""))
}
names=c("sic3_1976",fy,ly,mu,emp,pay)
sics3=apply(df.main[,sics],1,FUN=min,na.rm=TRUE)

df.main=df.main[,names] ##Reordering all the variables
names(df.main)[1]="sic3" ##renaming the sic3 var
df.main$sic3=sics3
sum(as.numeric(is.na(df.main$sic3)))
##Now, we need to check that the data is behaving like we think it should be
##First, just checking the first few rows
head(df.main)

##Now, we need to only construct one firstyear, lastyear, and mu variables
##Checked for validity. They are all the same across years
df.main$firstyear=apply(df.main[,2:26],1,FUN=min,na.rm=TRUE)
df.main$lastyear=apply(df.main[,27:51],1,FUN=min,na.rm=TRUE)
df.main$mu=apply(df.main[,52:76],1,FUN=min,na.rm=TRUE)

##Now, subsetting the variables into the smaller data frame
names=c("sic3","firstyear","lastyear","mu",emp,pay)
df=df.main[,names]

fwrite(df,"longitudinalSynLBD.csv")
