##Graphs
##PSD 2018 code for "Synthetic data via Quantile Regression for Heavy-Tailed and Heteroskedastic Data"
##By Pistner, Slavkovic, and Vilhuber

source("config.R",echo=TRUE)

library("ggplot2",lib=dir.Rpackages)
library("data.table",lib=dir.Rpackages)
setwd(mydata)
df=fread("longitudinalSynLBD.csv",na.strings="NA",integer64="numeric")

setwd(paste(mydata,"/QR/QR3",sep=""))
syns=fread("QR_SynLBD_829.csv")
syns=round(syns)
data=df[which(df$sic3=="829"),]

setwd(paste(mydata,"/CART/CART5",sep=""))
syns1=fread("CART_SynLBD_829.csv")

data=as.data.frame(data)
syns1=as.data.frame(syns1)

data.all=rbind(data,syns,syns1)
data.all$Group=c(rep("Original",nrow(data)),rep("QR",nrow(syns)),rep("CART",nrow(syns1)))
# 
ggplot(data.all,aes(x=pay_2000,fill=Group))+geom_density(alpha=.5,position="dodge")+xlim(5000,40000)+xlab("Total Payroll for 2000 (SIC = 829)") + ylab("Density") + theme(axis.text=element_text(size=16),
                                                                                                                                                                          axis.title=element_text(size=16,face="bold"),
                                                                                                                                                                          legend.text=element_text(size=16),
                                                                                                                                                                          legend.title=element_text(size=16,face="bold"))                                                                                                                                                                          

ggsave(paste(results,"/829_plot.pdf",sep=""))

###SIC code 865
setwd(paste(mydata,"/QR/QR2",sep=""))
syns=fread("QR_SynLBD_865.csv")
syns=round(syns)
data=df[which(df$sic3=="865"),]

setwd(paste(mydata,"/CART/CART3",sep=""))
syns1=fread("CART_SynLBD_865.csv")

data=as.data.frame(data)
syns1=as.data.frame(syns1)

data.all=rbind(data,syns,syns1)
data.all$Group=c(rep("Original",nrow(data)),rep("QR",nrow(syns)),rep("CART",nrow(syns1)))
# 
ggplot(data.all,aes(x=pay_2000,fill=Group))+geom_density(alpha=.5,position="dodge")+xlim(1000,10000)+xlab("Total Payroll for 2000 (SIC = 865)") + ylab("Density") + theme(axis.text=element_text(size=16),
                                                                                                                                                                          axis.title=element_text(size=16,face="bold"),
                                                                                                                                                                          legend.text=element_text(size=16),
                                                                                                                                                                          legend.title=element_text(size=16,face="bold"))                                                                                                                                                                          


ggsave(paste(results,"/865_plot.pdf",sep=""))