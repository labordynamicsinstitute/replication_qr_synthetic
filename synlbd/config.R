## Functions and configuration
##PSD 2018 code for "Synthetic data via Quantile Regression for Heavy-Tailed and Heteroskedastic Data"
##By Pistner, Slavkovic, and Vilhuber

##Note that this assumes inside the working directory folder there is a Data folder
##Data folder has a CART folder
##CART folder has a 5 folders: CART1,...,CART5
##Each individual synthesis is nested in these folders

# optional: if you run from the command line, R should already be in 
#           right working directory. Otherwise set it here.
#home.dir = paste(here(),"/QuantileRegression/PSD 2018",sep="")
home.dir=getwd()
setwd(home.dir)

# SynLBD parameters
base = "/rdcprojects/tr/tr00612"
version = "2.0.2"
prefix="synlbd"

# directories
inputs=paste(base,"/data/synlbd/",version,sep="")
mydata=paste(home.dir,"Data",sep="/")
results=paste(home.dir,"results",sep="/")

print("=== Directories ===")
print(paste("Inputs:",inputs,sep=" "))
print(paste("Intermediate data:",mydata,sep=" "))
print(paste("Results:",results,sep=" "))

## R packages are installed into the project directory at time of first run
dir.Rpackages = paste(getwd(),"Rpackages",sep="/")
dir.Rpackages

