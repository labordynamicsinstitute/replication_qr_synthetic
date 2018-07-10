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

## R packages are installed into the project directory at time of first run
dir.Rpackages = paste(getwd(),"Rpackages",sep="/")
dir.Rpackages

