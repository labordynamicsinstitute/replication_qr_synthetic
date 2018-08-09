##Function to calculate utility and risk
##PSD 2018 code for "Synthetic data via Quantile Regression for Heavy-Tailed and Heteroskedastic Data"
##By Pistner, Slavkovic, and Vilhuber

source("config.R",echo=TRUE)

##Loading the required libraries
##See https://www2.vrdc.cornell.edu/news/synthetic-data-server/step-4-using-the-sds/ for instructions on how to download R packages
## on SDS

CRAN_base <- "file:/cac/contrib/mirror/CRAN/" # location of the local CRAN mirror - this will be different at validation!
# set CRAN base, ensuring that install.packages(") can find its files
local({
r <- getOption("repos")
r["CRAN"] <- CRAN_base
options(repos = r)
})

install.packages("MatrixModels",lib=dir.Rpackages)
install.packages("SparseM",lib=dir.Rpackages)
install.packages("quantreg",lib=dir.Rpackages)
install.packages("stringr",lib=dir.Rpackages)
install.packages("data.table",lib=dir.Rpackages)
install.packages("doParallel",lib=dir.Rpackages)
install.packages("rpart",lib=dir.Rpackages)
install.packages("sdcMicro",lib=dir.Rpackages)
install.packages("data.table",lib=dir.Rpackages)
install.packages("dbscan",lib=dir.Rpackages)
install.packages("here",lib=dir.Rpackages)
install.packages("synthpop",lib=dir.Rpackages)
install.packages("ggplot2",lib=dir.Rpackages)
install.packages("foreach",lib=dir.Rpackages)

