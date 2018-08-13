# Replication File for Census of Scotland Example

This is replication code for the Census of Scotland example in the paper "Synthetic Data via Quantile Regression for Heavy-Tailed and Heteroskedastic Data" by Michelle Pistner, [Aleksandra Slavković](http://personal.psu.edu/abs12/), and [Lars Vilhuber](https://lars.vilhuber.com/), accepted for [Privacy in statistical databases 2018](https://unescoprivacychair.urv.cat/psd2018/) (PSD2018).


## Data Structure

Data for this application was obtained from the [i-CeM project](https://www1.essex.ac.uk/history/research/icem/) at the University of Essex via Gillian Raab. The data consists of household and demographic information from the 1901 Census of Scotland. See our paper for more details on the data methods and synthesis.


## Code

All code is contained in the file `censusCode.R`. This code assumes that the data set is saved in  a `data` folder. It outputs two separate items into a `results` folder. This includes both a file containing the utility and  risk measures and a graph correpsonding to the  graph presented in  the paper.

Note that there are slight discrepancies in these graphs versus the orginal graphs presented in the paper. The graphs reported in the paper came from an earlier version of the code before a random seed has been set. This has since been corrected.
