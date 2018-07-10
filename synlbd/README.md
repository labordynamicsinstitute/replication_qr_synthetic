# Quantile Regressions for Synthesis - Synthetic LBD example

This is replication code for the paper "Synthetic Data via Quantile Regression for Heavy-Tailed and Heteroskedastic Data" by Michelle Pistner, [Aleksandra Slavković](http://personal.psu.edu/abs12/), and [Lars Vilhuber](https://lars.vilhuber.com/), accepted for [Privacy in statistical databases 2018](https://unescoprivacychair.urv.cat/psd2018/) (PSD2018).

## Data structure

The Synthetic LBD is at a central location on the server, see [config.R]().

Inside the working directory folder, there is a [Data]() folder, which in turn has a `CART` and a `QR` folder. Each folder in turn has five subfolders, for each run of the synthesizer.

TODO: incorporate creation of the directory structure into `01_install.R`.

## Processing steps

All processing steps need to be performed on the Synthetic Data Server at Cornell University.  Access to the Synthetic LBD is described at https://www2.vrdc.cornell.edu/news/synthetic-data-server/step-1-requesting-access-to-sds/. In order to run code with R, users can either submit code to the job scheduler using the command `qR`, or for longer running jobs, write a qsub job, which is then submitted with `qsub job.qsub`. By convention, for any file `job.qsub`, there will be an equivalent file `job.R`. In the instructions below, the appropriate command is listed. Runtimes can be very long, given the size of the input data. Analysis was run using R 3.0.2 on Linux machines with 256GB of RAM and 32 processors. More detailed resource requirements are detailed in each qsub file.


### Step 1
Install the necessary R packages
```
qR 01_install.R
```

### Step 2
Create the extract of the Synthetic LBD. Note that in this example, we save the data as a CSV file.
```
qsub 02_fileMerge.qsub
```
which in turn will call `02_fileMerge.R`.

### Step 3
Run the CART and QR synthesis:
```
qsub 03_a_synLBD_CART.qsub
qsub 03_b_synLBD_QR.qsub
```
Subject to resource availability, these can be run in parallel.

### Step 4
Summarize and compare:
```
qsub 04_SynLBD_utilityAndRisk.qsub
```

### Step 5
Create graphics

TODO.

## Funding
The Synthetic LBD data were accessed through the Synthetic Data Server at Cornell University, which is funded through NSF Grant SES-1042181 and BCS-0941226 and a grant from the Alfred P. Sloan foundation.
