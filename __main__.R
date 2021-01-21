########## 
##########
# This script runs all code and returns all output for the analysis presented in 
# in Green et al. (2021) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2020-11-10
##########
##########

# How to Run This Code =========================================================

# This file will run all the code needed to reproduce the analysis presented in
# the above-mentioned paper. There are a number of ways to fully reproduce this 
# analysis. 

# METHOD 1:

# The simplest way to reproduce this analysis and inspect the results is to 
# clone this open-access repository to your local machine, open this file,
# '__main__.R' and click the 'Source' button. Please note that if you do it this
# way you will have to check yourself to see that you have the right packages
# etc. 

# METHOD 2:

# Run this 

# Process the data =============================================================
library(here)
source(here('./code/data_preparation.R'))

# This script pulls together all of the unprocessed data that came from the 
# literature search process, collates it, and writes out a series of usable
# dataframe to perform the further analyses on. All of these dataframes exist
# in the repo but this code reproduces the data collation process. 

# Clean the data for multivariate analysis =====================================
rm(list = ls())
source(here('./code/pre_multivar_data_cleaning.R'))

# This script takes the data that were processed in the previous script and 
# does some cleaning to ensure all four datasets (primary vs. secondary trait 
# classification and abundance v. presence/absence) are consistent and ready
# to be used in the multivariate analyses 

# Run multivariate analyses ====================================================
rm(list = ls())
library(parallel)

# look for the cores
cores = detectCores()
cluster = makeCluster(4)
files = c(here('./code/primary_pa_multivar_models.R'),
          here('./code/primary_abund_multivar_models.R'),
          here('./code/secondary_pa_multivar_models.R'),
          here('./code/secondary_abund_multivar_models.R'))
for(file in files) {
  if (! file.exists(file)) {
    stop(paste(file, "does not exist"))
  }
}

parSapply(cluster, files, source)
stopCluster(cluster)

# This section sources four separate scripts, but in parallel, as each take 
# over four hours to run. NOTE: important to note here that the cluster is 
# being created on a machine with >4 cores so it is safe to make such a cluster
# without taking all the computing power from the entire machine. 
#
# There are four files, two running using the 'primary' trait classifications 
# (see paper for details) and two running using the 'secondary' trait
# classifications. All four scripts pull in data created in previous scripts
# and use them to run multivariate analyses on our data. Note that if left
# untouched, each of these will run very quickly as we have already run the 
# analysis to generate the .rds file that contains the model object. These are
# simply read in and used to create visual representations of the model results
# in the form of NMDS plots. Note that the succesful reading in of the files in
# the parSapply function is highly dependent on finding the files successfully.
# Thus, using the here() function successfully and the RProject will allow them
# to be read properly. This is checked with the for loop checking for the 
# existence of each file. 

# Create Assorted plots ========================================================
rm(list = ls())
source(here('./code/heatplots_wordclouds_timeseries.R'))

# In this script, a series of plots are created, including heat plots for the 
# number of studies with varying intersections, a number of word clouds that 
# depict the number of times specific traits were used etc., and then time 
# series plots that show how the number of studies centering on various 
# topics over time. 
























































































