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

# Primary PA multivariate analysis =============================================
rm(list = ls())
source(here('./code/primary_pa_multivar_models.R'))




























































































