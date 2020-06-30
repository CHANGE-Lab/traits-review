########## 
##########
# This code contains the multivariate modeling component of the analysis presented in
# in Green et al. (2020) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2020-06-27
##########
##########

library(devtools)
library(knitr)
library(tidyverse)
library(vegan)
library(viridis)
library(PNWColors)
library(mvabund)
library(reshape2)
library(here)

#separate the modeling into the P/A modeling (1/0) and then the abundance modeling
primary_pa = read_csv(here('./Data/Cole-Output-Data(readyforanalysis)/primary_traits_dummy.csv'))
primary_pa = primary_pa %>% 
  select(-`X1`)

secondary_pa = read_csv(here('./Data/Cole-Output-Data(readyforanalysis)/secondary_traits_empty_dummy.csv'))
secondary_pa = secondary_pa %>% 
  select(-`X1`)

#split species and sites
primary_