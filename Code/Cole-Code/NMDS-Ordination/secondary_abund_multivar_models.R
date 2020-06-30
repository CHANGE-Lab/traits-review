########## 
##########
# This code contains the multivariate modeling component of the analysis presented in
# in Green et al. (2020) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2020-06-30
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
secondary_abundance = read_csv(here('./Data/Cole-Output-Data(readyforanalysis)/secondary_traits_dummy_abundance_models.csv'))

#split species and sites
secondary_abundance_sites = data.frame(secondary_abundance[,1:10])
secondary_abundance_species = data.frame(secondary_abundance[,11:ncol(secondary_abundance)])

#run actual ordination - try with both k = 2 & 3
secondary_abundance_ord_k3 = metaMDS(secondary_abundance_species,
                              distance = 'bray',
                              trymax = 100,
                              k = 3)
plot(secondary_abundance_ord_k3)
secondary_abundance_ord_k2 = metaMDS(secondary_abundance_species,
                              distance = 'jaccard',
                              trymax = 100,
                              k = 2)
plot(secondary_abundance_ord_k2)

#extract scores
secondary_abundance_ord_k2_scores = data.frame(scores(secondary_abundance_ord_k2))
secondary_abundance_ord_k3_scores = data.frame(scores(secondary_abundance_ord_k3))

