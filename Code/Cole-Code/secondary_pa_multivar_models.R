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
secondary_pa = read_csv(here('./Data/Cole-Output-Data(readyforanalysis)/secondary_traits_dummy_pa_models.csv'))

#split species and sites
secondary_pa_sites = data.frame(secondary_pa[,1:10])
secondary_pa_species = data.frame(secondary_pa[,11:ncol(secondary_pa)])

#run actual ordination - try with both k = 2 & 3
# secondary_pa_ord_k3 = metaMDS(secondary_pa_species,
#                             distance = 'jaccard',
#                             trymax = 100,
#                             k = 3)
# plot(secondary_pa_ord_k3)
secondary_pa_ord_k2 = metaMDS(secondary_pa_species,
                            distance = 'jaccard',
                            trymax = 100,
                            k = 2)
plot(secondary_pa_ord_k2)

#extract scores
secondary_pa_ord_k2_scores = data.frame(scores(secondary_pa_ord_k2))
#secondary_pa_ord_k3_scores = data.frame(scores(secondary_pa_ord_k3))



