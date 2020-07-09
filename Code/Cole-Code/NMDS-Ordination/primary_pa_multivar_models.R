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
primary_pa = read_csv(here('./Data/Cole-Output-Data(readyforanalysis)/primary_traits_dummy_pa_models.csv'))

#split species and sites
primary_pa_sites = data.frame(primary_pa[,1:10])
primary_pa_species = data.frame(primary_pa[,11:ncol(primary_pa)])

#run actual ordination - try with both k = 2 & 3
primary_pa_ord_k4 = metaMDS(primary_pa_species,
                         distance = 'jaccard',
                         trymax = 1000,
                         k = 4)
saveRDS(primary_pa_ord_k4, 
        here('./Data/Cole-nMDS-Intermediate/primary_pa_ord.rds'))
plot(primary_pa_ord_k4)
# primary_pa_ord_k2 = metaMDS(primary_pa_species,
#                             distance = 'jaccard',
#                             trymax = 100,
#                             k = 2)
# plot(primary_pa_ord_k2)

#extract scores
#primary_pa_ord_k2_scores = data.frame(scores(primary_pa_ord_k2))
#primary_pa_ord_k2_scores = data.frame(scores(primary_pa_ord_k2))

############################## Plotting pipeline ###############################

#extract scores
primary_pa_k4_scores <- data.frame(scores(primary_pa_ord_k4)) 
primary_pa_k4_scores$points <- rownames(primary_pa_k4_scores) 
primary_pa_scores = cbind(primary_pa_sites, primary_pa_k4_scores)
str(primary_pa_scores)

########### Get Hulls

primary_pa_scores$Ecosystem = 
  as.factor(primary_pa_scores$Ecosystem)
primary_pa_scores$GlobalChangeCat = 
  as.factor(primary_pa_scores$GlobalChangeCat)
primary_pa_scores$Taxonomic = 
  as.factor(primary_pa_scores$Taxonomic)

#ecosystem
eco = as.character(unique(primary_pa_scores$Ecosystem))
for(i in 1:length(eco)) {
  temp = eco[i]
  df = primary_pa_scores[
    primary_pa_scores$Ecosystem == temp, 
  ][chull(primary_pa_scores[
    primary_pa_scores$Ecosystem == temp,
    c("NMDS1", "NMDS2")
  ]), ]
  assign(paste0('grp_eco_',temp), df)
}
hull_ecosystem = rbind(grp_eco_Terrestrial, grp_eco_Freshwater, 
                       grp_eco_Marine, grp_eco_Multiple)

#taxonomic
tax = as.character(unique(primary_pa_scores$Taxonomic))
for(i in 1:length(tax)) {
  temp = tax[i]
  df = primary_pa_scores[
    primary_pa_scores$Taxonomic == temp, 
  ][chull(primary_pa_scores[
    primary_pa_scores$Taxonomic == temp,
    c("NMDS1", "NMDS2")
  ]), ]
  assign(paste0('grp_tax_',temp), df)
}
hull_taxonomic = rbind(grp_tax_Birds, grp_tax_Insects, grp_tax_Mammals, 
                       grp_tax_Multiple, grp_tax_Herpetofauna, grp_tax_Plants,
                       grp_tax_Plankton, grp_tax_Other, grp_tax_Fish)

#taxonomic group

#first make new classifications 


tax = as.character(unique(primary_pa_scores$Taxonomic))
for(i in 1:length(tax)) {
  temp = tax[i]
  df = primary_pa_scores[
    primary_pa_scores$Taxonomic == temp, 
  ][chull(primary_pa_scores[
    primary_pa_scores$Taxonomic == temp,
    c("NMDS1", "NMDS2")
  ]), ]
  assign(paste0('grp_tax_',temp), df)
}
hull_taxonomic = rbind(grp_tax_Birds, grp_tax_Insects, grp_tax_Mammals, 
                       grp_tax_Multiple, grp_tax_Herpetofauna, grp_tax_Plants,
                       grp_tax_Plankton, grp_tax_Other, grp_tax_Fish)







#Terrestrial
grp_terrest <- 
  primary_pa_k4_treat_scores[
    primary_pa_k4_treat_scores$Ecosystem
    == "Terrestrial", 
    ][chull(primary_pa_k4_treat_scores[primary_pa_k4_treat_scores$Ecosystem == 
                                         "Terrestrial",
                                       c("NMDS1", "NMDS2")]), ] 
#Freshwater
grp_fresh <- 
  primary_pa_k4_treat_scores[
    primary_pa_k4_treat_scores$Ecosystem
    == "Freshwater", 
  ][chull(primary_pa_k4_treat_scores[primary_pa_k4_treat_scores$Ecosystem == 
                                       "Freshwater",
                                     c("NMDS1", "NMDS2")]), ]
#Marine
grp_marine <- 
  primary_pa_k4_treat_scores[
    primary_pa_k4_treat_scores$Ecosystem
    == "Marine", 
  ][chull(primary_pa_k4_treat_scores[primary_pa_k4_treat_scores$Ecosystem == 
                                       "Marine",
                                     c("NMDS1", "NMDS2")]), ]
#Marine
grp_multiple <- 
  primary_pa_k4_treat_scores[
    primary_pa_k4_treat_scores$Ecosystem
    == "Multiple", 
  ][chull(primary_pa_k4_treat_scores[primary_pa_k4_treat_scores$Ecosystem == 
                                       "Multiple",
                                     c("NMDS1", "NMDS2")]), ]
hull_ecosystem = rbind(grp_t, grp_fresh, grp_marine, grp_multiple)









