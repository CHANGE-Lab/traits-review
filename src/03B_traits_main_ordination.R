########## 
##########
# This code contains the multivariate modeling component of the analysis presented in
# in Green et al. (2021) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Cole Brookson & Natasha A. Hardy
# DATE OF CREATION: 2022-03-21
##########
##########

# set-up =======================================================================

library(tidyverse)
library(vegan)
library(viridis)
library(mvabund)
library(here)
here::here()
library(cowplot)

trait_main_use = 
  read_csv(here(paste0('./data/processed-data',
                       '/review_traits_clean_models.csv')))

# Ordination ===================================================================

###### begin NOTE ##############################################################
# The below section has been commented out for simpler use. The ordination takes 
# a good while to run, easily over 2 hours on a normal laptop, so we've saved
# the result as an RDS object, which can be easily read into the environment 
# using the code provided. This will allow the user to inspect the result and
# subsequent plots that are made in this script, without re-running the 
# ordination. Note that due to the nature of the analysis, re-running the actual
# ordination will result in slightly different (not significantly) results each
# time, and thus should be avoided. All statistics about the ordination itself 
# can be accessed via the RDS object after reading it into the environment.
###### end NOTE ################################################################

# This needs to be done a
traits_sites_use <- as.data.frame(trait_main_use[,1:9])
traits_species_use <- trait_main_use[,10:ncol(trait_main_use)] #1411 trait columns

range(traits_species_use)

# Note that these data are already presence absence
# Using Jaccard dissimilarity for binary data.
# Note that the stress decreases as we decrease the number of observations, however, 
# I'm concerned that it becomes so low as to indicate that we do not have enough 
# observations vs. traits (data are too wide because #traits > # obs)

# Run time = 3h

# Ordination based on using 1 replicate per study (DOI) #727 obs
traits_ord_use <- metaMDS(traits_species_use,
                          distance = "jaccard",
                          trymax = 1000, #for testing trymax = 50
                          maxits = 1000,
                          k = 4) 

##Model convergence issues
#Based on ordination goodness fit/stress checks, we could use k = 2-3.
#Don't want to redo this right now as it will take ~2h to rerun.
#Likely the data are zero inflated

plot(traits_ord_use) #plots species (here traits) as black dots, sites (here studies) as red crosses, not labelled

#Plot sites (row numbers) and species (traits) as labels #run the whole chunk of text below
ordiplot(traits_ord_use, type = "none") #creates blank plot
orditorp(traits_ord_use, display = "species", col = "black", air=0.01) #labels traits
orditorp(traits_ord_use, display = "sites", col = "red", air=0.01) #labels traits


ggsave("review_nmdsinitial.jpeg", width = 8, height = 8, dpi = 300)

saveRDS(traits_ord_use, here('./data/nmds-intermediate/traits_ord_use.rds'))

#Check this
#traits_ord_use = readRDS(here::here('./data/nmds-intermediate/traits_ord_use.rds'))


