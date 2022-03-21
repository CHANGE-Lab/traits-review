########## 
##########
# This code contains the multivariate modeling component of the analysis presented in
# in Green et al. (2021) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Natasha A. Hardy
# DATE OF CREATION: 2022-03-21
##########
##########

# set-up =======================================================================

library(tidyverse)
library(vegan)
library(viridis)
library(mvabund)
library(here)
library(cowplot)


trait_abundance = 
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

