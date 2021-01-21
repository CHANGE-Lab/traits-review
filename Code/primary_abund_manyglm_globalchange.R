########## 
##########
# This code contains the a multivariate regression component  of the analysis 
# presented in Green et al. (2020) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Cole B. Brookson & Natasha A. Hardy
# DATE OF CREATION: 2021-01-20
##########
##########

# set-up =======================================================================

library(tidyverse)
library(vegan)
library(mvabund)
library(reshape2)
library(here)

primary_abundance = 
  read_csv(here(paste0('./data/processed-data',
                       '/primary_traits_dummy_abundance_models.csv')))

# Multivariate Regression ======================================================

# split into 'sites' and 'species' just to put it into typical ecological
# # multivariate context
primary_abundance_traits = data.frame(primary_abundance[,1:11])
primary_abundance_species = 
  data.frame(primary_abundance[,12:ncol(primary_abundance)])

# need to make it an mvabund readable object
primary_abundance_mv = mvabund(primary_abundance_species) 

# figure out what error distribution works
primary_check_matrix = as.matrix(primary_abundance_species)
hist(primary_check_matrix)

# run multivar glm with poisson distribution
mv_gc_poisson_primabun = manyglm(primary_abundance_mv ~  
                                   primary_abundance_traits$GlobalChangeCat, 
                                 data= primary_abundance_traits,
                                 family = 'poisson')
mv_gc_nb_primabun = manyglm(primary_abundance_mv ~ 
                              primary_abundance_traits$GlobalChangeCat, 
                            data= primary_abundance_traits,
                            family = 'negative.binomial')

# compare 
plot(mv_gc_poisson_primabun) 
qqnorm(residuals(mv_gc_poisson_primabun)[which(
  residuals(mv_gc_poisson_primabun)<10000)])
plot(mv_gc_nb_primabun) 
qqnorm(residuals(mv_gc_nb_primabun)[which(
  residuals(mv_gc_nb_primabun)<10000)])

###### begin NOTE ##############################################################
# Okay so the negative binomial model definitely fits better. Moving forward 
# with that fit, but I'll be saving both model objects just in case for 
# future use. 
###### end NOTE ################################################################

saveRDS(mv_gc_nb_primabun, 
        here("./data/manyglm-intermediate/mv_gc_nb_primabun.rds"))
saveRDS(mv_gc_poisson_primabun, 
        here("./data/manyglm-intermediate/mv_gc_poisson_primabun.rds")) 

# Output manipulation ==========================================================

# model output significance test
# mv_gc_nb_primabun_an = anova.manyglm(mv_gc_nb_primabun)
#saveRDS(mv_gc_nb_primabun_an, 
       # here("./data/manyglm-intermediate/mv_gc_nb_primabun_anova.rds")) 
mv_gc_nb_primabun_an = 
  readRDS(here("./data/manyglm-intermediate/mv_gc_nb_primabun_anova.rds"))

# make into a factor
levels(as.factor(primary_abundance_traits$GlobalChangeCat))
write_csv(mv_gc_nb_primabun_an$table, 
          here("./output-tables/mv_gc_nb_primabun_anova_table.csv"))

# get univariate p-values (individual adjusted p-values for species/traits) 
# mv_gc_nb_primabun_an_uni = anova.manyglm(mv_gc_nb_primabun,p.uni="adjusted")
# saveRDS(mv_gc_nb_primabun_an_uni, 
#         here("./data/manyglm-intermediate/mv_gc_nb_primabun_univs.rds")) 
mv_gc_nb_primabun_an_uni = 
  readRDS(here("./data/manyglm-intermediate/mv_gc_nb_primabun_univs.rds"))

# Get the direction of effect fof each species with the main effect
gc_coef_primabun = coef(mv_gc_nb_primabun)

# figure out what the top traits are - recall traits are our 'species' here
mv_gc_nb_primabun_species = 
  sort(mv_gc_nb_primabun_an$uni.test[2,],
       decreasing=T,index.return=T)[1:5] #sort and select top species/traits
mv_gc_nb_primabun_species$ix[1:5] #the column #s of the top impacted spp/traits

# Need > 50% deviance explainaed --> result = 25 traits explain > 50% deviance
sum(mv_gc_nb_primabun_an$uni.test[2,mv_gc_nb_primabun_species$ix[1:3]])*100/
  sum(mv_gc_nb_primabun_an$uni.test[2,]) #25 spp explained = 63.83021% Deviance

# get a dataframe of the top 20 traits 
gc_top_primabun = 
  data.frame(dimnames(primary_abundance_species)[[2]][
    mv_gc_nb_primabun_species$ix[1:5]]) 
gc_top_primabun = gc_top_primabun %>% 
  rename('traits' = names(gc_top_primabun))

# write table for amount of deviance explained 
write_csv(gc_top_primabun, here("./output-tables/gc_top5_primabun.csv"))

# Look at Top Coefficients =====================================================




















