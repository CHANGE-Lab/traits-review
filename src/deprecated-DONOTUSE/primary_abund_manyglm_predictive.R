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

###### begin NOTE ##############################################################
# Some below sections have been commented out for simpler use. Some of the 
# regressions take a good while to run, so we've saved the result as an RDS 
# object which can be easily read into the environment using the code provided.
###### end NOTE ################################################################

# split into 'sites' and 'species' just to put it into typical ecological
# multivariate context
primary_abundance_traits = data.frame(primary_abundance[,1:11])
primary_abundance_species = 
  data.frame(primary_abundance[,12:ncol(primary_abundance)])

# need to make it an mvabund readable object
primary_abundance_mv = mvabund(primary_abundance_species) 

# go ahead with negative binomial 
mv_pd_nb_primabun = manyglm(primary_abundance_mv ~ 
                              primary_abundance_traits$PredictiveCat, 
                            data= primary_abundance_traits,
                            family = 'negative.binomial')
plot(mv_pd_nb_primabun) 
qqnorm(residuals(mv_pd_nb_primabun)[which(residuals(mv_pd_nb_primabun)<10000)])

saveRDS(mv_pd_nb_primabun, 
        here("./data/manyglm-intermediate/mv_pd_nb_primabun.rds"))

# model output significance test
# mv_pd_nb_primabun_an = anova.manyglm(mv_pd_nb_primabun)
# saveRDS(mv_pd_nb_primabun_an, 
#         here("./data/manyglm-intermediate/mv_pd_nb_primabun_anova.rds")) 
mv_pd_nb_primabun_an = 
  readRDS(here("./data/manyglm-intermediate/mv_pd_nb_primabun_anova.rds"))

# write a table for this 
write_csv(mv_pd_nb_primabun_an$table,
          here("./output-tables/mv_pd_nb_primabun_anova_table.csv"))

# individual adjusted p-values for species/traits - get univariate p-values
# mv_pd_nb_primabun_an_uni = anova.manyglm(mv_pd_nb_primabun,p.uni="adjusted")
# saveRDS(mv_pd_nb_primabun_an_uni, 
#         here("./data/manyglm-intermediate/mv_pd_nb_primabun_univs.rds")) 
mv_pd_nb_primabun_an_uni = 
  readRDS(here("./data/manyglm-intermediate/mv_pd_nb_primabun_univs.rds"))

# get the direction of effect for each species with the main effect
pd_coef_primabun = coef(mv_pd_nb_primabun)

#figure out what the top traits are - recall traits are our 'species' here
mv_pd_nb_primabun_species = 
  sort(mv_pd_nb_primabun_an$uni.test[2,],
       decreasing=T,index.return=T)[1:5] #sort and select top species/traits
mv_pd_nb_primabun_species$ix[1:5] #the column #s of the top  impacted spp/traits

#Need > 50% deviance explainaed --> result = 25 traits explain > 50% deviance
sum(mv_pd_nb_primabun_an$uni.test[2,mv_pd_nb_primabun_species$ix[1:5]])*100/
  sum(mv_pd_nb_primabun_an$uni.test[2,]) #25 species = 81.25088% Deviance
#3 species explained >85% deviance

pd_top_primabun = 
  data.frame(dimnames(primary_abundance_species)[[2]][
    mv_pd_nb_primabun_species$ix[1:5]]) #df with the names of the top 20 traits

pd_top_primabun = pd_top_primabun %>% 
  dplyr::rename('traits' = names(pd_top_primabun))

# write table for amount of deviance explained 
write_csv(pd_top_primabun, here("./output-tables/primary_predictive_top5.csv")) 

# Look at Top Coefficients =====================================================

# Create df to combine coef values,  also p-values from univ anovas & the top 20
pd_coef_prim = data.frame(t(pd_coef_primabun)) 
pd_coef_prim$traits = rownames(pd_coef_prim) #convert rownames to a column
pd_coef_prim = pd_coef_prim %>% 
  dplyr::rename('coef_intercept' = `X.Intercept.`, 
                'coef_pd_yes' = names(pd_coef_prim)[2])
pd_top_coeffs = merge(pd_top_primabun, pd_coef_prim,
                      by.x = 'traits',
                      by.y = 'traits') 

# need to join with test statistic values
pd_an_test = as.data.frame(t( # first transpose coef_filter
  mv_pd_nb_primabun_an_uni$uni.test)) 
pd_an_test$traits = rownames(pd_an_test) #convert rownames to a column
pd_an_test = pd_an_test %>% 
  dplyr::rename('deviance_explained' = names(pd_an_test)[2])

pd_top_coeffs = merge(pd_top_coeffs,
                      pd_an_test,
                      by.x = 'traits',
                      by.y = 'traits')
pd_top_coeffs = pd_top_coeffs %>%
  select(-"(Intercept)")

# need to join with p-values
pd_an_pvalue = data.frame(t( # first transpose coef_filter
  mv_pd_nb_primabun_an_uni$uni.p)) 
pd_an_pvalue$traits = rownames(pd_an_pvalue) #convert rownames to a column

pd_an_pvalue = pd_an_pvalue %>% 
  select(-names(pd_an_pvalue)[1]) 
pd_an_pvalue = pd_an_pvalue%>% 
  dplyr::rename('p_value' = names(pd_an_pvalue)[1])

pd_top_coeffs = merge(pd_top_coeffs, 
                      pd_an_pvalue,
                      by.x = 'traits',
                      by.y = 'traits') 
write_csv(pd_top_coeffs, 
          here("./output-tables/primary_predictive_top_coefs.csv"))

#See how many papers actually have those traits
papers_with_top_3_pd = primary_abundance_species
top_3_pd = pd_top_primabun$traits
papers_with_top_3_pd = papers_with_top_3_pd[top_3_pd]

rownames(papers_with_top_3_pd) = primary_abundance_traits$DOI
papers_with_top_3_pd = 
  papers_with_top_3_pd[rowSums(papers_with_top_3_pd[, -1])>0, ]




















