########## 
##########
# This code contains a multivariate modeling component of the analysis presented 
# in Green et al. (2020) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2020-07-03•
##########
##########

# set-up =======================================================================

library(tidyverse)
library(vegan)
library(mvabund)
library(reshape2)
library(here)


secondary_abundance = 
  read_csv(here(paste0('./data/processed-data',
                       '/secondary_traits_dummy_abundance_models.csv')))

# Multivariate Regression ======================================================

###### begin NOTE ##############################################################
# Some below sections have been commented out for simpler use. Some of the 
# regressions take a good while to run, so we've saved the result as an RDS 
# object which can be easily read into the environment using the code provided.
###### end NOTE ################################################################

# split into 'sites' and 'species' just to put it into typical ecological
# multivariate context
secondary_abundance_species = 
  data.frame(secondary_abundance[,12:ncol(secondary_abundance)])
secondary_abundance_traits = data.frame(secondary_abundance[,1:11])
secondary_abundance_mv = mvabund(secondary_abundance_species)

#try poisson and neg. binom.
mv_pred_poisson = manyglm(secondary_abundance_mv ~ 
                            secondary_abundance_traits$PredictiveCat, 
                          data= secondary_abundance_traits,
                          family = 'poisson')
plot(mv_pred_poisson) 

mv_pred_nb = manyglm(secondary_abundance_mv ~ 
                       secondary_abundance_traits$PredictiveCat, 
                     data= secondary_abundance_traits,
                     family = 'negative.binomial')
#family=binomial("cloglog")) 
plot(mv_pred_nb) 

# NOTE - okay, neg. binom definitely  better but I'll save both just in case
saveRDS(mv_pred_nb, 
        here('./data/manyglm-intermediate/mv_pred_nb_secondary.rds')) 
saveRDS(mv_pred_poisson, 
        here('./data/manyglm-intermediate/mv_pred_poisson_secondary.rds')) 

#model output significance test
# mv_pred_nb_an = anova.manyglm(mv_pred_nb)
# saveRDS(mv_pred_nb_an, 
#         here('./data/manyglm-intermediate/mv_pred_nb_anova_secondary.rds')) 
mv_pred_nb_an = 
  readRDS(here('./data/manyglm-intermediate/mv_pred_nb_anova_secondary.rds'))
write_csv(mv_pred_nb_an$table, 
          here('./output-tables/mv_pred_nb_anova_table_secondary.csv')) 

#individual adjusted p-values for species/traits - get univariate p-values
# mv_pred_nb_an_uni = anova.manyglm(mv_pred_nb,p.uni="adjusted") 
# saveRDS(mv_pred_nb_an_uni, 
#         here('./data/manyglm-intermediate/mv_pred_univs.rds')) 
mv_pred_nb_an_uni = 
  readRDS(here('./data/manyglm-intermediate/mv_pred_univs.rds'))

#Get the direction of effect fof each species with the main effect
pred_coef = coef(mv_pred_nb)

#figure out what the top traits are - recall traits are our 'species' here
mv_pred_nb_species = 
  sort(mv_pred_nb_an$uni.test[2,],
       decreasing=T,index.return=T)[1:25] #sort and select top species/traits
mv_pred_nb_species$ix[1:25] #the column #s of the top imabundancected spp/traits

sum(mv_pred_nb_an$uni.test[2,mv_pred_nb_species$ix[1:25]])*100/
  sum(mv_pred_nb_an$uni.test[2,]) #25 species explained = 50.70% Deviance

pred_top = 
  data.frame(dimnames(secondary_abundance_species)[[2]][mv_pred_nb_species$ix[
    1:25]]) #df with the names of the top 20 traits
pred_top = pred_top %>% 
  rename('traits' = names(pred_top))

#How much deviance explained?
write_csv(pred_top, 
          here('./output-tables/secondary_predictive_top.csv')) 

#Now combine traits with their coeffs and p-values
pred_coef_l = data.frame(t(pred_coef)) 
pred_coef_l$traits = rownames(pred_coef_l) #convert rownames to a column
pred_coef_l = pred_coef_l %>% 
  rename('coef_intercept' = `X.Intercept.`, 
         'coef_pred_yes' = names(pred_coef_l)[2])
pred_top_coeffs = merge(pred_top, pred_coef_l,
                        by.x = 'traits',
                        by.y = 'traits') 

colnames(pred_top_coeffs)

#need to join with test statistic values
pred_an_test = as.data.frame(t( # first transpose coef_filter
  mv_pred_nb_an_uni$uni.test)) 
pred_an_test$traits = rownames(pred_an_test) #convert rownames to a column
pred_an_test = pred_an_test %>% 
  rename('deviance_explained' = names(pred_an_test)[2])
pred_top_coeffs = merge(pred_top_coeffs,
                        pred_an_test,
                        by.x = 'traits',
                        by.y = 'traits')
pred_top_coeffs = pred_top_coeffs %>%
  select(-"(Intercept)")

#need to join with p-values
pred_an_pvalue = data.frame(t( # first transpose coef_filter
  mv_pred_nb_an_uni$uni.p)) 
pred_an_pvalue$traits = rownames(pred_an_pvalue) #convert rownames to a column
pred_an_pvalue = pred_an_pvalue %>% 
  select(-names(pred_an_pvalue)[1]) 
pred_an_pvalue = pred_an_pvalue%>% 
  rename('p_value' = names(pred_an_pvalue)[1])
pred_top_coeffs = merge(pred_top_coeffs, 
                        pred_an_pvalue,
                        by.x = 'traits',
                        by.y = 'traits') 
write_csv(pred_top_coeffs, 
          here('./output-tables/secondary_predictive_top_coefs.csv'))

#See how many abundance papers actually have those traits
papers_with_top_25_pred = secondary_abundance_species
top_25_pred = pred_top$traits
papers_with_top_25_pred = papers_with_top_25_pred[top_25_pred]

rownames(papers_with_top_25_pred) = secondary_abundance_traits$DOI
papers_with_top_25_pred = 
  papers_with_top_25_pred[rowSums(papers_with_top_25_pred[, -1])>0, ]
