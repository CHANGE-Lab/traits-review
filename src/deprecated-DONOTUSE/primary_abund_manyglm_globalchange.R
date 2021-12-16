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
write_csv(gc_top_primabun, 
          here("./output-tables/primary_globalchange_top5.csv"))

# Look at Top Coefficients =====================================================

# create df to combine coef values, also p-values from univ anovas & the top 20
gc_coef_prim = data.frame(t(gc_coef_primabun)) 
gc_coef_prim$traits = rownames(gc_coef_prim) #convert rownames to a column
gc_coef_prim = gc_coef_prim %>% 
  dplyr::rename('coef_intercept' = `X.Intercept.`, 
                'coef_gc_yes' = names(gc_coef_prim)[2])

gc_top_coeffs = merge(gc_top_primabun, gc_coef_prim,
                      by.x = 'traits',
                      by.y = 'traits') 

# now join the df with test statistic values
gc_an_test = as.data.frame(t( # first transpose coef_filter
  mv_gc_nb_primabun_an_uni$uni.test)) 
gc_an_test$traits = rownames(gc_an_test) #convert rownames to a column
gc_an_test = gc_an_test %>% 
  dplyr::rename('deviance_explained' = names(gc_an_test)[2])
gc_top_coeffs = merge(gc_top_coeffs,
                      gc_an_test,
                      by.x = 'traits',
                      by.y = 'traits')
gc_top_coeffs = gc_top_coeffs %>%
  select(-"(Intercept)")

# now need to join again with p-values
gc_an_pvalue = data.frame(t( # first transpose coef_filter
  mv_gc_nb_primabun_an_uni$uni.p)) 
gc_an_pvalue$traits = rownames(gc_an_pvalue) #convert rownames to a column
gc_an_pvalue = gc_an_pvalue %>% 
  select(-names(gc_an_pvalue)[1]) 
gc_an_pvalue = gc_an_pvalue%>% 
  dplyr::rename('p_value' = names(gc_an_pvalue)[1])
gc_top_coeffs = merge(gc_top_coeffs, 
                      gc_an_pvalue,
                      by.x = 'traits',
                      by.y = 'traits') 
# write table of this 
write_csv(gc_top_coeffs, 
          here("./output-tables/primary_globalchange_top_coefs.csv"))

# see how many papers actually have those traits
papers_with_top_3_gc = primary_abundance_species
top_3_gc = gc_top_primabun$traits
papers_with_top_3_gc = papers_with_top_3_gc[top_3_gc]

rownames(papers_with_top_3_gc) = primary_abundance_traits$DOI
papers_with_top_3_gc = 
  papers_with_top_3_gc[rowSums(papers_with_top_3_gc[, -1])>0, ]












