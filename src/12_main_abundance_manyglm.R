########## 
##########
# This code contains the multivariate modeling component of the analysis presented in
# in Green et al. (2020) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Cole B. Brookson & Natasha Hardy
# DATE OF CREATION: 2022-03-30
##########
##########

# set-up =======================================================================

library(devtools)
library(knitr)
library(tidyverse)
library(vegan)
library(viridis)
library(PNWColors)
library(mvabund)
library(reshape2)
library(here)

#subset data into the two groups needed
trait_main_use = 
  read_csv(here(paste0('./data/processed-data',
                       '/review_traits_clean_models.csv')))

# split into 'sites' and 'species' just to put it into typical ecological
# multivariate context
traits_sites_use <- as.data.frame(trait_main_use[,1:10])
traits_species_use <- trait_main_use[,11:ncol(trait_main_use)] #1411 trait columns

# secondary_abundance_species == traits_species_use
# secondary_abundance_traits == traits_sites_use

traits_species_mv = mvabund(traits_species_use)
# secondary_abundance_mv == traits_species_mv

## Find appropriate distribution ================================================

main_check_matrix = as.matrix(traits_species_use)
hist(main_check_matrix)
hist(main_check_matrix[main_check_matrix > 0])

### Global Change Driver ----
#try poisson and neg. binom.
mv_gc_poisson = manyglm(traits_species_mv ~ 
                          traits_sites_use$`Global Change Driver`, 
                        data= traits_sites_use,
                        family = 'poisson')
plot(mv_gc_poisson) 
qqnorm(residuals(mv_gc_poisson)[which(residuals(mv_gc_poisson)<10000)])

mv_gc_nb = manyglm(traits_species_mv ~ 
                     traits_sites_use$`Global Change Driver`, 
                   data= traits_sites_use,
                   family = 'negative.binomial')
#family=binomial("cloglog")) 
plot(mv_gc_nb) 
qqnorm(residuals(mv_gc_nb)[which(residuals(mv_gc_nb)<10000)])

# NOTE - okay, neg. binom definitely  better but I'll save both just in case
saveRDS(mv_gc_nb, here('./data/manyglm-intermediate/mv_gc_nb_main.rds')) 
saveRDS(mv_gc_poisson, 
        here('./data/manyglm-intermediate/mv_gc_pois_main.rds'))
# Reload
mv_gc_nb = readRDS(here('./data/manyglm-intermediate/mv_gc_nb_main.rds'))
mv_gc_poisson = readRDS(here('./data/manyglm-intermediate/mv_gc_pois_main.rds'))

### model output =================================================================

#model output significance test
mv_gc_nb_an = anova.manyglm(mv_gc_nb)
saveRDS(mv_gc_nb_an, 
        here('./data/manyglm-intermediate/mv_gc_nb_anova_main.rds')) 
mv_gc_nb_an = readRDS(here('./data/manyglm-intermediate/mv_gc_nb_anova_main.rds'))
#save and extract analysis of deviance output table
write_csv(mv_gc_nb_an$table, 
          here('./data/manyglm-intermediate/mv_gc_nb_anova_table_main.csv')) 

#individual adjusted p-values for species/traits - get univariate p-values
mv_gc_nb_an_uni = anova.manyglm(mv_gc_nb,p.uni="adjusted") 
saveRDS(mv_gc_nb_an_uni, 
        here('./data/manyglm-intermediate/mv_gc_univs_main.rds')) 
mv_gc_nb_an_uni = readRDS(here('./data/manyglm-intermediate/mv_gc_univs_main.rds'))
#Get the direction of effect fof each species with the main effect
gc_coef = coef(mv_gc_nb)

#figure out what the top traits are - recall traits are our 'species' here
mv_gc_nb_species = 
  sort(mv_gc_nb_an$uni.test[2,],
       decreasing=T,index.return=T)[1:25] #sort and select top species/traits
mv_gc_nb_species$ix[1:25] #the column numbers of the top  impacted spp/traits

#Need > 50% deviance explainaed --> result = 25 traits explain > 50% deviance
sum(mv_gc_nb_an$uni.test[2,mv_gc_nb_species$ix[1:25]])*100/
  sum(mv_gc_nb_an$uni.test[2,]) #25 species explained = 53.65588% Deviance

gc_top = 
  data.frame(dimnames(traits_species_use)[[2]][mv_gc_nb_species$ix[
    1:25]]) #df with the names of the top 20 traits
gc_top = gc_top %>% 
  rename('traits' = names(gc_top))
str(gc_top)


#How much deviance explained?
write_csv(gc_top, here('./data/manyglm-intermediate/mv_gc_top_main.csv')) 

#Now combine traits with their coeffs and p-values

#Create df to combine coef values,  also p-values from univ anovas & the top 20
gc_coef_l = data.frame(t(gc_coef)) 
gc_coef_l$traits = rownames(gc_coef_l) #convert rownames to a column
gc_coef_l = gc_coef_l %>% 
  rename('coef_intercept' = `X.Intercept.`, 
         'coef_gc_yes' = names(gc_coef_l)[2])

gc_top_coeffs = merge(gc_top, gc_coef_l,
                      by.x = 'traits',
                      by.y = 'traits') 

colnames(gc_top_coeffs)

#need to join with test statistic values
gc_an_test = as.data.frame(t(mv_gc_nb_an_uni$uni.test)) #first transpose coef_filter
gc_an_test$traits = rownames(gc_an_test) #convert rownames to a column

gc_an_test = gc_an_test %>% 
  rename('deviance_explained' = names(gc_an_test)[2])

gc_top_coeffs = merge(gc_top_coeffs,
                      gc_an_test,
                      by.x = 'traits',
                      by.y = 'traits')
gc_top_coeffs = gc_top_coeffs %>%
  select(-"(Intercept)")

#need to join with p-values
gc_an_pvalue = data.frame(t(mv_gc_nb_an_uni$uni.p)) #first transpose coef_filter
gc_an_pvalue$traits = rownames(gc_an_pvalue) #convert rownames to a column

gc_an_pvalue = gc_an_pvalue %>% 
  select(-names(gc_an_pvalue)[1]) 
gc_an_pvalue = gc_an_pvalue%>% 
  rename('p_value' = names(gc_an_pvalue)[1])

gc_top_coeffs = merge(gc_top_coeffs, 
                      gc_an_pvalue,
                      by.x = 'traits',
                      by.y = 'traits') 

write_csv(gc_top_coeffs, 
          here('./data/manyglm-intermediate/gc_top_coefs_main.csv'))

#See how many papers actually have those traits
papers_with_top_25_gc = traits_species_use
top_25_gc = gc_top$traits
papers_with_top_25_gc = papers_with_top_25_gc[top_25_gc]

rownames(papers_with_top_25_gc) = traits_sites_use$DOI
papers_with_top_25_gc = papers_with_top_25_gc[
  rowSums(papers_with_top_25_gc[, -1])>0, ]
