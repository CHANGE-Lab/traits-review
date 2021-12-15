########## 
##########
# This code contains the multivariate modeling component of the analysis presented in
# in Green et al. (2020) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2021-12-15
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

`%notin%` = Negate(`%in%`)

#subset data into the two groups needed
primary_abundance = 
  read_csv(
    here('./data/processed-data/primary_traits_dummy_abundance_models.csv'))

primary_abundance_species = 
  data.frame(primary_abundance[,12:ncol(primary_abundance)])

primary_abundance_traits = data.frame(primary_abundance[,1:11])
primary_abundance_mv = mvabund(primary_abundance_species)

# figure out distribution ======================================================

primary_check_matrix = as.matrix(primary_abundance_species)
hist(primary_check_matrix)
hist(primary_check_matrix[primary_check_matrix > 0])

#try poisson and neg. binom.
mv_gc_poisson = manyglm(primary_abundance_mv ~ 
                          primary_abundance_traits$GlobalChangeCat, 
                        data= primary_abundance_traits,
                        family = 'poisson')
plot(mv_gc_poisson) 
qqnorm(residuals(mv_gc_poisson)[which(residuals(mv_gc_poisson)<10000)])

mv_gc_nb = manyglm(primary_abundance_mv ~ 
                     primary_abundance_traits$GlobalChangeCat, 
                   data= primary_abundance_traits,
                   family = 'negative.binomial')
#family=binomial("cloglog")) 
plot(mv_gc_nb) 
qqnorm(residuals(mv_gc_nb)[which(residuals(mv_gc_nb)<10000)])

# NOTE - okay, neg. binom definitely  better but I'll save both just in case
saveRDS(mv_gc_nb, here('./data/manyglm-intermediate/mv_gc_nb_prim.rds')) 
saveRDS(mv_gc_poisson, here('./data/manyglm-intermediate/mv_gc_poisson_prim.rds')) 

#model output significance test
mv_gc_nb_an = anova.manyglm(mv_gc_nb)
saveRDS(mv_gc_nb_an, here('./data/manyglm-intermediate/mv_gc__nb_anova_prim.rds')) 
#mv_gc_nb_an = readRDS(here('./data/manyglm-intermediate/mv_gc__nb_anova.rds'))
write_csv(mv_gc_nb_an$table, 
          here('./data/manyglm-intermediate/mv_gc_nb_anova_table_prim.csv')) 

#individual adjusted p-values for species/traits - get univariate p-values
mv_gc_nb_an_uni = anova.manyglm(mv_gc_nb,p.uni="adjusted") 
saveRDS(mv_gc_nb_an_uni, here('./data/manyglm-intermediate/mv_gc_univs_prim.rds')) 
#mv_gc_nb_an_uni = readRDS(here('./data/manyglm-intermediate/mv_gc_univs.rds'))
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
  data.frame(dimnames(primary_abundance_species)[[2]][mv_gc_nb_species$ix[
    1:25]]) #df with the names of the top 20 traits
gc_top = gc_top %>% 
  rename('traits' = names(gc_top))
str(gc_top)


#How much deviance explained?
write_csv(gc_top, here('./data/manyglm-intermediate/mv_gc_top_prim.csv')) 

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

write_csv(gc_top_coeffs, here('./data/manyglm-intermediate/gc_top_coefs_prim.csv'))

#See how many papers actually have those traits
papers_with_top_25_gc = primary_abundance_species
top_25_gc = gc_top$traits
papers_with_top_25_gc = papers_with_top_25_gc[top_25_gcggg]

rownames(papers_with_top_25_gc) = primary_abundance_traits$DOI
papers_with_top_25_gc = papers_with_top_25_gc[rowSums(papers_with_top_25_gc[, -1])>0, ]