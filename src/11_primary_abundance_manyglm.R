########## 
##########
# This code contains the multivariate modeling component of the analysis presented in
# in Green et al. (2022) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Natasha Hardy & Cole Brookson
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

#try poisson and neg. binom.
mv_gc_poisson_primabun = 
  manyglm(primary_abundance_mv ~ GlobalChangeCat, 
          data= primary_abundance_traits,
          family = 'poisson')
plot(mv_gc_poisson_primabun) 

qqnorm(residuals(mv_gc_poisson_primabun)[
  which(residuals(mv_gc_poisson_primabun)<10000)])

mv_gc_nb_primabun = manyglm(primary_abundance_mv ~ GlobalChangeCat, 
                            data= primary_abundance_traits,
                            family = 'negative.binomial')
#family=binomial("cloglog")) 
plot(mv_gc_nb_primabun) 
qqnorm(residuals(mv_gc_nb_primabun)[which(residuals(mv_gc_nb_primabun)<10000)])

# NOTE - okay, neg. binom definitely better but I'll save both just in case
saveRDS(mv_gc_nb_primabun, 
        here("./data/manyglm-intermediate/mv_gc_nb_primabun.rds")) #, here('./Code')
saveRDS(mv_gc_poisson_primabun, 
        here("./data/manyglm-intermediate/mv_gc_poisson_primabun.rds")) 

# global change ================================================================

#try poisson and neg. binom.
mv_gc_poisson_primabun = 
  manyglm(primary_abundance_mv ~ GlobalChangeCat, 
          data= primary_abundance_traits,
          family = 'poisson')
plot(mv_gc_poisson_primabun) 
qqnorm(residuals(mv_gc_poisson_primabun)[
  which(residuals(mv_gc_poisson_primabun)<10000)])

mv_gc_nb_primabun = manyglm(primary_abundance_mv ~ 
                              primary_abundance_traits$GlobalChangeCat, 
                            data= primary_abundance_traits,
                            family = 'negative.binomial')
#family=binomial("cloglog")) 
plot(mv_gc_nb_primabun) 
qqnorm(residuals(mv_gc_nb_primabun)[which(residuals(mv_gc_nb_primabun)<10000)])

# NOTE - okay, neg. binom definitely better but I'll save both just in case
saveRDS(mv_gc_nb_primabun, 
        here("./data/manyglm-intermediate/mv_gc_nb_primabun.rds"))
saveRDS(mv_gc_poisson_primabun, 
        here("./data/manyglm-intermediate/mv_gc_poisson_primabun.rds"))

# deal with output =============================================================

#model output significance test
mv_gc_nb_primabun_an = anova.manyglm(mv_gc_nb_primabun)
saveRDS(mv_gc_nb_primabun_an, 
        here("./data/manyglm-intermediate/mv_gc_nb_primabun_anova.rds")) 
levels(as.factor(primary_abundance_traits$GlobalChangeCat))

#mv_gc_nb_an = readRDS(here('./Data/Cole-Output-ManyGLM/mv_gc__nb_anova.rds'))

write_csv(mv_gc_nb_primabun_an$table, 
          here("./output-tables/mv_gc_nb_primabun_anova_table.csv"))

#individual adjusted p-values for species/traits - get univariate p-values
mv_gc_nb_primabun_an_uni = anova.manyglm(mv_gc_nb_primabun,p.uni="adjusted")

saveRDS(mv_gc_nb_primabun_an_uni, 
        here("./data/manyglm-intermediate/mv_gc_nb_primabun_univs.rds"))
        
#mv_gc_nb_an_uni = readRDS(here('./Data/Cole-Output-ManyGLM/mv_gc_univs.rds'))
#Get the direction of effect fof each species with the main effect
gc_coef_primabun = coef(mv_gc_nb_primabun)

#figure out what the top traits are - recall traits are our 'species' here
mv_gc_nb_primabun_species = 
  sort(mv_gc_nb_primabun_an$uni.test[2,],
       decreasing=T,index.return=T)[1:5] #sort and select top species/traits
mv_gc_nb_primabun_species$ix[1:5] #the column numbers of the top impacted spp/traits

#Need > 50% deviance explainaed --> result = 25 traits explain > 50% deviance
sum(mv_gc_nb_primabun_an$uni.test[2,mv_gc_nb_primabun_species$ix[1:3]])*100/
  sum(mv_gc_nb_primabun_an$uni.test[2,]) #25 species explained = 53.65588% Deviance
#3 species explained >90% deviance

gc_top_primabun = 
  data.frame(dimnames(primary_abundance_species)[[2]][
    mv_gc_nb_primabun_species$ix[1:5]]) #df with the names of the top 20 traits

gc_top_primabun = gc_top_primabun %>% 
  dplyr::rename('traits' = names(gc_top_primabun))

str(gc_top_primabun)
#These were traits: chr "habitat" "morphology" "resource.acquisition"

#How much deviance explained?
write_csv(gc_top_primabun, here("./output-tables/gc_top5_primabun.csv"))

# top coeffs ===================================================================


#Create df to combine coef values, also p-values from univ anovas & the top 20
gc_coef_prim = data.frame(t(gc_coef_primabun)) 
gc_coef_prim$traits = rownames(gc_coef_prim) #convert rownames to a column
str(gc_coef_prim)

gc_coef_prim = gc_coef_prim %>% 
  dplyr::rename('coef_intercept' = `X.Intercept.`, 
                'coef_gc_yes' = names(gc_coef_prim)[2])

gc_top_coeffs = merge(gc_top_primabun, gc_coef_prim,
                      by.x = 'traits',
                      by.y = 'traits') 

colnames(gc_top_coeffs)

#need to join with test statistic values
gc_an_test = as.data.frame(t(mv_gc_nb_primabun_an_uni$uni.test)) #first transpose coef_filter
gc_an_test$traits = rownames(gc_an_test) #convert rownames to a column

gc_an_test = gc_an_test %>% 
  dplyr::rename('deviance_explained' = names(gc_an_test)[2])

gc_top_coeffs = merge(gc_top_coeffs,
                      gc_an_test,
                      by.x = 'traits',
                      by.y = 'traits')
gc_top_coeffs = gc_top_coeffs %>%
  select(-"(Intercept)")

#need to join with p-values
gc_an_pvalue = data.frame(t(mv_gc_nb_primabun_an_uni$uni.p)) #first transpose coef_filter
gc_an_pvalue$traits = rownames(gc_an_pvalue) #convert rownames to a column

gc_an_pvalue = gc_an_pvalue %>% 
  select(-names(gc_an_pvalue)[1]) 
gc_an_pvalue = gc_an_pvalue%>% 
  dplyr::rename('p_value' = names(gc_an_pvalue)[1])

gc_top_coeffs = merge(gc_top_coeffs, 
                      gc_an_pvalue,
                      by.x = 'traits',
                      by.y = 'traits') 

write_csv(gc_top_coeffs, here("./output-tables/gc_top_coefs.csv"))

#See how many papers actually have those traits
papers_with_top_3_gc = primary_abundance_species
top_3_gc = gc_top_primabun$traits
papers_with_top_3_gc = papers_with_top_3_gc[top_3_gc]

rownames(papers_with_top_3_gc) = primary_abundance_traits$DOI
papers_with_top_3_gc = papers_with_top_3_gc[
  rowSums(papers_with_top_3_gc[, -1])>0, ]

# predictive studies ===========================================================

mv_pd_nb_primabun = manyglm(primary_abundance_mv ~ 
                              PredictiveCat, 
                            data= primary_abundance_traits,
                            family = 'negative.binomial')
#family=binomial("cloglog")) 
plot(mv_pd_nb_primabun) 
qqnorm(residuals(mv_pd_nb_primabun)[which(residuals(mv_pd_nb_primabun)<10000)])

saveRDS(mv_pd_nb_primabun, 
        here("./data/manyglm-intermediate/mv_pd_nb_primabun.rds"))

# predictive output ============================================================

#model output significance test
mv_pd_nb_primabun_an = anova.manyglm(mv_pd_nb_primabun)
saveRDS(mv_pd_nb_primabun_an, 
        here("./data/manyglm-intermediate/mv_pd_nb_primabun_anova.rds")) 

#mv_gc_nb_an = readRDS(here('./Data/Cole-Output-ManyGLM/mv_gc__nb_anova.rds'))

write_csv(mv_pd_nb_primabun_an$table, 
          here("./output-tables/mv_pd_nb_primabun_anova_table.csv"))

#individual adjusted p-values for species/traits - get univariate p-values
mv_pd_nb_primabun_an_uni = anova.manyglm(mv_pd_nb_primabun,p.uni="adjusted")

saveRDS(mv_pd_nb_primabun_an_uni, 
        here("./data/manyglm-intermediate/mv_pd_nb_primabun_univs.rds")) 

#mv_gc_nb_an_uni = readRDS(here('./Data/Cole-Output-ManyGLM/mv_gc_univs.rds'))
#Get the direction of effect fof each species with the main effect
pd_coef_primabun = coef(mv_pd_nb_primabun)

#figure out what the top traits are - recall traits are our 'species' here
mv_pd_nb_primabun_species = 
  sort(mv_pd_nb_primabun_an$uni.test[2,],
       decreasing=T,index.return=T)[1:5] #sort and select top species/traits
mv_pd_nb_primabun_species$ix[1:5] #the column numbers of the top  impacted spp/traits

#Need > 50% deviance explainaed --> result = 25 traits explain > 50% deviance
sum(mv_pd_nb_primabun_an$uni.test[2,mv_pd_nb_primabun_species$ix[1:5]])*100/
  sum(mv_pd_nb_primabun_an$uni.test[2,]) #25 species explained = 53.65588% Deviance
#3 species explained >85% deviance

pd_top_primabun = 
  data.frame(dimnames(primary_abundance_species)[[2]][
    mv_pd_nb_primabun_species$ix[
    1:5]]) #df with the names of the top 20 traits

pd_top_primabun = pd_top_primabun %>% 
  dplyr::rename('traits' = names(pd_top_primabun))

str(pd_top_primabun)
#These were traits: chr  "habitat" "morphology" "resource.acquisition"

#How much deviance explained?
write_csv(pd_top_primabun, here("./output-tables/pd_top5_primabun.csv")) 

# predictive top coeffs ========================================================


#Create df to combine coef values,  also p-values from univ anovas & the top 20
pd_coef_prim = data.frame(t(pd_coef_primabun)) 
pd_coef_prim$traits = rownames(pd_coef_prim) #convert rownames to a column
str(pd_coef_prim)

pd_coef_prim = pd_coef_prim %>% 
  dplyr::rename('coef_intercept' = `X.Intercept.`, 
                'coef_pd_yes' = names(pd_coef_prim)[2])

pd_top_coeffs = merge(pd_top_primabun, pd_coef_prim,
                      by.x = 'traits',
                      by.y = 'traits') 

colnames(pd_top_coeffs)

#need to join with test statistic values
pd_an_test = as.data.frame(t(mv_pd_nb_primabun_an_uni$uni.test)) #first transpose coef_filter
pd_an_test$traits = rownames(pd_an_test) #convert rownames to a column

pd_an_test = pd_an_test %>% 
  dplyr::rename('deviance_explained' = names(pd_an_test)[2])

pd_top_coeffs = merge(pd_top_coeffs,
                      pd_an_test,
                      by.x = 'traits',
                      by.y = 'traits')
pd_top_coeffs = pd_top_coeffs %>%
  select(-"(Intercept)")

#need to join with p-values
pd_an_pvalue = data.frame(t(mv_pd_nb_primabun_an_uni$uni.p)) #first transpose coef_filter
pd_an_pvalue$traits = rownames(pd_an_pvalue) #convert rownames to a column

pd_an_pvalue = pd_an_pvalue %>% 
  select(-names(pd_an_pvalue)[1]) 
pd_an_pvalue = pd_an_pvalue%>% 
  dplyr::rename('p_value' = names(pd_an_pvalue)[1])

pd_top_coeffs = merge(pd_top_coeffs, 
                      pd_an_pvalue,
                      by.x = 'traits',
                      by.y = 'traits') 

write_csv(pd_top_coeffs, here("./output-tables/pd_top_coefs.csv"))

#See how many papers actually have those traits
papers_with_top_3_pd = primary_abundance_species
top_3_pd = pd_top_primabun$traits
papers_with_top_3_pd = papers_with_top_3_pd[top_3_pd]

rownames(papers_with_top_3_pd) = primary_abundance_traits$DOI
papers_with_top_3_pd = papers_with_top_3_pd[
  rowSums(papers_with_top_3_pd[, -1])>0, ]
