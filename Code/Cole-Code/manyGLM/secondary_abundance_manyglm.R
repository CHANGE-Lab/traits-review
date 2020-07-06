########## 
##########
# This code contains the multivariate modeling component of the analysis presented in
# in Green et al. (2020) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2020-07-03•
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

#subset data into the two groups needed
secondary_abundance = read_csv(here('./Data/Cole-Output-Data(readyforanalysis)/secondary_traits_dummy_abundance_models.csv'))
secondary_abundance_species = data.frame(secondary_abundance[,11:ncol(secondary_abundance)])
secondary_abundance_traits = data.frame(secondary_abundance[,1:10])
secondary_abundance_mv = mvabund(secondary_abundance_species)

#figure out what error distribution works
secondary_check_matrix = as.matrix(secondary_abundance_species)
hist(secondary_check_matrix)
hist(secondary_check_matrix[secondary_check_matrix > 0])

#try poisson and neg. binom.
mv_gc_poisson = manyglm(secondary_abundance_mv ~ 
                  secondary_abundance_traits$GlobalChangeCat, 
                data= secondary_abundance_traits,
                family = 'poisson')
summary(m_gc_poisson)
mv_gc_nb = manyglm(secondary_abundance_mv ~ 
                     secondary_abundance_traits$GlobalChangeCat, 
                   data= secondary_abundance_traits,
                   family = 'negative.binomial')
                #family=binomial("cloglog")) 
                #Build model for difference in publication trait assemblages based on assessment of global change factors
saveRDS(mv_gc, here('./Data/Cole-Output-ManyGLM/mv_gc.rds')) #Save model

##Model check
plot(mv_gc) #check residual vs. fitted values
qqnorm(residuals(mv_gc)[which(residuals(mv_gc)<10000)])
#qqnorm(residuals(mv_gc)[which(residuals(mv_gc)<10000)]); abline(c(0,1,col="red")) #check normality and fit


##Model output significance test
mv_gc.an <- anova.manyglm(mv_gc)
saveRDS(mv_gc.an, "mv_gc_anova.rds") #Save model
write.csv(mv_gc.an$table, "mv_gc_anova_table.csv") #save ANOVA table output
#Test was significant
#Note we don't want to use the summary call or Wald stat for presence/absence data

##Individual adjusted p-values for species/traits
mv_gc.an.uni <- anova.manyglm(mv_gc,p.uni="adjusted") #Get the univariate p-values for univariate tests
#View(mv_gc.an.uni$uni.test)
saveRDS(mv_gc.an.uni, "mv_gc_univs.rds") #Save model

#Get the direction of effect fof each species with the main effect
gc_coef <- coef(mv_gc) #save to R object


##Note could write loop and workflow for the calculation of deviance explained

##Top species
mv_gc.s <- sort(mv_gc.an$uni.test[2,],decreasing=T,index.return=T)[1:25] #sort and select top species/traits
mv_gc.s$ix[1:25] #the column numbers of the top most impacted spp/traits

##Deviance explained
#Need > 50% deviance explainaed --> result = 25 traits explain > 50% deviance
sum(mv_gc.an$uni.test[2,mv_gc.s$ix[1:25]])*100/sum(mv_gc.an$uni.test[2,]) #25 species explained = 51.93049% Deviance

##Save as dataframe
gc_top <- as.data.frame(dimnames(review_species_mv)[[2]][mv_gc.s$ix[1:25]]) #df with the names of the top 20 traits

str(gc_top)
names(gc_top)[names(gc_top)== "dimnames(review_species_mv)[[2]][mv_gc.s$ix[1:25]]"] <- "traits" #Rename column vector of traits
#How much deviance explained?
write.csv(gc_top, "mv_gc_top.csv")

###Combine traits with their coeffs and p-values
#Creat df to combine coef values, ideally also p-values from univ anovas and the top 20
gc_coef_l <- as.data.frame(t(gc_coef)) #first transpose coef_filter
gc_coef_l$traits <- rownames(gc_coef_l) #convert rownames to a column

gc_top_coeffs <- join(gc_top, gc_coef_l) #plyr left joint to obtain coeffs of top 20 traits

colnames(gc_top_coeffs)

gc_top_coeffs = gc_top_coeffs %>%
  dplyr::rename(coef_intercept = "(Intercept)", coef_gc_yes = "review_traits_mv$`Global Change`yes") #Clean up column names

#need to join with test statistic values
gc_an_test <- as.data.frame(t(mv_gc.an.uni$uni.test)) #first transpose coef_filter
gc_an_test$traits <- rownames(gc_an_test) #convert rownames to a column
gc_top_coeffs <- join(gc_top_coeffs, gc_an_test) #join with test statistic (deviance explained)
gc_top_coeffs = gc_top_coeffs %>%
  select(-"(Intercept)") %>%
  dplyr::rename(deviance_explained = "review_traits_mv$`Global Change`") #Clean up column names

#need to join with p-values
gc_an_pvalue <- as.data.frame(t(mv_gc.an.uni$uni.p)) #first transpose coef_filter
gc_an_pvalue$traits <- rownames(gc_an_pvalue) #convert rownames to a column
gc_top_coeffs <- join(gc_top_coeffs, gc_an_pvalue) #join with test statistic (deviance explained)
gc_top_coeffs = gc_top_coeffs %>%
  select(-"(Intercept)") %>%
  dplyr::rename(p_value = "review_traits_mv$`Global Change`") #Clean up column names

write.csv(gc_top_coeffs, "gc_top_coefs.csv")
