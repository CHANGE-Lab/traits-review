

### PRIMARY TRAITS CLASSIFICTION ####
## Analysis and plotting of rank abundance for traits (primary classifications; 9 traits)
# across studies grouped by different factors (global change drivers, taxonomic groups,etc.).


##### DATA ####

####load environmental (grouping) data for the studies:
enviro <- read.csv('rank_enviro_primary.csv')
str(enviro)

#change each to factors:
enviro$Ecosystem <- as.factor(enviro$Ecosystem)
enviro$GlobalChange <- as.factor(enviro$GlobalChange)
enviro$Taxonomic <- as.factor(enviro$Taxonomic)
enviro$Forecasting <- as.factor(enviro$Forecasting)
enviro$TOS <- as.factor(enviro$TOS)
enviro$filter <- as.factor(enviro$filter)


#### load trait matrix for primary classification of the studies:
traits <- read.csv('rank_matrix_primary.csv')
str(traits)


### LIBRARIES ###

#### load libraries:
library(BiodiversityR)



###########################################################################
###### RANK ABUND ANALYSIS & PLOTTING FOR EACH VARIABLE OF INTEREST BELOW #####
# log abundance and proportion seem to be most useful metrics for trait rank
###########################################################################


########### 1. Global change ####################

### A. Create summary of rank abundance for each globabl change driver: 
modsgc <- with(enviro, lapply(levels(GlobalChange), function(lev)
  rankabundance(traits, enviro, 'GlobalChange', lev)))


### B. View level order (for adding plot titles below):
levels(enviro$GlobalChange)
#1 = o
#2 = climate change
#3 = exploitation
#4 = global change broad
#5 = global change multiple
#6 = habitat degredation
#7 = invasion


### C. Create plots for desired metrics of rank abundance for the levels of the variable 

##### Rank metric: proportion- Arrange in 3 x 3 grid #####

par(mfrow = c(3,3))
 
rankabunplot(modsgc[[1]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "red", main = "No global change", ylim = c(0, 50))
rankabunplot(modsgc[[2]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "darkred", main = "Climate change", ylim = c(0, 50))
rankabunplot(modsgc[[3]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "purple", main = "Exploitation", ylim = c(0, 50))
rankabunplot(modsgc[[4]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "magenta", main = "Global change broad", ylim = c(0, 50))
rankabunplot(modsgc[[5]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "darkred", main = "Global change multiple", ylim = c(0, 50))
rankabunplot(modsgc[[6]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "purple", main = "Habitat degredation", ylim = c(0, 50))
rankabunplot(modsgc[[7]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "magenta", main = "Invasion", ylim = c(0, 50))



##### Rank metric: log abundance- Arrange in 3 x 3 grid #####

par(mfrow = c(3,3))

rankabunplot(modsgc[[1]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "red", main = "No global change") #, ylim = c(0, 50))
rankabunplot(modsgc[[2]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "darkred", main = "Climate change") #, ylim = c(0, 50))
rankabunplot(modsgc[[3]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "purple", main = "Exploitation") #, ylim = c(0, 50))
rankabunplot(modsgc[[4]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "magenta", main = "Global change broad")#, ylim = c(0, 50))
rankabunplot(modsgc[[5]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "darkred", main = "Global change multiple")#, ylim = c(0, 50))
rankabunplot(modsgc[[6]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "purple", main = "Habitat degredation")#, ylim = c(0, 50))
rankabunplot(modsgc[[7]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "magenta", main = "Invasion")#, ylim = c(0, 50))







########### 2. Forecasting ####################

### A. Create summary of rank abundance for predictive (1) vs descriptive (i.e. 0; not predictive): 
modspredict <- with(enviro, lapply(levels(Forecasting), function(lev)
  rankabundance(traits, enviro, 'Forecasting', lev)))

###  B. View level order for plot names:
levels(enviro$Forecasting)
#1 =  0
#2 =  1


### C. plots:

##### Proportion- Arrange in 1 x 2 grid #####
par(mfrow = c(1,2))
rankabunplot(modspredict[[1]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "red", main = "Descriptive", ylim = c(0, 40))
rankabunplot(modspredict[[2]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "darkred", main = "Predictive", ylim = c(0, 40))


##### Log abundance- Arrange in 1 x 2 grid #####
par(mfrow = c(1,2))
rankabunplot(modspredict[[1]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "red", main = "Descriptive")#, ylim = c(0,5000))
rankabunplot(modspredict[[2]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "darkred", main = "Predictive")#, ylim = c(0,5000))








################# 3. Taxonomy ##################

### A. Create summary of rank abundance for each taxonomic group:
modstax <- with(enviro, lapply(levels(Taxonomic), function(lev)
  rankabundance(traits, enviro, 'Taxonomic', lev)))


### B. View level order:
levels(enviro$Taxonomic)
#1 = Birds
#2 = Fish
#3 = Herptofauna
#4 = Insects
#5 = Mammals
#6 = Multiple
#7 = Other
#8 = Plankton
#9 = Plants

### C. plots:

############ Proportion - Arrange in 3 x 3 grid #####################

par(mfrow = c(3,3))

rankabunplot(modstax[[1]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "red", main = "Birds", ylim = c(0, 50))
rankabunplot(modstax[[2]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "darkred", main = "Fish", ylim = c(0, 50))
rankabunplot(modstax[[3]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "purple", main = "Herps", ylim = c(0, 50))
rankabunplot(modstax[[4]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "magenta", main = "Insects", ylim = c(0, 50))
rankabunplot(modstax[[5]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "black", main = "Mammals", ylim = c(0, 50))
rankabunplot(modstax[[6]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "pink", main = "Multiple", ylim = c(0, 50))
rankabunplot(modstax[[7]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "green", main = "Other", ylim = c(0, 50))
rankabunplot(modstax[[8]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "blue", main = "Plankton", ylim = c(0, 50))
rankabunplot(modstax[[9]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "dark blue", main = "Plants", ylim = c(0, 50))



############ Log abundance - Arrange in 3 x 3 grid #####################

par(mfrow = c(3,3))

rankabunplot(modstax[[1]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "red", main = "Birds")#, ylim = c(0, 50))
rankabunplot(modstax[[2]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "darkred", main = "Fish")#, ylim = c(0, 50))
rankabunplot(modstax[[3]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "purple", main = "Herps")#, ylim = c(0, 50))
rankabunplot(modstax[[4]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "magenta", main = "Insects")#, ylim = c(0, 50))
rankabunplot(modstax[[5]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "black", main = "Mammals")#, ylim = c(0, 50))
rankabunplot(modstax[[6]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "pink", main = "Multiple")#, ylim = c(0, 50))
rankabunplot(modstax[[7]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "green", main = "Other")#, ylim = c(0, 50))
rankabunplot(modstax[[8]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "blue", main = "Plankton")#, ylim = c(0, 50))
rankabunplot(modstax[[9]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "dark blue", main = "Plants")#, ylim = c(0, 50))









################# 4. Type of study (TOS) ################

### A. Create summary of rank abundance for each taxonomic group:
modstype <- with(enviro, lapply(levels(TOS), function(lev)
  rankabundance(traits, enviro, 'TOS', lev)))


### B. View level order:
levels(enviro$TOS)
#1 = Experiment
#2 = Metaanalysis
#3 = Observational
#4 = QModel
#5 = Review
#6 =TModel

### C. plots:

############ Proportion - Arrange in 3 x 3 grid #####################

par(mfrow = c(2,3))

rankabunplot(modstax[[1]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "red", main = "Experiment", ylim = c(0, 50))
rankabunplot(modstax[[2]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "darkred", main = "Metaanalysis", ylim = c(0, 50))
rankabunplot(modstax[[3]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "purple", main = "Observational", ylim = c(0, 50))
rankabunplot(modstax[[4]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "magenta", main = "QModel", ylim = c(0, 50))
rankabunplot(modstax[[5]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "black", main = "Review", ylim = c(0, 50))
rankabunplot(modstax[[6]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "pink", main = "TModel", ylim = c(0, 50))


############ Log abundance - Arrange in 3 x 3 grid #####################

par(mfrow = c(2,3))

rankabunplot(modstax[[1]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "red", main = "Experiment")#, ylim = c(0, 50))
rankabunplot(modstax[[2]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "darkred", main = "Metaanalysis")#, ylim = c(0, 50))
rankabunplot(modstax[[3]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "purple", main = "Observational")#, ylim = c(0, 50))
rankabunplot(modstax[[4]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "magenta", main = "QModel")#, ylim = c(0, 50))
rankabunplot(modstax[[5]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "black", main = "Review")#, ylim = c(0, 50))
rankabunplot(modstax[[6]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "pink", main = "TModel")#, ylim = c(0, 50))









################# 5. Filter level  ################

### A. Create summary of rank abundance for each level of environmental filtering:
modsfilter <- with(enviro, lapply(levels(filter), function(lev)
  rankabundance(traits, enviro, 'filter', lev)))


### B. View level order:
levels(enviro$filter)
#1 = Abiotic
#2 = Biotic
#3 = Dispersal
#4 = Trophic


### C. plots:

############ Proportion - Arrange in 3 x 3 grid #####################

par(mfrow = c(2,2))

rankabunplot(modstax[[1]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "red", main = "Abiotic", ylim = c(0, 45))
rankabunplot(modstax[[2]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "darkred", main = "Biotic", ylim = c(0, 45))
rankabunplot(modstax[[3]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "purple", main = "Dispersal", ylim = c(0, 45))
rankabunplot(modstax[[4]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "magenta", main = "Trophic", ylim = c(0, 45))

############ Log abundance - Arrange in 3 x 3 grid #####################

par(mfrow = c(2,2))

rankabunplot(modstax[[1]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "red", main = "Abiotic")#, ylim = c(0, 45))
rankabunplot(modstax[[2]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "darkred", main = "Biotic")#, ylim = c(0, 45))
rankabunplot(modstax[[3]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "purple", main = "Dispersal")#, ylim = c(0, 45))
rankabunplot(modstax[[4]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "magenta", main = "Trophic")#, ylim = c(0, 45))







########### 6. Ecosystem ####################

### A. Create summary of rank abundance for each ecosystem: 
modsgc <- with(enviro, lapply(levels(Ecosystem), function(lev)
  rankabundance(traits, enviro_second, 'Ecosystem', lev)))


### B. View level order (for adding plot titles below):
levels(enviro_second$Ecosystem)
#1 = Freshwater
#2 = Marine
#3 = Multiple
#4 = Terrestrial


### C. Create plots for desired metrics of rank abundance for the levels of the variable 

##### Rank metric: proportion- Arrange in 2 x 2 grid #####

par(mfrow = c(2,2))

rankabunplot(modsgc[[1]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "red", main = "Freshwater", ylim = c(0, 40))
rankabunplot(modsgc[[2]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "darkred", main = "Marine", ylim = c(0, 40))
rankabunplot(modsgc[[3]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "purple", main = "Multiple", ylim = c(0, 40))
rankabunplot(modsgc[[4]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "magenta", main = "Terrestrial", ylim = c(0, 40))


##### Rank metric: log abundance- Arrange in 2 x 2 grid #####

par(mfrow = c(2,2))

rankabunplot(modsgc[[1]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "red", main = "Freshwater")#, ylim = c(0, 13))
rankabunplot(modsgc[[2]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "darkred", main = "Marine")#, ylim = c(0, 13))
rankabunplot(modsgc[[3]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "purple", main = "Multiple")#, ylim = c(0, 13))
rankabunplot(modsgc[[4]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "magenta", main = "Terrestrial")#, ylim = c(0, 13))






################################
################################
################################
# extra code below
################################
################################
################################



########### Logabundance ###############

## check levels, plot highest y first 
rankabunplot(modstax[[9]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "red")

## add the rest

## change colour etc to separate the lines if wished
rankabunplot(modstax[[2]],scale='logabun', addit=TRUE, specnames=c(1,2,3,4,5), col = "darkred")
rankabunplot(modstax[[3]],scale='logabun', addit=TRUE, specnames=c(1,2,3,4,5), col = "purple")
rankabunplot(modstax[[4]],scale='logabun', addit=TRUE, specnames=c(1,2,3,4,5), col = "magenta")
rankabunplot(modstax[[5]],scale='logabun', addit=TRUE, specnames=c(1,2,3,4,5), col = "magenta")
rankabunplot(modstax[[6]],scale='logabun', addit=TRUE, specnames=c(1,2,3,4,5), col = "magenta")
rankabunplot(modstax[[7]],scale='logabun', addit=TRUE, specnames=c(1,2,3,4,5), col = "magenta")
rankabunplot(modstax[[8]],scale='logabun', addit=TRUE, specnames=c(1,2,3,4,5), col = "magenta")
rankabunplot(modstax[[1]],scale='logabun', addit=TRUE, specnames=c(1,2,3,4,5), col = "magenta")




############ Proportion #####################

## check levels, plot highest y first 
rankabunplot(modstax[[8]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "red")

## add the rest

## change colour etc to separate the lines if wished
rankabunplot(modstax[[9]],scale='proportion', addit=TRUE, specnames=c(1,2,3,4,5), col = "darkred")
rankabunplot(modstax[[3]],scale='proportion', addit=TRUE, specnames=c(1,2,3,4,5), col = "purple")
rankabunplot(modstax[[4]],scale='proportion', addit=TRUE, specnames=c(1,2,3,4,5), col = "magenta")
rankabunplot(modstax[[5]],scale='proportion', addit=TRUE, specnames=c(1,2,3,4,5), col = "magenta")
rankabunplot(modstax[[6]],scale='proportion', addit=TRUE, specnames=c(1,2,3,4,5), col = "magenta")
rankabunplot(modstax[[7]],scale='proportion', addit=TRUE, specnames=c(1,2,3,4,5), col = "magenta")
rankabunplot(modstax[[2]],scale='proportion', addit=TRUE, specnames=c(1,2,3,4,5), col = "magenta")
rankabunplot(modstax[[1]],scale='proportion', addit=TRUE, specnames=c(1,2,3,4,5), col = "magenta")







##### Log abundance- levels overlaid on single plot #####


## level 4 seems to be most extreme: draw it first 
rankabunplot(mods[[1]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "red")

## add the rest

## change colour etc to separate the lines if wished
rankabunplot(mods[[2]],scale='logabun', addit=TRUE, specnames=c(1,2,3,4,5), col = "darkred")
rankabunplot(mods[[3]],scale='logabun', addit=TRUE, specnames=c(1,2,3,4,5), col = "purple")
rankabunplot(mods[[6]],scale='logabun', addit=TRUE, specnames=c(1,2,3,4,5), col = "orange")
rankabunplot(mods[[7]],scale='logabun', addit=TRUE, specnames=c(1,2,3,4,5), col = "magenta")





# create ranks:
Rank.Abun.1 <- rankabundance(traits)

#plot for all data together
rankabunplot(Rank.Abun.1, addit = F, labels = "", scale = "abundance", type = "o")

#plot composition:

plotEcosystem <- rankabuncomp(traits,  y = enviro, factor = "Ecosystem", scale = "abundance", 
             scaledx = F, type ="o", legend = TRUE, title = "Ecosystem type" )

plot




##### SAMPLE DATA AND CODE FROM R DOCUMENTATION ####

#sample data:
library(vegan)
data(dune.env)
data(dune)

mods <- with(dune.env, lapply(levels(Management), function(lev)
  rankabundance(dune, dune.env, 'Management', lev)))


## level 4 seems to be most extreme: draw it first 
rankabunplot(mods[[4]],scale='abundance', addit=FALSE, specnames=c(1,2,3))

## add the rest

## change colour etc to separate the lines if wished
rankabunplot(mods[[3]],scale='abundance', addit=TRUE, specnames=c(1,2,3))
rankabunplot(mods[[2]],scale='abundance', addit=TRUE, specnames=c(1,2,3))
rankabunplot(mods[[1]],scale='abundance', addit=TRUE, specnames=c(1,2,3))



