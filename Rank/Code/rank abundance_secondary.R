

### SECONDARY TRAITS CLASSIFICATION####
## Analysis and plotting of rank abundance for traits (secondary classification; many traits!)
# across studies grouped by different factors (global change drivers, taxonomic groups,etc.).


##### DATA ####

####load environmental (grouping) data for the studies:
enviro_second <- read.csv('rank_enviro_secondary.csv')
str(enviro_second)

#change each to factors:
enviro_second$Ecosystem <- as.factor(enviro_second$Ecosystem)
enviro_second$GlobalChange <- as.factor(enviro_second$GlobalChange)
enviro_second$Taxonomic <- as.factor(enviro_second$Taxonomic)
enviro_second$Forecasting <- as.factor(enviro_second$Forecasting)
enviro_second$TOS <- as.factor(enviro_second$TOS)
enviro_second$filter <- as.factor(enviro_second$filter)


#### load trait matrix for secondary traits classification of the studies:
traits_second <- read.csv('rank_matrix_secondary.csv')
str(traits_second)


### LIBRARIES ###

#### load libraries:
library(BiodiversityR)



###########################################################################
###### RANK ABUND ANALYSIS & PLOTTING FOR EACH VARIABLE OF INTEREST BELOW #####
# log abundance and proportion seem to be most useful metrics for trait rank
###########################################################################


########### 1. Global change ####################

### A. Create summary of rank abundance for each global change driver: 
modsgc <- with(enviro_second, lapply(levels(GlobalChange), function(lev)
  rankabundance(traits_second, enviro_second, 'GlobalChange', lev)))


### B. View level order (for adding plot titles below):
levels(enviro_second$GlobalChange)
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

rankabunplot(modsgc[[1]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "red", main = "No global change", ylim = c(0, 15))
rankabunplot(modsgc[[2]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "darkred", main = "Climate change", ylim = c(0, 15))
rankabunplot(modsgc[[3]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "purple", main = "Exploitation", ylim = c(0, 15))
rankabunplot(modsgc[[4]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "magenta", main = "Global change broad", ylim = c(0, 15))
rankabunplot(modsgc[[5]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "darkred", main = "Global change multiple", ylim = c(0, 15))
rankabunplot(modsgc[[6]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "purple", main = "Habitat degredation", ylim = c(0, 15))
rankabunplot(modsgc[[7]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "magenta", main = "Invasion", ylim = c(0, 15))



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
modspredict <- with(enviro_second, lapply(levels(Forecasting), function(lev)
  rankabundance(traits_second, enviro_second, 'Forecasting', lev)))

###  B. View level order for plot names:
levels(enviro_second$Forecasting)
#1 =  0
#2 =  1


### C. plots:

##### Proportion- Arrange in 1 x 2 grid #####
par(mfrow = c(1,2))
rankabunplot(modspredict[[1]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "red", main = "Descriptive", ylim = c(0, 11))
rankabunplot(modspredict[[2]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "darkred", main = "Predictive", ylim = c(0, 11))


##### Log abundance- Arrange in 1 x 2 grid #####
par(mfrow = c(1,2))
rankabunplot(modspredict[[1]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "red", main = "Descriptive")#, ylim = c(0,5000))
rankabunplot(modspredict[[2]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "darkred", main = "Predictive")#, ylim = c(0,5000))








################# 3. Taxonomy ##################

### A. Create summary of rank abundance for each taxonomic group:
modstax <- with(enviro_second, lapply(levels(Taxonomic), function(lev)
  rankabundance(traits_second, enviro_second, 'Taxonomic', lev)))


### B. View level order:
levels(enviro_second$Taxonomic)
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

rankabunplot(modstax[[1]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "red", main = "Birds", ylim = c(0, 20))
rankabunplot(modstax[[2]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "darkred", main = "Fish", ylim = c(0, 20))
rankabunplot(modstax[[3]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "purple", main = "Herps", ylim = c(0, 20))
rankabunplot(modstax[[4]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "magenta", main = "Insects", ylim = c(0, 20))
rankabunplot(modstax[[5]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "black", main = "Mammals", ylim = c(0, 20))
rankabunplot(modstax[[6]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "pink", main = "Multiple", ylim = c(0, 20))
rankabunplot(modstax[[7]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "green", main = "Other", ylim = c(0, 20))
rankabunplot(modstax[[8]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "blue", main = "Plankton", ylim = c(0, 20))
rankabunplot(modstax[[9]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "dark blue", main = "Plants", ylim = c(0, 20))



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
modstype <- with(enviro_second, lapply(levels(TOS), function(lev)
  rankabundance(traits_second, enviro_second, 'TOS', lev)))


### B. View level order:
levels(enviro_second$TOS)
#1 = Experiment
#2 = Metaanalysis
#3 = Observational
#4 = QModel
#5 = Review
#6 =TModel

### C. plots:

############ Proportion - Arrange in 3 x 3 grid #####################

par(mfrow = c(2,3))

rankabunplot(modstax[[1]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "red", main = "Experiment", ylim = c(0, 18))
rankabunplot(modstax[[2]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "darkred", main = "Metaanalysis", ylim = c(0, 18))
rankabunplot(modstax[[3]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "purple", main = "Observational", ylim = c(0, 18))
rankabunplot(modstax[[4]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "magenta", main = "QModel", ylim = c(0, 18))
rankabunplot(modstax[[5]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "black", main = "Review", ylim = c(0, 18))
rankabunplot(modstax[[6]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "pink", main = "TModel", ylim = c(0, 18))


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
modsfilter <- with(enviro_second, lapply(levels(filter), function(lev)
  rankabundance(traits_second, enviro_second, 'filter', lev)))


### B. View level order:
levels(enviro_second$filter)
#1 = Abiotic
#2 = Biotic
#3 = Dispersal
#4 = Trophic


### C. plots:

############ Proportion - Arrange in 3 x 3 grid #####################

par(mfrow = c(2,2))

rankabunplot(modstax[[1]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "red", main = "Abiotic", ylim = c(0, 17))
rankabunplot(modstax[[2]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "darkred", main = "Biotic", ylim = c(0, 17))
rankabunplot(modstax[[3]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "purple", main = "Dispersal", ylim = c(0, 17))
rankabunplot(modstax[[4]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "magenta", main = "Trophic", ylim = c(0, 17))

############ Log abundance - Arrange in 3 x 3 grid #####################

par(mfrow = c(2,2))

rankabunplot(modstax[[1]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "red", main = "Abiotic")#, ylim = c(0, 45))
rankabunplot(modstax[[2]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "darkred", main = "Biotic")#, ylim = c(0, 45))
rankabunplot(modstax[[3]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "purple", main = "Dispersal")#, ylim = c(0, 45))
rankabunplot(modstax[[4]],scale='logabun', addit=FALSE, specnames=c(1,2,3,4,5), col = "magenta", main = "Trophic")#, ylim = c(0, 45))





########### 6. Ecosystem ####################

### A. Create summary of rank abundance for each ecosystem: 
modsgc <- with(enviro_second, lapply(levels(Ecosystem), function(lev)
  rankabundance(traits_second, enviro_second, 'Ecosystem', lev)))


### B. View level order (for adding plot titles below):
levels(enviro_second$Ecosystem)
#1 = Freshwater
#2 = Marine
#3 = Multiple
#4 = Terrestrial


### C. Create plots for desired metrics of rank abundance for the levels of the variable 

##### Rank metric: proportion- Arrange in 2 x 2 grid #####

par(mfrow = c(2,2))

rankabunplot(modsgc[[1]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "red", main = "Freshwater", ylim = c(0, 13))
rankabunplot(modsgc[[2]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "darkred", main = "Marine", ylim = c(0, 13))
rankabunplot(modsgc[[3]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "purple", main = "Multiple", ylim = c(0, 13))
rankabunplot(modsgc[[4]],scale='proportion', addit=FALSE, specnames=c(1,2,3,4,5), col = "magenta", main = "Terrestrial", ylim = c(0, 13))


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
Rank.Abun.1 <- rankabundance(traits_second)

#plot for all data together
rankabunplot(Rank.Abun.1, addit = F, labels = "", scale = "abundance", type = "o")

#plot composition:

plotEcosystem <- rankabuncomp(traits_second,  y = enviro_second, factor = "Ecosystem", scale = "abundance", 
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



