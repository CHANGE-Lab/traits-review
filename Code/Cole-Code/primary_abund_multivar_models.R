########## 
##########
# This code contains the multivariate modeling component of the analysis presented in
# in Green et al. (2020) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2020-06-30
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

#separate the modeling into the P/A modeling (1/0) and then the abundance modeling
primary_abundance = read_csv(here('./Data/Cole-Output-Data(readyforanalysis)/primary_traits_dummy_abundance_models.csv'))

#split species and sites
primary_abundance_sites = data.frame(primary_abundance[,1:10])
primary_abundance_species = data.frame(primary_abundance[,11:ncol(primary_abundance)])

#run actual ordination - try with both k = 2 & 3
set.seed(00001)
primary_abundance_ord_k3 = metaMDS(primary_abundance_species,
                                     distance = 'bray',
                                     trymax = 100,
                                     k = 3)
plot(primary_abundance_ord_k3)
summary(primary_abundance_ord_k3)
stressplot(primary_abundance_ord_k3)
primary_abundance_ord_k3$stress

primary_abundance_ord_k2 = metaMDS(primary_abundance_species,
                                     distance = 'jaccard',
                                     trymax = 100,
                                     k = 2)
plot(primary_abundance_ord_k2)
summary(primary_abundance_ord_k2)
stressplot(primary_abundance_ord_k2)
primary_abundance_ord_k2$stress

#extract scores -- looks like k = 3 is the way to go 
primary_abundance_ord_k3_scores = data.frame(scores(primary_abundance_ord_k3))
primary_abundance_ord_k3_scores = cbind(primary_abundance_ord_k3_scores,
                                        primary_abundance_sites)
rownames(primary_abundance_ord_k3_scores) = primary_abundance_ord_k3_scores$DOI


###################################### make all NMDS plots

######################## Ecosystem

#split
primary_abundance_ord_k3_scores$Ecosystem = 
  as.character(primary_abundance_ord_k3_scores$Ecosystem)

primary_abundance_ord_k3_scores_split_ecosystem = 
  split(primary_abundance_ord_k3_scores, 
        primary_abundance_ord_k3_scores$Ecosystem)

#lapply with chull
primary_abundance_ord_k3_scores_applied_ecosystem = 
  lapply(primary_abundance_ord_k3_scores_split_ecosystem, 
  function(df){
  df[chull(df), ]
})

#combine things
primary_abundance_ord_k3_scores_combined_ecosystem = 
  do.call(rbind, 
          primary_abundance_ord_k3_scores_applied_ecosystem)

#plot things 
primary_abundance_ord_k3_plot_ecosystem = ggplot() + 
  geom_polygon(data=primary_abundance_ord_k3_scores_combined_ecosystem,
               aes(x=NMDS1,y=NMDS2, 
                   fill=fct_rev(Ecosystem),
                   group=fct_rev(Ecosystem)), 
               alpha=0.30) + # add the convex hulls
  geom_point(data=primary_abundance_ord_k3_scores_combined_ecosystem,
             aes(x=NMDS1,y=NMDS2, 
                 colour = fct_rev(Ecosystem)), 
             size=2) + # add the point markers
  coord_equal() +
  theme_bw()  +
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        #panel.background = element_rect(fill = "lightgrey"), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank()) +
  scale_fill_viridis(discrete = TRUE, option="magma",
                     begin = 0.8, end = 0.2, name = "Ecosystem") +
  scale_colour_viridis(discrete = TRUE, option="magma",
                       begin = 0.8, end = 0.2, name = 'Ecosystem')

primary_abundance_ord_k3_plot_ecosystem

######################## Global Change

#split
primary_abundance_ord_k3_scores$GlobalChangeCat = 
  as.character(primary_abundance_ord_k3_scores$GlobalChangeCat)

primary_abundance_ord_k3_scores_split_change = 
  split(primary_abundance_ord_k3_scores, 
        primary_abundance_ord_k3_scores$GlobalChangeCat)

#lapply with chull
primary_abundance_ord_k3_scores_applied_change = 
  lapply(primary_abundance_ord_k3_scores_split_change, 
         function(df){
           df[chull(df), ]
         })

#combine things
primary_abundance_ord_k3_scores_combined_change = 
  do.call(rbind, 
          primary_abundance_ord_k3_scores_applied_change)

#plot things 
primary_abundance_ord_k3_plot_change = ggplot() + 
  geom_polygon(data=primary_abundance_ord_k3_scores_combined_change,
               aes(x=NMDS1,y=NMDS2, 
                   fill=fct_rev(GlobalChangeCat),
                   group=fct_rev(GlobalChangeCat)), 
               alpha=0.30) + # add the convex hulls
  geom_point(data=primary_abundance_ord_k3_scores_combined_change,
             aes(x=NMDS1,y=NMDS2, 
                 colour = fct_rev(GlobalChangeCat)), 
             size=2) + # add the point markers
  coord_equal() +
  theme_bw()  +
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        #panel.background = element_rect(fill = "lightgrey"), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank()) +
  scale_fill_viridis(discrete = TRUE, option="magma",
                     begin = 0.8, end = 0.2, name = "Global Change") +
  scale_colour_viridis(discrete = TRUE, option="magma",
                       begin = 0.8, end = 0.2, name = 'Global Change')

primary_abundance_ord_k3_plot_change
## Taxonomic Specific
#split
primary_abundance_ord_k3_scores$GlobalChangeCat = 
  as.character(primary_abundance_ord_k3_scores$GlobalChangeCat)

primary_abundance_ord_k3_scores_split_change = 
  split(primary_abundance_ord_k3_scores, 
        primary_abundance_ord_k3_scores$GlobalChangeCat)

#lapply with chull
primary_abundance_ord_k3_scores_applied_change = 
  lapply(primary_abundance_ord_k3_scores_split_change, 
         function(df){
           df[chull(df), ]
         })

#combine things
primary_abundance_ord_k3_scores_combined_change = 
  do.call(rbind, 
          primary_abundance_ord_k3_scores_applied_change)

#plot things 
primary_abundance_ord_k3_plot_change = ggplot() + 
  geom_polygon(data=primary_abundance_ord_k3_scores_combined_change,
               aes(x=NMDS1,y=NMDS2, 
                   fill=fct_rev(GlobalChangeCat),
                   group=fct_rev(GlobalChangeCat)), 
               alpha=0.30) + # add the convex hulls
  geom_point(data=primary_abundance_ord_k3_scores_combined_change,
             aes(x=NMDS1,y=NMDS2, 
                 colour = fct_rev(GlobalChangeCat)), 
             size=2) + # add the point markers
  coord_equal() +
  theme_bw()  +
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        #panel.background = element_rect(fill = "lightgrey"), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank()) +
  scale_fill_viridis(discrete = TRUE, option="magma",
                     begin = 0.8, end = 0.2, name = "Global Change") +
  scale_colour_viridis(discrete = TRUE, option="magma",
                       begin = 0.8, end = 0.2, name = 'Global Change')

primary_abundance_ord_k3_plot_change
## Taxonomic Group

## Type of Study

## Filter










