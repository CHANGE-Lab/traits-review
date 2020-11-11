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
library(cowplot)

#separate the modeling into the P/A modeling (1/0) and then the abundance modeling
secondary_pa = read_csv(here('./Data/Cole-Output-Data(readyforanalysis)/secondary_traits_dummy_pa_models.csv'))

#split species and sites
secondary_pa_sites = data.frame(secondary_pa[,1:10])
secondary_pa_species = data.frame(secondary_pa[,11:ncol(secondary_pa)])

#run actual ordination - try with both k = 2 & 3
# secondary_pa_ord_k3 = metaMDS(secondary_pa_species,
#                             distance = 'jaccard',
#                             trymax = 100,
#                             k = 3)
# plot(secondary_pa_ord_k3)
secondary_pa_ord_k2 = metaMDS(secondary_pa_species,
                            distance = 'jaccard',
                            trymax = 1000,
                            k = 4)
saveRDS(secondary_pa_ord_k2, here('./Model Output/Secondary_pa/nMDS-Ordinations/secondary_abundance_ord.rds'))
secondary_pa_ord_k2 = readRDS(here('./Model Output/Secondary_pa/nMDS-Ordinations/secondary_abundance_ord.rds'))
plot(secondary_pa_ord_k2)

#extract scores
############################## Plotting pipeline ###############################

#extract scores
secondary_pa_k4_scores <- data.frame(scores(secondary_pa_ord_k2)) 
secondary_pa_k4_scores$points <- rownames(secondary_pa_k4_scores) 
secondary_pa_scores = cbind(secondary_pa_sites, secondary_pa_k4_scores)

secondary_pa_scores = 
  secondary_pa_scores[
    which(secondary_pa_scores$NMDS1 < 0.05 &
            secondary_pa_scores$NMDS1 > -0.05 &
            secondary_pa_scores$NMDS2 > -0.05 &
            secondary_pa_scores$NMDS2 < 0.05),]

hist(secondary_pa_scores$NMDS1)
hist(secondary_pa_scores$NMDS2)



#add species
secondary_pa_trait_scores = data.frame(scores(secondary_pa_ord_k2, 'species'))
secondary_pa_trait_scores$species = rownames(secondary_pa_trait_scores)
secondary_pa_trait_scores$species[6] = 'life history'
secondary_pa_trait_scores$species[8] = 'resource acquisition'
str(secondary_pa_scores)

########### Get Hulls

secondary_pa_scores$Ecosystem = 
  as.factor(secondary_pa_scores$Ecosystem)
secondary_pa_scores$GlobalChangeCat = 
  as.factor(secondary_pa_scores$GlobalChangeCat)
secondary_pa_scores$Taxonomic = 
  as.factor(secondary_pa_scores$Taxonomic)
secondary_pa_scores$TOS = 
  as.factor(secondary_pa_scores$TOS)
secondary_pa_scores$Filter = 
  as.factor(secondary_pa_scores$Filter)
secondary_pa_scores$GlobalChange = 
  as.factor(secondary_pa_scores$GlobalChange)
secondary_pa_scores$PredictiveCat = 
  as.factor(secondary_pa_scores$PredictiveCat)

#ecosystem
eco = as.character(unique(secondary_pa_scores$Ecosystem))
for(i in 1:length(eco)) {
  temp = eco[i]
  df = secondary_pa_scores[
    secondary_pa_scores$Ecosystem == temp, 
  ][chull(secondary_pa_scores[
    secondary_pa_scores$Ecosystem == temp,
    c("NMDS1", "NMDS2")
  ]), ]
  assign(paste0('grp_sec_pa_eco_',temp), df)
}
hull_sec_pa_ecosystem = rbind(grp_sec_pa_eco_Terrestrial, grp_sec_pa_eco_Freshwater, 
                          grp_sec_pa_eco_Marine, grp_sec_pa_eco_Multiple)

#taxonomic
tax = as.character(unique(secondary_pa_scores$Taxonomic))
for(i in 1:length(tax)) {
  temp = tax[i]
  df = secondary_pa_scores[
    secondary_pa_scores$Taxonomic == temp, 
  ][chull(secondary_pa_scores[
    secondary_pa_scores$Taxonomic == temp,
    c("NMDS1", "NMDS2")
  ]), ]
  assign(paste0('grp_sec_pa_tax_',temp), df)
}
hull_sec_pa_taxonomic = rbind(grp_sec_pa_tax_Birds, grp_sec_pa_tax_Insects, 
                          grp_sec_pa_tax_Mammals, grp_sec_pa_tax_Multiple, 
                          grp_sec_pa_tax_Herpetofauna, grp_sec_pa_tax_Plants,
                          grp_sec_pa_tax_Plankton, grp_sec_pa_tax_Other, 
                          grp_sec_pa_tax_Fish)

#taxonomic group
#first make new classifications 
secondary_pa_scores = secondary_pa_scores %>% 
  mutate(TaxonomicGroup = 
           ifelse(secondary_pa_scores$Taxonomic %in% 
                    c('Mammals', 'Birds', 'Herpetofauna', 'Fish'), 'Vertebrates', 
                  ifelse(secondary_pa_scores$Taxonomic %in%
                           c('Insects', 'Plankton'), 'Invertebrates', 
                         ifelse(secondary_pa_scores$Taxonomic == 'Other', 'Other',
                                ifelse(secondary_pa_scores$Taxonomic == 'Multiple', 'Multiple',
                                       ifelse(secondary_pa_scores$Taxonomic == 'Plants', 'Plants', NA))))))
#now put them in the normal loop
tax_group = as.character(unique(secondary_pa_scores$TaxonomicGroup))
for(i in 1:length(tax_group)) {
  temp = tax_group[i]
  df = secondary_pa_scores[
    secondary_pa_scores$TaxonomicGroup == temp, 
  ][chull(secondary_pa_scores[
    secondary_pa_scores$TaxonomicGroup == temp,
    c("NMDS1", "NMDS2")
  ]), ]
  assign(paste0('grp_sec_pa_taxgroup_',temp), df)
}
hull_sec_pa_taxonomic_group = rbind(grp_sec_pa_taxgroup_Invertebrates, 
                                grp_sec_pa_taxgroup_Multiple,
                                grp_sec_pa_taxgroup_Other, 
                                grp_sec_pa_taxgroup_Plants,
                                grp_sec_pa_taxgroup_Vertebrates)

#TOS
levels(secondary_pa_scores$TOS)[levels(secondary_pa_scores$TOS)==
                                'TModel']='Theory'
levels(secondary_pa_scores$TOS)[levels(secondary_pa_scores$TOS)==
                                'QModel']='Observational'
tos = as.character(unique(secondary_pa_scores$TOS))
for(i in 1:length(tos)) {
  temp = tos[i]
  df = secondary_pa_scores[
    secondary_pa_scores$TOS == temp, 
  ][chull(secondary_pa_scores[
    secondary_pa_scores$TOS == temp,
    c("NMDS1", "NMDS2")
  ]), ]
  assign(paste0('grp_sec_pa_tos_',temp), df)
}
hull_sec_pa_tos = rbind(grp_sec_pa_tos_Observational, grp_sec_pa_tos_Experiment, 
                    grp_sec_pa_tos_Metanalysis, grp_sec_pa_tos_Theory, 
                    grp_sec_pa_tos_Review)

#Filter
levels(secondary_pa_scores$Filter)[levels(secondary_pa_scores$Filter)==
                                   "Fundamental"] <- "Abiotic"
levels(secondary_pa_scores$Filter)[levels(secondary_pa_scores$Filter)==
                                   "Physical"] <- "Dispersal"
levels(secondary_pa_scores$Filter)[levels(secondary_pa_scores$Filter)==
                                   "Ecological"] <- "Biotic"
fil = as.character(unique(secondary_pa_scores$Filter))
for(i in 1:length(fil)) {
  temp = fil[i]
  df = secondary_pa_scores[
    secondary_pa_scores$Filter == temp, 
  ][chull(secondary_pa_scores[
    secondary_pa_scores$Filter == temp,
    c("NMDS1", "NMDS2")
  ]), ]
  assign(paste0('grp_sec_pa_fil_',temp), df)
}
hull_sec_pa_fil = rbind(grp_sec_pa_fil_Abiotic, grp_sec_pa_fil_Biotic,
                    grp_sec_pa_fil_Dispersal, grp_sec_pa_fil_Trophic)

#global change category
levels(secondary_pa_scores$GlobalChange)[levels(secondary_pa_scores$GlobalChange)==
                                         0] <- "Not Assessed"
gc = as.character(unique(secondary_pa_scores$GlobalChange))
for(i in 1:length(gc)) {
  temp = gc[i]
  df = secondary_pa_scores[
    secondary_pa_scores$GlobalChange == temp, 
  ][chull(secondary_pa_scores[
    secondary_pa_scores$GlobalChange == temp,
    c("NMDS1", "NMDS2")
  ]), ]
  assign(paste0('grp_sec_pa_gc_',temp), df)
}
hull_sec_pa_gc = rbind(`grp_sec_pa_gc_Climate Change`, grp_sec_pa_gc_Exploitation,
                   `grp_sec_pa_gc_Global Change Broad`, 
                   `grp_sec_pa_gc_Global Change Multiple`,
                   `grp_sec_pa_gc_Habitat Degredation`, grp_sec_pa_gc_Invasion,
                   `grp_sec_pa_gc_Not Assessed`)

#predictive category
pred = as.character(unique(secondary_pa_scores$PredictiveCat))
for(i in 1:length(pred)) {
  temp = pred[i]
  df = secondary_pa_scores[
    secondary_pa_scores$PredictiveCat == temp, 
  ][chull(secondary_pa_scores[
    secondary_pa_scores$PredictiveCat == temp,
    c("NMDS1", "NMDS2")
  ]), ]
  assign(paste0('grp_sec_pa_pred_',temp), df)
}
hull_sec_pa_pred = rbind(grp_sec_pa_pred_no, grp_sec_pa_pred_yes)

########### Make Plots

#ecosystem
secondary_pa_eco_plot <- ggplot() + 
  geom_polygon(data=hull_sec_pa_ecosystem, 
               aes(x=NMDS1,y=NMDS2, 
                   fill=Ecosystem, 
                   group=Ecosystem),alpha=0.30) + 
  geom_point(data=secondary_pa_scores, 
             aes(x=NMDS1,y=NMDS2, colour = Ecosystem), size=2) + 
  coord_equal() +
  theme_bw()  +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),  
        axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(), 
        plot.background = element_blank()) +
  scale_fill_viridis(option = 'magma', discrete = TRUE, begin = 0.8, end = 0.2, 
                     name = "Ecosystem") +
  scale_colour_viridis(option = 'magma',discrete = TRUE, begin = 0.8, end = 0.2, 
                       name = "Ecosystem") 
ggsave(here('./Figures/secondary_pa/secondary_pa_plot_eco_small.png'), 
       plot = secondary_pa_eco_plot, 
       width = 8, height = 8, dpi = 200)
ggsave(here('./Figures/secondary_pa/secondary_pa_plot_eco_large.png'), 
       plot = secondary_pa_eco_plot, 
       width = 8, height = 8, dpi = 1200)

#tax group
secondary_pa_tax_plot <- ggplot() + 
  geom_polygon(data=hull_sec_pa_taxonomic_group, 
               aes(x=NMDS1,y=NMDS2, 
                   fill=TaxonomicGroup, 
                   group=TaxonomicGroup),alpha=0.30) + 
  geom_point(data=secondary_pa_scores, 
             aes(x=NMDS1,y=NMDS2, colour = TaxonomicGroup), size=2) + 
  coord_equal() +
  theme_bw()  +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),  
        axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(), 
        plot.background = element_blank()) +
  scale_fill_viridis(option = 'magma', discrete = TRUE, begin = 0.8, end = 0.2, 
                     name = "Taxa") +
  scale_colour_viridis(option = 'magma',discrete = TRUE, begin = 0.8, end = 0.2, 
                       name = "Taxa")
ggsave(here('./Figures/secondary_pa/secondary_pa_plot_tax_small.png'), 
       plot = secondary_pa_tax_plot, 
       width = 8, height = 8, dpi = 200)
ggsave(here('./Figures/secondary_pa/secondary_pa_plot_tax_large.png'), 
       plot = secondary_pa_tax_plot, 
       width = 8, height = 8, dpi = 1200)

#TOS
secondary_pa_tos_plot <- ggplot() + 
  geom_polygon(data=hull_sec_pa_tos, 
               aes(x=NMDS1,y=NMDS2, 
                   fill=TOS, 
                   group=TOS),alpha=0.30) + 
  geom_point(data=secondary_pa_scores, 
             aes(x=NMDS1,y=NMDS2, colour = TOS), size=2) + 
  coord_equal() +
  theme_bw()  +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),  
        axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(), 
        plot.background = element_blank()) +
  scale_fill_viridis(option = 'magma', discrete = TRUE, begin = 0.8, end = 0.2, 
                     name = "Study Type") +
  scale_colour_viridis(option = 'magma',discrete = TRUE, begin = 0.8, end = 0.2, 
                       name = "Study Type")
ggsave(here('./Figures/secondary_pa/secondary_pa_plot_tos_small.png'), 
       plot = secondary_pa_tos_plot, 
       width = 8, height = 8, dpi = 200)
ggsave(here('./Figures/secondary_pa/secondary_pa_plot_tos_large.png'), 
       plot = secondary_pa_tos_plot, 
       width = 8, height = 8, dpi = 1200)

#filter
secondary_pa_fil_plot <- ggplot() + 
  geom_polygon(data=hull_sec_pa_fil, 
               aes(x=NMDS1,y=NMDS2, 
                   fill=Filter, 
                   group=Filter),alpha=0.30) + 
  geom_point(data=secondary_pa_scores, 
             aes(x=NMDS1,y=NMDS2, colour = Filter), size=2) + 
  coord_equal() +
  theme_bw()  +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),  
        axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(), 
        plot.background = element_blank()) +
  scale_fill_viridis(option = 'magma', discrete = TRUE, begin = 0.8, end = 0.2, 
                     name = "Filter") +
  scale_colour_viridis(option = 'magma',discrete = TRUE, begin = 0.8, end = 0.2, 
                       name = "Filter")
ggsave(here('./Figures/secondary_pa/secondary_pa_plot_fil_small.png'), 
       plot = secondary_pa_fil_plot, 
       width = 8, height = 8, dpi = 200)
ggsave(here('./Figures/secondary_pa/secondary_pa_plot_fil_large.png'), 
       plot = secondary_pa_fil_plot, 
       width = 8, height = 8, dpi = 1200)

#global change
secondary_pa_gc_plot <- ggplot() + 
  geom_polygon(data=hull_sec_pa_gc, 
               aes(x=NMDS1,y=NMDS2, 
                   fill=GlobalChange, 
                   group=GlobalChange), alpha=0.30) + 
  geom_point(data=secondary_pa_scores, 
             aes(x=NMDS1,y=NMDS2, colour = GlobalChange), size=2) + 
  coord_equal() +
  theme_bw()  +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),  
        axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(), 
        plot.background = element_blank()) +
  scale_fill_viridis(option = 'magma', discrete = TRUE, begin = 0.8, end = 0.2, 
                     name = "Global Change Driver") +
  scale_colour_viridis(option = 'magma',discrete = TRUE, begin = 0.8, end = 0.2, 
                       name = "Global Change Driver")
ggsave(here('./Figures/secondary_pa/secondary_pa_plot_gc_small.png'), 
       plot = secondary_pa_gc_plot, 
       width = 8, height = 8, dpi = 200)
ggsave(here('./Figures/secondary_pa/secondary_pa_plot_gc_large.png'), 
       plot = secondary_pa_gc_plot, 
       width = 8, height = 8, dpi = 1200)

#predictive
secondary_pa_pred_plot <- ggplot() + 
  geom_polygon(data=hull_sec_pa_pred, 
               aes(x=NMDS1,y=NMDS2, 
                   fill=PredictiveCat, 
                   group=PredictiveCat), alpha=0.30) + 
  geom_point(data=secondary_pa_scores, 
             aes(x=NMDS1,y=NMDS2, colour = PredictiveCat), size=2) + 
  coord_equal() +
  theme_bw()  +
  theme(axis.text.x = element_blank(),  
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),  
        axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(), 
        plot.background = element_blank()) +
  scale_fill_viridis(option = 'magma', discrete = TRUE, begin = 0.8, end = 0.2, 
                     name = "Predictive") +
  scale_colour_viridis(option = 'magma',discrete = TRUE, begin = 0.8, end = 0.2, 
                       name = "Predictive")
ggsave(here('./Figures/secondary_pa/secondary_pa_plot_pred_small.png'), 
       plot = secondary_pa_pred_plot, 
       width = 8, height = 9, dpi = 200)
ggsave(here('./Figures/secondary_pa/secondary_pa_plot_pred_large.png'), 
       plot = secondary_pa_pred_plot, 
       width = 8, height = 9, dpi = 1200)


## Merge plots

## We need to save the ordinations so I don't have to rerun them for this task

#All 6 panels

review_nMDS_secondary_pa6 = 
  plot_grid(secondary_pa_gc_plot, secondary_pa_fil_plot, secondary_pa_pred_plot, 
            secondary_pa_tos_plot, secondary_pa_eco_plot, secondary_pa_tax_plot, 
            labels = c("A", "B", "C", "D", "E", "F"), 
            label_x = 0.12, vjust = 3.5, ncol = 2, 
            rel_widths = c(1, 1), align = "v")

ggsave(here('./Figures/secondary_pa/review_nMDS_secondary_pa6_small.png'), 
       plot = review_nMDS_secondary_pa6,
       width = 15, height = 12, dpi = 200)
ggsave(here('./Figures/secondary_pa/review_nMDS_secondary_pa6_large.png'), 
       plot = review_nMDS_secondary_pa6,
       width = 15, height = 12, dpi = 1200)



#Alternative panel arrangements
#All 4 panels, grey background
#review_nMDS_4 = plot_grid(trait_nMDS_clim, trait_nMDS_filt, trait_nMDS_tos, trait_nMDS_ecos, labels = c("A", "B", "C", "D"), label_x = 0.10, vjust = 3.1, ncol = 2, rel_widths = c(1, 1),  align = "v")
#review_nMDS_4
#ggsave("review_nMDS_4whitelab.jpeg", plot = review_nMDS_4, width = 12, height = 7, dpi = 300)

```




