########## 
##########
# This code contains the multivariate modeling component of the analysis presented in
# in Green et al. (2020) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Cole B. Brookson & Natasha Hardy
# DATE OF CREATION: 2020-06-30
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
library(cowplot)


primary_abundance = 
  read_csv(here(paste0('./data/processed-data',
                       '/primary_traits_dummy_abundance_models.csv')))

# Ordination ===================================================================

###### begin NOTE ##############################################################
# The below section has been commented out for simpler use. The ordination takes 
# a good while to run, easily over 2 hours on a normal laptop, so we've saved
# the result as an RDS object, which can be easily read into the environment 
# using the code provided. This will allow the user to inspect the result and
# subsequent plots that are made in this script, without re-running the 
# ordination. Note that due to the nature of the analysis, re-running the actual
# ordination will result in slightly different (not significantly) results each
# time, and thus should be avoided. All statistics about the ordination itself 
# can be accessed via the RDS object after reading it into the environment.
###### end NOTE ################################################################

# split into 'sites' and 'species' just to put it into typical ecological
# # multivariate context
primary_abundance_sites = data.frame(primary_abundance[,1:11])
primary_abundance_species = 
  data.frame(primary_abundance[,12:ncol(primary_abundance)])

# run the ordination
# set.seed(0002)
# primary_abundance_ord_iso = metaMDS(primary_abundance_species,
#                                      #distance = 'bray',
#                                     trymax = 1000,
#                                     k = 3)
# 
# saveRDS(primary_abundance_ord_iso, 
#         here('./data/nmds-intermediate/primary_abundance_ord_iso.rds'))
primary_abundance_ord_iso  =
  readRDS(here('./data/nmds-intermediate/primary_abundance_ord_iso.rds'))
# 
# # Get results from ordination ==================================================

# extract scores
primary_ab_k4_scores = data.frame(scores(primary_abundance_ord_iso))
primary_ab_k4_scores$points = rownames(primary_ab_k4_scores)
primary_ab_scores = cbind(primary_abundance_sites, primary_ab_k4_scores)

# add species
primary_ab_trait_scores = data.frame(scores(primary_abundance_ord_iso,
                                            'species'))
primary_ab_trait_scores$species = rownames(primary_ab_trait_scores)
primary_ab_trait_scores$species[7] = 'life history'
primary_ab_trait_scores$species[10] = 'resource acquisition'

# Plotting pipeline ============================================================

primary_ab_scores$Ecosystem =
  as.factor(primary_ab_scores$Ecosystem)
primary_ab_scores$GlobalChangeCat =
  as.factor(primary_ab_scores$GlobalChangeCat)
primary_ab_scores$Taxonomic =
  as.factor(primary_ab_scores$Taxonomic)
primary_ab_scores$TOS =
  as.factor(primary_ab_scores$TOS)
primary_ab_scores$Filter =
  as.factor(primary_ab_scores$Filter)
primary_ab_scores$GlobalChange =
  as.factor(primary_ab_scores$GlobalChange)
primary_ab_scores$PredictiveCat =
  as.factor(primary_ab_scores$PredictiveCat)

#ecosystem
eco = as.character(unique(primary_ab_scores$Ecosystem))
for(i in 1:length(eco)) {
  temp = eco[i]
  df = primary_ab_scores[
    primary_ab_scores$Ecosystem == temp,
  ][chull(primary_ab_scores[
    primary_ab_scores$Ecosystem == temp,
    c("NMDS1", "NMDS2")
  ]), ]
  assign(paste0('grp_ab_eco_',temp), df)
}
hull_ab_ecosystem = rbind(grp_ab_eco_Terrestrial, grp_ab_eco_Freshwater,
                          grp_ab_eco_Marine, grp_ab_eco_Multiple)

#taxonomic
tax = as.character(unique(primary_ab_scores$Taxonomic))
for(i in 1:length(tax)) {
  temp = tax[i]
  df = primary_ab_scores[
    primary_ab_scores$Taxonomic == temp,
  ][chull(primary_ab_scores[
    primary_ab_scores$Taxonomic == temp,
    c("NMDS1", "NMDS2")
  ]), ]
  assign(paste0('grp_ab_tax_',temp), df)
}
hull_ab_taxonomic = rbind(grp_ab_tax_Birds, grp_ab_tax_Insects,
                          grp_ab_tax_Mammals, grp_ab_tax_Multiple,
                          grp_ab_tax_Herpetofauna, grp_ab_tax_Plants,
                          grp_ab_tax_Plankton, grp_ab_tax_Other,
                          grp_ab_tax_Fish)

# taxonomic group
# first make new classifications
primary_ab_scores = primary_ab_scores %>%
  mutate(TaxonomicGroup =
           ifelse(primary_ab_scores$Taxonomic %in%
                    c('Mammals', 'Birds', 'Herpetofauna', 'Fish'),
                  'Vertebrates',
           ifelse(primary_ab_scores$Taxonomic %in%
                    c('Insects', 'Plankton'),
                  'Invertebrates',
           ifelse(primary_ab_scores$Taxonomic == 'Other',
                  'Other',
           ifelse(primary_ab_scores$Taxonomic == 'Multiple',
                  'Multiple',
           ifelse(primary_ab_scores$Taxonomic == 'Plants',
                  'Plants',
                  NA))))))

# now put them in the normal loop
tax_group = as.character(unique(primary_ab_scores$TaxonomicGroup))
for(i in 1:length(tax_group)) {
  temp = tax_group[i]
  df = primary_ab_scores[
    primary_ab_scores$TaxonomicGroup == temp,
  ][chull(primary_ab_scores[
    primary_ab_scores$TaxonomicGroup == temp,
    c("NMDS1", "NMDS2")
  ]), ]
  assign(paste0('grp_ab_taxgroup_',temp), df)
}
hull_ab_taxonomic_group = rbind(grp_ab_taxgroup_Invertebrates,
                                grp_ab_taxgroup_Multiple,
                                grp_ab_taxgroup_Other,
                                grp_ab_taxgroup_Plants,
                                grp_ab_taxgroup_Vertebrates)

#TOS
levels(primary_ab_scores$TOS)[levels(primary_ab_scores$TOS)==
                                'TModel']='Theory'
levels(primary_ab_scores$TOS)[levels(primary_ab_scores$TOS)==
                                'QModel']='Observational'
tos = as.character(unique(primary_ab_scores$TOS))
for(i in 1:length(tos)) {
  temp = tos[i]
  df = primary_ab_scores[
    primary_ab_scores$TOS == temp,
  ][chull(primary_ab_scores[
    primary_ab_scores$TOS == temp,
    c("NMDS1", "NMDS2")
  ]), ]
  assign(paste0('grp_ab_tos_',temp), df)
}
hull_ab_tos = rbind(grp_ab_tos_Observational, grp_ab_tos_Experiment,
                    grp_ab_tos_Metanalysis, grp_ab_tos_Theory,
                    grp_ab_tos_Review)

#Filter
levels(primary_ab_scores$Filter)[levels(primary_ab_scores$Filter)==
                                   "Fundamental"] = "Abiotic"
levels(primary_ab_scores$Filter)[levels(primary_ab_scores$Filter)==
                                   "Physical"] = "Dispersal"
levels(primary_ab_scores$Filter)[levels(primary_ab_scores$Filter)==
                                   "Ecological"] = "Biotic"
fil = as.character(unique(primary_ab_scores$Filter))
for(i in 1:length(fil)) {
  temp = fil[i]
  df = primary_ab_scores[
    primary_ab_scores$Filter == temp,
  ][chull(primary_ab_scores[
    primary_ab_scores$Filter == temp,
    c("NMDS1", "NMDS2")
  ]), ]
  assign(paste0('grp_ab_fil_',temp), df)
}
hull_ab_fil = rbind(grp_ab_fil_Abiotic, grp_ab_fil_Biotic,
                    grp_ab_fil_Dispersal, grp_ab_fil_Trophic)

#global change category
levels(primary_ab_scores$GlobalChange)[levels(primary_ab_scores$GlobalChange)==
                                         0] = "Not Assessed"
levels(primary_ab_scores$GlobalChange)[levels(primary_ab_scores$GlobalChange)==
                                        'Global Change Broad'] = "Multiple"
levels(primary_ab_scores$GlobalChange)[levels(primary_ab_scores$GlobalChange)==
                                         'Global Change Multiple'] = "Multiple"
gc = as.character(unique(primary_ab_scores$GlobalChange))
for(i in 1:length(gc)) {
  temp = gc[i]
  df = primary_ab_scores[
    primary_ab_scores$GlobalChange == temp,
  ][chull(primary_ab_scores[
    primary_ab_scores$GlobalChange == temp,
    c("NMDS1", "NMDS2")
  ]), ]
  assign(paste0('grp_ab_gc_',temp), df)
}
hull_ab_gc = rbind(`grp_ab_gc_Climate Change`, grp_ab_gc_Exploitation,
                   `grp_ab_gc_Multiple`,
                   `grp_ab_gc_Habitat Degredation`, grp_ab_gc_Invasion,
                   `grp_ab_gc_Not Assessed`)

#predictive category
pred = as.character(unique(primary_ab_scores$PredictiveCat))
for(i in 1:length(pred)) {
  temp = pred[i]
  df = primary_ab_scores[
    primary_ab_scores$PredictiveCat == temp,
  ][chull(primary_ab_scores[
    primary_ab_scores$PredictiveCat == temp,
    c("NMDS1", "NMDS2")
  ]), ]
  assign(paste0('grp_ab_pred_',temp), df)
}
hull_ab_pred = rbind(grp_ab_pred_no, grp_ab_pred_yes)

########### Make Plots

#ecosystem
primary_ab_eco_plot = ggplot() +
  geom_polygon(data=hull_ab_ecosystem,
               aes(x=NMDS1,y=NMDS2,
                   fill=Ecosystem,
                   group=Ecosystem),alpha=0.30) +
  geom_point(data=primary_ab_scores,
             aes(x=NMDS1,y=NMDS2, colour = Ecosystem), size=2) +
  coord_equal(xlim = c(-1.3, 2.5)) +
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
                       name = "Ecosystem") +
  geom_label(data = primary_ab_trait_scores,
             aes(x =  NMDS1,
                 y = NMDS2,
                 label = species),
             alpha = 0.8,
             size = 3,
             position = position_jitter(width = 0.3, height = 0.3, seed = 10))
ggsave(here('./figures/primary-abundance/primary_ab_plot_eco_small.png'),
       plot = primary_ab_eco_plot,
       width = 8, height = 8, dpi = 200)
ggsave(here('./figures/primary-abundance/primary_ab_plot_eco_large.png'),
       plot = primary_ab_eco_plot,
       width = 8, height = 8, dpi = 600)

#tax group
primary_ab_tax_plot = ggplot() +
  geom_polygon(data=hull_ab_taxonomic_group,
               aes(x=NMDS1,y=NMDS2,
                   fill=TaxonomicGroup,
                   group=TaxonomicGroup),alpha=0.30) +
  geom_point(data=primary_ab_scores,
             aes(x=NMDS1,y=NMDS2, colour = TaxonomicGroup), size=2) +
  coord_equal(xlim = c(-1.3, 2.5)) +
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
                       name = "Taxa")+
  geom_label(data = primary_ab_trait_scores,
             aes(x =  NMDS1,
                 y = NMDS2,
                 label = species),
             alpha = 0.8,
             size = 3,
             position = position_jitter(width = 0.3, height = 0.3, seed = 10))
ggsave(here('./figures/primary-abundance/primary_ab_plot_tax_small.png'),
       plot = primary_ab_tax_plot,
       width = 8, height = 8, dpi = 200)
ggsave(here('./figures/primary-abundance/primary_ab_plot_tax_large.png'),
       plot = primary_ab_tax_plot,
       width = 8, height = 8, dpi = 600)

#TOS
primary_ab_tos_plot = ggplot() +
  geom_polygon(data=hull_ab_tos,
               aes(x=NMDS1,y=NMDS2,
                   fill=TOS,
                   group=TOS),alpha=0.30) +
  geom_point(data=primary_ab_scores,
             aes(x=NMDS1,y=NMDS2, colour = TOS), size=2) +
  coord_equal(xlim = c(-1.3, 2.5)) +
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
                       name = "Study Type")+
  geom_label(data = primary_ab_trait_scores,
             aes(x =  NMDS1,
                 y = NMDS2,
                 label = species),
             alpha = 0.8,
             size = 3,
             position = position_jitter(width = 0.3, height = 0.3, seed = 10))
ggsave(here('./figures/primary-abundance/primary_ab_plot_tos_small.png'),
       plot = primary_ab_tos_plot,
       width = 8, height = 8, dpi = 200)
ggsave(here('./figures/primary-abundance/primary_ab_plot_tos_large.png'),
       plot = primary_ab_tos_plot,
       width = 8, height = 8, dpi = 600)

#filter
primary_ab_fil_plot = ggplot() +
  geom_polygon(data=hull_ab_fil,
               aes(x=NMDS1,y=NMDS2,
                   fill=Filter,
                   group=Filter),alpha=0.30) +
  geom_point(data=primary_ab_scores,
             aes(x=NMDS1,y=NMDS2, colour = Filter), size=2) +
  coord_equal(xlim = c(-1.3, 2.5)) +
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
                       name = "Filter")+
  geom_label(data = primary_ab_trait_scores,
             aes(x =  NMDS1,
                 y = NMDS2,
                 label = species),
             alpha = 0.8,
             size = 3,
             position = position_jitter(width = 0.3, height = 0.3, seed = 10))
ggsave(here('./figures/primary-abundance/primary_ab_plot_fil_small.png'),
       plot = primary_ab_fil_plot,
       width = 8, height = 8, dpi = 200)
ggsave(here('./figures/primary-abundance/primary_ab_plot_fil_large.png'),
       plot = primary_ab_fil_plot,
       width = 8, height = 8, dpi = 600)

#global change
primary_ab_gc_plot = ggplot() +
  geom_polygon(data=hull_ab_gc,
               aes(x=NMDS1,y=NMDS2,
                   fill=GlobalChange,
                   group=GlobalChange), alpha=0.30) +
  geom_point(data=primary_ab_scores,
             aes(x=NMDS1,y=NMDS2, colour = GlobalChange), size=2) +
  coord_equal(xlim = c(-1.3, 2.5)) +
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
                       name = "Global Change Driver")+
  geom_label(data = primary_ab_trait_scores,
             aes(x =  NMDS1,
                 y = NMDS2,
                 label = species),
             alpha = 0.8,
             size = 3,
             position = position_jitter(width = 0.3, height = 0.3, seed = 10))
ggsave(here('./figures/primary-abundance/primary_ab_plot_gc_small.png'),
       plot = primary_ab_gc_plot,
       width = 8, height = 8, dpi = 200)
ggsave(here('./figures/primary-abundance/primary_ab_plot_gc_large.png'),
       plot = primary_ab_gc_plot,
       width = 8, height = 8, dpi = 600)

#predictive
primary_ab_pred_plot = ggplot() +
  geom_polygon(data=hull_ab_pred,
               aes(x=NMDS1,y=NMDS2,
                   fill=PredictiveCat,
                   group=PredictiveCat), alpha=0.30) +
  geom_point(data=primary_ab_scores,
             aes(x=NMDS1,y=NMDS2, colour = PredictiveCat), size=2) +
  coord_equal(xlim = c(-1.3, 2.5)) +
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
                       name = "Predictive")+
  geom_label(data = primary_ab_trait_scores,
             aes(x =  NMDS1,
                 y = NMDS2,
                 label = species),
             alpha = 0.8,
             size = 3,
             position = position_jitter(width = 0.3, height = 0.3, seed = 10))
ggsave(here('./figures/primary-abundance/primary_ab_plot_pred_small.png'),
       plot = primary_ab_pred_plot,
       width = 8, height = 9, dpi = 200)
ggsave(here('./figures/primary-abundance/primary_ab_plot_pred_large.png'),
       plot = primary_ab_pred_plot,
       width = 8, height = 9, dpi = 600)


## Merge plots

#All 6 panels

review_nMDS_primary_abun6 =
  plot_grid(primary_ab_tax_plot, primary_ab_tos_plot, primary_ab_eco_plot,
            primary_ab_fil_plot, primary_ab_gc_plot, primary_ab_pred_plot,
            labels = c("A", "B", "C", "D", "E", "F"),
            label_x = 0.15, vjust = 3.5, ncol = 2,
            rel_widths = c(1, 1), align = "v")

ggsave(here('./figures/primary-abundance/review_nMDS_primary_abun6_small.png'),
       plot = review_nMDS_primary_abun6,
       width = 16, height = 12, dpi = 200)
ggsave(here('./figures/primary-abundance/review_nMDS_primary_abun6_large.png'),
       plot = review_nMDS_primary_abun6,
       width = 16, height = 12, dpi = 600)





