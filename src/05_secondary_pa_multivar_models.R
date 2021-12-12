########## 
##########
# This code contains the multivariate modeling component of the analysis presented in
# in Green et al. (2021) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2020-06-30
##########
##########

# set-up =======================================================================

library(tidyverse)
library(vegan)
library(viridis)
library(mvabund)
library(here)

secondary_pa = read_csv(here(paste0('./data/processed-data',
                                    '/secondary_traits_dummy_pa_models.csv')))

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
# multivariate context
secondary_pa_sites = data.frame(secondary_pa[,1:11])
secondary_pa_species = data.frame(secondary_pa[,12:ncol(secondary_pa)])

#run the ordination
# set.seed(0002)
# secondary_pa_ord_k4 = metaMDS(secondary_pa_species,
#                             distance = 'jaccard',
#                             trymax = 1000,
#                             k = 4)
# saveRDS(secondary_pa_ord_k4,
#         here('./data/nmds-intermediate/secondary_abundance_ord.rds'))
secondary_pa_ord_k4 = 
  readRDS(here('./data/nmds-intermediate/secondary_abundance_ord.rds'))

# Get results from ordination ==================================================

# extract scores
secondary_pa_k4_scores = data.frame(scores(secondary_pa_ord_k4))
secondary_pa_k4_scores$points = rownames(secondary_pa_k4_scores)
secondary_pa_scores = cbind(secondary_pa_sites, secondary_pa_k4_scores)

hist(secondary_pa_scores$NMDS1, breaks = 100)
hist(secondary_pa_scores$NMDS2, breaks = 100)
hist(secondary_pa_scores$NMDS1[which(secondary_pa_scores$NMDS1 < 1 & 
                                       secondary_pa_scores$NMDS1 > -2)], 
     breaks = 100)
hist(secondary_pa_scores$NMDS2[which(secondary_pa_scores$NMDS2 < 1 & 
                                       secondary_pa_scores$NMDS2 > -2)], 
     breaks = 100)

secondary_pa_scores = # this gets rid of 9 rows
  secondary_pa_scores[ 
    which(secondary_pa_scores$NMDS1 < 0 &
            secondary_pa_scores$NMDS1 > -0.3 &
            secondary_pa_scores$NMDS2 > -0.2 &
            secondary_pa_scores$NMDS2 < 0.3),]


# add species
secondary_pa_trait_scores = data.frame(scores(secondary_pa_ord_k4, 'species'))
secondary_pa_trait_scores$species = rownames(secondary_pa_trait_scores)
secondary_pa_trait_scores$species[6] = 'life history'
secondary_pa_trait_scores$species[8] = 'resource acquisition'

# Plotting pipeline ============================================================

secondary_pa_scores$Ecosystem =
  as.factor(secondary_pa_scores$Ecosystem)
secondary_pa_scores$GlobalChangeCat =
  as.factor(secondary_pa_scores$GlobalChangeCat)
secondary_pa_scores$Taxonomic =
  as.factor(secondary_pa_scores$Taxonomic)
secondary_pa_scores$TaxonomicGroup =
  as.factor(secondary_pa_scores$TaxonomicGroup)
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
hull_sec_pa_ecosystem = rbind(grp_sec_pa_eco_Terrestrial,
                              grp_sec_pa_eco_Freshwater,
                              grp_sec_pa_eco_Marine,
                              grp_sec_pa_eco_Multiple)

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
hull_sec_pa_taxonomic_group = rbind(grp_sec_pa_taxgroup_Invertebrate,
                                grp_sec_pa_taxgroup_Multiple,
                                grp_sec_pa_taxgroup_Other,
                                grp_sec_pa_taxgroup_Plants,
                                grp_sec_pa_taxgroup_Vertebrate)

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
                                   "Fundamental"] = "Abiotic"
levels(secondary_pa_scores$Filter)[levels(secondary_pa_scores$Filter)==
                                   "Physical"] = "Dispersal"
levels(secondary_pa_scores$Filter)[levels(secondary_pa_scores$Filter)==
                                   "Ecological"] = "Biotic"
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
levels(secondary_pa_scores$GlobalChange
       )[levels(secondary_pa_scores$GlobalChange)==
                                         0] = "Not Assessed"
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
hull_sec_pa_gc = rbind(`grp_sec_pa_gc_Climate Change`,
                       grp_sec_pa_gc_Exploitation,
                       `grp_sec_pa_gc_Global Change Broad`,
                       `grp_sec_pa_gc_Global Change Multiple`,
                       `grp_sec_pa_gc_Habitat Degredation`,
                       grp_sec_pa_gc_Invasion,
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
secondary_pa_eco_plot = ggplot() +
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
ggsave(here('./figures/secondary-pa/secondary_pa_plot_eco_small.png'),
       plot = secondary_pa_eco_plot,
       width = 8, height = 8, dpi = 200)
ggsave(here('./figures/secondary-pa/secondary_pa_plot_eco_large.png'),
       plot = secondary_pa_eco_plot,
       width = 8, height = 8, dpi = 600)

#tax group
secondary_pa_tax_plot = ggplot() +
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
ggsave(here('./figures/secondary-pa/secondary_pa_plot_tax_small.png'),
       plot = secondary_pa_tax_plot,
       width = 8, height = 8, dpi = 200)
ggsave(here('./figures/secondary-pa/secondary_pa_plot_tax_large.png'),
       plot = secondary_pa_tax_plot,
       width = 8, height = 8, dpi = 600)

#TOS
secondary_pa_tos_plot = ggplot() +
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
ggsave(here('./figures/secondary-pa/secondary_pa_plot_tos_small.png'),
       plot = secondary_pa_tos_plot,
       width = 8, height = 8, dpi = 200)
ggsave(here('./figures/secondary-pa/secondary_pa_plot_tos_large.png'),
       plot = secondary_pa_tos_plot,
       width = 8, height = 8, dpi = 600)

#filter
secondary_pa_fil_plot = ggplot() +
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
ggsave(here('./figures/secondary-pa/secondary_pa_plot_fil_small.png'),
       plot = secondary_pa_fil_plot,
       width = 8, height = 8, dpi = 200)
ggsave(here('./figures/secondary-pa/secondary_pa_plot_fil_large.png'),
       plot = secondary_pa_fil_plot,
       width = 8, height = 8, dpi = 600)

#global change
secondary_pa_gc_plot = ggplot() +
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
ggsave(here('./figures/secondary-pa/secondary_pa_plot_gc_small.png'),
       plot = secondary_pa_gc_plot,
       width = 8, height = 8, dpi = 200)
ggsave(here('./figures/secondary-pa/secondary_pa_plot_gc_large.png'),
       plot = secondary_pa_gc_plot,
       width = 8, height = 8, dpi = 600)

#predictive
secondary_pa_pred_plot = ggplot() +
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
ggsave(here('./figures/secondary-pa/secondary_pa_plot_pred_small.png'),
       plot = secondary_pa_pred_plot,
       width = 8, height = 9, dpi = 200)
ggsave(here('./figures/secondary-pa/secondary_pa_plot_pred_large.png'),
       plot = secondary_pa_pred_plot,
       width = 8, height = 9, dpi = 600)


## Merge plots

#All 6 panels

review_nMDS_secondary_pa6 =
  plot_grid(secondary_pa_gc_plot, secondary_pa_fil_plot, secondary_pa_pred_plot,
            secondary_pa_tos_plot, secondary_pa_eco_plot, secondary_pa_tax_plot,
            labels = c("A", "B", "C", "D", "E", "F"),
            label_x = 0.12, vjust = 3.5, ncol = 2,
            rel_widths = c(1, 1), align = "v")

ggsave(here('./figures/secondary-pa/review_nMDS_secondary_pa6_small.png'),
       plot = review_nMDS_secondary_pa6,
       width = 15, height = 12, dpi = 200)
ggsave(here('./figures/secondary-pa/review_nMDS_secondary_pa6_large.png'),
       plot = review_nMDS_secondary_pa6,
       width = 15, height = 12, dpi = 600)








