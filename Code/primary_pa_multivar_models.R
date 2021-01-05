########## 
##########
# This code contains the multivariate modeling component of the analysis 
# presented in in Green et al. (2021) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2020-06-27
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


primary_pa = read_csv(here(paste0('./data/processed-data',
                                  '/primary_traits_dummy_pa_models.csv')))

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
primary_pa_sites = data.frame(primary_pa[,1:11])
primary_pa_species = data.frame(primary_pa[,12:ncol(primary_pa)])

# run actual ordination
set.seed(0002)
primary_pa_ord_k4 = metaMDS(primary_pa_species,
                         distance = 'jaccard',
                         trymax = 1000,
                         k = 4)
saveRDS(primary_pa_ord_k4,
        here('./data/nmds-intermediate/primary_pa_ord.rds'))
plot(primary_pa_ord_k4)

#primary_pa_ord_k4 = readRDS(here('./data/nmds-intermediate/primary_pa_ord.rds'))

# Get results from ordination ==================================================
# 
# # extract scores
# primary_pa_k4_scores = data.frame(scores(primary_pa_ord_k4))
# primary_pa_k4_scores$points = rownames(primary_pa_k4_scores)
# primary_pa_scores = cbind(primary_pa_sites, primary_pa_k4_scores)
# 
# # add species
# primary_pa_trait_scores = data.frame(scores(primary_pa_ord_k4, 'species'))
# primary_pa_trait_scores$species = rownames(primary_pa_trait_scores)
# primary_pa_trait_scores$species[6] = 'life history'
# primary_pa_trait_scores$species[8] = 'resource acquisition'
# 
# # Plotting pipeline ============================================================
# 
# primary_pa_scores$Ecosystem =
#   as.factor(primary_pa_scores$Ecosystem)
# primary_pa_scores$GlobalChangeCat =
#   as.factor(primary_pa_scores$GlobalChangeCat)
# primary_pa_scores$Taxonomic =
#   as.factor(primary_pa_scores$Taxonomic)
# primary_pa_scores$TOS =
#   as.factor(primary_pa_scores$TOS)
# primary_pa_scores$Filter =
#   as.factor(primary_pa_scores$Filter)
# primary_pa_scores$GlobalChange =
#   as.factor(primary_pa_scores$GlobalChange)
# primary_pa_scores$PredictiveCat =
#   as.factor(primary_pa_scores$PredictiveCat)
# 
# #ecosystem
# eco = as.character(unique(primary_pa_scores$Ecosystem))
# for(i in 1:length(eco)) {
#   temp = eco[i]
#   df = primary_pa_scores[
#     primary_pa_scores$Ecosystem == temp,
#   ][chull(primary_pa_scores[
#     primary_pa_scores$Ecosystem == temp,
#     c("NMDS1", "NMDS2")
#   ]), ]
#   assign(paste0('grp_pa_eco_',temp), df)
# }
# hull_pa_ecosystem = rbind(grp_pa_eco_Terrestrial, grp_pa_eco_Freshwater,
#                        grp_pa_eco_Marine, grp_pa_eco_Multiple)
# 
# #taxonomic
# tax = as.character(unique(primary_pa_scores$Taxonomic))
# for(i in 1:length(tax)) {
#   temp = tax[i]
#   df = primary_pa_scores[
#     primary_pa_scores$Taxonomic == temp,
#   ][chull(primary_pa_scores[
#     primary_pa_scores$Taxonomic == temp,
#     c("NMDS1", "NMDS2")
#   ]), ]
#   assign(paste0('grp_pa_tax_',temp), df)
# }
# hull_pa_taxonomic = rbind(grp_pa_tax_Birds, grp_pa_tax_Insects,
#                           grp_pa_tax_Mammals, grp_pa_tax_Multiple,
#                           grp_pa_tax_Herpetofauna, grp_pa_tax_Plants,
#                           grp_pa_tax_Plankton, grp_pa_tax_Other,
#                           grp_pa_tax_Fish)
# 
# #taxonomic group
# #first make new classifications
# primary_pa_scores = primary_pa_scores %>%
#   mutate(TaxonomicGroup =
#            ifelse(primary_pa_scores$Taxonomic %in%
#                     c('Mammals', 'Birds', 'Herpetofauna', 'Fish'), 
#                   'Vertebrates',
#            ifelse(primary_pa_scores$Taxonomic %in%
#                     c('Insects', 'Plankton'), 'Invertebrates',
#            ifelse(primary_pa_scores$Taxonomic == 'Other', 'Other',
#            ifelse(primary_pa_scores$Taxonomic == 'Multiple', 'Multiple',
#            ifelse(primary_pa_scores$Taxonomic == 'Plants', 'Plants', NA))))))
# #now put them in the normal loop
# tax_group = as.character(unique(primary_pa_scores$TaxonomicGroup))
# for(i in 1:length(tax_group)) {
#   temp = tax_group[i]
#   df = primary_pa_scores[
#     primary_pa_scores$TaxonomicGroup == temp,
#   ][chull(primary_pa_scores[
#     primary_pa_scores$TaxonomicGroup == temp,
#     c("NMDS1", "NMDS2")
#   ]), ]
#   assign(paste0('grp_pa_taxgroup_',temp), df)
# }
# hull_pa_taxonomic_group = rbind(grp_pa_taxgroup_Invertebrates,
#                                 grp_pa_taxgroup_Multiple,
#                                 grp_pa_taxgroup_Other,
#                                 grp_pa_taxgroup_Plants,
#                                 grp_pa_taxgroup_Vertebrates)
# 
# #TOS
# levels(primary_pa_scores$TOS)[levels(primary_pa_scores$TOS)==
#                                 'TModel']='Theory'
# levels(primary_pa_scores$TOS)[levels(primary_pa_scores$TOS)==
#                                 'QModel']='Observational'
# tos = as.character(unique(primary_pa_scores$TOS))
# for(i in 1:length(tos)) {
#   temp = tos[i]
#   df = primary_pa_scores[
#     primary_pa_scores$TOS == temp,
#   ][chull(primary_pa_scores[
#     primary_pa_scores$TOS == temp,
#     c("NMDS1", "NMDS2")
#   ]), ]
#   assign(paste0('grp_pa_tos_',temp), df)
# }
# hull_pa_tos = rbind(grp_pa_tos_Observational, grp_pa_tos_Experiment,
#                           grp_pa_tos_Metanalysis, grp_pa_tos_Theory,
#                           grp_pa_tos_Review)
# 
# #Filter
# levels(primary_pa_scores$Filter)[levels(primary_pa_scores$Filter)==
#                                   "Fundamental"] = "Abiotic"
# levels(primary_pa_scores$Filter)[levels(primary_pa_scores$Filter)==
#                                   "Physical"] = "Dispersal"
# levels(primary_pa_scores$Filter)[levels(primary_pa_scores$Filter)==
#                                   "Ecological"] = "Biotic"
# fil = as.character(unique(primary_pa_scores$Filter))
# for(i in 1:length(fil)) {
#   temp = fil[i]
#   df = primary_pa_scores[
#     primary_pa_scores$Filter == temp,
#   ][chull(primary_pa_scores[
#     primary_pa_scores$Filter == temp,
#     c("NMDS1", "NMDS2")
#   ]), ]
#   assign(paste0('grp_pa_fil_',temp), df)
# }
# hull_pa_fil = rbind(grp_pa_fil_Abiotic, grp_pa_fil_Biotic,
#                     grp_pa_fil_Dispersal, grp_pa_fil_Trophic)
# 
# #global change category
# levels(primary_pa_scores$GlobalChange)[levels(primary_pa_scores$GlobalChange)==
#                                    0] = "Not Assessed"
# gc = as.character(unique(primary_pa_scores$GlobalChange))
# for(i in 1:length(gc)) {
#   temp = gc[i]
#   df = primary_pa_scores[
#     primary_pa_scores$GlobalChange == temp,
#   ][chull(primary_pa_scores[
#     primary_pa_scores$GlobalChange == temp,
#     c("NMDS1", "NMDS2")
#   ]), ]
#   assign(paste0('grp_pa_gc_',temp), df)
# }
# hull_pa_gc = rbind(`grp_pa_gc_Climate Change`, grp_pa_gc_Exploitation,
#                     `grp_pa_gc_Global Change Broad`,
#                     `grp_pa_gc_Global Change Multiple`,
#                     `grp_pa_gc_Habitat Degredation`, grp_pa_gc_Invasion,
#                     `grp_pa_gc_Not Assessed`)
# 
# #predictive category
# pred = as.character(unique(primary_pa_scores$PredictiveCat))
# for(i in 1:length(pred)) {
#   temp = pred[i]
#   df = primary_pa_scores[
#     primary_pa_scores$PredictiveCat == temp,
#   ][chull(primary_pa_scores[
#     primary_pa_scores$PredictiveCat == temp,
#     c("NMDS1", "NMDS2")
#   ]), ]
#   assign(paste0('grp_pa_pred_',temp), df)
# }
# hull_pa_pred = rbind(grp_pa_pred_no, grp_pa_pred_yes)
# 
# ########### Make Plots
# 
# #ecosystem
# primary_pa_eco_plot = ggplot() +
#   geom_polygon(data=hull_pa_ecosystem,
#                aes(x=NMDS1,y=NMDS2,
#                    fill=Ecosystem,
#                    group=Ecosystem),alpha=0.30) +
#   geom_point(data=primary_pa_scores,
#              aes(x=NMDS1,y=NMDS2, colour = Ecosystem), size=2) +
#   coord_equal() +
#   theme_bw()  +
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         axis.title.x = element_text(size=18),
#         axis.title.y = element_text(size=18),
#         legend.title = element_text(size = 18),
#         legend.text = element_text(size = 18),
#         panel.background = element_rect(fill = "white"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.background = element_blank()) +
#   scale_fill_viridis(option = 'magma', discrete = TRUE, begin = 0.8, end = 0.2,
#                      name = "Ecosystem") +
#   scale_colour_viridis(option = 'magma',discrete = TRUE, begin = 0.8, end = 0.2,
#                        name = "Ecosystem") +
#   geom_label(data = primary_pa_trait_scores,
#                   aes(x =  NMDS1,
#                       y = NMDS2,
#                       label = species),
#                   alpha = 0.8,
#                   size = 3,
#              position = position_jitter(width = 0, height = 0.1, seed = 7))
# ggsave(here('./Output-Figs/Cole-nMDS/primary_pa_plot_eco_small.png'),
#        plot = primary_pa_eco_plot,
#        width = 8, height = 8, dpi = 200)
# ggsave(here('./Output-Figs/Cole-nMDS/primary_pa_plot_eco_large.png'),
#        plot = primary_pa_eco_plot,
#        width = 8, height = 8, dpi = 1200)
# 
# #tax group
# primary_pa_tax_plot = ggplot() +
#   geom_polygon(data=hull_pa_taxonomic_group,
#                aes(x=NMDS1,y=NMDS2,
#                    fill=TaxonomicGroup,
#                    group=TaxonomicGroup),alpha=0.30) +
#   geom_point(data=primary_pa_scores,
#              aes(x=NMDS1,y=NMDS2, colour = TaxonomicGroup), size=2) +
#   coord_equal() +
#   theme_bw()  +
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         axis.title.x = element_text(size=18),
#         axis.title.y = element_text(size=18),
#         legend.title = element_text(size = 18),
#         legend.text = element_text(size = 18),
#         panel.background = element_rect(fill = "white"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.background = element_blank()) +
#   scale_fill_viridis(option = 'magma', discrete = TRUE, begin = 0.8, end = 0.2,
#                      name = "Taxa") +
#   scale_colour_viridis(option = 'magma',discrete = TRUE, begin = 0.8, end = 0.2,
#                        name = "Taxa")+
#   geom_label(data = primary_pa_trait_scores,
#              aes(x =  NMDS1,
#                  y = NMDS2,
#                  label = species),
#              alpha = 0.8,
#              size = 3,
#              position = position_jitter(width = 0, height = 0.1, seed = 7))
# ggsave(here('./Output-Figs/Cole-nMDS/primary_pa_plot_tax_small.png'),
#        plot = primary_pa_tax_plot,
#        width = 8, height = 8, dpi = 200)
# ggsave(here('./Output-Figs/Cole-nMDS/primary_pa_plot_tax_large.png'),
#        plot = primary_pa_tax_plot,
#        width = 8, height = 8, dpi = 1200)
# 
# #TOS
# primary_pa_tos_plot = ggplot() +
#   geom_polygon(data=hull_pa_tos,
#                aes(x=NMDS1,y=NMDS2,
#                    fill=TOS,
#                    group=TOS),alpha=0.30) +
#   geom_point(data=primary_pa_scores,
#              aes(x=NMDS1,y=NMDS2, colour = TOS), size=2) +
#   coord_equal() +
#   theme_bw()  +
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         axis.title.x = element_text(size=18),
#         axis.title.y = element_text(size=18),
#         legend.title = element_text(size = 18),
#         legend.text = element_text(size = 18),
#         panel.background = element_rect(fill = "white"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.background = element_blank()) +
#   scale_fill_viridis(option = 'magma', discrete = TRUE, begin = 0.8, end = 0.2,
#                      name = "Study Type") +
#   scale_colour_viridis(option = 'magma',discrete = TRUE, begin = 0.8, end = 0.2,
#                        name = "Study Type")+
#   geom_label(data = primary_pa_trait_scores,
#              aes(x =  NMDS1,
#                  y = NMDS2,
#                  label = species),
#              alpha = 0.8,
#              size = 3,
#              position = position_jitter(width = 0, height = 0.1, seed = 7))
# ggsave(here('./Output-Figs/Cole-nMDS/primary_pa_plot_tos_small.png'),
#        plot = primary_pa_tos_plot,
#        width = 8, height = 8, dpi = 200)
# ggsave(here('./Output-Figs/Cole-nMDS/primary_pa_plot_tos_large.png'),
#        plot = primary_pa_tos_plot,
#        width = 8, height = 8, dpi = 1200)
# 
# #filter
# primary_pa_fil_plot = ggplot() +
#   geom_polygon(data=hull_pa_fil,
#                aes(x=NMDS1,y=NMDS2,
#                    fill=Filter,
#                    group=Filter),alpha=0.30) +
#   geom_point(data=primary_pa_scores,
#              aes(x=NMDS1,y=NMDS2, colour = Filter), size=2) +
#   coord_equal() +
#   theme_bw()  +
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         axis.title.x = element_text(size=18),
#         axis.title.y = element_text(size=18),
#         legend.title = element_text(size = 18),
#         legend.text = element_text(size = 18),
#         panel.background = element_rect(fill = "white"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.background = element_blank()) +
#   scale_fill_viridis(option = 'magma', discrete = TRUE, begin = 0.8, end = 0.2,
#                      name = "Filter") +
#   scale_colour_viridis(option = 'magma',discrete = TRUE, begin = 0.8, end = 0.2,
#                        name = "Filter")+
#   geom_label(data = primary_pa_trait_scores,
#              aes(x =  NMDS1,
#                  y = NMDS2,
#                  label = species),
#              alpha = 0.8,
#              size = 3,
#              position = position_jitter(width = 0, height = 0.1, seed = 7))
# ggsave(here('./Output-Figs/Cole-nMDS/primary_pa_plot_fil_small.png'),
#        plot = primary_pa_fil_plot,
#        width = 8, height = 8, dpi = 200)
# ggsave(here('./Output-Figs/Cole-nMDS/primary_pa_plot_fil_large.png'),
#        plot = primary_pa_fil_plot,
#        width = 8, height = 8, dpi = 1200)
# 
# #global change
# primary_pa_gc_plot = ggplot() +
#   geom_polygon(data=hull_pa_gc,
#                aes(x=NMDS1,y=NMDS2,
#                    fill=GlobalChange,
#                    group=GlobalChange), alpha=0.30) +
#   geom_point(data=primary_pa_scores,
#              aes(x=NMDS1,y=NMDS2, colour = GlobalChange), size=2) +
#   coord_equal() +
#   theme_bw()  +
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         axis.title.x = element_text(size=18),
#         axis.title.y = element_text(size=18),
#         legend.title = element_text(size = 18),
#         legend.text = element_text(size = 18),
#         panel.background = element_rect(fill = "white"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.background = element_blank()) +
#   scale_fill_viridis(option = 'magma', discrete = TRUE, begin = 0.8, end = 0.2,
#                      name = "Global Change Driver") +
#   scale_colour_viridis(option = 'magma',discrete = TRUE, begin = 0.8, end = 0.2,
#                        name = "Global Change Driver")+
#   geom_label(data = primary_pa_trait_scores,
#              aes(x =  NMDS1,
#                  y = NMDS2,
#                  label = species),
#              alpha = 0.8,
#              size = 3,
#              position = position_jitter(width = 0, height = 0.1, seed = 7))
# ggsave(here('./Output-Figs/Cole-nMDS/primary_pa_plot_gc_small.png'),
#        plot = primary_pa_gc_plot,
#        width = 8, height = 8, dpi = 200)
# ggsave(here('./Output-Figs/Cole-nMDS/primary_pa_plot_gc_large.png'),
#        plot = primary_pa_gc_plot,
#        width = 8, height = 8, dpi = 1200)
# 
# #predictive
# primary_pa_pred_plot = ggplot() +
#   geom_polygon(data=hull_pa_pred,
#                aes(x=NMDS1,y=NMDS2,
#                    fill=PredictiveCat,
#                    group=PredictiveCat), alpha=0.30) +
#   geom_point(data=primary_pa_scores,
#              aes(x=NMDS1,y=NMDS2, colour = PredictiveCat), size=2) +
#   coord_equal() +
#   theme_bw()  +
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         axis.title.x = element_text(size=18),
#         axis.title.y = element_text(size=18),
#         legend.title = element_text(size = 18),
#         legend.text = element_text(size = 18),
#         panel.background = element_rect(fill = "white"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.background = element_blank()) +
#   scale_fill_viridis(option = 'magma', discrete = TRUE, begin = 0.8, end = 0.2,
#                      name = "Predictive") +
#   scale_colour_viridis(option = 'magma',discrete = TRUE, begin = 0.8, end = 0.2,
#                        name = "Predictive")+
#   geom_label(data = primary_pa_trait_scores,
#              aes(x =  NMDS1,
#                  y = NMDS2,
#                  label = species),
#              alpha = 0.8,
#              size = 3,
#              position = position_jitter(width = 0, height = 0.1, seed = 7))
# ggsave(here('./Output-Figs/Cole-nMDS/primary_pa_plot_pred_small.png'),
#        plot = primary_pa_pred_plot,
#        width = 8, height = 9, dpi = 200)
# ggsave(here('./Output-Figs/Cole-nMDS/primary_pa_plot_pred_large.png'),
#        plot = primary_pa_pred_plot,
#        width = 8, height = 9, dpi = 1200)
# 
# 
# 
# 
# 
# 
