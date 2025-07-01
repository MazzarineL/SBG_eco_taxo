source("D:/gitrepo/SBG_eco_taxo/src/plot 500 species/cover_garden_500.R")
#source("D:/gitrepo/SBG_eco_taxo/src/full_otl_taxo_family.R") ###long



library(ggplot2)
library(dplyr)
library(ggtree)
library(rotl)


## TODO libraries are missing but found in the previous functions, fix
## TODO This will be improved
taxonomy_family_full = read.csv("D:/gitrepo/SBG_eco_taxo/data/taxonomy_family_genus_full.csv")

 
taxonomy_merge <-
      left_join(taxonomy_family_full,
        taxonomy_family_Garden,
        by = c("name" = "genus"))



taxonomy_merge$pres[is.na(taxonomy_merge$pres)] <- 0
taxonomy_merge <- taxonomy_merge[!is.na(taxonomy_merge$ott_id.family),]


    my_tree <- rotl::tol_induced_subtree(ott_ids = taxonomy_merge$ott_id.family)

    sp_name <- gsub("_.*", "", my_tree$tip.label)

    my_tree$tip.label <- sp_name

    species <- taxonomy_merge$name.family
    g <- split(species, taxonomy_merge$pres)


tree_plot <- ggtree::ggtree(my_tree, layout = "circular") +
geom_tiplab(size=1.5, offset=2)
g1 <- groupOTU(tree_plot, g, "species") + 
  aes(color = species) +
  theme(
    legend.position = "none"
  ) + 
  new_scale_fill() +
  geom_fruit(
    data = cover_genus_garden_full,
    geom = geom_tile,
    mapping = aes(y = family, x = 3, fill = name.order),  
    pwidth = 0.02,  
    offset = 0.02,  
    axis.params = list(
      axis = "x",
      text.angle = -45,
      hjust = 0
    ),
    grid.params = list()
  ) + 
  geom_tiplab(size = 0, color = NA) +  # Supprime les labels des tips s'ils causent des problèmes
  geom_nodepoint(size = 0, color = NA) +  # Supprime les points aux noeuds de l'arbre
  scale_color_manual(
    values = c("#9fc5e8", "#990000"),
    labels = c("Family not selected", "Family selected")
  )

g1

  
ggsave(filename = "D:/gitrepo/SBG_eco_taxo/inst/coverage family genus plot result/family_tree_BG.png", plot = g1)



