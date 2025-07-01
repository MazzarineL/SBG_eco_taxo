source("D:/gitrepo/SBG_eco_taxo/src/format_data.R")
#source("D:/gitrepo/SBG_eco_taxo/src/full_otl_taxo_family.R") ###long



library(ggplot2)
library(dplyr)
library(ggtree)
library(rotl)

neu_bota_G <- neu_bota %>%
  dplyr::select(genre, garden)

 neu_bota_G <- neu_bota_G %>%
  dplyr::rename(genus = genre) 

fri_tax_G <- fri_tax %>%
  dplyr::select(genus, garden) 

lau_res_G <- lau_res %>%
  dplyr::select(genus, garden) 

gen_res_G <- gen_res %>%
  dplyr::select(genus, garden) 

taxonomy_family_Garden <- rbind(fri_tax_G,neu_bota_G,lau_res_G,gen_res_G)
taxonomy_family_Garden <- taxonomy_family_Garden %>%
  mutate(pres = 1)


## TODO libraries are missing but found in the previous functions, fix
## TODO This will be improved
taxonomy_family_full = read.csv("D:/gitrepo/SBG_eco_taxo/data/taxonomy_family_genus_full.csv")


sub_tax_from_to <- data.frame(
    from = taxonomy$parent_uid,
    to = taxonomy$uid
  )

  g <- graph.data.frame(sub_tax_from_to, directed = TRUE)


  sub_g <-
    subcomponent(g, "10210", mode = "out") ## tracheophyta ott id

  g2 <- induced_subgraph(g, sub_g)

  id_sel <- as.numeric(V(g2)$name)

  taxonomy_final <- taxonomy[taxonomy$uid %in% id_sel, ]

  taxonomy_family <- taxonomy_final %>%
    filter(rank == "genus" & is.na(flags))

  taxonomy_family_full <- get_lineage_taxo(taxonomy_family)

fix(taxonomy_family_full)



taxonomy_family_full = read.csv("D:/gitrepo/SBG_eco_taxo/data/taxonomy_family_genus_full.csv")

unique_values <- unique(taxonomy_family_full$parent_uid)

for (value in unique_values) {
  sub_g <- subcomponent(g, value, mode = "out")
  g2 <- induced_subgraph(g, sub_g)
  id_sel <- as.numeric(V(g2)$name)
  taxonomy_final <- taxonomy[taxonomy$uid %in% id_sel, ]
  taxonomy_family <- taxonomy_final %>% filter(rank == "genus" & is.na(flags))
  taxonomy_family_full <- get_lineage_taxo(taxonomy_family)
  fix(taxonomy_family_full)
}






 
taxonomy_merge <-
      left_join(taxonomy_family_full,
        taxonomy_family_Garden,
        by = c("name" = "genus"))


taxonomy_merge$pres[is.na(taxonomy_merge$pres)] <- 0
taxonomy_merge <- taxonomy_merge[!is.na(taxonomy_merge$parent_uid),]



for (unique(taxonomy_merge$name ))
    my_tree <- tol_induced_subtree(ott_ids = taxonomy_merge$parent_uid)

    sp_name <- gsub("_.*", "", my_tree$tip.label)

    my_tree$tip.label <- sp_name

    species <- taxonomy_merge$name.family
    g <- split(species, taxonomy_merge$pres)


    tree_plot <- ggtree::ggtree(my_tree, layout = "circular") +
 geom_tiplab(size=1.5, offset=0.5)

 g1 <- groupOTU(tree_plot, g, "species") + aes(color = species) +
  theme(legend.position = "right") + scale_color_manual(name = "Family", values = c("orange", "darkgreen"),labels = c("Not available", "Available"))
    ##### add order


#ggsave(filename = "D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/family_tree_BG.png", plot = g1)