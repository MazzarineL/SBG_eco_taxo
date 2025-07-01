library(tibble)
library(ggplot2)
library(dplyr)
library(ggtree)
library(rotl)
library(readr)
library(archive)
library(igraph)
library(myTAI)
library(castor)
library(tidyverse)
library(slider)
library(tidyquant)  


cover_family_garden <- read.csv("D:/gitrepo/SBG_eco_taxo/data/cover_family_garden_500.csv", sep=";")


##########MAJUSCULE
family_test <- cover_family_garden$family[cover_family_garden$total_available != 0]
family_test <- "Rosaceae"
genus_priority <- data.frame() 

for (i in family_test) {
##########

#import de l'arbre

mono_id <- rotl::tnrs_match_names(paste(i))

taxonomy <- readr::read_delim(
      archive::archive_read("D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/otl/ott3.3.tgz", file = "ott3.3/taxonomy.tsv"),
      col_types = cols(),
      delim = "|",
      escape_double = FALSE,
      trim_ws = TRUE)

taxonomy %>%
    dplyr::filter(rank == "domain")

sub_tax_from_to <- data.frame(
    from = taxonomy$parent_uid,
    to = taxonomy$uid )

g <- igraph::graph.data.frame(sub_tax_from_to, directed = TRUE)

sub_g <-igraph::subcomponent(g, paste(mono_id$ott_id), mode = "out") 

g2 <- igraph::induced_subgraph(g, sub_g)

id_sel <- as.numeric(V(g2)$name)

taxonomy_full <- taxonomy[taxonomy$uid %in% id_sel, ]

taxonomy_genus <- taxonomy_full %>% filter(rank == "genus" & is.na(flags))




tree <- rotl::tol_induced_subtree(ott_ids = taxonomy_genus$uid)

tree$tip.label <- gsub("^x_|\\(genus_in_kingdom_Archaeplastida\\)_|_.*", "", tree$tip.label)



#coloration des branches selon presence

cover_genus_garden <- read.csv("D:/gitrepo/SBG_eco_taxo/data/cover_genus_family_garden.csv", sep=";")

cover_genus_select <- cover_genus_garden %>%
  dplyr::filter(family == paste(family_test))
cover_genus_select$genus <- gsub("^x ", "", cover_genus_select$genus)

###obtenir l'ordre sur l'arbre
p <- ggtree(tree) + geom_tiplab() + 
    geom_hilight(node = 12, extendto = 2.5)


df_rangement <- data.frame(genus = get_taxa_name(p))

df_rangement <- merge(df_rangement, cover_genus_select, by = "genus", all.x = TRUE, sort = FALSE)


####moyenne mobile 
df_rangement <- df_rangement %>%
  mutate(date = as.Date('2023-01-01') + row_number() - 1)



df_rangement <- df_rangement %>% 
  mutate( 
    reg_7day = slide_dbl(
      pres, # calculer sur les new_cases
      .f = ~sum(.x, na.rm = T), # la fonction est sum() avec les valeurs manquantes supprimées
      .before = 3,
      .after = 3, 
      .step = 1))


plot_moy <- ggplot(data = df_rangement)+
  geom_line(mapping = aes(x = date, y = indexed_7day), size = 1)



df_rangement <- df_rangement %>%
  mutate(pres = if_else(reg_7day == 0 & pres == 0, 3, pres))

df_rangement_priority <- df_rangement[df_rangement$pres == 3, ]
genus_priority <- rbind(genus_priority, df_rangement_priority)
}
genus_priority <- data.frame(genus_priority)

fix(genus_priority)
