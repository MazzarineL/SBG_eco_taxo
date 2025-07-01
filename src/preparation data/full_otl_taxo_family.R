library(readr)
library(archive)
library(dplyr)
library(igraph)
library(rotl)
library(plyr)
library(OpenTreeChronograms)

# File can bve directly downloaded
  # download.file('http://files.opentreeoflife.org/ott/ott3.3/ott3.3.tgz', method = "wget", extra = "-r -p --random-wait")

  # and opened from the tar archive

  taxonomy <-
    read_delim(
      archive_read("D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/otl/ott3.3.tgz", file = "ott3.3/taxonomy.tsv"),
      col_types = cols(),
      delim = "|",
      escape_double = FALSE,
      trim_ws = TRUE
    )

  taxonomy %>%
    filter(rank == "domain")

  # taxonomy %>%
  #  filter(name =="Asparagales")

  # Here we use I graph to create a graph from the datatable of OTL tax input

  sub_tax_from_to <- data.frame(
    from = taxonomy$parent_uid,
    to = taxonomy$uid
  )

  g <- graph.data.frame(sub_tax_from_to, directed = TRUE)

  # plot(g)

  # print_all(g)

  # And the subcomponent to filter for all node below Archaeplastida https://igraph.org/r/doc/subcomponent.html

  sub_g <-
    subcomponent(g, "10210", mode = "out") ## tracheophyta ott id

  g2 <- induced_subgraph(g, sub_g)

  id_sel <- as.numeric(V(g2)$name)

  taxonomy_final <- taxonomy[taxonomy$uid %in% id_sel, ]
  
  taxonomy_family <- taxonomy_final %>%
  filter(rank %in% c("species", "genus", "family") & is.na(flags))








# Étape 1 : Créer taxonomy_family_full avec species et NA pour genus et family
taxonomy_family_full <- taxonomy_family %>%
  filter(rank == "species") %>%
  select(species = name) %>%
  mutate(genus = NA, family = NA)

# Initialiser la barre de progression
total <- nrow(taxonomy_family_full)
pb <- txtProgressBar(min = 0, max = total, style = 3)

# Étape 2 : Remplir genus et family pour chaque espèce
for (i in 1:total) {
  species_name <- taxonomy_family_full$species[i]
  
  # Trouver l'index de l'espèce dans taxonomy_family
  species_index <- which(taxonomy_family$name == species_name & taxonomy_family$rank == "species")
  
  # Initialiser les variables pour stocker genus et family
  genus_name <- NA
  family_name <- NA
  
  # Étape 3 : Remonter pour trouver genus et family
  while (species_index > 1) {
    species_index <- species_index - 1
    if (is.na(genus_name) && taxonomy_family$rank[species_index] == "genus") {
      genus_name <- taxonomy_family$name[species_index]
    }
    if (is.na(family_name) && taxonomy_family$rank[species_index] == "family") {
      family_name <- taxonomy_family$name[species_index]
    }
    if (!is.na(genus_name) && !is.na(family_name)) {
      break
    }
  }
  
  # Assigner genus et family au dataframe
  taxonomy_family_full$genus[i] <- genus_name
  taxonomy_family_full$family[i] <- family_name
  
  # Mettre à jour la barre de progression
  setTxtProgressBar(pb, i)
}

# Fermer la barre de progression
close(pb)







 

fix(taxonomy_family_full)

write.csv(taxonomy_family_full, "D:/gitrepo/SBG_eco_taxo/data/all_species_taxonomy_full.csv", row.names = FALSE )
