source("D:/gitrepo/SBG_eco_taxo/src/preparation data/format_data.R")
#source("D:/gitrepo/SBG_eco_taxo/src/full_otl_taxo_family.R") ###long
taxonomy_family_full <- read.csv("D:/gitrepo/SBG_eco_taxo/data/taxonomy_family_genus_full.csv")

source("D:/gitrepo/SBG_eco_taxo/src/tree plot/plot_SBG_taxo_coverage.R")

library(ggtree)
library(ggtreeExtra)

################################### ne pas lancer
familles_uniques <- unique(taxonomy_merge$name.family)
# Parcourir chaque famille unique
for (famille in familles_uniques) {
  # Obtenir les lignes correspondant à la famille en cours
  lignes_famille <- taxonomy_merge[taxonomy_merge$name.family == famille, ]
  # Si la famille a au moins une valeur avec pres égal à 1
  if (any(lignes_famille$pres == 1)) {
    # Mettre à jour la colonne pres à 1 pour toutes les lignes de la famille en cours
    taxonomy_merge$pres[taxonomy_merge$name.family == famille] <- 1
  }
}
###############################################



# Initialiser une liste pour stocker les résultats
results_list <- list()

# Parcourir chaque famille unique
for (family in familles_uniques) {
  # Filtrer les données pour la famille en cours
  family_data <- subset(taxonomy_merge, family == name.family)
  
  # Compter le nombre de genres uniques
  genres_total <- n_distinct(family_data$name)
  
  # Créer une liste avec les noms uniques dans "name" toutes associées à un 0
  unique_names <- unique(family_data$name)
  association_list <- rep(0, length(unique_names))
  names(association_list) <- unique_names
  
  # Parcourir chaque valeur unique de "name"
  for (i in 1:length(unique_names)) {
    # Vérifier dans family_data à quoi elle est associée dans pres
    associated_value <- family_data$pres[family_data$name == unique_names[i]]
    
    # Si c'est un 1, changer le 0 en 1 dans la liste
    if (any(associated_value == 1)) {
      association_list[unique_names[i]] <- 1
    }
  }
  
  # Extraire le nombre de "name" unique avec un 1 associé
  genres_available <- sum(association_list == 1)
  
  # Stocker les résultats dans la liste
  results_list[[family]] <- c(family, genres_available, genres_total)
}

# Convertir la liste en dataframe
result_dataframe <- do.call(rbind, results_list)
colnames(result_dataframe) <- c("family", "genres_available","genres_total")
result_dataframe <- data.frame(result_dataframe)



result_dataframe$genres_available <- as.numeric(result_dataframe$genres_available)
result_dataframe$genres_total <- as.numeric(result_dataframe$genres_total)

result_dataframe$value <- with(result_dataframe, genres_available / genres_total)
result_dataframe$value[is.na(result_dataframe$value) | is.infinite(result_dataframe$value) | is.nan(result_dataframe$value)] <- 0




cover_family_garden = read.csv("D:/gitrepo/SBG_eco_taxo/data/cover_family_garden_500.csv", sep=";")

cover_family_garden$total <- ifelse(cover_family_garden$total > 150, 150, cover_family_garden$total)

cover_family_garden <- cover_family_garden %>%
  mutate(log_tot = log(total))
cover_family_garden$log_tot <- ifelse(cover_family_garden$log_tot == 0, 0.33, cover_family_garden$log_tot)


cover_family_garden$cover <- ceiling(cover_family_garden$cover)

cover_family_garden <- subset(cover_family_garden, !is.na(cover))



tree_plot <- ggtree::ggtree(my_tree, layout ="circular") +
  theme(legend.position = "right", 
        legend.key.size = unit(3, "lines")) + 
  geom_tiplab(size=2, offset=0.5)

g1 <- groupOTU(tree_plot, g, "species") + aes(color = species) +
 theme(legend.position = "right",
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 18)) + 
  scale_color_manual(name = "Family", values = c("orange", "darkgreen"), labels = c("Not available", "Available"))


g1 <- g1 + ggtreeExtra::geom_fruit(data=result_dataframe,
               geom=geom_bar,
               mapping=aes(y=family, x=value, fill = "Rate of genus available"),
               width=1,
               offset = 0.20,
               stat="identity",
               orientation="y") +
      labs(fill = "Genus") + 
      
      ggtreeExtra::geom_fruit(data=cover_family_garden,
               geom=geom_bar,
               mapping=aes(y=family, x=total, fill = "Total of genus"),
               width=1,
               pwidth=0.38, 
               offset = 0.10,
               stat="identity",
               orientation="y") +
               scale_fill_manual(values = c("darkblue","darkgreen")) +
      labs(fill = "Genus") 
g1


ggsave(filename = "D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/family_tree_BG_genus_tot_nefrila.png", 
       plot = g1, 
       width = 22, 
       height = 22)





#pdf("D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/barplot_family_genus.pdf")
barplot <- barplot(cover_family_garden$total, 
                    names.arg = cover_family_garden$family, 
                    xlab = "Family", 
                    ylab = "Total", 
                    col = "darkblue")
#dev.off()




pdf("D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/barplot_family_genus_ordered.pdf")
cover_family_garden <- cover_family_garden[order(cover_family_garden$total, decreasing = TRUE), ]
# Créer le graphique avec les barres ordonnées du plus grand au plus petit
barplot <- barplot(cover_family_garden$total, 
                    names.arg = cover_family_garden$family, 
                    xlab = "Family", 
                    ylab = "Total", 
                    col = "darkblue")
dev.off()


  taxonomy_merge_genus <- taxonomy_merge[!duplicated(taxonomy_merge$name), ]
 taxonomy_merge_genus <- data.frame( taxonomy_merge_genus)
table(taxonomy_merge_genus$pres)


  taxonomy_merge_family <- taxonomy_merge[!duplicated(taxonomy_merge$name.family), ]
 taxonomy_merge_family <- data.frame( taxonomy_merge_family)
table(taxonomy_merge_family$pres)
