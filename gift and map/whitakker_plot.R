source("D:/gitrepo/SBG_eco_taxo/src/preparation data/format_data.R")



#devtools::install_github("valentinitnelav/plotbiomes")

library(plotbiomes)
library(dplyr)
library(vegan)
library(plotbiomes)
library(ggplot2)
##################################
####      Whitakker      ######
##################################



whit_part1 <- read.csv("D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/data_env_gift_complet.csv",sep=",")
whit_part2 <- read.csv("D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/data_env_restant.csv",sep=";")


whit_part1 <- whit_part1 %>%
  dplyr::select(species, where(is.numeric))

whit_part1 <- whit_part1 %>%
  group_by(species) %>%
  summarise_all(mean, na.rm = TRUE)


whit_part2 <- whit_part2[!duplicated(whit_part2$species), ]


data_env_select <- whit_part1[, c(1,5,6)]
mean_df_select <- whit_part2[, c(1,4, 5)]
colnames(data_env_select) <- c("species","temperature", "precipitation")
colnames(mean_df_select) <- c("species", "temperature", "precipitation")


mean_df_select$temperature <- mean_df_select$temperature / 10

data_clim <- rbind(mean_df_select, data_env_select)  

data_clim$precipitation <- as.numeric(data_clim$precipitation)
data_clim$temperature <- as.numeric(data_clim$temperature)
data_clim$species <- as.factor(data_clim$species)
data_clim$precipitation <- data_clim$precipitation / 10




# Sélectionner les colonnes 'genre' et 'espece' et les fusionner dans un nouveau dataframe
cover_species_garden_full <- read.csv("D:/gitrepo/SBG_eco_taxo/data/cover_species_garden_500.csv")


# Parcourir chaque valeur unique dans genus de cover_species_garden_full
unique_species <- unique(cover_species_garden_full$species)

for (species in unique_species) {
  # Sélectionner les lignes correspondantes dans select_taxo
  select_taxo <- cover_species_garden_full[cover_species_garden_full$species == species, ]
  
  # Extraire les valeurs uniques dans la colonne garden
  unique_gardens <- unique(select_taxo$garden)
  
  # Trier les valeurs uniques par ordre alphabétique et les combiner en une chaîne
  sorted_gardens <- sort(unique_gardens)
  code_garden <- paste(sorted_gardens, collapse = "_")
  
  # Mettre cette chaîne dans cover_species_garden_full$code_garden pour le species en cours
  cover_species_garden_full$code_garden[cover_species_garden_full$species == species] <- code_garden
}

cover_species_garden_full <- cover_species_garden_full %>%
  dplyr::distinct(species, .keep_all = TRUE)


setdiff(data_clim$species,cover_species_garden_full$species)

data_clim <- merge(data_clim, cover_species_garden_full, by = "species")
data_clim$jardin <- as.factor(data_clim$code_garden)


data_clim <- data_clim %>%
  dplyr::mutate(temperature = ifelse(species %in% c("Drosera spathulata", "Duvalia modesta"), 25, temperature))


summary(data_clim)




# Votre code ggplot avec l'ajout de scale_color_manual()
plot <- whittaker_base_plot() +
  geom_point(data = data_clim, 
             aes(x = temperature, 
                 y = precipitation,
                 color = code_garden),  
             size   = 1,             
             shape  = 16,
             alpha  = 0.8) +
  scale_color_manual(
    name = "Family",
    values = c(
      "fr_ne"    = "orange", 
      "fr_la"    = "darkgreen", 
      "fr_la_ne" = "darkblue", 
      "fr"       = "purple", 
      "ne"       = "red", 
      "la"       = "brown",
      "la_ne"    = "pink",
      "NA" = "grey"
    ),
    labels = c(
      "fr_ne"    = "Available in Fribourg and Neuchâtel", 
      "fr_la"    = "Available in Fribourg and Lausanne", 
      "fr_la_ne" = "Available in Fribourg, Lausanne, and Neuchâtel", 
      "fr"       = "Available in Fribourg",
      "ne"       = "Available in Neuchâtel", 
      "la"       = "Available in Lausanne",
      "la_ne"    = "Available in Lausanne and Neuchâtel",
      "NA" = "Not available"
    ),
    breaks = c(
      "fr_ne", 
      "fr_la", 
      "fr_la_ne", 
      "fr", 
      "ne", 
      "la", 
      "la_ne", 
      "NA"
    )
  ) +
  labs(title = "Whitakker plot")
print(plot)



# Enregistrement du graphique
ggsave(filename = "D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/whitakker_plot_bothgarden.png", plot = plot)
