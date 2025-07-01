

######################################################
##########              GIFT           ###############
######################################################
library(GIFT)
library(dplyr)



misc_env <- GIFT::GIFT_env_meta_misc()
raster_env <- GIFT::GIFT_env_meta_raster()


env_bota <- read.csv("D:/gitrepo/SBG_eco_taxo/data/cover_species_garden.csv")


# Trouver les espèces présentes dans plus d’un jardin
species_multiple_gardens <- env_bota %>%
  group_by(species) %>%
  summarise(nb_gardens = n_distinct(garden)) %>%
  filter(nb_gardens > 1) %>%
  pull(species)

# Filtrer les espèces du jardin "ch" qui sont uniques à ce jardin
env_bota <- env_bota %>%
  filter(garden == "ch", !species %in% species_multiple_gardens) %>%
  select(species, genus)





env_bota <- env_bota[!duplicated(env_bota),]

env_bota <- env_bota %>% filter(species != ""& genus != "")


env_bota <- env_bota %>% dplyr::mutate(epithet = sapply(strsplit(as.character(species), " "), function(x) x[2]))
variable_list <- c("wc2.0_bio_30s_01", "wc2.0_bio_30s_12")
env_bota <- subset(env_bota, !is.na(genus) & !is.na(epithet))

# Supprimer les epithet contenant des caractères suspects
env_bota <- env_bota %>%
  filter(!grepl("[^A-Za-zÀ-ÿ -]", epithet))  # garde seulement les caractères valides

# Initialisation de la barre de progression
total_iterations <- nrow(env_bota)
progress <- 0

# Initialisation de la liste pour stocker les résultats agrégés
df_loca <- data.frame(stringsAsFactors = FALSE)


# Initialisation
total_iterations <- nrow(env_bota)
progress <- 0
df_loca <- data.frame(stringsAsFactors = FALSE)

# Boucle sur chaque ligne
for (i in 1:total_iterations) {
  current_genus <- env_bota$genus[i]
  current_epithet <- env_bota$epithet[i]

  # TRY : encapsule tout le bloc potentiellement à risque
  tryCatch({
    current_lookup <- GIFT::GIFT_species_lookup(genus = current_genus, epithet = current_epithet)

    current_distribution <- GIFT_species_distribution(
      genus = current_genus,
      epithet = current_epithet,
      aggregation = TRUE
    )

    current_distribution <- current_distribution[current_distribution$native == 1, ]
    current_distribution <- current_distribution[!is.na(current_distribution$entity_ID), ]

    # Limiter à 50 occurrences
    if (nrow(current_distribution) > 50) {
      current_distribution <- current_distribution[sample(1:nrow(current_distribution), 50), ]
    }

    current_data_env <- GIFT::GIFT_env(
      entity_ID = current_distribution$entity_ID,
      miscellaneous = c("longitude", "latitude", "biome"),
      rasterlayer = c("wc2.0_bio_30s_01", "wc2.0_bio_30s_12"),
      sumstat = c("mean")
    )

    if (nrow(current_data_env) != 0) {
      current_data_env$species <- paste(current_genus, current_epithet, sep = " ")
      df_loca <- rbind(df_loca, current_data_env)
    }
  }, error = function(e) {
    cat(sprintf("\n Erreur avec la ligne %d : %s %s. Ignorée.\n", i, current_genus, current_epithet))
  })

  progress <- progress + 1
  cat(sprintf("\rProgression: %d%%", round(progress / total_iterations * 100)))
}


df_loca_sauv <- df_loca

# Enregistrer le dataframe en tant que fichier CSV
write.csv(df_loca, "D:/gitrepo/SBG_eco_taxo/data/gift/data_env_gift_champex.csv", row.names = FALSE)
