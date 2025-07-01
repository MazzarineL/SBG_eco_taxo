
######################################################
##########              GIFT           ###############
######################################################
library(GIFT)
library(dplyr)



misc_env <- GIFT::GIFT_env_meta_misc()
raster_env <- GIFT::GIFT_env_meta_raster()


env_bota <- read.csv("D:/gitrepo/SBG_eco_taxo/data/cover_species_garden.csv")

env_bota <- env_bota %>% dplyr::filter(garden =="la")
env_bota <- env_bota %>% dplyr::select(species, genus)


#neu_bota_viv <- neu_bota[!is.na(neu_bota$vivant), ]
#env_bota <- data.frame(species = neu_bota_viv$determination_acceptee, genus = neu_bota_viv$genre)

env_bota <- env_bota[!duplicated(env_bota),]

env_bota <- env_bota %>% filter(species != ""& genus != "")


env_bota <- env_bota %>% dplyr::mutate(epithet = sapply(strsplit(as.character(species), " "), function(x) x[2]))
variable_list <- c("wc2.0_bio_30s_01", "wc2.0_bio_30s_12")
env_bota <- subset(env_bota, !is.na(genus) & !is.na(epithet))



# Initialisation de la barre de progression
total_iterations <- nrow(env_bota)
progress <- 0

# Initialisation de la liste pour stocker les résultats agrégés
df_loca <- data.frame(stringsAsFactors = FALSE)


# Boucle à travers chaque ligne de env_bota
for (i in 1:total_iterations) {
  # Extraire les valeurs "genus" et "epithet" de la ligne i
  current_genus <- env_bota$genus[i]
  current_epithet <- env_bota$epithet[i]
  
  # Appeler la fonction GIFT_species_lookup
  current_lookup <- GIFT::GIFT_species_lookup(genus = current_genus, epithet = current_epithet)
  
 # Appeler la fonction GIFT_species_distribution
    current_distribution <- GIFT_species_distribution(
    genus = current_genus,
    epithet = current_epithet,
    aggregation = TRUE)
 ##### filter native 
    current_distribution <- current_distribution[current_distribution$native ==1,]
    current_distribution <- current_distribution[!is.na(current_distribution$entity_ID),]
 ##### limit to 50 occurence per species
    if(nrow(current_distribution) >50){current_distribution <- current_distribution[sample(1:nrow(current_distribution),50),]} 

    current_data_env  <- GIFT::GIFT_env(entity_ID = current_distribution$entity_ID,
                           miscellaneous = c("longitude", "latitude", "biome"),
                           rasterlayer = c("wc2.0_bio_30s_01", "wc2.0_bio_30s_12"),
                           sumstat = c("mean"))

  
 
  # Vérifier si current_data_env est vide
  if(nrow(current_data_env) != 0) {
    current_data_env$species <- paste(current_genus, current_epithet, sep = " ")
    df_loca <- rbind(df_loca, current_data_env)
    
  }
  
  # Mise à jour de la barre de progression
  progress <- progress + 1
  cat(sprintf("\rProgression: %d%%", round(progress / total_iterations * 100)))
}

df_loca_sauv <- df_loca

# Convertir toutes les colonnes en numérique, sauf la colonne 'species'
data_env[, -which(names(data_env) == "species")] <- lapply(data_env[, -which(names(data_env) == "species")], as.numeric)

# Enregistrer le dataframe en tant que fichier CSV
write.csv(df_loca, "D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/data_env_gift_lausanne.csv", row.names = FALSE)
